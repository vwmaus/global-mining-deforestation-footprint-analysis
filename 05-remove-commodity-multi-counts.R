library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(sf)
library(purrr)

# ------------------------------------------------------------------------------
# SETUP PARAMETERS
# ------------------------------------------------------------------------------
gee_version_all <- "20260326"
year_start <- 2000
year_end <- 2023

# ------------------------------------------------------------------------------
# PART 1: LOAD POLYGON COMMODITY INFO
# ------------------------------------------------------------------------------
cat("Loading polygon commodity data...\n")
# Note: Ensure this path matches your actual data structure
mine_polygons_path <- "./data/20260325-all_materials/mine_polygons.gpkg"

if(!file.exists(mine_polygons_path)) {
  stop(paste("File not found:", mine_polygons_path))
}

mine_polygons <- st_read(mine_polygons_path) |>
  st_drop_geometry() |>
  as_tibble() |>
  mutate(
    # Use id_cluster or id depending on your GEE output mapping
    primary_materials_list = ifelse(is.na(primary_materials_list), "Unknown", primary_materials_list),
    materials_list = ifelse(is.na(materials_list), "Unknown", materials_list),
    # For allocation, we often use the primary materials list
    list_of_commodities = primary_materials_list
  ) 

# FIX: Check if isoa3 or country_isoa3 exists and select accordingly
if("isoa3" %in% names(mine_polygons)) {
  mine_polygons <- mine_polygons |> select(id, list_of_commodities, isoa3)
} else if ("country_isoa3" %in% names(mine_polygons)) {
  mine_polygons <- mine_polygons |> select(id, list_of_commodities, isoa3 = country_isoa3)
} else {
  warning("Neither 'isoa3' nor 'country_isoa3' found in geopackage. Assigning 'Unknown'.")
  mine_polygons <- mine_polygons |> 
    mutate(isoa3 = "Unknown") |> 
    select(id, list_of_commodities, isoa3)
}

# ------------------------------------------------------------------------------
# PART 2: LOAD FOREST LOSS DATA AND JOIN
# ------------------------------------------------------------------------------
cat("Loading forest loss data and joining with commodities...\n")
# Path to the output of script 04
input_file <- "./output/global_mining_forest_loss_2000-2023.csv"

if(!file.exists(input_file)) {
  stop(paste("Input file not found:", input_file, ". Ensure script 04 ran successfully."))
}

mines_gee_all <- read_csv(input_file, show_col_types = FALSE) |>
  # Join with polygon metadata to get commodities and country codes
  left_join(mine_polygons, by = "id") |>
  mutate(
    list_of_commodities = str_replace_all(list_of_commodities, "Alumina|Aluminum", "Bauxite"),
    list_of_commodities = ifelse(is.na(list_of_commodities), "Unknown", list_of_commodities),
    number_of_commodities = str_count(list_of_commodities, ",") + 1
  ) |>
  relocate(list_of_commodities, number_of_commodities, isoa3, .after = year)

# ------------------------------------------------------------------------------
# PART 3: CALCULATE ADJUSTED SHARES
# ------------------------------------------------------------------------------
cat("Calculating adjusted shares and totals...\n")

mines_gee_all <- mines_gee_all |>
    group_by(isoa3, year) |>
    # Calculate total mining loss per country/year (sum of all polygons)
    mutate(across(starts_with("area_forest_loss"), ~ sum(.x, na.rm = TRUE), .names = "{.col}.total_country")) |>
    group_by(year) |>
    # Calculate global total mining loss per year
    mutate(across(starts_with("area_forest_loss") & !contains(".total"), ~ sum(.x, na.rm = TRUE), .names = "{.col}.total_global")) |>
    ungroup() |>
    # Simple share: divide polygon loss by number of commodities it produces
    mutate(across(starts_with("area_forest_loss") & !contains(".total"), ~ .x / number_of_commodities, .names = "{.col}.adj_share"))

# ------------------------------------------------------------------------------
# PART 4: COMMODITY-LEVEL AGGREGATION
# ------------------------------------------------------------------------------
cat("Expanding by commodity and re-scaling...\n")

# Get list of unique commodities
unique_commodities <- mines_gee_all |>
  pull(list_of_commodities) |>
  str_split(pattern = ",\\s*") |>
  unlist() |>
  unique() |>
  sort() 

# Expand the dataset so one row per commodity per polygon
mines_comm <- lapply(unique_commodities, function(m) {
  mines_gee_all |>
    filter(str_detect(list_of_commodities, str_c("(^|,)", fixed(m), "(,|$)"))) |>
    mutate(material_name = m) |>
    group_by(isoa3, material_name, year) |>
    # Aggregating all polygons for this commodity in this country/year
    reframe(
      n_polygons = n_distinct(id),
      across(starts_with("area_forest_loss") & !contains(".total"), sum, na.rm = TRUE), 
      across(contains(".total"), first)
    )
}) |>
  bind_rows()

# Re-scale commodity values so sum(commodities) == total_country_loss
mines_comm <- mines_comm |>
  group_by(isoa3, year) |>
  mutate(
    across(
      starts_with("area_forest_loss_") & !contains(".total") & !contains(".adj"),
      ~ {
        suffix <- sub("area_forest_loss_", "", cur_column())
        total_col <- paste0("area_forest_loss_", suffix, ".total_country")
        current_sum <- sum(.x, na.rm = TRUE)
        if (current_sum == 0) return(0)
        (.x / current_sum) * get(total_col)
      },
      .names = "{.col}.adj_country"
    )) |>
  group_by(year) |>
  # Global re-scaling (optional depending on use case, usually country-scale is enough)
  mutate(
    across(
      starts_with("area_forest_loss_") & !contains(".total") & !contains(".adj") & !contains(".adj_country"),
      ~ {
        suffix <- sub("area_forest_loss_", "", cur_column())
        total_col <- paste0("area_forest_loss_", suffix, ".total_global")
        current_sum <- sum(.x, na.rm = TRUE)
        if (current_sum == 0) return(0)
        (.x / current_sum) * get(total_col)
      },
      .names = "{.col}.adj_global"
    )) |>
  ungroup()

# ------------------------------------------------------------------------------
# PART 5: EXPORT
# ------------------------------------------------------------------------------
cat("Writing output...\n")
output_path <- str_c("./output/global_commodity_forest_loss_", gee_version_all, ".csv")
write_csv(mines_comm, output_path)

cat("\n--- Summary Verification ---\n")
# Check if global totals match
original_total <- sum(mines_gee_all$area_forest_loss_ALL, na.rm = TRUE)
adj_total <- sum(mines_comm$area_forest_loss_ALL.adj_country, na.rm = TRUE)

cat(sprintf("Original Global Loss: %f\n", original_total))
cat(sprintf("Adjusted Global Loss: %f\n", adj_total))
cat(sprintf("Difference: %f\n", original_total - adj_total))

cat("Done!\n")
