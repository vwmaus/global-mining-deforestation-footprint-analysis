library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(sf)

gee_version_all <- "20240829a" # Maus et al. + OSM

# ------------------------------------------------------------------------------
# create commodities forest loss talbe
#mines_gee_all <- readr::read_csv(str_c("./output/global_mining_and_quarry_forest_loss_",gee_version_all,".csv"))

# Step 1: Load only the column names
file_path <- str_c("./output/global_mining_and_quarry_forest_loss_", gee_version_all, ".csv")
column_names <- names(read_csv(file_path, n_max = 0))

# Step 2: Identify columns starting with "area_forest_loss_" and any other needed columns
needed_columns <- column_names[starts_with("area_forest_loss_", vars = column_names)]
additional_columns <- c("id", "id_hcluster", "isoa3", "country", "year", "list_of_commodities", "number_of_commodities")  # Add any other columns you need

# Combine the columns
columns_to_load <- c(additional_columns, needed_columns)

# Step 3: Load only the selected columns
mines_gee_all <- read_csv(file_path, col_types = cols_only(!!!setNames(rep("?", length(columns_to_load)), columns_to_load))) |>
    #filter(isoa3 %in% c("BRA")) |>
    #transmute(id, id_hcluster, list_of_commodities, number_of_commodities, isoa3, country, year, area_forest_loss_000 = area_forest_loss_000) |>
    mutate(list_of_commodities = str_replace_all(list_of_commodities, "Alumina|Aluminum", "Bauxite")) |>
    mutate(list_of_commodities = ifelse(is.na((list_of_commodities)), "Unknown", list_of_commodities)) |>
    group_by(id_hcluster) |>
    mutate(list_of_commodities = list_of_commodities |> 
           str_split(pattern = ",") |>                # Split each string into a list of items
           unlist() |>                                # Flatten the list
           unique() |>                                # Remove duplicates
           sort() |>                                  # Sort alphabetically (optional)
           paste(collapse = ",")) |>                  # Recombine into a comma-separated string
    ungroup() |>
    mutate(number_of_commodities = str_count(list_of_commodities, ",") + 1) |>
    relocate(number_of_commodities, .after = list_of_commodities) |>
    group_by(isoa3, country, year) |>
    mutate(across(starts_with("area_forest_loss"), ~ sum(.x), .names = "{.col}.total_country")) |>
    group_by(year) |>
    mutate(across(starts_with("area_forest_loss") & ! matches(".total"), ~ sum(.x), .names = "{.col}.total_global")) |>
    ungroup() |>
    mutate(across(starts_with("area_forest_loss") & ! matches(".total"), ~ .x / number_of_commodities, .names = "{.col}.adj_share"))

gc()

# Get list of commodities
unique_commodities <- mines_gee_all |>
  pull(list_of_commodities) |>
  str_split(pattern = ",") |>
  unlist() |>
  unique() |>
  sort() 

# mines_gee_all |>
#   reframe(across(starts_with("area_forest_loss") & ! matches(".adj_share"), sum),
#           across(ends_with("adj_share"), ~ sum(.x * number_of_commodities))) |>
#   pivot_longer(all_of(starts_with("area")))         

#### Calculate deforestation per country and commodity
def_country_comm <- lapply(unique_commodities, function(m) {
  filter(mines_gee_all, str_detect(list_of_commodities, str_c("(^|,)", fixed(m), "(,|$)"))) |>
    mutate(material_name = m) |>
    group_by(isoa3, country, material_name, year) |>
    reframe(id=paste(id, collapse = ","), across(starts_with("area_forest_loss") & !matches(".total"), sum), across(matches(".total"), unique))
}) |>
  bind_rows() |>
  group_by(isoa3, country, year) |>
  mutate(
    across(
      starts_with("area_forest_loss_") & !matches(".total|.adj"),
      ~ {
        total_column_name <- paste0("area_forest_loss_", sub("area_forest_loss_", "", cur_column()), ".total_country")
        ifelse(.x == 0, 0, (.x / sum(.x)) * get(total_column_name))
      },
      .names = "{.col}.adj_country"
    )) |>
    group_by(year) |>
  mutate(
    across(
      starts_with("area_forest_loss_") & !matches(".total|.adj"),
      ~ {
        total_column_name <- paste0("area_forest_loss_", sub("area_forest_loss_", "", cur_column()), ".total_global")
        ifelse(.x == 0, 0, (.x / sum(.x)) * get(total_column_name))
      },
      .names = "{.col}.adj_global"
    )) |>
    ungroup()

# Check if total adjusted area equal/approximate the country mining area
#def_country_comm <- read_csv(str_c("./output/global_commodity_forest_loss_", gee_version_all, ".csv"))

def_country_comm |>
  ungroup() |>
  reframe(across(starts_with("area_forest_loss_000.") & !matches("total"), sum)) |>
  pivot_longer(all_of(starts_with("area")))

def_country_comm |>
  group_by(isoa3, country) |>
  reframe(area_forest_loss_000.total_country = sum(unique(area_forest_loss_000.total_country))) |>
  reframe(area_forest_loss_000.total_country = sum(area_forest_loss_000.total_country))

def_country_comm |>
  group_by(year) |>
  reframe(area_forest_loss_000.total_global = sum(unique(area_forest_loss_000.total_global))) |>
  reframe(area_forest_loss_000.total_global = sum(area_forest_loss_000.total_global))

write_csv(def_country_comm, str_c("./output/global_commodity_forest_loss_", gee_version_all, ".csv"))

# Check commodities
check_coal_gold <- def_country_comm |>
  ungroup() |>
  transmute(material_name, year, 
         area_multiple_counts = area_forest_loss_000 - area_forest_loss_025,
         area_adj_share = area_forest_loss_000.adj_share - area_forest_loss_025.adj_share,
         area_adj_country = area_forest_loss_000.adj_country - area_forest_loss_025.adj_country,
         area_adj_global = area_forest_loss_000.adj_global - area_forest_loss_025.adj_global,
         ) |>
  group_by(material_name) |>
  reframe(across(all_of(starts_with("area_")), sum)) |>
  filter(material_name %in% c("Coal", "Gold"))

# These results confirmed the hypothesis on the effect of allocation and rescaling
check_coal_gold
reframe(check_coal_gold, across(all_of(starts_with("area_")), ~ diff(.x)/sum(.x) ))

