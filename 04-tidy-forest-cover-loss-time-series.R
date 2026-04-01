# BEFORE RUNNING THIS SCRIPT
#   1. UPLOAD global_mining_and_quarry_20220203.gpkg to Google Earth Engine GEE platform
#   2. Run the script 01-gee-calculate-tree-cover-loss.js on GEE platform 
#   3. Download all files from Google drive folder "GEE" to the "./data/mining-tree-cover-loss-<version>"
#   4. The script below will tidy the tree cover loss time series coming from GEE and add commodities
# 
# The datasets generated in this script are published in Zenodo https://doi.org/10.5281/zenodo.7299103

library(dplyr)
library(tidyr)
library(stringr)
library(progress)
library(readr)
library(purrr)   # Added for memory-efficient list reduction

# Replace to process a different version
gee_version_all <- "20260326"

year_start <- 2000
year_end <- 2023

# ------------------------------------------------------------------------------
# tidy forest loss files for complete set of polygons
forest_loss_path <- 
  dir(str_c("./data/mining-tree-cover-loss-",gee_version_all), 
      pattern = "tree_cover_loss_mines_", full.names = TRUE) 

# This pattern will successfully capture both standard and PLANTED files
forest_loss_path <- forest_loss_path[str_detect(forest_loss_path, "_000_|_025_")]

treatments <- c("ALL", "HMA", "NMF", "NPA", "PLS", "PRT", "SNR", "SUN", "WLA")

# Initialize progress bar
pb <- progress_bar$new(
  format = "  Processing files [:bar] :percent in :elapsed (ETA: :eta)",
  total = length(forest_loss_path),
  clear = FALSE,
  width = 80,
  force = TRUE # Force progress bar to show in non-interactive sessions like R CMD BATCH
)

# Pre-allocate list for memory efficiency instead of using foreach .combine.
# Iteratively joining dataframes inside a loop causes heavy memory reallocation.
list_out <- vector("list", length(forest_loss_path))

for (i in seq_along(forest_loss_path)) {
  f <- forest_loss_path[i]
  
  # Use the progress bar in an interactive session, 
  # but use log-friendly lines & buffer flushing for R CMD BATCH
  if (interactive()) {
    pb$tick()
  } else {
    cat(sprintf("Processing file %d / %d: %s\n", i, length(forest_loss_path), basename(f)))
    flush.console() # Forces R to write to the .Rout file immediately so tail -f sees it
  }

  tree_cover <- str_remove_all(basename(f), "tree_cover_loss_mines_") |>
    str_remove_all(str_c("_", gee_version_all, ".csv"))

  mines_gee_raw <- read_csv(f, show_col_types = FALSE)
  
  # Handle completely empty files (e.g., highly filtered PLANTED files)
  if (nrow(mines_gee_raw) == 0) {
    empty_df <- tibble(id = character(), year = numeric())
    empty_df[[paste0("area_tree_cover_", tree_cover)]] <- numeric()
    empty_df[[paste0("area_forest_loss_", tree_cover)]] <- numeric()
    list_out[[i]] <- empty_df
    next # Skip the rest of the processing for this file
  }

  mines_gee <- mines_gee_raw |>
    select(id, tree_cover_2000 = treecover2000, all_of(ends_with("_loss"))) |> 
    pivot_longer(cols = -c(id, tree_cover_2000), names_to = "year", values_to = "tree_loss") |>
    mutate(
      year = as.numeric(str_remove_all(year, "_loss")) + 2000, 
      across(starts_with("tree_"), ~ as.numeric(.x) * 1e-4) # Forced as.numeric() to handle empty/character columns
    ) |> # from m2 to ha
    complete(id, year = full_seq(c(2000, max(year)), 1), fill = list(tree_loss = 0)) |> 
    arrange(id, year) |> 
    group_by(id) |>
    mutate(
      tree_cover_2000 = unique(na.omit(tree_cover_2000)),
      tree_cover_2000 = tree_cover_2000 - cumsum(tree_loss)
    ) |>
    rename(
      !!paste0("area_tree_cover_", tree_cover) := tree_cover_2000,
      !!paste0("area_forest_loss_", tree_cover) := tree_loss
    ) |>
    mutate(id = str_pad(as.character(id), 7, "0", side = 'left')) |> # correct ids when not char
    ungroup() |>
    filter(year_start <= year, year <= year_end)

  list_out[[i]] <- mines_gee
}

cat("\nCombining all processed files...\n")

# Efficiently combine the list of data frames at once
out <- reduce(list_out, full_join, by = c("id", "year"))

cat("Cleaning up and calculating treatment differences...\n")

# Replace NAs with 0 (since filtered-out rows from PLANTED files mean 0 planted area and 0 loss)
out <- out |>
  mutate(across(starts_with("area_"), ~replace_na(., 0)))

# Use mutate and across to compute the differences for all treatments and PLANTED dynamically
out <- out |>
  mutate(across(
    .cols = matches("area_tree_cover_.*000_"), 
    .names = "{str_replace(.col, '000_', '')}",
    .fns = ~ . - get(str_replace(cur_column(), "000_", "025_"))
  )) |>
  mutate(across(
    .cols = matches("area_forest_loss_.*000_"), 
    .names = "{str_replace(.col, '000_', '')}",
    .fns = ~ . - get(str_replace(cur_column(), "000_", "025_"))
  )) |>
  select(-matches("000|025|050|075|100"))

# summary loss
# reframe(out, across(starts_with("area_forest_loss"), ~sum(., na.rm = TRUE)))

# initial forest cover
# filter(out, year == year_start) |>
#  reframe(across(starts_with("area_tree_cover"), ~sum(., na.rm = TRUE)))

# final forest cover
# filter(out, year == year_end) |>
#  reframe(across(starts_with("area_tree_cover"), ~sum(., na.rm = TRUE)))

cat("Writing output...\n")
write_csv(out, str_c("./output/global_mining_forest_loss_",year_start,"-", year_end,".csv"))
cat("Done!\n")
