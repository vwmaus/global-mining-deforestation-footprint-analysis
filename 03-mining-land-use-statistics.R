library(stringr)
library(dplyr)
library(sf)
library(progress)
library(tibble)
library(readr)
library(tidyr)
library(ggplot2)
library(scales)
library(foreach)

# ------------------------------------------------------------------------------
# set input data version 
data_version <- "20220203"
path_mining_area <- str_c("./data/global_mining_and_quarry_",data_version,".gpkg") 
path_mining_cluster <- str_c("./data/hcluster_concordance_",data_version,".csv")
path_forest_loss <- "./data/global_mining_and_quarry_forest_loss.csv"

# ------------------------------------------------------------------------------
# read data and convert to ha
forest_loss <- read_csv(path_forest_loss) |> 
  mutate(across(matches("area_forest_loss", list(function(x) x*100))))

mining_cluster <- read_csv(path_mining_cluster) |> 
  mutate(area = area * 100)

# ------------------------------------------------------------------------------
# country mining area statistics
threshold <- 0.98
country_rank <- mining_cluster |> 
  group_by(isoa3) |> 
  summarise(area = sum(area), .groups = "drop") |> 
  arrange(desc(area)) |> 
  mutate(cum_area = cumsum(area / sum(area)), rank = row_number()) |> 
  mutate(isoa3 = ifelse(cum_area < threshold, as.character(isoa3), "ROW")) |> 
  group_by(isoa3) |> 
  summarise(area = sum(area), cum_area = tail(cum_area, 1), rank = rank[1]) |> 
  arrange(rank) |> 
  select(isoa3, rank)
  
gp <- mining_cluster |> 
  group_by(isoa3, type_of_commodities) |> 
  summarise(area = sum(area), .groups = "drop") |> 
  mutate(isoa3 = ifelse(isoa3 %in% country_rank$isoa3, isoa3, "ROW")) |> 
  group_by(isoa3, type_of_commodities) |> 
  summarise(area = sum(area), .groups = "drop") |> 
  left_join(country_rank) |> 
  arrange(rank) |> 
  mutate(isoa3 = factor(isoa3, levels = c(unique(isoa3)[unique(isoa3) != "ROW"], "ROW"))) |> 
  rename(`Area (M ha)` = area, Country = isoa3, `Commodity type` = type_of_commodities) |> 
  ggplot(aes(x = Country, y = `Area (M ha)`, fill = `Commodity type`)) + 
  scale_fill_brewer(palette = "Spectral") + 
  geom_bar(stat="identity") + 
  theme_linedraw() + 
  theme(legend.position = c(0.94, 0.78),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,family = "sans", size = 11),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = label_number(scale = 1e-6, accuracy = 0.1))

ggsave(filename = "./output/country_mining_area.pdf", plot = gp, bg = "#ffffff",
       width = 360, height = 100, units = "mm", scale = 1)


# ------------------------------------------------------------------------------
# country forest loss area statistics
threshold <- 0.997

country_rank <- select(forest_loss, isoa3, area = area_forest_loss_000) |> 
  group_by(isoa3) |> 
  summarise(area = sum(area, na.rm = TRUE), .groups = "drop") |> 
  arrange(desc(area)) |> 
  mutate(cum_area = cumsum(area / sum(area, na.rm = TRUE)), rank = row_number()) |> 
  mutate(isoa3 = ifelse(cum_area < threshold, as.character(isoa3), "ROW")) |> 
  group_by(isoa3) |> 
  summarise(area = sum(area, na.rm = TRUE), cum_area = tail(cum_area, 1), rank = rank[1]) |> 
  arrange(rank) |> 
  select(isoa3, rank)

gp <- select(forest_loss, id, area = area_forest_loss_000) |> 
  mutate(id = str_c("A", id)) |> 
  left_join(select(mining_cluster, -area)) |> 
  group_by(isoa3, type_of_commodities) |> 
  summarise(area = sum(area, na.rm = TRUE), .groups = "drop") |> 
  mutate(isoa3 = ifelse(isoa3 %in% country_rank$isoa3, isoa3, "ROW")) |> 
  group_by(isoa3, type_of_commodities) |> 
  summarise(area = sum(area, na.rm = TRUE), .groups = "drop") |> 
  left_join(country_rank) |> 
  arrange(rank) |> 
  mutate(isoa3 = factor(isoa3, levels = c(unique(isoa3)[unique(isoa3) != "ROW"], "ROW"))) |> 
  rename(`Area (K ha)` = area, Country = isoa3, `Commodity type` = type_of_commodities) |> 
  ggplot(aes(x = Country, y = `Area (K ha)`, fill = `Commodity type`)) + 
  scale_fill_brewer(palette = "Spectral") + 
  geom_bar(stat="identity") + 
  theme_linedraw() + 
  theme(legend.position = c(0.94, 0.78),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,family = "sans", size = 11),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = label_number(scale = 1e-3, accuracy = 0.1))

ggsave(filename = "./output/country_mining_forest_loss_area.pdf", plot = gp, bg = "#ffffff",
       width = 360, height = 100, units = "mm", scale = 1)

# ------------------------------------------------------------------------------
# forest loss time series
forest_loss_ts <- forest_loss |> 
  select(year, matches("area_forest_loss")) |> 
  group_by(year) |> 
  summarise(across(-matches("_p"), sum, na.rm = TRUE)) |> 
  filter(year > 2000, year < 2020) |> 
  pivot_longer(cols = c(-year, -area_forest_loss_000)) |> 
  mutate(`Initial forest cover` = str_remove(name, "area_forest_loss_") |> as.numeric() |> str_c(" %"),
         Year = year,
         `Area (K ha)` = as.numeric(value) * 100) 

gp <- forest_loss_ts |> 
  ggplot(aes(x = Year, y = `Area (K ha)`, fill = `Initial forest cover`)) + 
  scale_fill_brewer(palette = "Spectral") + 
  geom_bar(stat="identity") + 
  theme_linedraw() + 
  theme(legend.position = c(0.2, 0.82),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = label_number(scale = 1e-3, accuracy = 1))

ggsave(filename = "./output/forest_loss_area_time_series.pdf", plot = gp, bg = "#ffffff",
       width = 120, height = 120, units = "mm", scale = 1)








