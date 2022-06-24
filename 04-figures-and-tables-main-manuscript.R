# Generate figures and tables in the main paper

library(stringr)
library(dplyr)
library(sf)
library(progress)
library(tibble)
library(readr)
library(tidyr)
library(stringr)
library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(scales)
library(viridis)
library(ggpubr)
library(stars)
library(rworldmap)
library(cowplot)
library(xtable)
library(janitor)

source("R/00_plot_goode_homolosine_world_map.R")

# ------------------------------------------------------------------------------
# set input data version 
data_version <- "20220203"
path_mining_area <- str_c("./data/global_mining_and_quarry_",data_version,".gpkg") 
path_mining_cluster <- str_c("./data/hcluster_concordance_",data_version,".csv")
path_forest_loss <- "./data/global_mining_and_quarry_forest_loss.csv"
path_to_mining_polygons <- "./data/global_mining_and_quarry_20220203.gpkg"

# --------------------------------------------------------------------------------------
# define ggplot theme ------------------------------------------------------------------
textwidth <- 345 # Get from latex comand: \the\textwidth in pt -- divide by 2.835 to mm
textheight <- 550 # Get from latex comand: \the\textheight in pt -- divide by 2.835 to mm
font_size <- 14 # font size in pt 
pt_to_mm <- 2.835
font_family <- "sans"
th <- ggplot2::theme(axis.text = ggplot2::element_text(size = font_size, family = font_family), 
                     text = ggplot2::element_text(size = font_size, family = font_family)) 

make_grid_50x50 <- function(data){
  
  mine_features_points <- data |> 
    transmute(fl = ifelse(is.na(area_forest_loss_000), 0, area_forest_loss_000)) |> 
    mutate(geom = st_centroid(geom))
  
  mine_features_points_goode <- 
    st_transform(mine_features_points, crs = "+proj=igh +ellps=WGS84 +units=m +no_defs")
  
  grid_50 <- st_make_grid(mine_features_points_goode, cellsize = 50000) |> 
    st_as_sf() |> 
    st_filter(mine_features_points_goode)
  
  grid_50_forest_loss <- mine_features_points_goode |> 
    aggregate(grid_50, sum, na.rm = TRUE) |> 
    filter(fl > 0)
  
  return(grid_50_forest_loss)
  
}

# --------------------------------------------------------------------------------------
# fig1 - plot global tree cover loss 50x50 grid cells ----------------------------------
mine_features <- st_read(path_to_mining_polygons) |> 
  select(id)

forest_loss_groups <- read_csv(path_forest_loss) |> 
  filter(!is.na(year)) |> 
  filter(year > 2000, year < 2020) |> 
  group_by(id, isoa3, country, ecoregion, biome) |> 
  summarise(across(matches('area_forest_loss'), sum)) 

mining_cluster <- read_csv(path_mining_cluster) |> 
  select(id, id_hcluster, area, list_of_commodities, type_of_commodities) |> 
  rename(area_mine = area) |> 
  mutate(id = str_remove_all(id, 'A'))

mine_features_areas <- mine_features |> 
  left_join(mining_cluster) |> 
  left_join(forest_loss_groups)

# data check 
mine_features_areas |> 
  st_drop_geometry() |> 
  mutate(Unknown = type_of_commodities == "Unknown") |> 
  group_by(Unknown) |> 
  summarise(area = sum(area_mine)) |> 
  mutate(perc = area / sum(area))

mine_features_areas |> 
  st_drop_geometry() |> 
  mutate(Unknown = type_of_commodities == "Unknown") |> 
  group_by(Unknown) |> 
  summarise(area = sum(area_forest_loss_000, na.rm = TRUE)) |> 
  mutate(perc = area / sum(area))

grid_50_forest_loss <- make_grid_50x50(mine_features_areas)

# check total forest loss
sum(grid_50_forest_loss$fl)

W_gp <- plot_goode_homolosine_world_map(ocean_color = "#e5f1f8", land_color = "gray95", family = font_family,
                                        grid_color = "grey60", grid_size = 0.1,
                                        country_borders_color = "grey60", country_borders_size = 0.1) +
  ggplot2::geom_sf(data = grid_50_forest_loss, mapping = aes(fill = fl * 100), color = NA, lwd = 0, size = 0) + # * 100 from km2 to ha
  ggplot2::coord_sf(crs = "+proj=igh", expand = FALSE) +
  viridis::scale_fill_viridis(option = "turbo", begin = 0, end = 1, direction = 1, 
                              discrete = FALSE, 
                              trans = log10_trans(),
                              breaks = c(0.01, 2.5, 400, 60000),
                              labels = function(x) sprintf("%g", x)
                              ) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.box.spacing = unit(0.0, "cm"),
    legend.key.size = unit(0.3, "cm"),
    legend.key.width = unit(textheight/15, "mm"),
    plot.margin = margin(t = -1, r = -1, b = 0, l = -1, unit = "cm")
  ) +
  labs(fill = bquote(Area~(ha))) + 
  th

ggplot2::ggsave(plot = W_gp, bg = "#ffffff",
                filename = "output/fig-1-global-map.png",
                width = textwidth, height = 170, units = "mm", scale = 1)


# --------------------------------------------------------------------------------------
# tab-s1 to s3 plot selected countries bar plot ----------------------------------------
forest_loss <- read_csv(path_forest_loss) |> 
  filter(!is.na(year)) |> 
  filter(year > 2000, year < 2020)

mining_cluster <- mining_cluster <- read_csv(path_mining_cluster) |> 
  select(id, id_hcluster, area, list_of_commodities, type_of_commodities) |> 
  rename(area_mine = area) |> 
  mutate(id = str_remove_all(id, 'A'))

# commodity table 
tmp_table <- left_join(forest_loss, mining_cluster) |> 
  select(`Commodity cluster` = type_of_commodities,
         `Mining area` = area_mine, 
         `<25%` = area_forest_loss_025,
         `26-50%` = area_forest_loss_050, 
         `51-75%` = area_forest_loss_075,
         `76-100%` = area_forest_loss_100, 
         `Total loss` = area_forest_loss_000) |> 
  group_by(`Commodity cluster`) |> 
  summarise(across(everything(), ~sum(.x, na.rm = TRUE)*100)) |> # to ha
  arrange(desc(`Total loss`)) |> 
  adorn_totals("row") |> 
  xtable(digits = 0, caption = "Mining area and forest loss area from 2000 to 2019 per commodity cluster in hectares.", label = "tab:s1-commodities") 

xtable::print.xtable(tmp_table, include.rownames = FALSE, caption.placement = "top", booktabs = TRUE, hline.after = c(0, nrow(tmp_table)-1, nrow(tmp_table)),
                     add.to.row = list(pos = list(-1), command = c("\\hline\n&&\\multicolumn{4}{c}{Forest loss within each initial tree cover share}&\\\\ \n\\cmidrule(lr){3-6}\n")),
                     size="\\fontsize{10pt}{11pt}\\selectfont")

# biome table 
tmp_table <- left_join(forest_loss, mining_cluster) |> 
  select(`Biome` = biome,
         `Mining area` = area_mine, 
         `<25%` = area_forest_loss_025,
         `26-50%` = area_forest_loss_050, 
         `51-75%` = area_forest_loss_075,
         `76-100%` = area_forest_loss_100, 
         `Total loss` = area_forest_loss_000) |> 
  group_by(`Biome`) |> 
  summarise(across(everything(), ~sum(.x, na.rm = TRUE)*100)) |> # to ha
  arrange(desc(`Total loss`)) |> 
  adorn_totals("row") |> 
  xtable(digits = 0, caption = "Mining area and forest loss area from 2000 to 2019 per biome in hectares.", label = "tab:s1-biome") 

xtable::print.xtable(tmp_table, include.rownames = FALSE, caption.placement = "top", booktabs = TRUE, hline.after = c(0, nrow(tmp_table)-1, nrow(tmp_table)),
                     add.to.row = list(pos = list(-1), command = c("\\hline\n&&\\multicolumn{4}{c}{Forest loss within each initial tree cover share}&\\\\ \n\\cmidrule(lr){3-6}\n")),
                     size="\\fontsize{10pt}{11pt}\\selectfont")


# biome table 
tmp_table <- left_join(forest_loss, mining_cluster) |> 
  select(`Country` = country,
         `Mining area` = area_mine, 
         `<25%` = area_forest_loss_025,
         `26-50%` = area_forest_loss_050, 
         `51-75%` = area_forest_loss_075,
         `76-100%` = area_forest_loss_100, 
         `Total loss` = area_forest_loss_000) |> 
  group_by(`Country`) |> 
  summarise(across(everything(), ~sum(.x, na.rm = TRUE)*100)) |> # to ha
  arrange(desc(`Total loss`)) |> 
  adorn_totals("row") |> 
  xtable(digits = 0, caption = "Mining area and forest loss area from 2000 to 2019 per country in hectares.", label = "tab:s1-country") 

xtable::print.xtable(tmp_table, include.rownames = FALSE, caption.placement = "top", booktabs = TRUE, hline.after = c(0, nrow(tmp_table)-1, nrow(tmp_table)),
                     add.to.row = list(pos = list(-1), command = c("\\hline\n&&\\multicolumn{4}{c}{Forest loss within each initial tree cover share}&\\\\ \n\\cmidrule(lr){3-6}\n")),
                     size="\\fontsize{10pt}{11pt}\\selectfont", tabular.environment = "longtable")

# --------------------------------------------------------------------------------------
# fig-3 plot selected countries bar plot -----------------------------------------------


fract_forest_cover <- tibble::tibble(
  `Initial tree cover (%)` = factor(c("(0, 25]", "(25, 50]", "(50, 75]", "(75, 100]"), 
                                    levels = c("(0, 25]", "(25, 50]", "(50, 75]", "(75, 100]")),
  name = c("area_forest_loss_025", 
           "area_forest_loss_050", 
           "area_forest_loss_075", 
           "area_forest_loss_100"))

country_tbl <- tibble(
  isoa3 = c("IDN", "BRA", "RUS", "CAN", "USA", "AUS"),
  country = factor(c("Indonesia", "Brazil", "Russia", "Canada", "United States of America", "Australia"),
                   levels = c("Indonesia", "Brazil", "Russia", "Canada", "United States of America", "Australia")))

forest_loss_ts <- forest_loss |> 
  filter(isoa3 %in% country_tbl$isoa3) |> 
  select(isoa3, year, area_forest_loss_000, area_forest_loss_025, area_forest_loss_050, 
         area_forest_loss_075, area_forest_loss_100) |> 
  group_by(isoa3, year) |> 
  summarise(across(everything(), sum, na.rm = TRUE), .groups = 'drop') |> 
  filter(year > 2000, year < 2020) |> 
  pivot_longer(cols = c(-year, -area_forest_loss_000, -isoa3)) |> 
  left_join(fract_forest_cover) |> 
  left_join(country_tbl) |> 
  mutate(Year = year, area = value * 100) # convert to ha 

trend_bar <- forest_loss_ts |> 
  select(country, year, area_forest_loss_000) |> 
  distinct()

gp <- forest_loss_ts |> 
  ggplot(aes(x = Year, y = area, fill = `Initial tree cover (%)`)) + 
  facet_wrap(~country) + 
  geom_bar(stat="identity", width = 0.5) + 
  theme_linedraw() + 
  theme(axis.text = ggplot2::element_text(size = font_size), 
        text = ggplot2::element_text(size = font_size),
        legend.text = element_text(size = font_size),
        legend.title = element_text(size = font_size),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box.spacing = unit(0.0, "cm"),
        legend.key.size = unit(0.3, "cm")) + 
  scale_fill_grey(start = .7, end = 0, guide = guide_legend(direction = "horizontal", title.position = "top")) +
  scale_y_continuous(labels = label_number(scale = 1e-3, accuracy = 1)) + 
  scale_x_continuous(labels = seq(2000, 2015, 5), breaks = seq(2000, 2015, 5)) + 
  ylab("Area (K ha)") 

ggsave(filename = str_c("./output/barplot_top_six.png"), plot = gp, bg = "#ffffff",
       width = 345, height = 180, units = "mm", scale = 1)
