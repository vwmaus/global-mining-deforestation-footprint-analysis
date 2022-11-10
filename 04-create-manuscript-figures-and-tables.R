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


# ------------------------------------------------------------------------------
# join datasets
path_forest_loss <- str_c("./output/global_mining_and_quarry_forest_loss_",data_version,".csv")
path_to_mining_polygons <- str_c("./output/global_mining_and_quarry_",data_version,".gpkg")

mine_features <- st_read(path_to_mining_polygons) |> 
  select(id)

forest_loss <- read_csv(path_forest_loss) |> 
  filter(!is.na(year)) |> 
  filter(year > 2000, year < 2020) |> 
  mutate(list_of_commodities = str_replace_all(list_of_commodities, "Alumina", "Aluminum"),
         list_of_commodities = str_replace_all(list_of_commodities, "Bauxite", "Aluminum"),
         list_of_commodities = str_replace_all(list_of_commodities, "Zinc-Lead", "Zinc"),
         list_of_commodities = str_replace_all(list_of_commodities, "Heavy Rare Earths and Yttrium", "Unknown REE"),
         list_of_commodities = str_replace_all(list_of_commodities, "Rare Earth Elements", "Unknown REE"))

forest_loss_accum <- forest_loss  |> 
  group_by(id, list_of_commodities, isoa3, country, ecoregion, biome) |> 
  summarise(across(matches('area_forest_loss'), sum)) 

mine_features_areas <- mine_features |> 
  left_join(forest_loss_accum)

# data check 
mine_features_areas |> 
  st_drop_geometry() |> 
  mutate(Unknown = is.na(list_of_commodities)) |> 
  group_by(Unknown) |> 
  summarise(area = sum(area_forest_loss_000, na.rm = TRUE)) |> 
  mutate(perc = area / sum(area))

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
# fig-2 plot selected countries bar plot -----------------------------------------------

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

ggsave(filename = str_c("./output/fig-2-barplot-top-six-countries.png"), plot = gp, bg = "#ffffff",
       width = 345, height = 180, units = "mm", scale = 1)



# --------------------------------------------------------------------------------------
# fig-3 plot commodities bar plot ------------------------------------------------------
gp <- str_c(na.omit(forest_loss$list_of_commodities), collapse = ",") |> 
  str_split(",") |> 
  unlist() |> 
  unique() |> 
  sort() |> 
  lapply(function(i){
    forest_loss |> 
      filter(str_detect(list_of_commodities, i)) |> 
      transmute(Comodity = i,
                `Mining area` = area_mine, 
                `(0, 25]` = area_forest_loss_025,
                `(25, 50]` = area_forest_loss_050, 
                `(50, 75]` = area_forest_loss_075,
                `(75, 100]` = area_forest_loss_100, 
                `Total loss` = area_forest_loss_000) |> 
      group_by(Comodity) |> 
      summarise(across(everything(), ~sum(.x, na.rm = TRUE)*100))
  }) |> 
  bind_rows() |> 
  arrange(desc(`Total loss`)) |> 
  filter(`Total loss` >= 10000) |> # remove smaller than 10,000ha
  select(-`Mining area`, -`Total loss`) |> 
  mutate(Comodity = factor(Comodity, Comodity, Comodity)) |> 
  pivot_longer(cols = c(-Comodity), names_to = "Initial tree cover (%)", values_to = "Area") |> 
  ggplot(aes(x = Comodity, y = Area, fill = `Initial tree cover (%)`)) + 
  geom_bar(stat="identity", width = 0.5) 

gp <- gp +
  theme_linedraw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        axis.text = ggplot2::element_text(size = font_size), 
        text = ggplot2::element_text(size = font_size),
        legend.text = element_text(size = font_size),
        legend.title = element_text(size = font_size),
        legend.position = c(.85,.85),
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box.spacing = unit(0.0, "cm"),
        legend.key.size = unit(0.3, "cm")) + 
  scale_fill_grey(start = .7, end = 0, guide = guide_legend(direction = "horizontal", title.position = "top")) +
  scale_y_continuous(labels = label_number(scale = 1e-3, accuracy = 1)) + 
  ylab("Area (K ha)") + 
  xlab("")

ggsave(filename = str_c("./output/fig-3-barplot-commodities.png"), plot = gp, bg = "#ffffff",
       width = 420, height = 120, units = "mm", scale = 1)



# --------------------------------------------------------------------------------------
# supplementary tables -----------------------------------------------------------------

# biome table 
tmp_table <- select(forest_loss, `Biome` = biome,
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

xtable::print.xtable(tmp_table, table.placement = "!htpb", include.rownames = FALSE, caption.placement = "top", booktabs = TRUE, hline.after = c(0, nrow(tmp_table)-1, nrow(tmp_table)),
                     add.to.row = list(pos = list(-1), command = c("\\hline\n&&\\multicolumn{4}{c}{Forest loss within each initial tree cover share}&\\\\ \n\\cmidrule(lr){3-6}\n")),
                     size="\\fontsize{10pt}{11pt}\\selectfont", file = "./output/tab-s1-area-biome.tex")


# country table 
tmp_table <- select(forest_loss, `Country` = country,
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
  xtable(digits = 0, caption = "Mining area and forest loss area from 2000 to 2019 per country in hectares.", label = "tab:s2-country") 

xtable::print.xtable(tmp_table, table.placement = "!htpb", include.rownames = FALSE, caption.placement = "top", booktabs = TRUE, hline.after = c(0, nrow(tmp_table)-1, nrow(tmp_table)),
                     add.to.row = list(pos = list(-1), command = c("\\hline\n&&\\multicolumn{4}{c}{Forest loss within each initial tree cover share}&\\\\ \n\\cmidrule(lr){3-6}\n")),
                     size="\\fontsize{10pt}{11pt}\\selectfont", tabular.environment = "longtable", file = "./output/tab-s2-area-country.tex")


# commodity associated to forest cover loss
tmp_table <- str_c(na.omit(forest_loss$list_of_commodities), collapse = ",") |> 
  str_split(",") |> 
  unlist() |> 
  unique() |> 
  sort() |> 
  lapply(function(i){
    forest_loss |> 
      filter(str_detect(list_of_commodities, i)) |> 
      transmute(Comodity = i,
                `Mining area` = area_mine, 
                `<25%` = area_forest_loss_025,
                `26-50%` = area_forest_loss_050, 
                `51-75%` = area_forest_loss_075,
                `76-100%` = area_forest_loss_100, 
                `Total loss` = area_forest_loss_000) |> 
      group_by(Comodity) |> 
      summarise(across(everything(), ~sum(.x, na.rm = TRUE)*100))
  }) |> 
  bind_rows() |> 
  arrange(desc(`Total loss`)) |> 
  adorn_totals("row") |> 
  xtable(digits = 0, caption = "Mining area and forest loss area from 2000 to 2019 per commodity in hectares.", label = "tab:s3-commodities") 

xtable::print.xtable(tmp_table, table.placement = "!htpb", include.rownames = FALSE, caption.placement = "top", booktabs = TRUE, hline.after = c(0, nrow(tmp_table)-1, nrow(tmp_table)),
                     add.to.row = list(pos = list(-1), command = c("\\hline\n&&\\multicolumn{4}{c}{Forest loss within each initial tree cover share}&\\\\ \n\\cmidrule(lr){3-6}\n")),
                     size="\\fontsize{10pt}{11pt}\\selectfont", file = "./output/tab-s3-area-commodity.tex")


# protection level tree cover loss
# PROTECTED CLASSES 
# Ia (strict nature reserve)
# Ib (wilderness area)
# II (national park)
# III (natural monument or feature)
# IV (habitat/species management area)
# V (protected landscape/seascape)
# VI (PA with sustainable use of natural resources), 
# Not applicable, Not assigned, or Not reported -- these are aggregated andcalculated by differece

tmp_table <- select(forest_loss, `Country` = country,
                    Ia = area_forest_loss_000_Ia,
                    Ib = area_forest_loss_000_Ib,
                    II = area_forest_loss_000_II,
                    III = area_forest_loss_000_III, 
                    IV = area_forest_loss_000_IV,
                    V = area_forest_loss_000_V,
                    VI = area_forest_loss_000_VI,
                    `Total loss protected` = area_forest_loss_000_p,
                    `Total loss` = area_forest_loss_000) |> 
  group_by(`Country`) |> 
  summarise(across(everything(), ~sum(.x, na.rm = TRUE)*100)) |> # to ha
  arrange(desc(`Total loss protected`)) |> 
  adorn_totals("row") |> 
  xtable(digits = 0, caption = "Forest loss area from 2000 to 2019 per level of protection in hectares. The forest cover loss in this table incldues forest loss independently from the initial tree cover share.", label = "tab:s4-protection") 

xtable::print.xtable(tmp_table, table.placement = "!htpb", include.rownames = FALSE, caption.placement = "top", booktabs = TRUE, hline.after = c(0, nrow(tmp_table)-1, nrow(tmp_table)),
                     add.to.row = list(pos = list(-1), command = c("\\hline\n&&\\multicolumn{4}{c}{Forest loss within each protection level}&\\\\ \n\\cmidrule(lr){2-8}\n")),
                     size="\\fontsize{10pt}{11pt}\\selectfont", tabular.environment = "longtable", file = "./output/tab-s4-area-protection.tex")



# --------------------------------------------------------------------------------------
# figs1 - plot global tree cover loss 50x50 grid cells ---------------------------------
path_forest_loss <- "./output/global_mining_and_quarry_forest_loss_20221109.csv"
path_to_mining_polygons <- "./data/mining-tree-cover-loss-20221109/mining_features_20221109.geojson"
  
mine_features <- st_read(path_to_mining_polygons) |> 
  transmute(id = str_pad(id, 7, "0", side = 'left')) |> 
  rename(geom = geometry)

forest_loss <- read_csv(path_forest_loss) |> 
  filter(!is.na(year)) |> 
  filter(year > 2000, year < 2020) 

forest_loss_accum <- forest_loss  |> 
  group_by(id, isoa3, country, ecoregion, biome) |> 
  summarise(across(matches('area_forest_loss'), sum)) 

mine_features_areas <- mine_features |> 
  left_join(forest_loss_accum)

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
                filename = "output/fig-s1-global-map.png",
                width = textwidth, height = 170, units = "mm", scale = 1)


