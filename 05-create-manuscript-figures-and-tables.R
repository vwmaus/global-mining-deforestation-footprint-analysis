# Generate figures and tables in the main paper

library(stringr)
library(dplyr)
library(broom)
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
library(forcats)
library(e1071) 

source("R/00_plot_goode_homolosine_world_map.R")

# ------------------------------------------------------------------------------
# join datasets
path_forest_loss_all <- "./output/global_mining_and_quarry_forest_loss_20221123a.csv"
path_forest_loss_v2 <- "./output/global_mining_and_quarry_forest_loss_20221123b.csv"
path_to_mining_polygons <- "./output/global_mining_and_quarry_20220203.gpkg"

mine_features <- st_read(path_to_mining_polygons) |> 
  rename(area_mine = area)

forest_loss <- read_csv(path_forest_loss_all) |> 
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

# data check forest loss allocation
# commodity
mine_features_areas |> 
  st_drop_geometry() |> 
  mutate(Unknown = is.na(list_of_commodities)) |> 
  group_by(Unknown) |> 
  summarise(area = sum(area_forest_loss_000 - area_forest_loss_025, na.rm = TRUE)) |> 
  mutate(perc = area / sum(area)) |> 
  adorn_totals()

# biome
mine_features_areas |> 
  st_drop_geometry() |> 
  mutate(Unknown = is.na(biome)) |> 
  group_by(Unknown) |> 
  summarise(area = sum(area_forest_loss_000 - area_forest_loss_025, na.rm = TRUE)) |> 
  mutate(perc = area / sum(area)) |> 
  adorn_totals()

# country
mine_features_areas |> 
  st_drop_geometry() |> 
  mutate(Unknown = is.na(country)) |> 
  group_by(Unknown) |> 
  summarise(area = sum(area_forest_loss_000 - area_forest_loss_025, na.rm = TRUE)) |> 
  mutate(perc = area / sum(area)) |> 
  adorn_totals()

# data check mining area allocation
# commodity
mine_features_areas |> 
  st_drop_geometry() |> 
  mutate(Unknown = is.na(list_of_commodities)) |> 
  group_by(Unknown) |> 
  summarise(area = sum(area_mine, na.rm = TRUE)) |> 
  mutate(perc = area / sum(area)) |> 
  adorn_totals()

# biome
mine_features_areas |> 
  st_drop_geometry() |> 
  mutate(Unknown = is.na(biome)) |> 
  group_by(Unknown) |> 
  summarise(area = sum(area_mine, na.rm = TRUE)) |> 
  mutate(perc = area / sum(area)) |> 
  adorn_totals()

# country
mine_features_areas |> 
  st_drop_geometry() |> 
  mutate(Unknown = is.na(country)) |> 
  group_by(Unknown) |> 
  summarise(area = sum(area_mine, na.rm = TRUE)) |> 
  mutate(perc = area / sum(area)) |> 
  adorn_totals()

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

make_grid_50x50_25 <- function(data){
  
  mine_features_points <- data |> 
    transmute(fl = ifelse(is.na(area_forest_loss_000), 0, area_forest_loss_000) -
                   ifelse(is.na(area_forest_loss_025), 0, area_forest_loss_025)) |> 
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
# fig1 - plot global tree cover loss 50x50 grid cells (25-100]--------------------------
grid_50_forest_loss <- make_grid_50x50_25(mine_features_areas)

# check total forest loss
sum(grid_50_forest_loss$fl)
skewness(grid_50_forest_loss$fl, na.rm = TRUE) 

W_gp <- plot_goode_homolosine_world_map(ocean_color = "#e5f1f8", land_color = "gray95", family = font_family,
                                        grid_color = "grey75", grid_size = 0.1,
                                        country_borders_color = "grey75", country_borders_size = 0.1) +
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

# # --------------------------------------------------------------------------------------
# # zoom to south amareic and asia+australia ---------------------------------------------
# win_01 <- st_bbox(c(xmin = -41, ymin = -20, xmax = -73, ymax = 15), crs = 4326) |> 
#   st_as_sfc() |> 
#   st_transform(crs = "+proj=igh +ellps=WGS84 +units=m +no_defs") |> 
#   st_bbox()
#   
# win_02 <- st_bbox(c(xmin = 90, ymin = -10, xmax = 130, ymax = 20), crs = 4326) |> 
#   st_as_sfc() |> 
#   st_transform(crs = "+proj=igh +ellps=WGS84 +units=m +no_defs") |> 
#   st_bbox()
# 
# W_gp +
#   coord_sf(xlim = c(win_02[1], win_02[3]), ylim = c(win_02[2],  win_02[4]), crs = "+proj=igh", expand = FALSE) + 
#   theme(legend.position = "none", axis.title = element_blank(),
#         plot.margin = margin(t = -1, r = 0, b = 1, l = 0, unit = "cm")) 
# 
# W_gp +
#   coord_sf(xlim = c(win_01[1], win_01[3]), ylim = c(win_01[2],  win_01[4]), crs = "+proj=igh", expand = FALSE) + 
#   theme(legend.position = "none", axis.title = element_blank(),
#         plot.margin = margin(t = -1, r = 0, b = 1, l = 0, unit = "cm")) 
# 
# # --------------------------------------------------------------------------------------
# # build multiplot object ---------------------------------------------------------------
# multi_gp <- ggpubr::ggarrange(W_gp,
#                               ggarrange(SA_gp, AUS_IDN_gp, ncol = 2), 
#                               legend = "bottom", common.legend = TRUE,
#                               nrow = 2) 

ggplot2::ggsave(plot = W_gp, bg = "#ffffff",
                filename = "output/fig-1-global-map.png",
                width = textwidth, height = 170, units = "mm", scale = 1)



# --------------------------------------------------------------------------------------
# fig-2 plot selected countries bar plot -----------------------------------------------
fract_forest_cover <- tibble::tibble(
  `Initial tree cover (%)` = factor(c("(25, 50]", "(50, 75]", "(75, 100]"), 
                                    levels = c("(25, 50]", "(50, 75]", "(75, 100]")),
  name = c("area_forest_loss_050", 
           "area_forest_loss_075", 
           "area_forest_loss_100"))

# get top 6
country_tbl <- forest_loss |> 
  group_by(isoa3, country) |> 
  summarise(area = (sum(area_forest_loss_000, na.rm = TRUE) - sum(area_forest_loss_025, na.rm = TRUE))*100, .groups = "drop") |> 
  mutate(perc = 100 * area / sum(area)) |> 
  arrange(desc(area)) |> 
  transmute(isoa3, country = str_replace(country, "Russian Federation", "Russia")) |> 
  slice(1:6) |> 
  mutate(country = factor(country, levels = country))

forest_loss_ts <- forest_loss |> 
  filter(isoa3 %in% country_tbl$isoa3) |> 
  mutate(area_forest_loss_000 = ifelse(is.na(area_forest_loss_000), 0, area_forest_loss_000) - ifelse(is.na(area_forest_loss_025), 0, area_forest_loss_025)) |> 
  select(isoa3, year, area_forest_loss_000, area_forest_loss_050, area_forest_loss_075, area_forest_loss_100) |> 
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

sloop_pvalue <- trend_bar %>% 
  group_by(country) |> 
  mutate(area_forest_loss_000 = 100 * area_forest_loss_000) |> 
  summarise(out = list(tidy(lm(area_forest_loss_000 ~ year, data = cur_data())))) %>% 
  unnest(out) |> 
  filter(term == "year") |> 
  mutate(sloop_text = str_c("Forest loss annual increment: ", round(estimate, 0), " ha ", 
                               ifelse(p.value > 0.01, str_c("(p-value=",round(p.value, 2),")"), ifelse(p.value < 0.001, "(p-value<0.001)", str_c("(p-value=",round(p.value, 3),")"))))) |> 
  mutate(year = 2010, area = 45000)

gp <- ggplot() + 
  facet_wrap(~country) + 
  geom_bar(mapping = aes(x = Year, y = area, fill = `Initial tree cover (%)`), data = forest_loss_ts, stat="identity", width = 0.5) + 
  stat_smooth(aes(x = year, y = area_forest_loss_000*100, group = country), data = trend_bar, method = "lm", show.legend = FALSE, se = FALSE, color = "black", fill = "black", linewidth = .5) + 
  geom_text(mapping = aes(x = year, y = area, label = sloop_text), data = sloop_pvalue, size = 3.8) + 
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
  ylab("Annual forest loss (K ha)")

ggsave(filename = str_c("./output/fig-2-barplot-top-six-countries.png"), plot = gp, bg = "#ffffff",
       width = 345, height = 180, units = "mm", scale = 1)

# --------------------------------------------------------------------------------------
# fig-s2 plot Brazil bar plot ----------------------------------------------------------

fract_forest_cover <- tibble::tibble(
  `Initial tree cover (%)` = factor(c("(25, 50]", "(50, 75]", "(75, 100]"), 
                                    levels = c("(25, 50]", "(50, 75]", "(75, 100]")),
  name = c("area_forest_loss_050", 
           "area_forest_loss_075", 
           "area_forest_loss_100"))

country_tbl <- tibble(
  isoa3 = c("BRA"),
  country = factor(c("Brazil"),
                   levels = c("Brazil")))

forest_loss_ts <- forest_loss |> 
  filter(isoa3 %in% country_tbl$isoa3) |> 
  mutate(area_forest_loss_000 = ifelse(is.na(area_forest_loss_000), 0, area_forest_loss_000) - ifelse(is.na(area_forest_loss_025), 0, area_forest_loss_025)) |> 
  select(isoa3, year, area_forest_loss_000, area_forest_loss_050, 
         area_forest_loss_075, area_forest_loss_100) |> 
  group_by(isoa3, year) |> 
  summarise(across(everything(), sum, na.rm = TRUE), .groups = 'drop') |> 
  filter(year > 2000, year < 2020) |> 
  pivot_longer(cols = c(-year, -area_forest_loss_000, -isoa3)) |> 
  left_join(fract_forest_cover) |> 
  left_join(country_tbl) |> 
  mutate(Year = year, area = value * 100) |> # convert to ha 
  mutate(Period = ifelse(year < 2013, "Year < 2013", "Year >= 2013"))

trend_bar <- forest_loss_ts |> 
  select(country, year, area_forest_loss_000) |> 
  mutate(area_forest_loss_000 = area_forest_loss_000) |> 
  distinct() |> 
  mutate(Period = ifelse(year < 2013, "Year < 2013", "Year >= 2013"))

sloop_pvalue <- trend_bar %>% 
  group_by(Period) |> 
  mutate(area_forest_loss_000 = 100 * area_forest_loss_000) |> 
  summarise(out = list(tidy(lm(area_forest_loss_000 ~ year, data = cur_data())))) %>% 
  unnest(out) |> 
  filter(term == "year") |> 
  mutate(sloop_text = str_c(Period, ": ","Forest loss annual increment: ", round(estimate, 0), " ha ", 
                            ifelse(p.value > 0.01, str_c("(p-value=",round(p.value, 2),")"), ifelse(p.value < 0.001, "(p-value<0.001)", str_c("(p-value=",round(p.value, 3),")"))))) |> 
  mutate(year = c(2006, 2016), area = 25000) 

gp <- ggplot() + 
  geom_bar(mapping = aes(x = Year, y = area, fill = `Initial tree cover (%)`), 
           data = forest_loss_ts, stat="identity", width = 0.5) + 
  stat_smooth(aes(x = year, y = area_forest_loss_000*100, group = Period), 
              data = trend_bar, method = "lm", show.legend = FALSE, 
              se = FALSE, color = "black", fill = "black", linewidth = .5) + 
  geom_text(mapping = aes(x = year, y = area, label = sloop_text), data = sloop_pvalue, size = 3.8) + 
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
  scale_x_continuous(labels = seq(2000, 2019, 2), breaks = seq(2000, 2019, 2)) + 
  ylab("Annual forest loss (K ha)") 

ggsave(filename = str_c("./output/fig-s2-barplot-brazil.png"), plot = gp, bg = "#ffffff",
       width = 345, height = 140, units = "mm", scale = 1)


# --------------------------------------------------------------------------------------
# fig-s3 plot biomes bar plot ----------------------------------------------------------

fract_forest_cover <- tibble::tibble(
  `Initial tree cover (%)` = factor(c("(25, 50]", "(50, 75]", "(75, 100]"), 
                                    levels = c("(25, 50]", "(50, 75]", "(75, 100]")),
  name = c("area_forest_loss_050", 
           "area_forest_loss_075", 
           "area_forest_loss_100"))

forest_loss_ts <- forest_loss |> 
  filter(!is.na(biome)) |> 
  mutate(area_forest_loss_000 = ifelse(is.na(area_forest_loss_000), 0, area_forest_loss_000) - ifelse(is.na(area_forest_loss_025), 0, area_forest_loss_025)) |> 
  select(biome, year, area_forest_loss_000, area_forest_loss_050, area_forest_loss_075, area_forest_loss_100) |> 
  group_by(biome, year) |> 
  summarise(across(everything(), sum, na.rm = TRUE), .groups = 'drop') |> 
  filter(year > 2000, year < 2020) |> 
  pivot_longer(cols = c(-year, -area_forest_loss_000, -biome)) |> 
  left_join(fract_forest_cover) |> 
  mutate(Year = year, area = value * 100) # convert to ha 

trend_bar <- forest_loss_ts |> 
  select(biome, year, area_forest_loss_000) |> 
  mutate(area_forest_loss_000 = area_forest_loss_000) |> 
  distinct()

sloop_pvalue <- trend_bar %>% 
  group_by(biome) |> 
  mutate(area_forest_loss_000 = 100 * area_forest_loss_000) |> 
  summarise(out = list(tidy(lm(area_forest_loss_000 ~ year, data = cur_data()))), area = max(area_forest_loss_000, na.rm = TRUE) + sd(area_forest_loss_000, na.rm = TRUE)/3) %>% 
  unnest(out) |> 
  filter(term == "year") |> 
  mutate(sloop_text = str_c("Forest loss annual increment: ", round(estimate, 0), " ha ", 
                            ifelse(p.value > 0.01, str_c("(p-value=",round(p.value, 2),")"), ifelse(p.value < 0.001, "(p-value<0.001)", str_c("(p-value=",round(p.value, 3),")"))))) |> 
  mutate(year = 2010)

gp <- ggplot() + 
  facet_wrap(~biome, ncol = 3, scales = "free_y") + 
  geom_bar(mapping = aes(x = Year, y = area, fill = `Initial tree cover (%)`), data = forest_loss_ts, stat="identity", width = 0.5) + 
  stat_smooth(aes(x = year, y = area_forest_loss_000*100, group = biome), data = trend_bar, method = "lm", show.legend = FALSE, se = FALSE, color = "black", fill = "black", linewidth = .5) + 
  geom_text(mapping = aes(x = year, y = area, label = sloop_text), data = sloop_pvalue, size = 3.8) + 
  theme_linedraw() + 
  theme(axis.text = ggplot2::element_text(size = font_size), 
        text = ggplot2::element_text(size = font_size),
        legend.text = element_text(size = font_size),
        legend.title = element_text(size = font_size),
        legend.position = c(0.8,.1),
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box.spacing = unit(0.0, "cm"),
        legend.key.size = unit(0.3, "cm")) + 
  scale_fill_grey(start = .7, end = 0, guide = guide_legend(direction = "horizontal", title.position = "top")) +
  scale_y_continuous(labels = label_number(scale = 1e-3)) + 
  scale_x_continuous(labels = seq(2000, 2015, 5), breaks = seq(2000, 2015, 5)) + 
  ylab("Annual forest loss (K ha)")

ggsave(filename = str_c("./output/fig-s3-barplot-biomes.png"), plot = gp, bg = "#ffffff",
       width = 420, height = 500, units = "mm", scale = 1)



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
        transmute(Commodity = i,
                `(25, 50]` = area_forest_loss_050, 
                `(50, 75]` = area_forest_loss_075,
                `(75, 100]` = area_forest_loss_100, 
                `Total loss` = area_forest_loss_000 - area_forest_loss_025) |> 
      group_by(Commodity) |> 
      summarise(across(everything(), ~sum(.x, na.rm = TRUE)*100))
  }) |> 
  bind_rows() |> 
  arrange(desc(`Total loss`)) |> 
  filter(`Total loss` >= 10000) |> # remove smaller than 10,000ha
  select(-`Total loss`) |> 
  mutate(Commodity = factor(Commodity, Commodity, Commodity)) |> 
  pivot_longer(cols = c(-Commodity), names_to = "Initial tree cover (%)", values_to = "Area") |> 
  ggplot(aes(x = Commodity, y = Area, fill = `Initial tree cover (%)`)) + 
  geom_bar(stat="identity", width = 0.5) +
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
  scale_y_continuous(labels = label_number(scale = 1e-6, accuracy = 0.1)) + 
  ylab("Forest loss (M ha)") + 
  xlab("Associated commodity")

ggsave(filename = str_c("./output/fig-3-barplot-commodities.png"), plot = gp, bg = "#ffffff",
       width = 420, height = 120, units = "mm", scale = 1)



# --------------------------------------------------------------------------------------
# supplementary tables -----------------------------------------------------------------

# country table 
tmp_table <- select(forest_loss, id, `Country` = country,
                    `(0, 25]%` = area_forest_loss_025,
                    `(25, 50]%` = area_forest_loss_050, 
                    `(50, 75]%` = area_forest_loss_075,
                    `(75, 100]%` = area_forest_loss_100, 
                    `Forest cover loss` = area_forest_loss_000) |>
  mutate(`Forest cover loss` = `Forest cover loss` - `(0, 25]%`) |> 
  group_by(id, `Country`) |> 
  summarise(across(everything(), ~sum(.x, na.rm = TRUE)), .groups = "drop") |> 
  full_join(st_drop_geometry(select(mine_features, id, Country = country, area_mine))) |> 
  rename("Mining area" = area_mine) |> 
  ungroup() |> 
  select(-id) |> 
  group_by(`Country`) |> 
  summarise(across(everything(), ~sum(.x, na.rm = TRUE)*100)) |> # to ha
  arrange(desc(`Forest cover loss`)) |> 
  mutate(per_fl = round(100*`Forest cover loss`/sum(`Forest cover loss`),1), per_ma = round(100*`Mining area`/sum(`Mining area`),1)) |> 
  adorn_totals("row") |> 
  mutate(`Forest cover loss` = ifelse(Country == "Total", round(`Forest cover loss`, 0), str_c(round(`Forest cover loss`, 0), " (", round(per_fl,1),"%)")), 
         `Mining area` = ifelse(Country == "Total", round(`Mining area`, 0), str_c(round(`Mining area`,0), " (", round(per_ma,1),"%)"))) |> 
  select(-per_fl, -per_ma) |> 
  xtable(digits = 0, 
         caption = 
         "Accumulated forest cover loss from 2000 to 2019 and the total mining area per country in hectares.
         The column \\textit{Forest cover loss} includes only areas with initial tree cover higher than 25\\%.", label = "tab:s1-country") 

xtable::print.xtable(tmp_table, table.placement = "!htpb", include.rownames = FALSE, caption.placement = "top", booktabs = TRUE, hline.after = c(0, nrow(tmp_table)-1, nrow(tmp_table)),
                     add.to.row = list(pos = list(-1), command = c("\\hline\n&\\multicolumn{4}{c}{Loss within each initial tree cover share}&&\\\\ \n\\cmidrule(lr){2-5}\n")),
                     size="\\fontsize{10pt}{11pt}\\selectfont", tabular.environment = "longtable", file = "./output/tab-s1-area-country.tex", floating = FALSE)

# biome table 
tmp_table <- select(forest_loss, id, `Biome` = biome,
                    `(0, 25]%` = area_forest_loss_025,
                    `(25, 50]%` = area_forest_loss_050, 
                    `(50, 75]%` = area_forest_loss_075,
                    `(75, 100]%` = area_forest_loss_100, 
                    `Forest cover loss` = area_forest_loss_000) |> 
  mutate(`Forest cover loss` = `Forest cover loss` - `(0, 25]%`) |>
  group_by(id, `Biome`) |> 
  summarise(across(everything(), ~sum(.x, na.rm = TRUE))) |> # to ha
  full_join(st_drop_geometry(select(mine_features, id, area_mine))) |> 
  rename("Mining area" = area_mine) |> 
  ungroup() |> 
  select(-id) |> 
  group_by(`Biome`) |> 
  summarise(across(everything(), ~sum(.x, na.rm = TRUE)*100)) |> # to ha
  arrange(desc(`Forest cover loss`)) |> 
  mutate(Biome = ifelse(is.na(Biome),"Unassigned (Mining polygon does not intersect any biome)",Biome)) |> 
  mutate(per_fl = round(100*`Forest cover loss`/sum(`Forest cover loss`),1), per_ma = round(100*`Mining area`/sum(`Mining area`),1)) |> 
  adorn_totals("row") |> 
  mutate(`Forest cover loss` = ifelse(Biome == "Total", round(`Forest cover loss`, 0), str_c(round(`Forest cover loss`, 0), " (", round(per_fl,1),"%)")), 
         `Mining area` = ifelse(Biome == "Total", round(`Mining area`, 0), str_c(round(`Mining area`,0), " (", round(per_ma,1),"%)"))) |> 
  select(-per_fl, -per_ma) |> 
  xtable(digits = 0, 
         caption = 
           "Accumulated forest cover loss from 2000 to 2019 and the total mining area per biome in hectares.
         The column \\textit{Forest cover loss} includes only areas with initial tree cover higher than 25\\%.", label = "tab:s2-biome")

xtable::print.xtable(tmp_table, table.placement = "!htpb", include.rownames = FALSE, caption.placement = "top", booktabs = TRUE, hline.after = c(0, nrow(tmp_table)-1, nrow(tmp_table)),
                     add.to.row = list(pos = list(-1), command = c("\\hline\n&\\multicolumn{4}{c}{Loss within each initial tree cover share}&&\\\\ \n\\cmidrule(lr){2-5}\n")),
                     size="\\fontsize{10pt}{11pt}\\selectfont", file = "./output/tab-s2-area-biome.tex", tabular.environment = "longtable", floating = FALSE)


# commodity associated to forest cover loss
tmp_table <- str_c(na.omit(forest_loss$list_of_commodities), collapse = ",") |> 
  str_split(",") |> 
  unlist() |> 
  unique() |> 
  sort() |> 
  lapply(function(i){
    forest_loss |> 
      filter(str_detect(list_of_commodities, i)) |> 
      transmute(Commodity = i,
                `(0, 25]%` = area_forest_loss_025,
                `(25, 50]%` = area_forest_loss_050, 
                `(50, 75]%` = area_forest_loss_075,
                `(75, 100]%` = area_forest_loss_100, 
                `Forest cover loss` = area_forest_loss_000 - `(0, 25]%`) |> 
      group_by(Commodity) |> 
      summarise(across(everything(), ~sum(.x, na.rm = TRUE)*100))
  }) |> 
  bind_rows() |> 
  arrange(desc(`Forest cover loss`)) |> 
  # adorn_totals("row") |> 
  xtable(digits = 0, 
         caption = 
         "Accumulated forest cover loss from 2000 to 2019 in hectares associated to mineral commodities. 
         The column \\textit{Forest cover loss} includes only areas with initial tree cover higher than 25\\%.
         Note that the row totals are not present on this table because that would include multiple countings
         of the same forest loss area associated with various commodities.", label = "tab:s3-commodities") 

xtable::print.xtable(tmp_table, table.placement = "!htpb", include.rownames = FALSE, caption.placement = "top", booktabs = TRUE, hline.after = c(0, nrow(tmp_table)),
                     add.to.row = list(pos = list(-1), command = c("\\hline\n&\\multicolumn{4}{c}{Loss within each initial tree cover share}&\\\\ \n\\cmidrule(lr){2-5}\n")),
                     size="\\fontsize{10pt}{11pt}\\selectfont", tabular.environment = "longtable", floating = FALSE, file = "./output/tab-s3-area-commodity.tex")

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

tmp_table <- transmute(forest_loss, `Country` = country,
                    Ia = area_forest_loss_000_Ia - area_forest_loss_025_Ia,
                    Ib = area_forest_loss_000_Ib - area_forest_loss_025_Ib,
                    II = area_forest_loss_000_II - area_forest_loss_025_II,
                    III = area_forest_loss_000_III - area_forest_loss_025_III, 
                    IV = area_forest_loss_000_IV - area_forest_loss_025_IV,
                    V = area_forest_loss_000_V - area_forest_loss_025_V,
                    VI = area_forest_loss_000_VI - area_forest_loss_025_VI,
                    `All levels` = area_forest_loss_000_p - area_forest_loss_025_p,
                    `Forest cover loss` = area_forest_loss_000 - area_forest_loss_025) |>
  group_by(`Country`) |> 
  summarise(across(everything(), ~sum(.x, na.rm = TRUE)*100)) |> # to ha
  arrange(desc(`All levels`)) |> 
  adorn_totals("row") |> 
  mutate(`Loss all levels (%)` = as.character(ifelse(`Forest cover loss`==0, 0, round(100*`All levels` / `Forest cover loss`, 1)))) |> 
  xtable(digits = 0, 
  caption = 
  "Accumulated forest cover loss from 2000 to 2019 within areas with different level of 
  protection according to the World Database on Protected Areas (WDPA)\\cite{UNEP-WCMC2021}. 
  The forest cover loss in this table incldues loss independently from the initial tree cover share. 
  The coloumn \\textit{All levels} also includes protected areas: not applicable, not assigned, or not reported. 
  This table includes only areas with initial tree cover share higher than 25\\%.", label = "tab:s4-protection") 

xtable::print.xtable(tmp_table, table.placement = "!htpb", include.rownames = FALSE, caption.placement = "top", booktabs = TRUE, hline.after = c(0, nrow(tmp_table)-1, nrow(tmp_table)),
                     add.to.row = list(pos = list(-1), command = c("\\hline\n&\\multicolumn{8}{c}{Forest loss within each protection level}&\\\\ \n\\cmidrule(lr){2-9}\n")),
                     size="\\fontsize{10pt}{11pt}\\selectfont", tabular.environment = "longtable", file = "./output/tab-s4-area-protection.tex", floating = FALSE)



# --------------------------------------------------------------------------------------
# figs1 - plot global tree cover loss 50x50 grid cells ---------------------------------
mine_features <- st_read(path_to_mining_polygons) |> 
  rename(area_mine = area)

forest_loss <- read_csv(path_forest_loss_v2) |> 
  filter(!is.na(year)) |> 
  filter(year > 2000, year < 2020) 

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
  summarise(area = sum(area_forest_loss_000 - area_forest_loss_025, na.rm = TRUE)) |> 
  mutate(perc = area / sum(area))

grid_50_forest_loss <- make_grid_50x50_25(mine_features_areas)

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

# --------------------------------------------------------------------------------------
# figs4 - distribution of mining area across latitudes 
agg_level <- 20
osm <- st_read("./data/osm_quarry_check_20211125.gpkg") |> 
  st_set_crs(4326) |> 
  transmute(Area = st_area(geom) |> units::set_units("ha") |> units::drop_units(), Latitude = st_coordinates(st_centroid(geom))[,2]) |>  
  st_drop_geometry() |> 
  as_tibble() |> 
  mutate(Latitude = cut(Latitude, breaks = seq(-60, 90, agg_level))) |> 
  group_by(Latitude) |> 
  summarise(osm = sum(Area, na.rm = TRUE)) 

wu <- st_read("./data/wu_quarry_check_20211125.gpkg") |> 
  st_set_crs(4326) |> 
  st_make_valid() |> 
  transmute(Area = st_area(geom) |> units::set_units("ha") |> units::drop_units(), Latitude = st_coordinates(st_centroid(geom))[,2]) |> 
  st_drop_geometry() |> 
  as_tibble() |> 
  mutate(Latitude = cut(Latitude, breaks = seq(-60, 90, agg_level))) |> 
  group_by(Latitude) |> 
  summarise(wu = sum(Area, na.rm = TRUE))

all <- st_read("./output/global_mining_and_quarry_20220203.gpkg") |> 
  st_set_crs(4326) |> 
  transmute(Area = st_area(geom) |> units::set_units("ha") |> units::drop_units(), Latitude = st_coordinates(st_centroid(geom))[,2]) |> 
  st_drop_geometry() |> 
  as_tibble() |> 
  mutate(Latitude = cut(Latitude, breaks = seq(-60, 90, agg_level))) |> 
  group_by(Latitude) |> 
  summarise(total = sum(Area, na.rm = TRUE))

# join datasets and calculate shares
gp <- left_join(all, wu) |> 
  left_join(osm) |> 
  mutate(overlap = (osm + wu) - total,
         osm = osm - overlap,
         wu = wu - overlap,
         check = osm + wu + overlap) |> 
  drop_na() |> 
  select(Latitude, `Maus et al. (2022)` = wu, OpenStreetMap = osm, Overlap = overlap) |> 
  pivot_longer(-Latitude, values_to = "Area", names_to = "Data source") |> 
  mutate(`Data source` = factor(as.character(`Data source`), levels = c("Maus et al. (2022)", "Overlap", "OpenStreetMap"))) |> 
  ggplot(aes(x = Area, y = Latitude, fill = `Data source`)) + 
  geom_bar(stat="identity", width = 0.8) + 
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
  #scale_fill_grey(start = .7, end = 0, guide = guide_legend(direction = "horizontal", title.position = "top")) +
  scale_fill_viridis(discrete = TRUE, option = "D", direction = -1) +
  scale_x_continuous(labels = label_number(scale = 1e-6, accuracy = 0.1)) +
  #scale_x_continuous(labels = seq(2000, 2015, 5), breaks = seq(2000, 2015, 5)) 
  xlab("Area (M ha)")

ggsave(filename = str_c("./output/fig-s4-spatial-distribution.png"), plot = gp, bg = "#ffffff",
       width = 345, height = 140, units = "mm", scale = 1)

