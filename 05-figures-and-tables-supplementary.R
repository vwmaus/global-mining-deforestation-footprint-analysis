# Plot global map of accumulated tree cover loss on a 50x50 grid cells 
# 

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
library(viridis)
library(ggpubr)

font_size = 12

# ------------------------------------------------------------------------------
# set input data version 
data_version <- "20220203"
path_mining_area <- str_c("./data/global_mining_and_quarry_",data_version,".gpkg") 
path_mining_cluster <- str_c("./data/hcluster_concordance_",data_version,".csv")
path_forest_loss <- "./data/global_mining_and_quarry_forest_loss.csv"

# ------------------------------------------------------------------------------
# read data -- all area is in km2 
forest_loss <- read_csv(path_forest_loss)

mining_cluster <- read_csv(path_mining_cluster)

mining_cluster |> 
  group_by(type_of_commodities) |> 
  summarise(area = sum(area)) |> 
  mutate(per = area / sum(area))

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
  summarise(area = sum(area) * 100, .groups = "drop") |> # convert to ha
  left_join(country_rank) |> 
  arrange(rank) |> 
  mutate(isoa3 = factor(isoa3, levels = c(unique(isoa3)[unique(isoa3) != "ROW"], "ROW"))) |> 
  rename(Country = isoa3, `Commodity type` = type_of_commodities) |> 
  ggplot(aes(x = Country, y = area, fill = `Commodity type`)) + 
  geom_bar(stat="identity") + 
  theme_linedraw() + 
  theme(legend.position = c(0.94, 0.74),
        legend.text = element_text(size = font_size),
        legend.title = element_text(size = font_size),
        legend.background = element_rect(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,family = "sans", size = 11),
        axis.title.x = element_blank()) +
  scale_fill_viridis_d(option = "turbo", direction = 1, begin = 0.0, end = 1.0,
                       guide = guide_legend(direction = "vertical", title.position = "top")) + 
  scale_y_continuous(labels = label_number(scale = 1e-6, accuracy = 0.1)) + 
  ylab("Area (M ha)")

ggsave(filename = "./output/country_mining_area.png", plot = gp, bg = "#ffffff",
       width = 360, height = 100, units = "mm", scale = 1)

# ------------------------------------------------------------------------------
# country forest loss area statistics
threshold <- 0.99

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
  summarise(area = sum(area, na.rm = TRUE) * 100, .groups = "drop") |> # convert to ha
  left_join(country_rank) |> 
  arrange(rank) |> 
  mutate(isoa3 = factor(isoa3, levels = c(unique(isoa3)[unique(isoa3) != "ROW"], "ROW"))) |> 
  rename(Country = isoa3, `Commodity type` = type_of_commodities) |> 
  ggplot(aes(x = Country, y = area, fill = `Commodity type`)) + 
  geom_bar(stat="identity") + 
  theme_linedraw() + 
  theme(legend.position = c(0.94, 0.74),
        legend.text = element_text(size = font_size),
        legend.title = element_text(size = font_size),
        legend.background = element_rect(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,family = "sans", size = 11),
        axis.title.x = element_blank()) +
  scale_fill_viridis_d(option = "turbo", direction = 1, begin = 0.0, end = 1.0,
                       guide = guide_legend(direction = "vertical", title.position = "top")) + 
  scale_y_continuous(labels = label_number(scale = 1e-3, accuracy = 1)) + 
  ylab("Area (K ha)")

ggsave(filename = "./output/country_mining_forest_loss_area.png", plot = gp, bg = "#ffffff",
       width = 360, height = 100, units = "mm", scale = 1)


select(forest_loss, id, area = area_forest_loss_000) |> 
  mutate(id = str_c("A", id)) |> 
  left_join(select(mining_cluster, -area)) |> 
  group_by(type_of_commodities) |> 
  summarise(area = sum(area, na.rm = TRUE), .groups = "drop") |> 
  mutate(perc = area / sum(area))

# base theme for bar plots 
thm_bar <- theme_linedraw() + 
  theme(legend.spacing.x = unit(0.1, 'cm'),
        axis.text = ggplot2::element_text(size = font_size), 
        text = ggplot2::element_text(size = font_size),
        legend.text = element_text(size = font_size),
        legend.title = element_text(size = font_size),
        legend.background = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(vjust = - 8, hjust = 0.02))
  
# ------------------------------------------------------------------------------
# global forest loss time series
forest_loss_ts <- forest_loss |> 
  select(year, area_forest_loss_000, area_forest_loss_025, area_forest_loss_050, 
         area_forest_loss_075, area_forest_loss_100) |> 
  group_by(year) |> 
  summarise(across(everything(), sum, na.rm = TRUE)) |> 
  filter(year > 2000, year < 2020) |> 
  pivot_longer(cols = c(-year, -area_forest_loss_000)) |> 
  mutate(`Initial forest cover` = str_remove(name, "area_forest_loss_") 
         |> as.numeric() |> str_c(" %") |> factor(levels = c("100 %", "75 %", "50 %", "25 %")),
         Year = year,
         area = value * 100) # convert to ha 

gp <- forest_loss_ts |> 
  ggplot(aes(x = Year, y = area, fill = `Initial forest cover`)) + 
  geom_bar(stat="identity") + 
  thm_bar +
  theme(legend.position = c(0.3, 0.82)) + 
  scale_fill_grey(start = 0.6, end = 0, guide = guide_legend(direction = "horizontal", title.position = "top")) +
  # scale_fill_viridis_d(option = "mako", direction = 1, begin = 0.2, end = 0.8,
                       # guide = guide_legend(direction = "horizontal", title.position = "top")) + 
  scale_y_continuous(labels = label_number(scale = 1e-3, accuracy = 1)) + 
  ylab("Area (K ha)") + 
  ggtitle("Global") 

ggsave(filename = "./output/forest_loss_area_time_series_global.png", plot = gp, bg = "#ffffff",
       width = 160, height = 120, units = "mm", scale = 1)


# ------------------------------------------------------------------------------
# def fun country bar plots
plot_country_bar <- function(data, legend.position = "None", breaks, iso3, country){
  
  fract_forest_cover <- tibble::tibble(
    `Initial tree cover (%)` = factor(c("(0, 25]", "(25, 50]", "(50, 75]", "(75, 100]"), 
                                      levels = c("(0, 25]", "(25, 50]", "(50, 75]", "(75, 100]")),
    name = c("area_forest_loss_025", 
             "area_forest_loss_050", 
             "area_forest_loss_075", 
             "area_forest_loss_100"))
  
  forest_loss_ts <- data |> 
    filter(isoa3 == iso3) |> 
    select(year, area_forest_loss_000, area_forest_loss_025, area_forest_loss_050, 
           area_forest_loss_075, area_forest_loss_100) |> 
    group_by(year) |> 
    summarise(across(everything(), sum, na.rm = TRUE)) |> 
    filter(year > 2000, year < 2020) |> 
    pivot_longer(cols = c(-year, -area_forest_loss_000)) |> 
    left_join(fract_forest_cover) |> 
    mutate(Year = year, area = value * 100) # convert to ha 
  
  trend_df <- group_by(forest_loss_ts, year) |> 
    summarise(area = sum(area))
  
  gp <- forest_loss_ts |> 
    ggplot(aes(x = Year, y = area, fill = `Initial tree cover (%)`)) + 
    geom_bar(stat="identity", width = 0.5) + 
    theme_linedraw() + 
    theme(legend.position = legend.position,
          legend.spacing.x = unit(0.1, 'cm'),
          axis.text = ggplot2::element_text(size = font_size), 
          text = ggplot2::element_text(size = font_size),
          legend.text = element_text(size = font_size),
          legend.title = element_text(size = font_size),
          legend.background = element_blank(),
          axis.title.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(vjust = - 8, hjust = 0.02)) + 
    scale_fill_grey(start = .7, end = 0, guide = guide_legend(direction = "vertical", title.position = "top")) +
    # scale_fill_viridis_d(option = "mako", direction = 1, begin = 0.2, end = 0.8,
    #                      guide = guide_legend(direction = "horizontal", title.position = "top")) + 
    scale_y_continuous(labels = label_number(scale = 1e-3, accuracy = 1), breaks = breaks) + 
    scale_x_continuous(labels = seq(2000, 2015, 5), breaks = seq(2000, 2015, 5)) + 
    ylab("Area (K ha)") + 
    ggtitle(country)
  
  ggsave(filename = str_c("./output/bar_forest_loss_area_time_series_",str_to_lower(country),".png"), plot = gp, bg = "#ffffff",
         width = 100, height = 100, units = "mm", scale = 1)
  
}

# ------------------------------------------------------------------------------
plot_country_bar(data = forest_loss, breaks = seq(0, 40000, 10000), iso3 = "IDN", country = "Indonesia")
plot_country_bar(data = forest_loss, breaks = seq(0, 20000, 10000), iso3 = "BRA", country = "Brazil")
plot_country_bar(data = forest_loss, breaks = seq(0, 20000, 5000), iso3 = "RUS", country = "Russia", legend.position = c(0.25, 0.7))
plot_country_bar(data = forest_loss, breaks = seq(0, 10000, 5000), iso3 = "CAN", country = "Canada")
plot_country_bar(data = forest_loss, breaks = seq(0, 8000, 2000), iso3 = "USA", country = "United States of America")
plot_country_bar(data = forest_loss, breaks = seq(0, 10000, 2000), iso3 = "AUS", country = "Australia")

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

# ------------------------------------------------------------------------------
# Table: Country tree cover extent
select(forest_loss, id, area = area_forest_loss_000_p) |> 
  mutate(id = str_c("A", id)) |> 
  left_join(select(mining_cluster, -area)) |> 
  group_by(type_of_commodities) |> 
  summarise(area = sum(area, na.rm = TRUE), .groups = "drop") |> 
  mutate(perc = area / sum(area))

