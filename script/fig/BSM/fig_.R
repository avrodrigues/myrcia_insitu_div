
# load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(sf)
library(rcartocolor)
library(patchwork)

source(here("script", "fig", "fig_00_map_themes.R"))

sf::sf_use_s2(FALSE)
# load data ---------------------------------------------------------------

evo_df <- read.csv(here("data", "evoregions_stand_names_df.csv"))
site_xy <- evo_df[,1:2]

avg_age_half_df <- read.csv("output/bg_stochastic_map/avg_age_half_df.csv", row.names = 1)
avg_age_recent_df <- read.csv("output/bg_stochastic_map/avg_age_recent_df.csv", row.names = 1)
insitu_div_df <- read.csv("output/bg_stochastic_map/div_insitu_prop_df.csv", row.names = 1)


bsm_summary_df <- data.frame(
  site_xy, 
  bsm_mean_half = rowMeans(avg_age_half_df), 
  bsm_sd_half = apply(avg_age_half_df, 1, sd),
  bsm_mean_recent = rowMeans(avg_age_recent_df), 
  bsm_sd_recent = apply(avg_age_recent_df, 1, sd),
  bsm_mean_insitu_div = rowMeans(insitu_div_df), 
  bsm_sd_insitu_div = apply(insitu_div_df, 1, sd)
  
)

cor(bsm_summary_df[, -c(1,2)])


# Atlantic Forest Integrated Limits - Muylaert et al 2018
AF_sf <- read_sf(
  here::here("data", "shape", "limits_AF", 
             "ma_limite_integrador_muylaert_et_al_2018_wgs84.shp"))


af_limits <- list(
  x = c(st_bbox(AF_sf)[1], -32),
  y = st_bbox(AF_sf)[c(2,4)]
)

bsm_metrics_sf <- st_as_sf(
  bsm_summary_df,
  coords = c("x","y"), 
  crs = st_crs(AF_sf)
)

bsm_metrics_AF <- st_filter(bsm_metrics_sf, AF_sf)
site_xy_AF <- st_coordinates(bsm_metrics_AF$geometry)

bsm_metrics_AF <- 
  bsm_metrics_AF %>% 
  mutate(
    x = site_xy_AF[,1],
    y = site_xy_AF[,2],
    .before = bsm_mean_half
  )

# map customization -------------------------------------------------------

# |- theme_map_af ----
text_size <- 7

theme_map_af <- list(
  geom_sf(data = AF_sf, fill = NA, color = greys[7]),
  coord_sf(xlim = c(-58,-35), ylim = af_limits$y),
  theme(
    panel.background = element_rect(fill = "transparent", color = NA), 
    panel.grid = element_blank(), 
    text = element_text(color = greys[1], size = text_size*1.1), 
    plot.title = element_text(
      color = greys[2], 
      margin = margin(b=5, t = 5),
      hjust = 0.5
    ),
    #axis.text = element_text(color = greys[2], size = 8), 
    #axis.ticks = element_line(color = greys[3]), 
    #panel.border = element_rect(color = greys[4], fill = NA), 
    axis.title = element_blank(),
    legend.text = element_text(color = greys[2], size = text_size*0.9),
    legend.title = element_markdown(
      color = greys[2], face = "bold", size = text_size, margin = margin(b = 2)
    ),
    legend.position = "bottom",
    legend.key.height = unit(5, "points"), 
    legend.key.width = unit(15, "points"),
    legend.background = element_blank()
  ), 
  guides(
    fill = guide_colorsteps(
      show.limits = T, 
      title.vjust = 1,
      title.position = "top", 
      label.position = "bottom"))
)

# |- divergent color palette

div_cols <- rcartocolor::carto_pal(7, "Geyser") 

# maps --------------------------------------------------------------------

# |- map_age ----
map_age_bsm <- 
  bsm_metrics_AF %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = bsm_mean_half)) +
  scale_fill_stepsn(
    breaks = seq(0, 15, 3),
    limits = c(0,15),
    name = "Mean age of assemblage (BSM)" ,
    colours = rcartocolor::carto_pal(6, "Mint")
  ) +
  labs(title = "B - Age of assemblages") +
  theme_map_af

map_age_CV_bsm <- 
  bsm_metrics_AF %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = bsm_sd_half/bsm_mean_half)) +
  scale_fill_stepsn(
    n.breaks = 5,
    #breaks = seq(0, 15, 3),
    #limits = c(0,15),
    name = "CV Mean age of assemblage (BSM)" ,
    colours = rcartocolor::carto_pal(6, "Mint")
  ) +
  labs(title = "B - Age of assemblages") +
  theme_map_af

# |- map_insitu ----
map_insitu_div_bsm <- 
  bsm_metrics_AF %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = bsm_mean_insitu_div)) +
  scale_fill_stepsn(
    breaks = seq(0, 1, length.out = 6),
    limits = c(0,1),
    name = "DR<sub>*in situ*</sub>", 
    colours = div_cols
  ) +
  labs(title = "A - In situ diversification") +
  theme_map_af

map_insitu_div_CV_bsm <- 
  bsm_metrics_AF %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = bsm_sd_insitu_div/bsm_mean_insitu_div)) +
  scale_fill_stepsn(
    #n.breaks = 6,
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
    limits = c(0,1),
    name = "CV DR<sub>*in situ*</sub>", 
    colours = div_cols
  ) +
  labs(title = "A - In situ diversification") +
  theme_map_af


map_insitu_div_bsm + map_age_bsm
