# load packages -----------------------------------------------------------

library(tidyverse)
library(rcartocolor)
library(patchwork)
library(sf)
library(here)

sf::sf_use_s2(FALSE)

# load data ---------------------------------------------------------

source(here("script", "fig", "fig_00_map_themes.R"))

evo_metrics_df <- read.csv("output/bg_stochastic_map/bsm_metrics.csv", row.names = 1)

myrcia_disp_data_bsm <- readRDS("output/myrcia_disp_BSM.rds")

myrcia_disp_bsm <- myrcia_disp_data_bsm$disp_from_df_mean 

# Atlantic Forest Integrated Limits - Muylaert et al 2018
AF_sf <- read_sf(
  here::here("data", "shape", "limits_AF", 
             "ma_limite_integrador_muylaert_et_al_2018_wgs84.shp"))

af_limits <- list(
  x = c(st_bbox(AF_sf)[1], -32),
  y = st_bbox(AF_sf)[c(2,4)]
)

site_xy <- evo_metrics_df[,1:2]
disp_contribution <- 1 - evo_metrics_df$div_bsm_mean

disp_df_wide <- bind_cols(
  site_xy, myrcia_disp_bsm, disp_contribution = disp_contribution
)

disp_df_sd <- bind_cols(site_xy, myrcia_disp_data_bsm$disp_from_df_sd)

# prepare data ------------------------------------------------------------


disp_sf <- st_as_sf(
  disp_df_wide,
  coords = c("x","y"), 
  crs = st_crs(AF_sf)
)

disp_sf_sd <- st_as_sf(
  disp_df_sd,
  coords = c("x","y"), 
  crs = st_crs(AF_sf)
)

disp_metrics_AF <- st_filter(disp_sf, AF_sf)
disp_sd_AF <- st_filter(disp_sf_sd, AF_sf)
site_xy_AF <- st_coordinates(metrics_AF$geometry)

disp_af_long <- 
  disp_metrics_AF %>% 
  mutate(
    x = site_xy_AF[,1],
    y = site_xy_AF[,2],
    .before = 1
  ) %>% 
  pivot_longer(
    cols = starts_with("mean_"),
    names_to = "disp_from",
    values_to = "disp_prop"
  ) %>% 
  mutate(
    disp_prop_weighted = disp_contribution * disp_prop, 
    disp_from = word(disp_from, 2, sep = "_")
  ) %>% 
  drop_na() 

disp_sd_af_long <- 
  disp_sd_AF %>% 
  mutate(
    x = site_xy_AF[,1],
    y = site_xy_AF[,2],
    .before = 1
  ) %>% 
  pivot_longer(
    cols = starts_with("sd_"),
    names_to = "disp_from",
    values_to = "disp_prop_sd"
  ) %>% 
  mutate(
    disp_from = word(disp_from, 2, sep = "_")
  ) %>% 
  drop_na()


# map theme ---------------------------------------------------------------


# |- coast_layer ----
coast_layer <- #list(
  geom_sf(data = sf_coast_pol, fill = greys[9], color = greys[3]) 

# |- theme_map_af ----
#theme_set(theme())

theme_map_af <- list(
  coord_sf(xlim = c(-58,-35), ylim = af_limits$y),
  theme(
    panel.background = element_rect(fill = bg, color = NA), 
    panel.border = element_rect(color = greys[4], fill = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid = element_blank(), 
    text = element_text(color = greys[1]), 
    title = element_text(color = greys[2]),
    axis.text = element_blank(), #element_text(color = greys[2], size = 8), 
    axis.ticks = element_blank(), #element_line(color = greys[3]), 
    axis.title = element_blank(),
    legend.text = element_text(color = greys[2], size = 8),
    legend.title = element_text(
      color = greys[2], face = "bold", size = 9, margin = margin(b = 2)
    ),
    legend.position = "right",
    legend.key.height = unit(20, "points"), 
    legend.key.width = unit(9, "points"),
    legend.background = element_blank()
  ), 
  guides(
    fill = guide_colorsteps(
      show.limits = T, 
      title.vjust = 1,
      title.position = "right", 
      label.position = "left")
  )
  
)



# Mapping -----------------------------------------------------------------

map_disp <- 
  disp_af_long %>%
  mutate(disp_from = glue::glue("Source: Evoregion {disp_from}")) %>% 
  ggplot() + 
  #coast_layer +
  geom_sf(data = AF_sf, fill = NA, color = greys[7]) +
  geom_raster(aes(x = x, y = y, fill = disp_prop_weighted)) +
  scale_fill_stepsn(
    breaks = seq(0, 0.80, 0.15) ,
    limits = c(0, 0.80),
    name = "1 - DR in situ",
    colours = rcartocolor::carto_pal(5, "Burg"), 
    guide = guide_colorsteps(show.limits = T), 
    na.value = "transparent"
  ) +
  facet_wrap("disp_from") +
  labs(title = "Contribution of evoregions as source of lineages for the assemblage") +
  theme_map_af +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0.01, size = 8, margin = margin(b = 2)), 
    plot.margin = margin(5,5,5,5)
  )


map_disp_sd <- 
  disp_sd_af_long %>%
  mutate(disp_from = glue::glue("Source: Evoregion {disp_from}")) %>% 
  ggplot() + 
  #coast_layer +
  geom_sf(data = AF_sf, fill = NA, color = greys[7]) +
  geom_raster(aes(x = x, y = y, fill = disp_prop_sd)) +
  scale_fill_stepsn(
    # breaks = seq(0, 0.80, 0.15) ,
    # limits = c(0, 0.80),
    # name = "1 - DR in situ",
    colours = rcartocolor::carto_pal(5, "Burg"), 
    guide = guide_colorsteps(show.limits = T), 
    na.value = "transparent"
  ) +
  facet_wrap("disp_from") +
  labs(title = "Contribution of evoregions as source of lineages for the assemblage") +
  theme_map_af +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0.01, size = 8, margin = margin(b = 2)), 
    plot.margin = margin(5,5,5,5)
  )


ggsave(
  here("fig", "disp_from_v3.png"), 
  map_disp, 
  width = 7.2, 
  height = 9
)








