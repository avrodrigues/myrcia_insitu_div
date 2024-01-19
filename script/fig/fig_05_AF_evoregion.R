
# load packages -----------------------------------------------------------

library(tidyverse)
library(rcartocolor)
library(patchwork)
library(sf)
library(here)


# load data and theme ---------------------------------------------------------

source(here("script", "fig", "fig_00_map_themes.R"))
myrcia_disp <- readRDS(here("output", "myrcia_disp.rds"))
theme_set(theme_bw())

evo_metrics_2 <- bind_cols(
  evo_metrics_df,  myrcia_disp
)

sf::sf_use_s2(FALSE)

# Atlantic Forest Integrated Limits - Muylaert et al 2018
AF_sf <- read_sf(
  here::here("data", "shape", "limits_AF",
             "ma_limite_integrador_muylaert_et_al_2018_wgs84.shp"))


af_limits <- list(
  x = c(st_bbox(AF_sf)[1], -32),
  y = st_bbox(AF_sf)[c(2,4)]
)

evo_metrics_sf <- st_as_sf(
  evo_metrics_2,
  coords = c("x","y"), 
  crs = st_crs(AF_sf)
)

metrics_AF <- st_filter(evo_metrics_sf, AF_sf)
site_xy_AF <- st_coordinates(metrics_AF$geometry)

metrics_AF <- 
  metrics_AF %>% 
  mutate(
    x = site_xy_AF[,1],
    y = site_xy_AF[,2],
    .before = evoregion
  )

# |- coast_layer ----
coast_layer <- #list(
  geom_sf(data = sf_coast_pol, fill = greys[9], color = greys[3]) 



# map evoregion -----------------------------------------------------------

map_insert_evo <- 
evo_metrics_df %>%
  ggplot() + 
  coast_layer +
   geom_raster(aes(x = x, y = y, fill = evoregion), show.legend = F) +
   scale_fill_manual(values = colors_evo) + 
  #geom_sf(data = AF_sf, alpha = 0.8, fill = greys[2]) +
  coord_sf(
    xlim = map.limits$x, # c(-90, -34), 
    ylim = map.limits$y #c(-55, 15)) +
  ) +
  theme_evoregions +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank()
  )

map_insert_af <-
  evo_metrics_df %>%
  ggplot() +
  coast_layer +
  #geom_raster(aes(x = x, y = y, fill = evoregion)) +
  #scale_fill_manual(values = colors_evo) +
  geom_sf(data = AF_sf, alpha = 0.8, fill = greys[2]) +
  coord_sf(
    xlim = c(-90, -34),
    ylim = c(-55, 15)
  ) +
  theme_evoregions +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

map_insert_evo + map_insert_af

map_evo_af <- 
metrics_AF %>%
  ggplot() +
  geom_sf(data = sf_coast_pol, fill = greys[9], color = greys[3]) +
  geom_raster(aes(x = x, y = y, fill = evoregion)) +
  scale_fill_manual(
    values = colors_evo, 
    name = "Evoregions", 
    limits = LETTERS[1:5],
  ) +
  coord_sf(xlim = c(-60,-33), ylim = af_limits$y,
    label_axes = "-NE-") +
  theme_evoregions +
  theme(legend.background = element_blank()) +
  guides(fill = guide_legend(title.position = "top"))

map_evo_af_2 <- 
map_evo_af %>% 
  lemon::reposition_legend(
    "bottom right", 
    x = 0.95,
    y = 0.025,
    #offset = 0.025
    just = 1,
    plot = T
  )

map_insert <- map_insert_af + map_insert_evo

chart_evo_af <- 
  wrap_plots(map_evo_af_2) + inset_element(
  map_insert, left = 0.075, bottom = 0.65, right = 0.47, top = .995)

# Save maps ---------------------------------------------------------------
ggsave(
  here("fig", "map_evo_af.png"),
  map_evo_af_2, 
  width = 6.5,
  height = 8
)

ggsave(
  here("fig", "map_insert_af.png"),
  map_insert_af, 
  width = 5,
  height = 6.5
)

ggsave(
  here("fig", "map_insert_evo.png"),
  map_insert_evo, 
  width = 5,
  height = 4.5
)
