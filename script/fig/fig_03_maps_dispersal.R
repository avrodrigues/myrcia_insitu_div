
# load packages -----------------------------------------------------------

library(tidyverse)
library(rcartocolor)
library(patchwork)
library(sf)
library(here)

sf::sf_use_s2(FALSE)

# load data ---------------------------------------------------------

source(here("script", "fig", "fig_00_map_themes.R"))
myrcia_disp <- readRDS(here("output", "myrcia_disp.rds"))
dim(myrcia_disp)

# Atlantic Forest Integrated Limits - Muylaert et al 2018
AF_sf <- read_sf(
  here::here("data", "shape", "limits_AF", 
             "ma_limite_integrador_muylaert_et_al_2018_wgs84.shp"))

af_limits <- list(
  x = c(st_bbox(AF_sf)[1], -32),
  y = st_bbox(AF_sf)[c(2,4)]
)

site_xy <- evo_metrics_df[,1:2]
disp_contribution <- 1 - evo_metrics_df$div_insitu_prop

disp_df_wide <- bind_cols(
  site_xy, myrcia_disp, disp_contribution = disp_contribution
  )


# prepare data ------------------------------------------------------------


disp_sf <- st_as_sf(
  disp_df_wide,
  coords = c("x","y"), 
  crs = st_crs(AF_sf)
)

disp_metrics_AF <- st_filter(disp_sf, AF_sf)
site_xy_AF <- st_coordinates(metrics_AF$geometry)

disp_af_long <- 
  disp_metrics_AF %>% 
  mutate(
    x = site_xy_AF[,1],
    y = site_xy_AF[,2],
    .before = B
  ) %>% 
  pivot_longer(
    cols = 3:8,
    names_to = "disp_from",
    values_to = "disp_prop"
  ) %>% 
  mutate(
    disp_prop_weighted = disp_contribution * disp_prop
  )


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
    legend.title = element_markdown(
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
  filter(!disp_from %in% c("AB", "W")) %>%
  mutate(disp_from = glue::glue("Source: Evoregion {disp_from}")) %>% 
  ggplot() + 
  #coast_layer +
  geom_sf(data = AF_sf, fill = NA, color = greys[7]) +
  geom_raster(aes(x = x, y = y, fill = disp_prop_weighted)) +
  scale_fill_stepsn(
    breaks = seq(0, 0.80, 0.15) ,
    limits = c(0, 0.80),
    name = "1 - DR<sub>*in situ*</sub>",
    colours = rcartocolor::carto_pal(5, "Burg"), 
    guide = guide_colorsteps(show.limits = T), 
    na.value = "transparent"
  ) +
  facet_wrap("disp_from") +
  labs(title = "Contribution of evoregions as source of lineages for the assemblage") +
  theme_map_af +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0.01, size = 10, margin = margin(b = 2)), 
    plot.margin = margin(5,5,5,5)
  )


ggsave(
  here("fig", "disp_from.png"), 
  map_disp, 
  width = 7.2, 
  height = 9
)








