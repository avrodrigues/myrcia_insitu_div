# load packages -----------------------------------------------------------

library(tidyverse)
library(glue)
library(rcartocolor)
library(patchwork)
library(sf)
library(here)
library(GGally)


# load data and theme ---------------------------------------------------------

source(here("script", "fig", "fig_00_map_themes.R"))
myrcia_disp <- readRDS(here("output", "myrcia_disp.rds"))
theme_set(theme_void())

# species composition
myrcia_comp <- read.csv(here("data", "W.csv"))

richness <- rowSums(myrcia_comp)

evo_metrics_2 <- bind_cols(
  evo_metrics_df, richness = richness, myrcia_disp
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


# prepare data and plots --------------------------------------------------


names(metrics_AF)

metrics_AF %>% 
  ggplot() +
  geom_boxplot(aes(x = evoregion, y = div_insitu_prop)) +
  theme_bw()


metrics_AF %>% 
  filter(evoregion %in% c("A", "B")) %>% 
  as_tibble() %>% 
  select(richness, ses_mpd, age_halfedge, div_insitu_prop, evoregion) %>% 
ggpairs(columns = 1:4,        # Columns
        aes(color = evoregion,  # Color by group (cat. variable)
            alpha = 0.5)) +
  scale_color_manual(values = colors_evo) +
  scale_fill_manual(values = colors_evo)


