
# load packages -----------------------------------------------------------

library(tidyverse)
library(biscale)
library(cowplot)
library(rcartocolor)
library(viridis)
library(patchwork)
library(sf)
library(here)


# load data and theme ---------------------------------------------------------

source(here("script", "fig", "fig_00_map_themes.R"))


# maps --------------------------------------------------------------------
map_jetz <- 
  evo_metrics_df %>%
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = div_jetz)) +
  geom_sf(data = sf_coast, color = greys[3]) +
  scale_fill_stepsn(
    # breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
    # limits = c(0,1),
    name = "*In situ* DR", 
    colours = rcartocolor::carto_pal(5, "PurpOr"), 
    guide = guide_colorsteps(show.limits = T)
  ) +
  theme_map_continuous +
  theme(legend.position = "left")

map_insitu <- 
  evo_metrics_df %>%
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = div_insitu_prop)) +
  geom_sf(data = sf_coast, color = greys[3]) +
  scale_fill_stepsn(
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
    limits = c(0,1),
    name = "*In situ* DR", 
    colours = rcartocolor::carto_pal(5, "Earth") %>% rev(), 
    guide = guide_colorsteps(show.limits = T)
  ) +
  theme_map_continuous 

map_age <- 
evo_metrics_df %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = age_halfedge)) +
  geom_sf(data = sf_coast, color = greys[3]) +
  scale_fill_stepsn(
    n.breaks = 5,
    name = "Mean age<br>of assemblage" ,
    colours = rcartocolor::carto_pal(5, "BluYl"), 
    guide = guide_colorsteps(show.limits = T)
  ) +
  theme_map_continuous

map_mpd <- 
  evo_metrics_df %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = ses_mpd)) + 
  geom_sf(data = sf_coast, color = greys[3]) +
  scale_fill_stepsn(
      breaks = c(-4.2, -3.0, -1.8, -0.6,  0.6,  1.8),
      limits = c(-4.2, 1.8),
      name = "SES MPD" ,
      colours = rcartocolor::carto_pal(7, "Geyser")[1:5], 
      guide = guide_colorsteps(show.limits = T)
    )+
  theme_map_continuous 

insitu_chart <- map_insitu + map_age + map_mpd +
  plot_annotation(tag_levels = "A")

ggsave(
  here("fig", "insitu_chart.png"),
  insitu_chart,
  width = 15, 
  height = 5
)

#################################

data_bi <- 
  evo_metrics_df %>% 
  bi_class(x = ses_mpd, y = div_insitu_prop, style = "equal", dim = 3)

map <- 
  ggplot() +
  geom_raster(data = data_bi, mapping = aes(x = x, y = y, fill = bi_class), show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink2", dim = 3) +
  theme_bw() +
  coord_equal()

legend <- 
  bi_legend(pal = "GrPink2",
            dim = 3,
            ylab = "In Situ Div",
            xlab = "SES_MPD ",
            size = 8)

bi_map <- 
  ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.1, .1, 0.3, 0.3)
