
# load packages -----------------------------------------------------------

library(tidyverse)
library(glue)
library(rcartocolor)
library(patchwork)
library(sf)
library(here)


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


# map customization -------------------------------------------------------

# |- coast_layer ----
coast_layer <- #list(
  geom_sf(data = sf_coast_pol, fill = greys[9], color = greys[3]) 

# |- theme_map_af ----
theme_map_af <- list(
  coord_sf(xlim = c(-58,-35), ylim = af_limits$y),
  theme(
    panel.background = element_rect(fill = bg, color = NA), 
    panel.grid = element_blank(), 
    text = element_text(color = greys[1]), 
    plot.title = element_markdown(
      color = greys[2], 
      margin = margin(b=5, t = 5),
      hjust = 0.01
    ),
    #axis.text = element_text(color = greys[2], size = 8), 
    #axis.ticks = element_line(color = greys[3]), 
    panel.border = element_rect(color = greys[4], fill = NA), 
    axis.title = element_blank(),
    legend.text = element_text(color = greys[2], size = 8),
    legend.title = element_markdown(
      color = greys[2], face = "bold", size = 9, margin = margin(b = 2)
    ),
    legend.position = "left",
    legend.key.height = unit(20, "points"), 
    legend.key.width = unit(9, "points"),
    legend.background = element_blank()
  ), 
  guides(
    fill = guide_colorsteps(
      show.limits = T, 
      title.vjust = 1,
      title.position = "right", 
      label.position = "left"))
)

# |- divergent color palette

div_cols <- rcartocolor::carto_pal(7, "Temps")

# maps --------------------------------------------------------------------

# |- map_mpd ----
map_mpd <- 
  metrics_AF %>% 
  ggplot() + 
  #coast_layer +
  geom_sf(data = AF_sf, fill = NA, color = greys[7]) +
  geom_raster(aes(x = x, y = y, fill = ses_mpd)) +
  scale_fill_stepsn(
    breaks = seq(-4.2, 1.8, length.out = 6),
    limits = c(-4.2, 1.8),
    name = "SES MPD", 
    colours = div_cols[1:6]
  ) +
  labs(title = "B - Phylogenetic structure") +
  theme_map_af


## |- map_richness ----
map_richness <- 
metrics_AF %>% 
  ggplot() + 
  #coast_layer +
  geom_sf(data = AF_sf, fill = NA, color = greys[7]) +
  geom_raster(aes(x = x, y = y, fill = richness)) +
  scale_fill_stepsn(
  #  breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
  #  limits = c(0,1),
    name = "\\# Species", 
    colours = rcartocolor::carto_pal(5, "PurpOr")
  ) +
  labs(title = "A - Species richness") +
  theme_map_af

# |- map_insitu_div ----
map_insitu_div <- 
metrics_AF %>% 
  ggplot() + 
  #coast_layer +
  geom_sf(data = AF_sf, fill = NA, color = greys[7]) +
  geom_raster(aes(x = x, y = y, fill = div_insitu_prop)) +
  scale_fill_stepsn(
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
    limits = c(0,1),
    name = "DR<sub>*in situ*</sub>", 
    colours = rcartocolor::carto_pal(7, "Temps")[2:6]
  ) +
  labs(title = "C - *In situ* diversification") +
  theme_map_af

# |- map_age ----
map_age <- 
metrics_AF %>% 
  ggplot() + 
  #coast_layer +
  geom_sf(data = AF_sf, fill = NA, color = greys[7]) +
  geom_raster(aes(x = x, y = y, fill = age_halfedge)) +
  scale_fill_stepsn(
    breaks = seq(0, 15, 3),
    limits = c(0,15),
    name = "Mean age<br>of assemblage" ,
    colours = rcartocolor::carto_pal(6, "Mint")[1:6]
  ) +
  labs(title = "D - Age of assemblages") +
  theme_map_af

# |- map_jetz ----
map_jetz <- 
metrics_AF %>% 
  ggplot() + 
  #coast_layer +
  geom_sf(data = AF_sf, fill = NA, color = greys[7]) +
  geom_raster(aes(x = x, y = y, fill = div_jetz*10)) +
  scale_fill_stepsn(
    breaks = c(0.70, 0.85, 1.00, 1.15, 1.30),
    limits = c(0.7, 1.3),
    name = "DR [x10]" ,
    colours = rcartocolor::carto_pal(5, "PinkYl")#[1:5]
  ) +
  theme_map_af


# |- chart in situ div ----

map_list <- list(
  map_richness = map_richness,
  map_mpd = map_mpd, 
  map_insitu_div = map_insitu_div, 
  map_age = map_age  
  )

place_legend_in <- function(map, plot = FALSE){
  map %>% 
  lemon::reposition_legend(
    "top left",
    #x = 0.02,
    #y = 0.025,
    offset = 0.075,
    just = 1,
    plot = plot
  )
}

map_list_leg_in <- map(1:length(map_list), function(i){
  place_legend_in(map_list[[i]], plot = F)
})

chart_insitu <- wrap_plots(map_list_leg_in) +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  here("fig", "chart_insitu_v2.png"), 
  chart_insitu, 
  width = 8,
  height = 11.5
)


# New map - 3D ---------------------------

(metrics_AF$age_recent/max(metrics_AF$age_recent))|> hist()

metrics_AF$div_insitu_prop|> hist()

(metrics_AF$ses_mpd/(max(abs(metrics_AF$ses_mpd)))) |> hist()

max_sesmpd <- (max(abs(metrics_AF$ses_mpd)))
min_sesmpd <- max_sesmpd*-1
range_sesmpd <- max_sesmpd-min_sesmpd

mpd_01 <- ((metrics_AF$ses_mpd - min_sesmpd)/range_sesmpd)
age_01 <- (metrics_AF$age_recent/max(metrics_AF$age_recent))
div_01 <- metrics_AF$div_insitu_prop

df.metareg.cont <- data.frame(
  
  Div = metrics_AF$div_insitu_prop, 
  Age = mpd_01,
  SES.MPD = mpd_01)

# defining colors
rgb.color  <- rgb(age_01, div_01, mpd_01)
rgb.color.fill <- (unique(rgb.color))
names(rgb.color.fill) <- rgb.color.fill

metrics_AF$summ_color <- rgb.color

metrics_AF %>% 
  ggplot() + 
  #coast_layer +
  geom_raster(aes(x = x, y = y, fill = summ_color), show.legend = F) +
  geom_sf(data = AF_sf, fill = NA, color = greys[1]) +
  scale_fill_manual(values = rgb.color.fill) 
  theme_map_af
