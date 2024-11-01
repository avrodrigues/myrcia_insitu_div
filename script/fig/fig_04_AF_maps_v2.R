
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
# |- map_insitu_div ----
map_insitu_div <- 
  metrics_AF %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = div_insitu_prop)) +
  scale_fill_stepsn(
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
    limits = c(0,1),
    name = "DR<sub>*in situ*</sub>", 
    colours = div_cols
  ) +
  labs(title = "A - In situ diversification") +
  theme_map_af

# |- map_age ----
map_age <- 
  metrics_AF %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = age_halfedge)) +
  scale_fill_stepsn(
    breaks = seq(0, 15, 3),
    limits = c(0,15),
    name = "Mean age of assemblage" ,
    colours = rcartocolor::carto_pal(6, "Mint")
  ) +
  labs(title = "B - Age of assemblages") +
  theme_map_af

# |- map_mpd ----
map_mpd <- 
  metrics_AF %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = ses_mpd)) +
  scale_fill_stepsn(
    breaks = seq(-4.2, 1.8, length.out = 6),
    limits = c(-4.2, 1.8),
    name = "SES MPD", 
    colours = div_cols[1:6]
  ) +
  labs(title = "C - Phylogenetic structure") +
  theme_map_af


# |- map_richness ----
map_richness <- 
metrics_AF %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = richness)) +
  scale_fill_stepsn(
    name = "\\# Species", 
    colours = rcartocolor::carto_pal(5, "Mint")
  ) +
  labs(title = "D - Species richness") +
  theme_map_af




# |- map_jetz ----
map_jetz <- 
metrics_AF %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = div_jetz*10)) +
  scale_fill_stepsn(
    breaks = c(0.70, 0.85, 1.00, 1.15, 1.30),
    limits = c(0.7, 1.3),
    name = "DR [x10]" ,
    colours = rcartocolor::carto_pal(5, "Geyser")#[1:5]
  ) +
  geom_sf(data = AF_sf, fill = NA, color = greys[2]) +
  theme_map_af


# |- chart in situ div ----

map_list <- list(
  map_insitu_div = map_insitu_div, 
  map_age = map_age, 
  map_mpd = map_mpd, 
  map_richness = map_richness
  )

## defining the horizontal position ----
start <- numeric(4)
end <- numeric(4)

init_value <- 0
panel_size <- 0.26
overlap <- 0.03

for(i in 1:4){
  if(i == 1){
    start[i] <- init_value 
    end[i] <- init_value + panel_size
  } else {
    start[i] <- end[i-1] - overlap
    end[i] <- start[i] + panel_size
  }
  
}

offset <- (1-end[4])/2
start <- start + offset
end <- end + offset

## producing the chart ----

chart_insitu <- ggplot() + 
  theme(
    plot.background = element_rect(fill = bg, color = "grey70")
  ) +
  inset_element(
    map_insitu_div, #map_list_leg_in[[1]],
    start[1], 0, end[1], 1
    ) +
  inset_element(
    map_age, #map_list_leg_in[[2]],
    start[2], 0, end[2], 1
  ) +
  inset_element(
    map_mpd, #map_list_leg_in[[3]],
    start[3], 0, end[3], 1
  ) +
  inset_element(
    map_richness, #map_list_leg_in[[4]],
    start[4], 0, end[4], 1
  ) 
 



ggsave(
  here("fig", "chart_insitu_v2.png"), 
  chart_insitu, 
  width = 6,
  height = 3.2
)


