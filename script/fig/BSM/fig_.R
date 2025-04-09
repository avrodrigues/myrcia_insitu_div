
# load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(sf)
library(rcartocolor)
library(patchwork)
library(ggplot2)

source(here("script", "fig", "fig_00_map_themes.R"))

sf::sf_use_s2(FALSE)
# load data ---------------------------------------------------------------

evo_df <- read.csv(here("data", "evoregions_stand_names_df.csv"))
site_xy <- evo_df[,1:2]

avg_age_half_df <- read.csv("output/bg_stochastic_map/avg_age_half_df.csv", row.names = 1)
avg_age_recent_df <- read.csv("output/bg_stochastic_map/avg_age_recent_df.csv", row.names = 1)
insitu_div_df <- read.csv("output/bg_stochastic_map/div_insitu_prop_df.csv", row.names = 1)


evo_metrics_df <- readRDS("output/evo_metrics_df_BSM.rds")
  
  
cor(evo_metrics_df[, -c(1,2)])


# Atlantic Forest Integrated Limits - Muylaert et al 2018
AF_sf <- read_sf(
  here::here("data", "shape", "limits_AF", 
             "ma_limite_integrador_muylaert_et_al_2018_wgs84.shp"))


af_limits <- list(
  x = c(st_bbox(AF_sf)[1], -32),
  y = st_bbox(AF_sf)[c(2,4)]
)

evo_metrics_sf <- st_as_sf(
  evo_metrics_df,
  coords = c("x","y"), 
  crs = st_crs(AF_sf)
)

evo_metrics_AF <- st_filter(evo_metrics_sf, AF_sf)
site_xy_AF <- st_coordinates(evo_metrics_AF$geometry)

evo_metrics_AF <- 
  evo_metrics_AF %>% 
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
map_age_recent_bsm <- 
  evo_metrics_AF %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = bsm_mean_recent)) +
  scale_fill_stepsn(
    breaks = seq(0, 15, 3),
    limits = c(0,15),
    name = "Mean age of assemblage (BSM)" ,
    colours = rcartocolor::carto_pal(6, "Mint")
  ) +
  labs(title = "B - Age of assemblages") +
  theme_map_af


map_age_sd_recent_bsm <- 
  evo_metrics_AF %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = bsm_sd_recent)) +
  scale_fill_stepsn(
    n.breaks = 5,
    #breaks = seq(0, 15, 3),
    #limits = c(0,15),
    name = "SD age of assemblage (BSM)" ,
    colours = rcartocolor::carto_pal(6, "Mint")
  ) +
  labs(title = "B - Age of assemblages (Recent)") +
  theme_map_af

map_age_half_bsm <- 
  evo_metrics_AF %>% 
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


map_age_sd_half_bsm <- 
  evo_metrics_AF %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = bsm_sd_half)) +
  scale_fill_stepsn(
    n.breaks = 5,
    #breaks = seq(0, 15, 3),
    #limits = c(0,15),
    name = "SD Mean age of assemblage (BSM)" ,
    colours = rcartocolor::carto_pal(6, "Mint")
  ) +
  labs(title = "B - Age of assemblages (half)") +
  theme_map_af

# |- map_insitu ----
map_insitu_div_bsm <- 
  evo_metrics_AF %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = bsm_mean_insitu_div)) +
  scale_fill_stepsn(
    breaks = seq(0, 1, length.out = 6),
    limits = c(0,1),
    name = "Mean DR<sub>*in situ*</sub> (BSM)", 
    colours = div_cols
  ) +
  labs(title = "A - In situ diversification") +
  theme_map_af

map_insitu_div_sd_bsm <- 
  evo_metrics_AF %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = bsm_sd_insitu_div)) +
  scale_fill_stepsn(
    n.breaks = 6,
    #breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
    #limits = c(0,1),
    name = "CV DR<sub>*in situ*</sub>", 
    colours = div_cols
  ) +
  labs(title = "A - In situ diversification") +
  theme_map_af

map_mpd <- 
  evo_metrics_AF %>% 
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









# |- chart in situ div ----

map_list <- list(
  map_insitu_div = map_insitu_div_bsm, 
  map_age = map_age_bsm, 
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
    map_insitu_div_bsm, #map_list_leg_in[[1]],
    start[1], 0, end[1], 1
  ) +
  inset_element(
    map_age_half_bsm, #map_list_leg_in[[2]],
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
  here("fig", "chart_insitu_BSM.png"), 
  chart_insitu, 
  width = 6,
  height = 3.2
)

### uncertainties chart

start_uc <- numeric(3)
end_uc <- numeric(3)

init_value <- 0
panel_size <- 0.35
overlap <- 0.03

for(i in 1:3){
  if(i == 1){
    start_uc[i] <- init_value 
    end_uc[i] <- init_value + panel_size
  } else {
    start_uc[i] <- end_uc[i-1] - overlap
    end_uc[i] <- start_uc[i] + panel_size
  }
  
}


uc_chart <- ggplot() + 
 theme(
    plot.background = element_rect(fill = bg, color = "grey70")
  ) +
  inset_element(
    map_insitu_div_sd_bsm, #map_list_leg_in[[1]],
    start_uc[1], 0, end_uc[1], 1
  ) +
  inset_element(
    map_age_sd_half_bsm, #map_list_leg_in[[2]],
    start_uc[2], 0, end_uc[2], 1
  ) +
  inset_element(
    map_age_sd_recent_bsm, #map_list_leg_in[[3]],
    start_uc[3], 0, end_uc[3], 1
  )
