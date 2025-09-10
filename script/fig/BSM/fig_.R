
# load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(sf)
library(rcartocolor)
library(patchwork)
library(ggplot2)
library(ggtext)


source(here("script", "fig", "fig_00_map_themes.R"))

sf::sf_use_s2(FALSE)
# load data ---------------------------------------------------------------

bsm_metrics <- read.csv("output/bg_stochastic_map/bsm_metrics.csv", row.names = 1)

mpd_data <- readRDS("output/ses_mpd_myrcia.rds")


evo_metrics_df <- data.frame(bsm_metrics, ses_mpd = mpd_data$mpd.obs.z)

cor(na.omit(evo_metrics_df[, -c(1,2,3)]))


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
    .before = age_bsm_mean
  )

# map customization -------------------------------------------------------

# |- theme_map_af ----
text_size <- 6

theme_map_af <- list(
  geom_sf(data = AF_sf, fill = NA, color = greys[7]),
  coord_sf(xlim = c(-58,-35), ylim = af_limits$y),
  theme_void(),
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    panel.background = element_rect(fill = NA, color = NA), 
    panel.grid = element_blank(), 
    text = element_text(color = greys[1], size = text_size*1.1), 
    plot.title = element_text(
      color = greys[2], 
      margin = margin(b=5, t = 5),
      hjust = 0.5
    ),
    # axis.text = element_text(color = greys[2], size = 8), 
    # axis.ticks = element_line(color = greys[3]), 
    # panel.border = element_rect(color = greys[4], fill = NA), 
    axis.title = element_blank(),
    legend.text = element_text(color = greys[2], size = text_size*0.9),
    legend.title = element_text(
      color = greys[2], face = "bold", size = text_size, margin = margin(b = 2)
    ),
    legend.position = "bottom",
    legend.key.height = unit(5, "points"), 
    legend.key.width = unit(14, "points"),
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
cont_cols <- rcartocolor::carto_pal(8, "Sunset")

# maps --------------------------------------------------------------------

# |- map_age ----
map_age <- 
  evo_metrics_AF %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = age_bsm_mean)) +
  scale_fill_stepsn(
    breaks = seq(0, 15, 3),
    limits = c(0,15),
    name = "Mean age (BSM)" ,
    colours = cont_cols
  ) +
  labs(title = "Age of assemblages") +
  theme_map_af


# check range values
range(evo_metrics_AF$age_bsm_sd/evo_metrics_AF$age_bsm_mean)

# add values arround it to the code below
break_vec_age <- exp(seq(log(0.1), log(4.1), length.out = 5)) %>% 
  round(2)

map_age_cv <- 
  evo_metrics_AF %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = age_bsm_sd/age_bsm_mean)) +
  scale_fill_stepsn(
    breaks = break_vec_age,
    limits = range(break_vec_age),
    name = "CV age of assemblage (BSM)" ,
    colours = cont_cols
  ) +
  labs(title = "Age of assemblages") +
  theme_map_af


# |- map_jetz ----

map_DR_jetz <- 
  evo_metrics_AF %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = DR_jetz)) +
  scale_fill_stepsn(
    #breaks = seq(0, 1, length.out = 6),
    #limits = c(0,1),
    n.breaks = 8,
    name = "DR Jetz", 
    colours = cont_cols
  ) +
  labs(title = "Diversification") +
  theme_map_af

# |- map_insitu ----

map_DR_insitu_bsm_mean <- 
  evo_metrics_AF %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = DR_insitu_bsm_mean)) +
  scale_fill_stepsn(
    limits = c(0.08,.18), #one outlier value (0.38) was removed from the map 
    n.breaks = 7,
    name = "Mean DR in situ (BSM)", 
    colours = cont_cols, 
    na.value = "NA"
  ) +
  labs(title = "In situ diversification") +
  theme_map_af

map_DR_insitu_bsm_cv <- 
  evo_metrics_AF %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = DR_insitu_bsm_sd/DR_insitu_bsm_mean)) +
  scale_fill_stepsn(
    n.breaks = 6,
    #breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
    #limits = c(0,1),
    name = "CV DR in situ (BSM)", 
    colours =  cont_cols,
    na.value = "transparent"
  ) +
  labs(title = "In situ diversification") +
  theme_map_af

# |- map_prop ----

map_DR_prop_bsm_mean <- 
  evo_metrics_AF %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = DR_prop_bsm_mean)) +
  scale_fill_stepsn(
    breaks = seq(0, 1, length.out = 6),
    limits = c(0,1),
    #n.breaks = 8,
    name = "Mean DR prop (BSM)", 
    colours = div_cols
  ) +
  labs(title = "Prop. in situ diversification") +
  theme_map_af

map_DR_prop_bsm_cv <- 
  evo_metrics_AF %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = DR_prop_bsm_sd/DR_prop_bsm_mean)) +
  scale_fill_stepsn(
    n.breaks = 6,
    name = "CV DR prop (BSM)", 
    na.value = "transparent",
    colours =  cont_cols,
    limits = c(0,.4)
  ) +
  labs(title = "Proportional in situ diversification") +
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
  labs(title = "Phylogenetic structure") +
  theme_map_af

# |- map_richness ----
map_richness <- 
  metrics_AF %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = richness)) +
  scale_fill_stepsn(
    limits = c(3, 55),
    name = "# Species", 
    colours = cont_cols
  ) +
  labs(title = "Species richness") +
  theme_map_af



# |- charts -------


# Function to arrange ggplots horizontally
combine_inset_plots <- function(
    plot_list,
    panel_size = 0.21,
    overlap = 0.02, 
    bg = "#FAF8F4") {
  n <- length(plot_list)
  
  # allocate start and end positions
  start <- numeric(n)
  end <- numeric(n)
  
  init_value <- 0
  
  for (i in seq_len(n)) {
    if (i == 1) {
      start[i] <- init_value
      end[i] <- init_value + panel_size
    } else {
      start[i] <- end[i-1] - overlap
      end[i] <- start[i] + panel_size
    }
  }
  
  # center them
  offset <- (1 - end[n]) / 2
  start <- start + offset
  end <- end + offset
  
  # initialize empty ggplot
  base_plot <- ggplot() + 
    theme(
      plot.background = element_rect(fill = bg, color = "grey70"), 
      panel.background = element_rect(fill = bg, color = NA)
      )
  
  # add inset plots iteratively
  for (i in seq_len(n)) {
    base_plot <- base_plot +
      inset_element(plot_list[[i]], start[i], 0, end[i], 1)
  }
  
  return(base_plot)
}


# |- insitu figure main text -----


list_chart_insitu_BSM <- list(
  map_prop_div = map_DR_prop_bsm_mean, 
  map_age = map_age, 
  map_mpd = map_mpd, 
  map_richness = map_richness
)

chart_insitu <- combine_inset_plots(
  list_chart_insitu_BSM, 
  panel_size = 0.26,
  overlap = 0.03
  )

ggsave(
  here("fig", "chart_insitu_BSM.png"), 
  chart_insitu, 
  width = 7,
  height = 3.5
)


# |- comparison of DR metrics ----
list_chart_DR_metrics <- list(
  map_DR_jetz,
  map_DR_insitu_bsm_mean, 
  map_DR_prop_bsm_mean
)

chart_DR <- combine_inset_plots(
  list_chart_DR_metrics, 
  panel_size = .36, 
  overlap = .04)

ggsave(
  here("fig", "chart_DR_metrics.png"), 
  chart_DR, 
  width = 6,
  height = 3.2
)


# |- BSM uncertainty ----
list_chart_BSM_uncertainty <- list(
  map_age_cv,
  map_DR_insitu_bsm_cv, 
  map_DR_prop_bsm_cv
)

chart_uncertainty <- combine_inset_plots(
  list_chart_BSM_uncertainty, 
  panel_size = .35, 
  overlap = .04)


ggsave(
  here("fig", "chart_uncertainty.png"), 
  chart_uncertainty, 
  width = 7,
  height = 4
)

# |- uncertainty summary

evo_metrics_AF %>% 
  mutate(
    cv_age = age_bsm_sd/age_bsm_mean, 
    cv_DR_insitu = DR_insitu_bsm_sd/DR_insitu_bsm_mean,
    cv_DR_prop = DR_prop_bsm_sd/DR_prop_bsm_mean
  ) %>% 
  summarise(
    across(starts_with("cv"), ~median(.x, na.rm = T))
  )


  

