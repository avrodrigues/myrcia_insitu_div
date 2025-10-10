
# load packages -----------------------------------------------------------

library(tidyverse)
library(rcartocolor)
library(patchwork)
library(sf)
library(here)
library(terra)
library(tidyterra)
library(scales)
library(geobr)
library(geodata)


# load data and theme ---------------------------------------------------------

source(here("script", "fig", "fig_00_map_themes.R"))
myrcia_disp <- readRDS(here("output", "myrcia_disp.rds"))
theme_set(theme_bw())

evo_metrics_2 <- bind_cols(
  evo_metrics_df
)

sf::sf_use_s2(FALSE)

# Atlantic Forest Integrated Limits - Muylaert et al 2018
AF_sf <- read_sf(
  here::here("data", "shape", "limits_AF",
             "ma_limite_integrador_muylaert_et_al_2018_wgs84.shp"))

mydir <- "data/elevation"

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


## Elevation
r_init <- elevation_global(res = 5, path = mydir)
names(r_init) <- "alt"

elev_af_crop <- r_init %>%
  crop(AF_sf)

## Create hillshade effect

slope <- terrain(elev_af_crop, "slope", unit = "radians")
aspect <- terrain(elev_af_crop, "aspect", unit = "radians")
hill <- shade(slope, aspect, 30, 330, normalize=TRUE)

# normalize names
names(hill) <- "shades"

# Hillshading, but we need a palette
pal_greys <- hcl.colors(1000, "Grays")

# Use a vector of colors

hill <- hill %>% 
  mask(AF_sf)

elev_af <- elev_af_crop %>% 
  mask(AF_sf)

index <- hill %>%
  mutate(index_col = rescale(shades, to = c(1, length(pal_greys)))) %>%
  mutate(index_col = round(index_col)) %>%
  pull(index_col)


# Get cols
vector_cols <- pal_greys[index]

# Need to avoid resampling
# and dont use aes

hill_plot <- ggplot() +
  geom_spatraster(
    data = hill, fill = vector_cols, maxcell = Inf,
    alpha = 1
  )

r_limits <- minmax(elev_af_crop) %>% as.vector()

# Rounded to lower and upper 500
r_limits <- c(floor(r_limits[1] / 500), ceiling(r_limits[2] / 500)) * 500

# And making min value to 0.
r_limits <- pmax(r_limits, 0)

bg <- "#FAF8F4"



# map theme ---------------------------------------------------------------

theme_map_ <- function(base_text_size = 10) {
  theme_void() +
    theme(
      plot.background = element_rect("transparent", colour = NA),
      panel.background = element_rect("transparent", colour = NA),
      legend.position = "right",
      legend.title = element_text(size = base_text_size*1.2 ),
      legend.text = element_text(size = base_text_size ),
      legend.key = element_rect("grey30"),
      legend.background = element_rect(fill = "transparent", color = NA), 
      legend.box.background = element_rect(fill = "transparent", color = NA)  
    )
}



# map evoregion -----------------------------------------------------------

(map_evo <- 
evo_metrics_df %>%
  ggplot() + 
  coast_layer +
   geom_raster(aes(x = x, y = y, fill = phy_con), show.legend = T) +
   scale_fill_manual(values = colors_evo) + 
  #geom_sf(data = AF_sf, color = greys[5], fill = NA, linewidth = 1) +
  coord_sf(
    xlim = map.limits$x, # c(-90, -34), 
    ylim = map.limits$y #c(-55, 15)) +
  ) +
  theme_map_()
)


# map AF ------------------------------------------------------------------

map_SA_af <-
  evo_metrics_df %>%
  ggplot() +
  coast_layer +
  #geom_raster(aes(x = x, y = y, fill = evoregion)) +
  #scale_fill_manual(values = colors_evo) +
  geom_sf(data = AF_sf, alpha = 0.8, fill = greys[2]) +
  coord_sf(
    xlim = c(-80, -37),
    ylim = c(-55, 10)
  ) +
  theme_map_()

# Map_evo_af ---- 
map_evo_af <- 
data.frame(st_coordinates(metrics_AF), metrics_AF) %>%
  ggplot() +
  #geom_sf(data = sf_coast_pol, fill = greys[9], color = greys[3]) +
  geom_raster(aes(x = X, y = Y, fill = phy_con)) +
  scale_fill_manual(
    values = colors_evo, 
    name = "Evoregions", 
    limits = LETTERS[1:5],
  ) +
  geom_sf(data = AF_sf, fill = NA, color = greys[7], size = 1) +
  coord_sf(xlim = c(-60,-35), ylim = af_limits$y) +
  theme_map_() +
  guides(fill = guide_legend(
    label.position = "right",
    title.position = "top"
    ))

map_evo_af_2 <- 
map_evo_af %>% 
  lemon::reposition_legend(
    "bottom right", 
    x = 0.95,
    y = 0.125,
    #offset = 0.025
    just = 1,
    plot = T
  )


# elevation maps ----


base_plot <- hill_plot +
  geom_spatraster(data = elev_af) +
  scale_fill_hypso_c(
    limits = r_limits,
    palette ="wiki-schwarzwald-cont",
    alpha = 0.65, 
    breaks = c(
      #seq(0, 500, 100),
      seq(0, 1500, 250),
      2000
    )
  ) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = bg, color = NA))




br_states <- read_state()

br_states_sel <- br_states %>% 
  filter(name_region %in% c("Sul", "Sudeste", "Nordeste") | 
           abbrev_state %in% c("GO", "MS"), 
         abbrev_state != "MA")

elev_plot <- base_plot +
  guides(fill = guide_legend(
    title = "Elevation (m)",
    # direction = "horizontal",
    # ncol = 1,
     keywidth = 0.5,
     keyheight = 1.75,
    # label.position = "right",
    # title.position = "top",
    override.aes = list(alpha = .8)
  )) +
  theme_void() +
  theme(
    plot.background = element_rect("transparent", colour = NA),
      legend.position = "right",
      legend.title = element_text(size = base_text_size*1.2 ),
      legend.text = element_text(size = base_text_size ),
      legend.key = element_rect("grey30"),
      #legend.spacing.y = unit(0, "pt")
  )

elev_plot_states <- elev_plot +
  geom_sf(data = br_states_sel, fill = NA, color = "grey30"#,linewidth = .5
          ) +
  scale_x_continuous(limits = c(-58, -34)) 





# Save maps ---------------------------------------------------------------
ggsave(
  here("fig", "map_evo.png"),
  map_evo, 
  width = 6.5,
  height = 8
)


ggsave(
  here("fig", "map_evo_af.png"),
  map_evo_af, 
  width = 6.5,
  height = 8
)

ggsave(
  here("fig", "map_insert_af.png"),
  map_SA_af, 
  width = 5,
  height = 6.5
)

ggsave(
  here("fig", "map_elevation.png"),
  elev_plot_states, 
  width = 6.5,
  height = 8
)
