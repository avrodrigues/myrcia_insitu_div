library(terra)
library(tidyterra)
library(ggplot2)
library(dplyr)
library(scales)
library(sf)
library(purrr)
library(geobr)

# Get the data
library(geodata)

bg <- "#FAF8F4"

mydir <- "data/elevation"
AF_sf <- read_sf(
  here::here("data", "shape", "limits_AF", 
             "ma_limite_integrador_muylaert_et_al_2018_wgs84.shp"))

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



hypso_pal <- c(
  "arctic", "arctic_bathy", "arctic_hypso", "c3t1", "colombia", 
  "colombia_bathy", "colombia_hypso", "dem_poster", "dem_print", 
  "dem_screen", "etopo1", "etopo1_bathy", "etopo1_hypso", 
  "gmt_globe", "gmt_globe_bathy", "gmt_globe_hypso", 
  "meyers", "meyers_bathy", "meyers_hypso", "moon", "moon_bathy", "moon_hypso", 
  "nordisk-familjebok", "nordisk-familjebok_bathy", "nordisk-familjebok_hypso",
  "pakistan", "spain", "usgs-gswa2", "utah_1", "wiki-2.0", "wiki-2.0_bathy",
  "wiki-2.0_hypso", "wiki-schwarzwald-cont")

grad <- hypso.colors2(10, "dem_poster")

pal <- hypso_pal[13]


r_limits <- minmax(elev_af_crop) %>% as.vector()

# Rounded to lower and upper 500
r_limits <- c(floor(r_limits[1] / 500), ceiling(r_limits[2] / 500)) * 500

# And making min value to 0.
r_limits <- pmax(r_limits, 0)


base_text_size <- 10

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


elev_plot <- base_plot +
  guides(fill = guide_legend(
    title = "   m.",
    direction = "horizontal",
    nrow = 1,
    keywidth = 1.75,
    keyheight = 0.5,
    label.position = "bottom",
    title.position = "right",
    override.aes = list(alpha = .8)
  )) +
  theme(
    plot.background = element_rect(bg, colour = NA),
    plot.margin = margin(20, 20, 20, 20),
    axis.text = element_text(size = base_text_size * 0.7),
    legend.position = "bottom",
    legend.title = element_text(size = base_text_size * 0.8),
    legend.text = element_text(size = base_text_size * 0.8),
    legend.key = element_rect("grey30"),
    legend.spacing.x = unit(0, "pt")
  )

br_states <- read_state()

br_states_sel <- br_states %>% 
  filter(name_region %in% c("Sul", "Sudeste", "Nordeste") | 
           abbrev_state %in% c("GO", "MS"), 
         abbrev_state != "MA")
# maps ----

elev_plot +
  geom_sf(data = AF_sf, fill = NA, color = "grey30", linewidth = .5) +
  scale_x_continuous(limits = c(-58, -34))

elev_plot +
  geom_sf(data = br_states_sel, fill = NA, color = "grey30", linewidth = .5) +
  scale_x_continuous(limits = c(-58, -34))
