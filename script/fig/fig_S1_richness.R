
# load packages  ----------------------------------------------------------

library(tidyverse)



# load data ---------------------------------------------------------------


source("script/fig/fig_00_map_themes.R")


myrcia_dist <- read.csv("data/myrcia_binary_df_05_degree.csv")

W <- read.csv("data/W.csv")
W_xy <- read.csv("data/W_xy.csv")


# Atlantic Forest Integrated Limits - Muylaert et al 2018
AF_sf <- read_sf(
  here::here("data", "shape", "limits_AF", 
             "ma_limite_integrador_muylaert_et_al_2018_wgs84.shp"))



# prepare data ------------------------------------------------------------
# summarize richness

# all species
myrcia_rich <- myrcia_dist %>% 
  group_by(x, y) %>% 
  summarise(richness_total = sum(presence)) 

# phylogeny's species
richness_phy <- rowSums(W)

# merge data
myrcia_rich_df <- left_join(W_xy, myrcia_rich, by = c("x", "y")) %>% 
  mutate(richness_phy = richness_phy)

myrcia_rich_sf <- st_as_sf(
  myrcia_rich_df,
  coords = c("x","y"), 
  crs = st_crs(AF_sf)
) %>% 
  mutate(x = W_xy[,1],
         y = W_xy[,2])

# compute correlation
cor(myrcia_rich_df$richness_total, myrcia_rich_df$richness_phy)



# Map ---------------------------------------------------------------------

# create a theme
theme_richness <- list(
  scale_fill_carto_c(name = "Richness  ", palette = "SunsetDark"),
  geom_sf(data = sf_coast, color = greys[5]),
  geom_sf(data = AF_sf, fill = NA,  color = greys[2]),
  theme_map_continuous 
)


# create the map
rich_map <- 
ggplot() +
  geom_raster(data = myrcia_rich_sf, aes(x, y, fill = richness_total)) +
  theme_richness +
  labs(title = "307 species") +
 
  
ggplot() +
  geom_raster(data = myrcia_rich_sf, aes(x, y, fill = richness_phy)) +
  theme_richness +
  labs(title = "96 species") 
  
# save the map
ggsave(
  "fig/SM_richness_plot.png", 
  rich_map,
  width = 10, 
  height = 6
)

