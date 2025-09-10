# load packages -----------------------------------------------------------

library(tidyverse)
library(glue)
library(rcartocolor)
library(patchwork)
library(sf)
library(here)
library(pvclust)
library(ggtree)
library(tidytree)

# load data and theme ---------------------------------------------------------

source(here("script", "fig", "fig_00_map_themes.R"))
myrcia_disp <- readRDS(here("output", "myrcia_disp.rds"))
theme_set(theme_void())

# species composition
myrcia_comp <- read.csv(here("data", "W.csv"))

richness <- rowSums(myrcia_comp)

evo_metrics_df <- readRDS("output/evo_metrics_df.rds")

evo_metrics_2 <- bind_cols(
  evo_metrics_df, richness = richness, myrcia_disp
)

sf::sf_use_s2(FALSE)

# Atlantic Forest Integrated Limits - Muylaert et al 2018
AF_sf <- read_sf(
  here::here("data", "shape", "limits_AF", 
             "ma_limite_integrador_muylaert_et_al_2018_wgs84.shp"))

# map customization -------------------------------------------------------

# |- coast_layer ----
coast_layer <- #list(
  geom_sf(data = sf_coast_pol, fill = greys[9], color = greys[3]) 

# |- theme_map_af ----
theme_map_af <- list(
  coord_sf(xlim = c(-58,-35), ylim = af_limits$y),
  theme(
    panel.background = element_rect(fill = "transparent", color = NA), 
    panel.grid = element_blank(), 
    text = element_text(color = greys[1]), 
    plot.title = element_markdown(
      color = greys[2], 
      margin = margin(b=5, t = 5),
      hjust = 0.01
    ),
    #axis.text = element_text(color = greys[2], size = 8), 
    #axis.ticks = element_line(color = greys[3]), 
    panel.border = element_rect(color = NA, fill = NA), 
    axis.title = element_blank(),
    legend.text = element_text(color = greys[2], size = 8),
    legend.title = element_markdown(
      color = greys[2], face = "bold", size = 9, margin = margin(b = 2)
    ),
    legend.position = "left",
    #legend.key.height = unit(20, "points"), 
    #legend.key.width = unit(9, "points"),
    legend.background = element_blank()
  )#, 
  # guides(
  #   fill = guide_colorsteps(
  #     show.limits = T, 
  #     title.vjust = 1,
  #     title.position = "right", 
  #     label.position = "left"))
)


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

# Clustering of in situ div, MPD and Age ----


metrics_AF_std <- metrics_AF %>% 
  select(x, y, age_halfedge, div_insitu_prop, ses_mpd) %>% 
  mutate(across(age_halfedge:ses_mpd, scale))

mtr_data <- as.data.frame(metrics_AF_std[3:6])[,-4]


# |- computing cluster and pca ----
clust_af <- cluster::agnes(mtr_data, method = "ward")

pca <- prcomp(mtr_data)

summary(pca)


# |- visualization ----
metrics_AF_std <- metrics_AF_std %>% 
  mutate(gr = as.factor(cutree(clust_af, 3)),
         PC1 = pca$x[,1], 
         PC2 = pca$x[,2])

gr_col <- c("1" = "#a9344f", 
            "2" = "#83a6c4", 
            "3" = "#fcc573")


### |-- map ----
gr_map <- ggplot(metrics_AF_std) + 
  geom_raster(aes(x = x, y = y, fill = gr), show.legend = T) +
  geom_sf(data = AF_sf, fill = NA, color = greys[1], lwd = 0.23) +
  scale_fill_manual(values = gr_col, name = "Group") + 
  geom_hline(yintercept = -23.43616, linetype = 2) +
  #annotate("text", label = "Tropic of Capricorn", x = -42.25, y = -24) +
  theme_map_af +
  theme(legend.position = "bottom")


ggsave(
  "fig/cluster_map.png",
  gr_map,
  width = 6.5,
  height = 8
)

### |-- cluster ----

p <-  ggtree(clust_af) 

# map(1:3, \(gr){
#   tidytree::MRCA(p$data, which(clus == gr) )
# })

d <- data.frame(node=c(574, 575, 573), type=c("1", "2", "3"))

offspring_l <- map(d$node, \(id){
  offspring(p$data, id) %>% 
    pull(node)
})


tree_data <- p$data %>% 
  mutate(
    group = case_when(
      node %in% offspring_l[[1]] ~ "1", 
      node %in% offspring_l[[2]] ~ "2", 
      node %in% offspring_l[[3]] ~ "3", 
    )
  )

clus <- cutree(clust_af, 3)


tree_data <- p$data %>% 
  mutate(
    group = case_when(
      node %in% offspring_l[[1]] ~ "1", 
      node %in% offspring_l[[2]] ~ "2", 
      node %in% offspring_l[[3]] ~ "3", 
    )
  )

cluster_dendrogram <- ggtree(
    tree_data,
    layout = "dendrogram",
    aes(x = x, color= group), 
    size = 1) +
  scale_color_manual(values = gr_col, 
                     name = "Group") + 
                     #label = c("Northern", "Central", "Southern")) +
  theme(legend.position = "bottom")

ggsave(
  "fig/cluster.png", 
  cluster_dendrogram,
  width = 7,
  height = 6
)


### |-- PCA ----

arw_df <- data.frame(
  x = 0, 
  y = 0, 
  xend =  pca$rotation[,1],
  yend = pca$rotation[,2], 
  label = c("Age", "In Situ\nDiversification", "SES MPD")
)

t_adjust_pos <- 1.6

pc_importance <- (summary(pca)$importance["Proportion of Variance",] %>% round(2)) * 100

cluster_pca <- ggplot() +
  geom_point(
    data = metrics_AF_std,
    aes(PC1, PC2, color = gr), 
    pch = 16, 
    #color = "white", 
    size = 1, 
    show.legend = F) +
  geom_label(
    data = arw_df, 
    aes(x = xend * t_adjust_pos , y = yend * t_adjust_pos, label = label),
    alpha = 0.7, 
    size = 2.5,
    fontface = "bold"
    ) +
  geom_segment(
    data = arw_df, 
    aes(x = x, y = y, xend = xend, yend = yend), 
    arrow = arrow(length = unit(0.075, "inches")), 
    linewidth = 0.5
    ) +
  scale_color_manual(values = gr_col) +
  labs(
    x = glue("PC1 ({pc_importance['PC1']}%)"),
    y = glue("PC2 ({pc_importance['PC2']}%)")
  ) +
  coord_equal() +
  theme_bw()



ggsave(
  "fig/cluster_pca.png", 
  cluster_pca,
  width = 5,
  height = 4
)

cluster_pca$layers[[2]] <- NULL
cluster_pca$layers[[3]] <- NULL

theme_tiny <- list(
  theme(
    text = element_text(size = 7), 
    axis.title = element_text(hjust = 0.2)
  )
)


cluster_pca + theme_tiny

ggsave(
  "fig/cluster_pca_tiny.png", 
  cluster_pca + theme_tiny,
  width = 3,
  height = 2.25
)
