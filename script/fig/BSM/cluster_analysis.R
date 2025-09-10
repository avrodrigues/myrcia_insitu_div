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


source("function/plot_cluster_pca.R")
source("function/plot_cluster_pca_3d.R")

# load data and theme ---------------------------------------------------------

source(here("script", "fig", "fig_00_map_themes.R"))
theme_set(theme_void())


myrcia_comp <- read.csv("data/W.csv")




bsm_metrics <- read.csv("output/bg_stochastic_map/bsm_metrics.csv", row.names = 1)

mpd_data <- readRDS("output/ses_mpd_myrcia.rds")

evo_metrics_df <- data.frame(
  bsm_metrics, 
  ses_mpd = mpd_data$mpd.obs.z, 
  richness = rowSums(myrcia_comp))


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
  evo_metrics_df,
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
    .before = 1
  )

# Clustering of in situ div, MPD and Age ----


metrics_AF_std <- metrics_AF %>% 
  select(x, y, age_bsm_mean, DR_prop_bsm_mean, ses_mpd, richness) %>% 
  mutate(across(age_bsm_mean:richness, scale))

mtr_data <- as.data.frame(metrics_AF_std[3:6])[,-5]


# |- computing cluster and pca ----
clust_af <- cluster::agnes(mtr_data, method = "ward")

pca <- prcomp(mtr_data)

summary(pca)


# |- visualization ----
metrics_AF_std <- metrics_AF_std %>% 
  mutate(
    gr = as.factor(cutree(clust_af, 4)),
    gr = case_when(
      gr == "1" ~  "3",
      gr == "2" ~  "4", 
      gr == "3" ~  "1",
      gr == "4" ~  "2"
    ),
    PC1 = pca$x[,1], 
    PC2 = pca$x[,2],
    PC3 = pca$x[,3]
        ) 



gr_col <- c("3" = "#a9344f", 
            "4" = "#83a6c4", 
            "1" = "#fcc573",
            "2" = "#7b5a28"
            )


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
  "fig/cluster_map_BSM.png",
  gr_map,
  width = 6.5,
  height = 8
)

### |-- cluster ----

p <-  ggtree(clust_af) 
clus <- metrics_AF_std$gr

# it takes some time to run 
group_node <- map_dfr(1:4, \(gr){
  tidytree::MRCA(p$data, which(clus == gr)) %>% 
    suppressMessages()
})

d <- data.frame(node=group_node$node, type=c("1", "2", "3", "4"))

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
      node %in% offspring_l[[4]] ~ "4"
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
  "fig/cluster_BSM.png", 
  cluster_dendrogram,
  width = 7,
  height = 6
)


### |-- PCA ----

arw_df_12 <- data.frame(
  x = 0, 
  y = 0, 
  xend =  pca$rotation[,1],
  yend = pca$rotation[,2], 
  label = c("Age", "DR prop", "SES MPD", "Richness")
)

arw_df_13 <- data.frame(
  x = 0, 
  y = 0, 
  xend =  pca$rotation[,1],
  yend = pca$rotation[,3], 
  label = c("Age", "DR prop", "SES MPD", "Richness")
)

arw_df_23 <- data.frame(
  x = 0, 
  y = 0, 
  xend =  pca$rotation[,2],
  yend = pca$rotation[,3], 
  label = c("Age", "DR prop", "SES MPD", "Richness")
)

t_adjust_pos <- 1.6

pc_importance <- (summary(pca)$importance["Proportion of Variance",] %>% round(2)) * 100

# Axis 1 and 2
pca_12 <- plot_cluster_pca(metrics_AF_std,
                 arw_df_12, 
                 pcs = c("PC1", "PC2"),
                 gr_col = gr_col, 
                 pc_importance = pc_importance, 
                 t_adjust_pos = 1.6)

# Axis 1 and 3
pca_13 <- plot_cluster_pca(metrics_AF_std,
                           arw_df_13, 
                           pcs = c("PC1", "PC3"),
                           gr_col = gr_col, 
                           pc_importance = pc_importance, 
                           t_adjust_pos = 1.6)

# Axis 2 and 3
pca_23 <- plot_cluster_pca(metrics_AF_std,
                           arw_df_23, 
                           pcs = c("PC2", "PC3"),
                           gr_col = gr_col, 
                           pc_importance = pc_importance, 
                           t_adjust_pos = 1.6)


design <- "
  1#
  23
"

cluster_pca <- pca_12 + pca_13 + pca_23 + 
  plot_layout(design = design, axis_titles = 'collect')





ggsave(
  "fig/cluster_pca_BSM.png", 
  cluster_pca,
  width = 8,
  height = 6
)

pca_12$layers[[2]] <- NULL
pca_12$layers[[3]] <- NULL

theme_tiny <- list(
  theme(
    text = element_text(size = 7), 
    axis.title = element_text(hjust = 0.2)
  )
)


pca_12 + theme_tiny

ggsave(
  "fig/cluster_pca_tiny_BSM.png", 
  pca_12 + theme_tiny,
  width = 3,
  height = 2.25
)


## |-- PCA 3D

arw_df <- data.frame(
  x = 0, 
  y = 0, 
  z = 0,
  xend =  pca$rotation[,1],
  yend = pca$rotation[,2], 
  zend = pca$rotation[,3],
  label = c("Age", "DR prop", "SES MPD", "Richness")
)

# only for interactive vizualization
plot_cluster_pca_3d(
  metrics_AF_std,
  arw_df = arw_df,   # must contain x, y, z, xend, yend, zend, label
  pcs = c("PC1", "PC2", "PC3"),
  gr_col = gr_col,
  pc_importance = pc_importance,
  t_adjust_pos = 1.2
)