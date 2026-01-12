# packages ----------------------------------------------------------------
library(ape)
library(BAMMtools)
library(here)
library(caper)
library(tidyverse)
library(Herodotools)
library(ggtree)
library(glue)

source("script/fig/fig_00_map_themes.R")

# load data ---------------------------------------------------------------
evo_df  <- read.csv(here("data", "evoregions_stand_names_df.csv"))
site_xy <- evo_df[, 1:2]
evo_mtx <- evo_df[, -c(1:2)]

W <- read.csv(here("data", "W.csv"))

tree <- read.tree(here("data", "000_phy_myrcia_cleaned_consensus.new"))

# Atlantic Forest Integrated Limits - Muylaert et al 2018
AF_sf <- read_sf(
  here::here("data", "shape", "limits_AF", 
             "ma_limite_integrador_muylaert_et_al_2018_wgs84.shp"))

# bamm event data ---------------------------------------------------------
edata <- getEventData(
  phy       = tree,
  eventdata = here("output/bamm/event_data.txt"),
  burnin    = 0.1,
  nsamples  = 500
)

# bamm tip rates ----------------------------------------------------------
tip_rates <- getTipRates(edata)

speciation_rate <- tip_rates$lambda.avg
extinction_rate <- tip_rates$mu.avg
net_div_rate    <- speciation_rate - extinction_rate

tip_rates_df <- tibble(
  tip          = names(speciation_rate),
  lambda       = speciation_rate,
  mu           = extinction_rate,
  net_div_rate = net_div_rate
)

# jetz ed rates -----------------------------------------------------------
ed_total <- calc_ed(tree)
jetz_sp  <- 1 / ed_total

# compare tip rates -------------------------------------------------------
plot(jetz_sp, tip_rates_df$net_div_rate)
cor_dr <- cor.test(jetz_sp, tip_rates_df$net_div_rate)

df_rates <- data.frame(tip_rates_df, jetz_sp)

comp_data <- comparative.data(
  phy       = tree,
  data      = df_rates,
  names.col = "tip"
)

pgls(jetz_sp ~ lambda, comp_data) %>% summary()

plot_df_rates <- ggplot(df_rates) +
  geom_point(aes(jetz_sp, net_div_rate)) +
  theme_bw() +
  labs(
    title = "Tip-based diversification estimates", 
    subtitle = glue("r = {round(cor_dr$estimate, 2)}"),
    x = "DR (Jetz)", 
    y = "Net diversification rate (BAMM)")


ggsave(
  "fig/df_rate_plot.png", 
  plot_df_rates, 
  width = 5, 
  height = 5
)

# prepare matrices --------------------------------------------------------
init_matrix <- matrix(
  NA,
  nrow = nrow(W),
  ncol = ncol(W),
  dimnames = dimnames(W)
)

# bamm site means ---------------------------------------------------------
bamm_lambda  <- setNames(tip_rates_df$net_div_rate, tip_rates_df$tip)
bamm_site_sp <- init_matrix

for (sp in colnames(W)) {
  bamm_site_sp[, sp] <- ifelse(W[, sp] == 1, bamm_lambda[sp], NA)
}

bamm_comm_mean <- apply(
  bamm_site_sp, 1,
  function(x) calc_harmonic_mean(x, na.rm = TRUE)
)

# jetz site means ---------------------------------------------------------
jetz_site_sp <- init_matrix

for (sp in colnames(W)) {
  jetz_site_sp[, sp] <- ifelse(W[, sp] == 1, jetz_sp[sp], NA)
}

jetz_comm_mean <- apply(
  jetz_site_sp, 1,
  function(x) calc_harmonic_mean(x, na.rm = TRUE)
)

# combine site data -------------------------------------------------------
site_mean_div <- tibble(
  x = site_xy[, 1],
  y = site_xy[, 2],
  bamm_comm_mean = bamm_comm_mean,
  jetz_comm_mean = jetz_comm_mean
)


# create a theme
theme_bamm_dr <- list(
  scale_fill_viridis_c(),
  geom_sf(data = sf_coast, color = greys[5]),
  geom_sf(data = AF_sf, fill = NA,  color = greys[2]),
  theme_map_continuous 
)

# plot bamm ---------------------------------------------------------------
plot_bamm <- ggplot() +
  geom_raster(data = site_mean_div, aes(x = x, y = y, fill = bamm_comm_mean)) +
  theme_bamm_dr +
  labs(fill = "BAMM comm. avg ")

# plot jetz ---------------------------------------------------------------
plot_jetz <-ggplot() +
  geom_raster(data = site_mean_div, aes(x, y, fill = jetz_comm_mean)) +
  theme_bamm_dr +
  labs(fill = "Jetz comm. avg ")


plot_bamm + plot_jetz

ggsave(
  "fig/map_bamm_dr.png", 
  plot_bamm + plot_jetz, 
  width = 12, 
  height = 7
)

cor(site_mean_div$jetz_comm_mean, site_mean_div$bamm_comm_mean)
