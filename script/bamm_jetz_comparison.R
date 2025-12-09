# packages ----------------------------------------------------------------
library(ape)
library(BAMMtools)
library(here)
library(caper)
library(tidyverse)
library(Herodotools)

# load data ---------------------------------------------------------------
evo_df  <- read.csv(here("data", "evoregions_stand_names_df.csv"))
site_xy <- evo_df[, 1:2]
evo_mtx <- evo_df[, -c(1:2)]

W <- read.csv(here("data", "W.csv"))

tree <- read.tree(here("data", "000_phy_myrcia_cleaned_consensus.new"))

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
cor.test(jetz_sp, tip_rates_df$net_div_rate)

df_rates <- data.frame(tip_rates_df, jetz_sp)

comp_data <- comparative.data(
  phy       = tree,
  data      = df_rates,
  names.col = "tip"
)

pgls(jetz_sp ~ lambda, comp_data) %>% summary()

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

# plot bamm ---------------------------------------------------------------
ggplot(site_mean_div, aes(x, y, fill = bamm_comm_mean)) +
  geom_tile() +
  scale_fill_viridis_c() +
  coord_equal()

# plot jetz ---------------------------------------------------------------
ggplot(site_mean_div, aes(x, y, fill = jetz_comm_mean)) +
  geom_tile() +
  scale_fill_viridis_c() +
  coord_equal()


cor(site_mean_div$jetz_comm_mean, site_mean_div$bamm_comm_mean)
