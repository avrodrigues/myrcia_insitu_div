
# load packages -----------------------------------------------------------

library(Herodotools)
library(here)
library(BioGeoBEARS)
library(ape)
library(dplyr)
library(furrr)

source("function/calc_bsm.R")
source("function/get_bsm_node_area.R")

# load data ---------------------------------------------------------------

resDECJ <- readRDS(
  here("data", "list_results_models_biogeobears_phy_consenso.rds"),
)$resDECj

phy.path <- here(
  "data", "000_phy_myrcia_cleaned_consensus.new"
)
myrcia_tree <- read.tree(phy.path)

geog.path <- here(
  "data", "000_areas_myrcia_phy_consensus.data"
)

# evoregion classification
evo_df <- read.csv(here("data", "evoregions_stand_names_df.csv"))
site_xy <- evo_df[,1:2]
evo_mtx <- data.frame(evoregion = evo_df[, -c(1:2)])

# species composition
myrcia_comp <- read.csv(here("data", "W.csv"))

# run BSM ------------------------------------------------------

bsm_res <- calc_bsm(
  BioGeoBEARS.data = resDECJ,
  phyllip.file = geog.path,
  tree.path = phy.path,
  max.maps = 100, 
  n.maps.goal = 100,
  seed = 1234,
  save_after_every_try = FALSE
)

# assemblage age and in situ diversification ---------------------------------

# getting the ancestral range area for each node 
list_node_area <- get_bsm_node_area(
  result_bsm = bsm_res, 
  BioGeoBEARS.data = resDECJ,
  phyllip.file = geog.path,
  tree.path = phy.path,
  max.range.size = 2
)

# calculating age arrival 
plan(multisession, workers = 10)

l_age_comm_recent <- future_map(seq_along(list_node_area), function(i) {
  calc_age_arrival(
    W = myrcia_comp, 
    tree = myrcia_tree, 
    ancestral.area = list_node_area[[i]], 
    biogeo = evo_mtx,
    age.no.ancestor = "recent"
  ) 
})
plan(sequential)

l_avg_age_recent <- purrr::map(l_age_comm_recent, function(x) x$mean_age_per_assemblage)
avg_age_recent_df <- purrr::list_cbind(l_avg_age_recent)
names(avg_age_recent_df) <- paste0("bsm_", 1:ncol(avg_age_recent_df))

# save dataframe
write.csv(avg_age_recent_df, "output/bg_stochastic_map/avg_age_recent_df.csv")


plan(multisession, workers = 10)
l_age_comm_half <- future_map(seq_along(list_node_area), function(i) {
 calc_age_arrival(
    W = myrcia_comp, 
    tree = myrcia_tree, 
    ancestral.area = list_node_area[[i]], 
    biogeo = evo_mtx,
    age.no.ancestor = "half.edge"
  ) 
})
plan(sequential)

l_avg_age_half <- purrr::map(l_age_comm_half, function(x) x$mean_age_per_assemblage)
avg_age_half_df <- purrr::list_cbind(l_avg_age_half)
names(avg_age_half_df) <- paste0("bsm_", 1:ncol(avg_age_half_df))

# save dataframe
write.csv(avg_age_half_df, "output/bg_stochastic_map/avg_age_half_df.csv")

# calculating in situ diversification
plan(multisession, workers = 10)

l_myrcia_diversification <- future_map(seq_along(list_node_area), function(i) {

myrcia_diversification <- calc_insitu_diversification(
  W = myrcia_comp,
  tree = myrcia_tree, 
  ancestral.area = list_node_area[[i]], 
  biogeo = evo_mtx, 
  diversification = "jetz",
  type = "equal.splits"
  )

})
plan(sequential)


# div_jetz = myrcia_diversification$Jetz_harmonic_mean_site,
# div_mb_jetz = myrcia_diversification$insitu_Jetz_harmonic_mean_site 
# ) %>% 
#   mutate(
#     div_insitu_prop = div_mb_jetz/div_jetz
#   )

l_div_jetz <- purrr::map(l_myrcia_diversification, function(x) 
  data.frame(div_jetz = x$Jetz_harmonic_mean_site))

div_jetz_df <- purrr::list_cbind(l_div_jetz)
names(div_jetz_df) <- paste0("bsm_", 1:ncol(div_jetz_df))


l_div_mb_jetz <- purrr::map(l_myrcia_diversification, function(x) 
  data.frame(div_mb_jetz = x$insitu_Jetz_harmonic_mean_site))

div_mb_jetz_df <- purrr::list_cbind(l_div_mb_jetz)
names(div_mb_jetz_df) <- paste0("bsm_", 1:ncol(div_mb_jetz_df))

div_insitu_prop_df <- div_mb_jetz_df/div_jetz_df

# save dataframe
write.csv(div_insitu_prop_df, "output/bg_stochastic_map/div_insitu_prop_df.csv")


## to be done

# plan(multisession, workers = 10)
# 
# 
# myrcia_disp <- calc_dispersal_from(
#   W = myrcia_comp,
#   tree = myrcia_tree, 
#   ancestral.area = node.area, 
#   biogeo = evo_mtx
# )
# 

