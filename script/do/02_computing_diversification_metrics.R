
# load packages -----------------------------------------------------------

library(Herodotools)
library(here)
library(BioGeoBEARS)
library(ape)
library(dplyr)


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

# assemblage age and in situ diversification ---------------------------------

node.area <- 
  get_node_range_BioGeoBEARS(
    resDECJ,
    phyllip.file = geog.path,
    myrcia_tree,
    max.range.size = 2
  )

# calculating age arrival 
age_comm_recent <- age_arrival(
  W = myrcia_comp, 
  tree = myrcia_tree, 
  ancestral.area = node.area, 
  biogeo = evo_mtx,
  age.no.ancestor = "recent"
  ) 

age_comm_half <- age_arrival(
  W = myrcia_comp, 
  tree = myrcia_tree, 
  ancestral.area = node.area, 
  biogeo = evo_mtx,
  age.no.ancestor = "half.edge"
  ) 

# calculating in situ diversification

myrcia_diversification <- db_diversification(
  W = myrcia_comp,
  tree = myrcia_tree, 
  ancestral.area = node.area, 
  biogeo = evo_mtx, 
  diversification = "jetz",
  type = "equal.splits"
  )

myrcia_disp <- dispersal_from(
  W = myrcia_comp,
  tree = myrcia_tree, 
  ancestral.area = node.area, 
  biogeo = evo_mtx
)

# organanizing results --------------------------------------------------------

div_age_df <- data.frame(
  age_recent = age_comm_recent$mean_age_per_assemblage$mean_age_arrival,
  age_halfedge = age_comm_half$mean_age_per_assemblage$mean_age_arrival,
  div_jetz = myrcia_diversification$Jetz_harmonic_mean_site,
  div_mb_jetz = myrcia_diversification$model_based_Jetz_harmonic_mean_site 
  ) %>% 
  mutate(
    div_insitu_prop = div_mb_jetz/div_jetz
)


# save results ------------------------------------------------------------

saveRDS(div_age_df, here("output", "div_age_df.rds"))
saveRDS(myrcia_disp, here("output", "myrcia_disp.rds"))

