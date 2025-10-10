
# load packages -----------------------------------------------------------

library(Herodotools)
library(here)
library(BioGeoBEARS)
library(ape)
library(dplyr)
library(furrr)

# source("function/calc_bsm.R")
# source("function/get_bsm_node_area.R")

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
  seed = 1234
)

# assemblage age and in situ diversification ---------------------------------

# prepare the insertions -------------------------------------------------------
## get_insert_df ----

insert_list <- get_insert_df(
  bsm_res,
  phyllip.file = geog.path,
  max.range.size = resDECJ$inputs$max_range_size
)


# getting the ancestral range area for each node 
list_node_area <- get_bsm_node_area(
  bsm = bsm_res, 
  BioGeoBEARS.data = resDECJ,
  phyllip.file = geog.path,
  tree.path = phy.path,
  max.range.size = resDECJ$inputs$max_range_size
)

# insert_nodes ----

bsm_tree <- insert_nodes(
  tree =  myrcia_tree, 
  inserts = insert_list, 
  node_area = list_node_area)


# calculating age arrival 
plan(multisession, workers = 10)

l_age_comm <- future_map(bsm_tree, function(bsm_map){
  
  tree <- bsm_map$phylo
  #tree$node.label <- NULL
  anc_area <- bsm_map$node_area %>% as.matrix()
  
  Herodotools::calc_age_arrival(W = myrcia_comp, 
                                tree = tree, 
                                ancestral.area = anc_area, 
                                biogeo = evo_mtx) 
  
})

plan(sequential)

l_avg_age <- purrr::map(l_age_comm, function(x) x$mean_age_per_assemblage)
avg_age_df <- purrr::list_cbind(l_avg_age)
names(avg_age_df) <- paste0("bsm_", 1:ncol(avg_age_df))

# Organize results
age_bsm_mtx <- sapply(
  l_age_comm, 
  function(x) x$mean_age_per_assemblage$mean_age_arrival
)



# calculating in situ diversification -----
plan(multisession, workers = 10)

l_div_insitu <- future_map(bsm_tree, function(bsm_map){
  
  tree <- bsm_map$phylo
  #tree$node.label <- NULL
  anc_area <- bsm_map$node_area 
  
  calc_insitu_diversification(
    W = as.matrix(myrcia_comp),
    tree = tree, 
    ancestral.area = anc_area, 
    biogeo = evo_mtx, 
    type = "equal.splits"
  )
  
})
plan(sequential)


l_DR_jetz <- purrr::map(l_div_insitu, function(x) 
  data.frame(DR_jetz = x$jetz_comm_mean))

DR_jetz_df <- purrr::list_cbind(l_DR_jetz)
names(DR_jetz_df) <- paste0("bsm_", 1:ncol(DR_jetz_df))

# Jetz in not dependend on the BSM models, so it should not change
max(apply(DR_jetz_df, 1, sd))
# SD across BSM models shows max sd as 1.005783e-17, which is negligible variation

l_DR_insitu <- purrr::map(l_div_insitu, function(x) 
  data.frame(DR_insitu = x$insitu_comm_mean))

DR_insitu_df <- purrr::list_cbind(l_DR_insitu)
names(DR_insitu_df) <- paste0("bsm_", 1:ncol(l_DR_insitu))

l_DR_prop <- purrr::map(l_div_insitu, function(x) 
  data.frame(div_insitu = x$prop_comm_mean))

DR_prop_df <- purrr::list_cbind(l_DR_prop)
names(DR_prop_df) <- paste0("bsm_", 1:ncol(DR_prop_df))

## summarize results ----

bsm_metrics <- cbind(
  evo_df, 
  age_bsm_mean = rowMeans(age_bsm_mtx),
  age_bsm_sd = apply(age_bsm_mtx, 1, sd),
  DR_jetz = DR_jetz_df[,1],
  DR_insitu_bsm_mean = rowMeans(DR_insitu_df, na.rm = T),
  DR_insitu_bsm_sd = apply(DR_insitu_df, 1, sd, na.rm = T),
  DR_prop_bsm_mean = rowMeans(DR_prop_df),
  DR_prop_bsm_sd = apply(DR_prop_df, 1, sd)
)

# save dataframe
write.csv(bsm_metrics, "output/bg_stochastic_map/bsm_metrics.csv")


## Dispersal from -----------------------------------------------------------

plan(multisession, workers = 10)


l_myrcia_disp <- future_map(bsm_tree, function(bsm_map){
  
  tree <- bsm_map$phylo
  anc_area <- bsm_map$node_area %>% as.matrix()
  
  Herodotools::calc_dispersal_from(
    W = myrcia_comp,
    tree = tree,
    ancestral.area = anc_area,
    biogeo = evo_mtx
  )
  
})

plan(sequential)



range_names_disp_df <- purrr::map(l_myrcia_disp, colnames) %>% 
  unlist() %>% 
  unique() %>% 
  sort()


disp_df <- data.frame(rowname = integer())

for(range in range_names_disp_df){
  
  disp_df[,range] <- numeric()
}


for(i in seq_along(l_myrcia_disp)){
  
  bsm_idx <- paste0("bsm_", i)
  df <- as.data.frame(l_myrcia_disp[[i]]) %>% 
    select(any_of(range_names_disp_df)) %>% 
    tibble::rownames_to_column() %>% 
    mutate(rowname = as.integer(rowname))
  
  disp_df <- disp_df %>% add_row(df)
  
}


disp_from_df_mean <- disp_df %>% 
  group_by(rowname) %>% 
  summarize(
    across(all_of(range_names_disp_df), ~mean(.x, na.rm = T)/sum(.x, na.rm = T), .names = "mean_{.col}")
  )

disp_from_df_sd <- disp_df %>% 
  group_by(rowname) %>% 
  summarize(
    across(all_of(range_names_disp_df), ~sd(.x, na.rm = T), .names = "sd_{.col}")
  )


disp_from_data <- list(
  disp_from_df_mean = disp_from_df_mean, 
  disp_from_df_sd = disp_from_df_sd
)

saveRDS(disp_from_data, here("output", "myrcia_disp_BSM.rds"))
