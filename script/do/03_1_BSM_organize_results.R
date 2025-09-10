# load package ------------------------------------------------------------

library(tidyverse)
library(here)



# load data ---------------------------------------------------------------

avg_age_recent_df <- read.csv("output/bg_stochastic_map/avg_age_recent_df.csv", 
                              row.names = 1)
avg_age_half_df <- read.csv("output/bg_stochastic_map/avg_age_half_df.csv", 
                            row.names = 1)
insitu_div_df <- read.csv("output/bg_stochastic_map/div_insitu_prop_df.csv", 
                               row.names = 1)


ses_mpd_df <- readRDS(here("output", "ses_mpd_myrcia.rds"))
evo_df <- read.csv(here("data", "evoregions_stand_names_df.csv"))
site_xy <- evo_df[,1:2]


bsm_summary_df <- data.frame(
  site_xy, 
  bsm_mean_half = rowMeans(avg_age_half_df), 
  bsm_sd_half = apply(avg_age_half_df, 1, sd),
  bsm_mean_recent = rowMeans(avg_age_recent_df), 
  bsm_sd_recent = apply(avg_age_recent_df, 1, sd),
  bsm_mean_insitu_div = rowMeans(insitu_div_df), 
  bsm_sd_insitu_div = apply(insitu_div_df, 1, sd)
)




evo_metrics_df <- bind_cols(bsm_summary_df, ses_mpd = ses_mpd_df$mpd.obs.z)




# save data ---------------------------------------------------------------


saveRDS(evo_metrics_df, here("output", "evo_metrics_df_BSM.rds"))
