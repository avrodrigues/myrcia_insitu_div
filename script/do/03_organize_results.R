# load package ------------------------------------------------------------

library(tidyverse)
library(here)



# load data ---------------------------------------------------------------

div_age_df <- readRDS(here("output", "div_age_df.rds"))
ses_mpd_df <- readRDS(here("output", "ses_mpd_myrcia.rds"))
evo_df <- read.csv(here("data", "evoregions_stand_names_df.csv"))


evo_metrics_df <- bind_cols(evo_df, div_age_df, ses_mpd = ses_mpd_df$mpd.obs.z)
names(evo_metrics_df)[3] <- "evoregion"



# save data ---------------------------------------------------------------


saveRDS(evo_metrics_df, here("output", "evo_metrics_df.rds"))
