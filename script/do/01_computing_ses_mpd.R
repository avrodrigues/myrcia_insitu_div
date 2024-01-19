
# load packages -----------------------------------------------------------

library(picante)
library(here)
library(SYNCSA)


# load data ---------------------------------------------------------------

phy.path <- here(
  "data", "000_phy_myrcia_cleaned_consensus.new"
)
myrcia_tree <- read.tree(phy.path)


# species composition
myrcia_comp <- read.csv(here("data", "W.csv"))

# Calculating SES MPD
dis <- cophenetic(myrcia_tree) #cophenetic distance matrix
org<- SYNCSA::organize.syncsa(comm=myrcia_comp, phylodist=dis) #organizing matrices

ses_mpd_myrcia <- ses.mpd(
  org$community, dis = org$phylodist, null.model = "taxa.labels"
)

saveRDS(ses_mpd_myrcia, here("output", "ses_mpd_myrcia.rds"))
