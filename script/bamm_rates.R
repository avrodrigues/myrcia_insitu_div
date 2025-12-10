library(BAMMtools)
library(ape)
library(dplyr)
library(tidyr)
library(purrr)

# ======================================================
# 1. Load your phylogeny
# ======================================================
tree <- read.tree("data/000_phy_myrcia_cleaned_consensus.new")

# Sanity check
is.ultrametric(tree)
min(tree$edge.length)

clade_samp <- read.csv("data/clade_sampling_fraction.csv")

# Expand tips from the "tips_pos" column
df_expanded <- clade_samp %>%
  mutate(tip_index = map(tips_pos, ~ {
    idx <- strsplit(.x, "-")[[1]]
    seq(as.integer(idx[1]), as.integer(idx[2]))
  })) %>%
  unnest(tip_index)

# Create per-tip sampling table
sampling_df <- df_expanded %>%
  transmute(
    species = tree$tip.label[tip_index],
    sampling_fraction = sampling_fraction
  )

# Add global sampling fraction at the top (placeholder 0.1)
global_sampling <- 0.1315068

# Write to text file in BAMM format
txt_file <- "data/bamm/sampling_fractions.txt"


# Write first line
writeLines(as.character(global_sampling), txt_file)

# Write per-tip fractions (2 columns only, tab-separated)
write.table(
  sampling_df,
  file = txt_file,
  append = TRUE,
  sep = "\t",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE
)


# ======================================================
# 2. Create BAMM prior file
# ======================================================
setBAMMpriors(tree, total.taxa = 730, outfile = "priorfile.txt")

# ======================================================
# 3. Write the BAMM control file
# ======================================================
control_lines <- c(
  "modeltype                   = speciationextinction",
  "treefile                    = data/000_phy_myrcia_cleaned_consensus.new",
  "runInfoFilename             = data/bamm/runInfo.txt",
  "eventDataOutfile            = output/bamm/event_data.txt",
  "mcmcOutfile                 = output/bamm/mcmc_out.txt",
  
  # sampling settings: either global or clade/tip-specific
  "useGlobalSamplingProbability = 1",
  "sampleProbsFilename         = data/bamm/sampling_fractions.txt",
  "initialNumberEvents         = 1",
  
  # MCMC settings
  "initializeModel             = 1",
  "globalSamplingFraction      = 0.1315068",
  "numberOfGenerations         = 5000000",
  "mcmcWriteFreq               = 10000",
  "eventDataWriteFreq          = 10000",
  "printFreq                   = 10000",
  "overwrite                   = 1",
  
  "updateEventLocationScale = 0.1",
  "updateEventRateScale  = 0.1",
  "updateLambdaInitScale = 0.1",
  "updateLambdaShiftScale = 0.1",
  "updateMuInitScale = 0.1",
  "updateRateEventNumber = 0.1",
  "updateRateEventPosition = 0.1",
  "updateRateEventRate = 0.1", 
  "updateRateLambda0 = 0.1", 
  "updateRateLambdaShift = 0.1", 
  "updateRateLambdaTimeMode = 0.1", 
  "updateRateMu0 = 0.1",
  
  # Prior on number of shifts
  "expectedNumberOfShifts      = 2.0",
  "lambdaInit0 = 0.1",
  "lambdaInitPrior = 1.4396004534244",
  "lambdaShift0 = 0.0001",
  "lambdaShiftPrior = 0.0413169249628402",
  "muInitPrior = 1.4396004534244",
  "lambdaIsTimeVariablePrior = 1",
  "localGlobalMoveRatio = 1.0",
  "muInit0 = 0.01", 
  "runMCMC = 1",
  "segLength = 0.05"
  
  
)

writeLines(control_lines, "data/bamm/controlfile.txt")


# ======================================================
# 4. Run BAMM
# ======================================================
# (Make sure bamm executable is available in your PATH)
system("bamm_win/bamm -c data/bamm/controlfile.txt")

# When the run finishes, BAMM will create:
#   event_data.txt
#   mcmc_out.txt
#   runInfo.txt

# ============================
# 5. Load Output into R
# ============================
edata <- getEventData(tree,
                      eventdata = "output/bamm/event_data.txt",
                      burnin = 0.1,    # choose appropriate burnin
                      nsamples = 500)  # optional subsampling

# Check MCMC convergence
mcmc <- read.csv("output/bamm/mcmc_out.txt")
plot(mcmc$logLik ~ mcmc$generation, type="l")

effectiveSize(mcmc$logLik)  # want ESS > 200

# ============================
# 6. Compute Tip Diversification Rates
# ============================

# Net diversification rate per tip
tip_rates <- getTipRates(edata)

# Extract vectors:
speciation_rate <- tip_rates$lambda.avg
extinction_rate <- tip_rates$mu.avg
net_div_rate <- speciation_rate - extinction_rate

# Combine into a clean table
tip_rates_df <- data.frame(
  tip = names(speciation_rate),
  lambda = speciation_rate,
  mu = extinction_rate,
  net_div_rate = net_div_rate
)

head(tip_rates_df)


# ============================
# 7. Plot Tip-Based Diversification Rates
# ============================

# Simple plot
plot(net_div_rate, pch=16, main="Net Diversification Rate per Tip")

# Rate-through-time (optional)
plotRateThroughTime(edata, avgCol = "red")

# Phylorate plot
plot.bammdata(edata, lwd = 2)
