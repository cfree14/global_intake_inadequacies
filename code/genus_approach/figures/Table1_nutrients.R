
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(GENuS)
library(nutriR)

# Directories
outdir <- "output"
plotdir <- "figures"
tabledir <- "tables"

# Read data
data_orig <- readRDS(file.path(outdir, "2011_subnational_nutrient_intake_inadequacy_estimates_simple.Rds"))



# Build data
################################################################################

nutr_key <- data_orig %>%
  select(nutrient, units, ar_source) %>%
  unique()
