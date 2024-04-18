
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(ggplot2)
library(tidyverse)
library(countrycode)

# Directories
outdir <- "output"
gisdir <- "data/world/processed"
plotdir <- "figures/gdd_approach"

# Read data
data_orig <- readRDS(file.path(outdir, "2018_subnational_nutrient_intake_inadequacy_estimates_full.Rds"))


# Read data
################################################################################

# Calculate stats
data_orig %>%
  filter(nutrient=="Calcium") %>%
  group_by(gdd_type) %>%
  summarize(npeople=sum(npeople, na.rm=T)/1e9) %>%
  ungroup() %>%
  mutate(ppeople=npeople/sum(npeople))
