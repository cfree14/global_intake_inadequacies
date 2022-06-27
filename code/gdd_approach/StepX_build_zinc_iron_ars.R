

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(GENuS)
library(tidyverse)

# Directories
indir <- "data/gdd/raw/GDD_FinalEstimates_01102022"
outdir <- "data/gdd/processed"
plotdir <- "data/gdd/figures"

# Get ARs
ars_orig <- nutriR::nrvs


# IOM
################################################################################

# IOM iron
iron_iom <- dris %>%
  filter(nutrient=="Iron" & dri_type=="Estimated Average Requirement (EAR)" & !stage %in% c("Pregnancy", "Lactation"))

# Plot
g <- ggplot(iron_iom, aes(x=age_range, y=value, color=sex, group=sex)) +
  geom_line() +
  theme_bw()
g
