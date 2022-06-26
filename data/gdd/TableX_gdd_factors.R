

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
tabledir <- "data/gdd/tables"

# Read key
data_orig <- read.csv(file.path(outdir, "GDD_factor_key.csv"), as.is=T)

# Format
################################################################################

# Format
data <- data_orig %>%
  # Simplify
  select(factor_type, factor, factor_units) %>%
  # Factor type
  mutate(factor_type=factor(factor_type, levels=c("Vitamins", "Minerals", "Fatty acids", "Macronutrients", "Beverages", "Foods"))) %>%
  # Rename factors
  mutate(factor=recode(factor,
                       "Folate"="Folate (vitamin B9)",
                       "Vitamin B1"="Thiamin (vitamin B1)",
                       "Vitamin B2"="Riboflavin (vitamin B2)",
                       "Vitamin B3"="Niacin (vitamin B3)",
                       "Vitamin B6"="Vitamin B6 (pyridoxine)",
                       "Vitamin B12"="Vitamin B12 (cobalamin)")) %>%
  # Arrange
  arrange(factor_type, factor)

# Export
write.csv(data, file.path(tabledir, "TableSX_gdd_factors.csv"), row.names=F)

