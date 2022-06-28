

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
outdir <- "output"
gisdir <- "data/world/processed"
tabledir <- "tables/gdd_approach"

# Read data
data_orig <- read.csv("data/gdd/processed/GDD_factor_key.csv", as.is=T)


# Build data
################################################################################

# Build data
data <- data_orig %>%
  select(factor_type, factor, factor_units) %>%
  mutate(factor_type=factor(factor_type,
                       levels=c("Vitamins", "Minerals", "Fatty acids",
                                "Macronutrients", "Beverages", "Foods"))) %>%
  arrange(factor_type, factor) %>%
  rename(type=factor_type, units=factor_units)


# Export table
################################################################################

# Export table
write.csv(data, file=file.path(tabledir, "TableS2_gdd_dietary_factors.csv"), row.names=F)



