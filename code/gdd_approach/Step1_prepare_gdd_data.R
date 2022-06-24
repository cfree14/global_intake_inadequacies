

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

# Read data
data_orig <- readRDS(data, file=file.path(outdir, "GDD_2018_intakes_national.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>%
  # Look at micronutrient
  filter(factor_type %in% c("Vitamins", "Minerals", "Fatty acids")) %>%
  # Use average across residences and educaton levels
  filter(residence=="All residences" & education=="All education levels") %>%
  # Remove All ages and Both sexes
  filter(age_range!="All ages" & sex!="Both sexes") %>%
  # Rename
  rename(nutrient_type=factor_type, nutrient=factor, units=factor_units) %>%
  # Add continent
  mutate(continent=countrycode::countrycode(iso3, "iso3c", "continent")) %>%
  # Simplify
  select(nutrient_type, nutrient, units, continent, country, iso3, sex, age_range, supply_med, supply_lo, supply_hi) %>%
  arrange(nutrient_type, nutrient, units, continent, country, iso3, sex, age_range)

# Inspect
str(data)
freeR::complete(data)

# Inspect more
table(data$iso3)
table(data$age_range)
table(data$sex)

# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "GDD_2018_intakes_national_for_analysis.Rds"))
