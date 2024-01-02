

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
outdir <- "output"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file.path(outdir, "2018_subnational_nutrient_intake_inadequacy_estimates_full.Rds"))

# Read region key
region_key <- readRDS(file="data/wb_regions/region_key.Rds") %>%
  # Fix Kosovo
  mutate(iso3=recode(iso3, "KOS"="XKX"))

# Simplify
################################################################################

# Simplify data
data <- data_orig %>%
  # Add region
  left_join(region_key %>% select(iso3, region), by="iso3") %>%
  mutate(region=ifelse(country=="Channel Islands", "Europe & Central Asia", region)) %>%
  # Select
  select(continent, region, iso3, country, nutrient, units, sex, age_range,
         supply_med, ar, ar_source, sev, npeople, ndeficient) %>%
  # Rename
  rename(intake=supply_med)

# Inspect
freeR::complete(data)

# Export simplified data
saveRDS(data, file.path(outdir, "2018_subnational_nutrient_intake_inadequacy_estimates_simple.Rds"))

