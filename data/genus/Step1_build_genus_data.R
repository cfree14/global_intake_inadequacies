

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(GENuS)
library(tidyverse)

# Directories
datadir <- "data/genus"

# Get data
data_cntry <- GENuS::genus_nutr_cntry
data_agesex <- GENuS::genus_nutr_agesex_2011


# Build scalar key
################################################################################

# Scalar key
scalar_key <- data_agesex %>%
  # Simplify
  select(iso3, country, nutrient, units_short, sex, age_range, supply_med) %>%
  arrange(iso3, country, nutrient, units_short, sex, age_range) %>%
  # Add 2011 country mean
  left_join(data_cntry %>% filter(year==2011) %>% select(iso3, nutrient, supply_med), by=c("iso3", "nutrient")) %>%
  # Rename columns
  rename(supply=supply_med.x, supply_cntry=supply_med.y) %>%
  # Calculate scalar
  mutate(scalar=supply/supply_cntry)




