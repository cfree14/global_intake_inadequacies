

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(GENuS)
library(tidyverse)

# Directories
outdir <- "data/genus/processed"

# Get data
data_cntry_orig <- GENuS::genus_nutr_cntry
data_agesex <- GENuS::genus_nutr_agesex_2011


# Build scalar key
################################################################################

# 2011 country wide median supply
data_cntry_2011 <- data_cntry_orig %>%
  # Simplify
  filter(year==2011)

# 2011 scalars for going from country-wide median supply to age-sex median supply
scalar_key_2011 <- data_agesex %>%
  # Simplify
  select(iso3, country, nutrient, units_short, sex, age_range, supply_med) %>%
  arrange(iso3, country, nutrient, units_short, sex, age_range) %>%
  # Add 2011 country median
  left_join(data_cntry_2011  %>% select(iso3, nutrient, supply_med), by=c("iso3", "nutrient")) %>%
  # Rename columns
  rename(supply=supply_med.x, supply_cntry=supply_med.y) %>%
  # Calculate scalar
  mutate(scalar=supply/supply_cntry)

# Calculate time series of age-sex median supplies
data <- data_cntry_orig %>%
  # Simplify
  select(iso3, country, nutrient, units_short, year, supply_med) %>%
  # Add scalar
  left_join(scalar_key_2011 %>% select(iso3, nutrient, units_short, sex, age_range, scalar)) %>%
  # Compute age/sex median supplies
  rename(supply_cntry_med=supply_med) %>%
  mutate(supply_agesex_med=supply_cntry_med*scalar) %>%
  # Do some formatting
  mutate(units_short=recode(units_short,
                            "microgram"="µg",
                            "microgram RAE"="µg RAE")) %>%
  # Arrange
  select(iso3, country, nutrient, units_short, sex, age_range, sex, year, supply_cntry_med, scalar, supply_agesex_med, everything()) %>%
  arrange(iso3, nutrient, sex, age_range, year)

# Inspect
range(data$year)

# Export GENuS data
saveRDS(data, file=file.path(outdir, "GENUS_1961_2011_country_agesex_nutrient_intakes.Rds"))


