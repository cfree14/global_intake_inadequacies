

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

# Add region
data <- data_orig %>%
  # Add region
  left_join(region_key %>% select(iso3, region), by="iso3") %>%
  mutate(region=ifelse(country=="Channel Islands", "Europe & Central Asia", region))

# Export full data
openxlsx::write.xlsx(data, file=file.path(file.path(outdir, "2018_subnational_nutrient_intake_inadequacy_estimates_full.xlsx")))

# Simplify data
data_simple <- data %>%
  # Select
  select(continent, region, iso3, country, gdd_type, nutrient, units, sex, age_range,
         supply_med, ar, ar_source, sev, npeople, ndeficient) %>%
  # Rename
  rename(intake=supply_med)

# Inspect
freeR::complete(data_simple)

# Export simplified data
saveRDS(data_simple, file.path(outdir, "2018_subnational_nutrient_intake_inadequacy_estimates_simple.Rds"))

# Export CSV
data1_simple <- data_simple %>%
  mutate(age_range=paste(age_range, "yrs"))
write.csv(data1_simple, file.path(outdir, "2018_subnational_nutrient_intake_inadequacy_estimates_simple.csv"), row.names = F)

