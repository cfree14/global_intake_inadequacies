

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
dvdir <- "output/dataverse"

# Read data
data_orig <- readRDS(file.path(outdir, "2018_subnational_nutrient_intake_inadequacy_estimates_full.Rds"))

# Read region key
region_key <- readRDS(file="data/wb_regions/region_key.Rds") %>%
  # Fix Kosovo
  mutate(iso3=recode(iso3, "KOS"="XKX"))


# Add region and export full
################################################################################

# Add region
data <- data_orig %>%
  # Add region
  left_join(region_key %>% select(iso3, region), by="iso3") %>%
  mutate(region=ifelse(country=="Channel Islands", "Europe & Central Asia", region))

# Export full data
saveRDS(data, file=file.path(dvdir, "diet_full.Rds"))
write.csv(data, file=file.path(dvdir, "diet_full.csv"), row.names=F)
openxlsx::write.xlsx(data, file=file.path(dvdir, "diet_full.xlsx"))


# Simplify and export reduced
################################################################################

# Simplify data
data_simple <- data %>%
  # Rename
  rename(intake=supply_med) %>%
  # Select
  select(continent, region, iso3, country,
         nutrient, units, sex, age_range,
         intake, ar, ar_source, sev, npeople, ndeficient)


# Inspect
freeR::complete(data_simple)

# Export simplified data
saveRDS(data_simple, file=file.path(dvdir, "diet_simple.Rds"))
write.csv(data_simple, file=file.path(dvdir, "diet_simple.csv"), row.names=F)
openxlsx::write.xlsx(data_simple, file=file.path(dvdir, "diet_simple.xlsx"))


