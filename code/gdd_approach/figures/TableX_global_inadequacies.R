

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
data_orig <- readRDS(file.path(outdir, "2018_subnational_nutrient_intake_inadequacy_estimates_simple.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>%
  # Remove Vitamin D
  filter(nutrient!="Vitamin D") %>%
  # Calculate global deficiency stats
  group_by(nutrient) %>%
  summarize(npeople=sum(npeople, na.rm=T),
            ndeficient=sum(ndeficient, na.rm=T)) %>%
  mutate(pdeficient=ndeficient/npeople) %>%
  ungroup() %>%
  # Arrange
  arrange(desc(pdeficient)) %>%
  # Format
  select(-npeople) %>%
  mutate(ndeficient=round(ndeficient/1e9, 2),
         pdeficient=round(pdeficient*100,1) %>% paste0(., "%"))


# Export table
################################################################################

# Export table
write.csv(data, file=file.path(tabledir, "TableSX_global_inadequacies.csv"), row.names=F)



