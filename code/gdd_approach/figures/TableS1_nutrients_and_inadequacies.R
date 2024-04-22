

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
  # Remove GDD borrowed countries
  filter(gdd_type=="Reported") %>%
  # Calculate global deficiency stats
  group_by(nutrient, units, ar_source) %>%
  summarize(npeople=sum(npeople, na.rm=T),
            ndeficient=sum(ndeficient, na.rm=T)) %>%
  mutate(pdeficient=ndeficient/npeople) %>%
  ungroup() %>%
  # Make numbers pretty
  mutate(ndeficient=ndeficient/1e9) %>%
  # Add nutrient label/tyep
  mutate(nutrient_label=recode(nutrient,
                               "Folate"="Folate (vitamin B9)",
                               "Niacin"="Niacin (vitamin B3)",
                               "Riboflavin"="Riboflavin (vitamin B2)",
                               "Thiamin"="Thiamin (vitamin B1)",
                               "Vitamin B12"="Vitamin B12 (cobalamin)",
                               "Vitamin B6"="Vitamin B6 (pyridoxine)")) %>%
  mutate(nutrient_type=ifelse(grepl("vitamin", tolower(nutrient_label)), "Vitamin", "Mineral")) %>%
  # Arrange
  select(-npeople) %>%
  select(nutrient_type, nutrient_label, nutrient, units, ar_source, everything()) %>%
  arrange(desc(nutrient_type), desc(ndeficient))


# Export table
################################################################################

# Export table
write.csv(data, file=file.path(tabledir, "TableS1_global_inadequacies.csv"), row.names=F)



