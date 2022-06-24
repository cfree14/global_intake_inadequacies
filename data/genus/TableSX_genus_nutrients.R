

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(GENuS)
library(tidyverse)

# Directories
outdir <- "data/genus/processed"
tabledir <- "data/genus/tables"

# Get data
data_agesex <- GENuS::genus_nutr_agesex_2011


# Build data
################################################################################

# Data
data <- data_agesex %>%
  select(nutrient_type, nutrient, units_short) %>%
  unique() %>%
  # Format/order nutrient type
  mutate(nutrient_type=recode_factor(nutrient_type,
                                     "Vitamin"="Vitamins",
                                     "Mineral"="Minerals",
                                     "Calories"="Macronutrients")) %>%
  # Format units
  mutate(units_short=recode(units_short, "microgram"="ug", "microgram RAE"="ug RAE")) %>%
  # Rename nutrients
  mutate(nutrient=recode(nutrient,
                       "Folate"="Folate (vitamin B9)",
                       "Thiamin"="Thiamin (vitamin B1)",
                       "Riboflavin"="Riboflavin (vitamin B2)",
                       "Niacin"="Niacin (vitamin B3)",
                       "Vitamin B6"="Vitamin B6 (pyridoxine)",
                       "Vitamin B12"="Vitamin B12 (cobalamin)")) %>%
  # Arrange
  arrange(nutrient_type, nutrient)

# Export
write.csv(data, file=file.path(tabledir, "TableSX_genus_nutrients.csv"), row.names=F)




