

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(GENuS)
library(tidyverse)

# Directories
datadir <- "data/wessells_brown"
gisdir <- "data/world/processed"

# Get data
data_orig <- readxl::read_excel(file.path(datadir, "Wessells_Brown_2012_Table_S2.xls"))


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  # Simplify
  select(country, year, estimated_fractional_absorption) %>%
  # Format year
  mutate(year=gsub('b', "", year) %>% as.numeric()) %>%
  # Add ISO3
  mutate(iso3=countrycode::countrycode(country, "country.name", "iso3c")) %>%
  # Get most recent
  group_by(country) %>%
  arrange(country, desc(year)) %>%
  slice(1) %>%
  ungroup()

# Export
saveRDS(data, file=file.path(datadir, "wessells_brown_2012_zinc_absorption.Rds"))




# Visualize data
################################################################################

# Read world data
world_lg <- readRDS(file=file.path(gisdir, "world_large.Rds"))
world_sm <- readRDS(file=file.path(gisdir, "world_small.Rds"))
world_centers <- readRDS(file=file.path(gisdir, "world_centroids.Rds"))








