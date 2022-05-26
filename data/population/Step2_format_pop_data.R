

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)
library(wbstats)
library(countrycode)

# Directories
datadir <- "data/population"

# Read data
data_orig <- readRDS(file=file.path(datadir, "WB_1960_2020_population_size_by_country_agesex_full.Rds"))


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Simplify
  select(-c(country_orig, iso3_orig, iso2_orig, indicator_id, indicator)) %>%
  # Rename
  rename(iso3=iso3_use, country=country_use, npeople=pop_size) %>%
  # Format sex
  mutate(sex=recode(sex, "female"="Females", "male"="Males"))

# Stats
stats <- data %>%
  group_by(year) %>%
  summarize(npeople=sum(npeople, na.rm=T)/1e9)

ggplot(stats, aes(x=year, y=npeople)) +
  geom_line()

# Plot data
################################################################################

# Read data
saveRDS(data, file=file.path(datadir, "WB_1960_2020_population_size_by_country_agesex.Rds"))

