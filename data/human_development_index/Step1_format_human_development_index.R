

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
datadir <- "data/human_development_index"
plotdir <- "figures"

# Read HDI
hdi_orig <- readxl::read_excel(file.path(datadir, "2020_statistical_annex_all.xlsx"), sheet=2)


# Format data
################################################################################

# Format HDI
hdi <- hdi_orig %>%
  # Remove header
  slice(8:nrow(.)) %>%
  # Remove footer
  slice(1:192) %>%
  # Simplify
  select(2:3) %>%
  # Rename
  setNames(c("country", "hdi")) %>%
  # Filter again
  filter(!is.na(hdi)) %>%
  # Convert
  mutate(hdi=as.numeric(hdi)) %>%
  # Add categories
  # Very high (>0.8), high (0.7-0.8), medium (0.55-0.7), low (<0.55)
  mutate(hdi_catg=cut(hdi, breaks=c(0, 0.55, 0.7, 0.8,1),
                      labels=c("Low", "Medium", "High", "Very high"))) %>%
  # Correct countries
  mutate(iso3=countrycode(country, "country.name", "iso3c"),
         country=countrycode(iso3, "iso3c", "country.name")) %>%
  # Arrange
  select(iso3, country, hdi, hdi_catg, everything())

# Export
saveRDS(hdi, file=file.path(datadir, "UNDP_2020_human_development_index.Rds"))



