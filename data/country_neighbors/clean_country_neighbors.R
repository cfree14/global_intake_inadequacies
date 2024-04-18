
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(ggplot2)
library(tidyverse)
library(countrycode)

# Directories
datadir <- "data/country_neighbors"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "country_neighbors_raw.xlsx"))



# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Format country
  mutate(country=stringr::str_trim(country)) %>%
  fill(country, .direction="down") %>%
  mutate(country=recode(country,
                        "Macau (People's Republic of China)"="Macau")) %>%
  # Format country
  mutate(country_use=countrycode::countrycode(country, "country.name", "country.name"),
         iso3=countrycode::countrycode(country_use, "country.name", "iso3c"))

sort(unique(data$country))
