

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
plotdir <- "figures"
indir <- "data/beal_etal_2017/raw"
outdir <- "data/beal_etal_2017/raw"

# Read data
data_orig <- read.csv(file.path(indir, "Beal_etal_2017_SuppData4.csv"), as.is=T)


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names() %>%
  rename(region=zone,
         nutrient_code=tagname,
         nutrient=micronutrient,
         intake=estimated_intake,
         requirement=requirements,
         npeople=population,
         pinadequate=prevalence_of_inadequate_intake)

# Inspect
str(data)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "Beal_1961_2011_inadequate_nutrient_intake_by_country.Rds"))

