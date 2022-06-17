

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
outdir <- "data/beal_etal_2017/processed"

# Format data #3
################################################################################

# Read data
data_orig <- read.csv(file.path(indir, "Beal_etal_2017_SuppData3.csv"), as.is=T)

# Format data
data <- data_orig %>%
  janitor::clean_names("snake") %>%
  mutate(nutrient=recode(nutrient,
                         "CA"="Calcium",
                         "CU"="Copper",
                         "FE"="Iron",
                         "FOL"="Folate",
                         "MG"="Magnesium",
                         "NIA"="Niacin",
                         "P"="Phosphorus",
                         "RIBF"="Riboflavin",
                         "THIA"="Thiamine",
                         "VITA_RAE"="Vitamin A (RAE)",
                         "VITB6"="Vitamin B6",
                         "VITC"="Vitamin C",
                         "ZN"="Zinc",
                         "VITB12"="Vitamin B12")) %>%
  # Add distribution type
  mutate(dist_type=ifelse(cv>0.3, "lognormal", "normal")) %>%
  # Arrange
  select(nutrient, units, dist_type, cv) %>%
  arrange(cv)

# Inspect
str(data)

# Export
saveRDS(data, file=file.path(outdir, "Beal_nutrient_cvs.Rds"))



# Format data #4
################################################################################

# Read data
data_orig <- read.csv(file.path(indir, "Beal_etal_2017_SuppData4.csv"), as.is=T)

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

# Export
saveRDS(data, file=file.path(outdir, "Beal_1961_2011_inadequate_nutrient_intake_by_country.Rds"))


