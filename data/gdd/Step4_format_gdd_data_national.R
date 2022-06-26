

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(GENuS)
library(tidyverse)

# Directories
indir <- "data/gdd/raw/GDD_FinalEstimates_01102022"
outdir <- "data/gdd/processed"

# Read keys
factor_key <- read.csv(file=file.path(outdir, "GDD_factor_key.csv"), as.is=T)
region_key <- read.csv(file=file.path(outdir, "GDD_region_key.csv"), as.is=T)
age_key <- read.csv(file=file.path(outdir, "GDD_age_group_key.csv"), as.is=T)


# Merge data
################################################################################

# Files to merge
files2merge <- list.files(file.path(indir, "Country-level estimates"))

# Loop through and merge
x <- files2merge[1]
data_orig <- purrr::map_df(files2merge, function(x){

  # Read data
  fdata_orig <- read.csv(file.path(indir, "Country-level estimates", x), as.is=T) %>%
    mutate(filename=x) %>%
    filter(year==2018)

})

# Country key
iso_key <- data_orig %>%
  select(iso3) %>%
  unique() %>%
  mutate(country=countrycode::countrycode(iso3, "iso3c", "country.name"))

# Format data
data <- data_orig %>%
  # Rename
  rename(region_code=superregion2, age_code=age, sex_code=female, residence_code=urban, education_code=edu,
         supply_med=median, supply_lo=lowerci_95, supply_hi=upperci_95,
         serving_med=serving, serving_lo=s_lowerci_95, serving_hi=s_upperci_95) %>%
  # Arrange and remove useless columns
  select(filename, region_code, iso3, age_code, sex_code, residence_code, education_code, year,
         supply_med, supply_lo, supply_hi,
         serving_med, serving_lo, serving_hi) %>%
  # Add factor into
  mutate(factor_code=gsub("_cnty.csv", "", filename)) %>%
  left_join(factor_key) %>%
  # Add region
  left_join(region_key) %>%
  # Add country
  left_join(iso_key, by="iso3") %>%
  # Add age
  left_join(age_key) %>%
  mutate(age_range=factor(age_range, levels=age_key$age_range)) %>%
  # Format sex
  mutate(sex=case_when(sex_code==0 ~ "Male",
                       sex_code==1 ~ "Female",
                       sex_code==999 ~ "Both sexes")) %>%
  # Format residence
  mutate(residence=case_when(residence_code==0 ~ "Rural",
                             residence_code==1 ~ "Urban",
                             residence_code==999 ~ "All residences")) %>%
  # Format education
  mutate(education=case_when(education_code==1 ~ "Low (0-6 years formal)",
                             education_code==2 ~ "Medium (6.01-12 years)",
                             education_code==3 ~ "High (12.01+ years)",
                             education_code==999 ~ "All education levels")) %>%
  # Arrange
  select(filename, factor_type, factor, factor_units, region, iso3, country,
         sex, age_range, residence, education, year,
         supply_med, supply_lo, supply_hi, serving_med, serving_lo, serving_hi) %>%
  arrange(factor_type, factor, region, iso3, sex, age_range)

# Inspect
# freeR::complete(data)

# Check
check <- data %>%
  filter(is.na(supply_med))
table(check$iso3)
table(check$factor)
table(check$residence) # basically just missing this info for rural
table(check$education)

# Export data
saveRDS(data, file=file.path(outdir, "GDD_2018_intakes_national.Rds"))



