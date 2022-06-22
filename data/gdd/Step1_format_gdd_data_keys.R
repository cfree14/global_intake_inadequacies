

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


# Factor codes
################################################################################

# Read key
data_orig <- readxl::read_excel(file.path(indir, "GDD 2018 Codebook_Jan 10 2022.xlsx"), sheet=1, skip=1)

# Format key
data <- data_orig %>%
  janitor::clean_names("snake") %>%
  rename(factor_code=numeric_code,
         factor=gdd_variable_label) %>%
  select(factor_code, factor)

# Export
write.csv(data, file=file.path(outdir, "GDD_factor_key.csv"), row.names = F)


# Country codes
################################################################################

# Read key
data_orig <- readxl::read_excel(file.path(indir, "GDD 2018 Codebook_Jan 10 2022.xlsx"), sheet=2, skip=1)

# Format key
data <- data_orig %>%
  select(1:2) %>%
  setNames(c("country", "iso3"))

# Export
write.csv(data, file=file.path(outdir, "GDD_country_key.csv"), row.names = F)


# Region codes
################################################################################

# Read key
data_orig <- readxl::read_excel(file.path(indir, "GDD 2018 Codebook_Jan 10 2022.xlsx"), sheet=2, skip=1)

# Format key
data <- data_orig %>%
  select(4:5) %>%
  setNames(c("region_code", "region")) %>%
  filter(!is.na(region_code))

# Export
write.csv(data, file=file.path(outdir, "GDD_region_key.csv"), row.names = F)

# Age codes
################################################################################

# Read key
data_orig <- readxl::read_excel(file.path(indir, "GDD 2018 Codebook_Jan 10 2022.xlsx"), sheet=2, skip=1)

# Format key
data <- data_orig %>%
  select(7:8) %>%
  setNames(c("age_range", "age_code")) %>%
  filter(!is.na(age_range))

# Export
write.csv(data, file=file.path(outdir, "GDD_age_group_key.csv"), row.names = F)


# Unit key
################################################################################

# Read key
data_orig <- readxl::read_excel(file.path(indir, "GDD 2018 Codebook_Jan 10 2022.xlsx"), sheet=3, skip=1)

# Format key
data <- data_orig %>%
  select(1:2) %>%
  setNames(c("factor", "factor_units")) %>%
  filter(!is.na(factor))

# Export
write.csv(data, file=file.path(outdir, "GDD_factor_unit_key.csv"), row.names = F)



