

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
datadir <- "data/gdd/processed"
tabledir <- "tables/gdd_approach"

# Read data
data_orig <- read.csv(file.path(datadir, "GDD_age_group_key.csv"), as.is=T)


# Build data
################################################################################

# Build data
data <- data_orig %>%
  filter(age_range!="All ages") %>%
  # Format original
  rename(age_range_gdd=age_range) %>%
  mutate(age_range_gdd=gsub("years", "yr", age_range_gdd),
         age_range_gdd=gsub("mo.", "mo", age_range_gdd)) %>%
  # Add WB group
  mutate(age_range_wb=recode(age_range_gdd,
                            "0-11 mo"="0-4 yr",
                            "12-23 mo"="0-4 yr",
                            "2-5 yr"="0-4 yr",
                            "6-10 yr"="5-9 yr",
                            "11-14 yr"="10-14 yr",
                            "15-19 yr"="15-19 yr",
                            "20-24 yr"="20-24 yr",
                            "25-29 yr"="25-29 yr",
                            "30-34 yr"="30-34 yr",
                            "35-39 yr"="35-39 yr",
                            "40-44 yr"="40-44 yr",
                            "45-49 yr"="45-49 yr",
                            "50-54 yr"="50-54 yr",
                            "55-59 yr"="55-59 yr",
                            "60-64 yr"="60-64 yr",
                            "65-69 yr"="65-69 yr",
                            "70-74 yr"="70-74 yr",
                            "75-79 yr"="75-79 yr",
                            "80-84 yr"="80+ yr",
                            "85-89 yr"="80+ yr",
                            "90-94 yr"="80+ yr",
                            "95+ yr"="80+ yr")) %>%
  # Simplify
  select(age_range_gdd, age_range_wb)



# Export table
################################################################################

# Export table
write.csv(data, file=file.path(tabledir, "TableS3_age_group_key.csv"), row.names=F)



