
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)
library(countrycode)

# Directories
datadir <- "data/stevens_etal_2022"



# Format data
################################################################################

# Read data
table2_orig <- readxl::read_excel(file.path(datadir, "Stevens_etal_2022_tables.xlsx"), sheet=2)

# Format data
table2 <- table2_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  # Fix country
  mutate(country=countrycode(country, "country.name", "country.name"),
         iso3=countrycode(country, "country.name", "iso3c")) %>%
  select(iso3, country, year, sample_size, everything()) %>%
  # Gather
  gather(key="nutrient", value="deficiency", 5:ncol(.)) %>%
  mutate(nutrient=str_to_sentence(nutrient),
         nutrient=recode(nutrient,
                         "Vitamin_a"="Vitamin A",
                         "Vitamin_b12"="Vitamin B12",
                         "Vitamin_d"="Vitamin D")) %>%
  # Split
  tidyr::separate(col = "deficiency", into=c("pdeficient", "pdeficient_range"), sep="% ", remove=T, convert=T) %>%
  mutate(
         pdeficient_range=gsub("\\(|\\)", "", pdeficient_range)) %>%
  tidyr::separate(col = "pdeficient_range", into=c("pdeficient_lo", "pdeficient_hi"), sep="-", remove=T, convert=T) %>%
  # Filter
  filter(!nutrient %in% c("Core", "Sentinel")) %>%
  # Add column
  mutate(group="Preschool-aged children aged 6–59 months")

# Inspect
str(table2)


# Format data
################################################################################

# Read data
table3_orig <- readxl::read_excel(file.path(datadir, "Stevens_etal_2022_tables.xlsx"), sheet=3)

# Format data
table3 <- table3_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  # Fix country
  mutate(country=countrycode(country, "country.name", "country.name"),
         iso3=countrycode(country, "country.name", "iso3c")) %>%
  select(iso3, country, year, sample_size, everything()) %>%
  # Gather
  gather(key="nutrient", value="deficiency", 5:ncol(.)) %>%
  mutate(nutrient=str_to_sentence(nutrient),
         nutrient=recode(nutrient,
                         "Vitamin_a"="Vitamin A",
                         "Vitamin_b12"="Vitamin B12",
                         "Vitamin_d"="Vitamin D")) %>%
  # Split
  tidyr::separate(col = "deficiency", into=c("pdeficient", "pdeficient_range"), sep="% ", remove=T, convert=T) %>%
  mutate(pdeficient_range=gsub("\\(|\\)", "", pdeficient_range)) %>%
  tidyr::separate(col = "pdeficient_range", into=c("pdeficient_lo", "pdeficient_hi"), sep="-", remove=T, convert=T) %>%
  # Filter
  filter(!nutrient %in% c("Core", "Sentinel")) %>%
  # Add column
  mutate(group="Non-pregnant women aged 15–49 years")

# Inspect
str(table2)

# Merge
################################################################################

# Merge
data <- bind_rows(table2, table3) %>%
  select(group, everything())

# Export
saveRDS(data, file=file.path(datadir, "stevens_etal_2022_data.Rds"))

# Plot data
g <- ggplot(data, aes(x=pdeficient, y=country)) +
  facet_grid(group~nutrient) +
  # Labels
  labs(x="% deficient", y="") +
  # Data
  geom_errorbar(mapping=aes(xmin=pdeficient_lo, xmax=pdeficient_hi, y=country), color="grey60", width=0) +
  geom_point() +
  # Theme
  theme_bw()
g

