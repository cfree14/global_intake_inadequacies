

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


# Country codes
################################################################################

# Read key
country_orig <- readxl::read_excel(file.path(indir, "GDD 2018 Codebook_Jan 10 2022.xlsx"), sheet=2, skip=1)

# Format key
country <- country_orig %>%
  select(1:2) %>%
  setNames(c("country", "iso3"))

# Export
write.csv(country, file=file.path(outdir, "GDD_country_key.csv"), row.names = F)


# Region codes
################################################################################

# Read key
region_orig <- readxl::read_excel(file.path(indir, "GDD 2018 Codebook_Jan 10 2022.xlsx"), sheet=2, skip=1)

# Format key
region <- region_orig %>%
  select(4:5) %>%
  setNames(c("region_code", "region")) %>%
  filter(!is.na(region_code))

# Export
write.csv(region, file=file.path(outdir, "GDD_region_key.csv"), row.names = F)


# Age codes
################################################################################

# Read key
age_orig <- readxl::read_excel(file.path(indir, "GDD 2018 Codebook_Jan 10 2022.xlsx"), sheet=2, skip=1)

# Format key
age <- age_orig %>%
  select(7:8) %>%
  setNames(c("age_range", "age_code")) %>%
  filter(!is.na(age_range)) %>%
  # Fix incorrect age code
  mutate(age_code=ifelse(age_code==2.5, 3.5, age_code))

# Export
write.csv(age, file=file.path(outdir, "GDD_age_group_key.csv"), row.names = F)


# Factor codes
################################################################################

# Read key
factor_codes_orig <- readxl::read_excel(file.path(indir, "GDD 2018 Codebook_Jan 10 2022.xlsx"), sheet=1, skip=1)
factor_units_orig <- readxl::read_excel(file.path(indir, "GDD 2018 Codebook_Jan 10 2022.xlsx"), sheet=3, skip=1)

# Format key
factor_codes <- factor_codes_orig %>%
  janitor::clean_names("snake") %>%
  rename(factor_code=numeric_code,
         factor=gdd_variable_label) %>%
  select(factor_code, factor)

# Format units key
factor_units <- factor_units_orig %>%
  select(1:2) %>%
  setNames(c("factor", "factor_units")) %>%
  filter(!is.na(factor)) %>%
  # Recode factors
  mutate(factor=recode(factor,
                       "Monounsaturated fat"="Monounsaturated fatty acids",
                       "Plant omega-3 (n-3) fat"="Plant omega-3 fatty acids",
                       "Seafood omega-3 (n-3) fat"="Seafood omega-3 fatty acids")) %>%
  # Recode units
  mutate(factor_units=recode(factor_units,
                             "% of total kcal per day (energy contribution)"="% of total kcal",
                             "cups/day (1 cup=8 oz)"="cups",
                             "grams per day"="g",
                             "micrograms (µg) per day"="µg",
                             "micrograms (µg) per day DFE"="µg DFE",
                             "milligrams (mg) per day"="mg",
                             "μg RAE/day (RAE=retinol activity equivalent)"="µg RAE",
                             "kcal per day"="kcal"))

# Nutrients
foods <- c("Beans and legumes",  "Cheese",   "Eggs",  "Fruits", "Non-starchy vegetables",
           "Nuts and seeds", "Other starchy vegetables", "Potatoes", "Refined grains",
           "Total processed meats",  "Total seafoods", "Unprocessed red meats", "Whole grains",
           "Yoghurt (including fermented milk)")
beverages <- c("Coffee", "Fruit juices", "Sugar-sweetened beverages",  "Tea",  "Total milk")
macronutrients <- c("Added sugars", "Dietary cholesterol", "Dietary fiber", "Dietary sodium", "Total carbohydrates", "Total protein")
fatty_acids <- c("Monounsaturated fatty acids",   "Plant omega-3 fatty acids", "Saturated fat", "Seafood omega-3 fatty acids", "Total omega-6 fatty acids")
minerals <- c("Calcium", "Iodine", "Iron", "Magnesium", "Potassium", "Selenium")
vitamins <- c( "Folate", "Vitamin A (RAE)", "Vitamin B1", "Vitamin B12", "Vitamin B2", "Vitamin B3", "Vitamin B6", "Vitamin C", "Vitamin D", "Vitamin E", "Zinc")

# Merge codes and units
factor <- factor_codes %>%
  # Recode for merge
  mutate(factor=recode(factor,
                       "Vitamin A w/ supplements"="Vitamin A with supplements",
                       "Total omega-6 fat"="Total omega-6 fatty acids",
                       "Plant omega-3 fat"="Plant omega-3 fatty acids",
                       "Seafood omega-3 fat"="Seafood omega-3 fatty acids")) %>%
  # Add units
  left_join(factor_units, by="factor") %>%
  # Clean up factor names
  mutate(factor=recode(factor,
                       "Total Milk"="Total milk",
                       "Vitamin A with supplements"="Vitamin A (RAE)",
                       "Vitamin B9 (Folate)"="Folate")) %>%
  # Add nutrient type
  mutate(factor_type=ifelse(factor %in% foods, "Foods", NA),
         factor_type=ifelse(factor %in% beverages, "Beverages", factor_type),
         factor_type=ifelse(factor %in% macronutrients, "Macronutrients", factor_type),
         factor_type=ifelse(factor %in% fatty_acids, "Fatty acids", factor_type),
         factor_type=ifelse(factor %in% vitamins, "Vitamins", factor_type),
         factor_type=ifelse(factor %in% minerals, "Minerals", factor_type))

# Inspect
table(factor$factor_units)
sort(factor$factor)

# Export
write.csv(factor, file=file.path(outdir, "GDD_factor_key.csv"), row.names = F)



