

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
popdir <- "data/population"
gdddir <- "data/gdd/processed"
hdidir <- "data/human_development_index"
outdir <- "output"
plotdir <- "figures"
tabledir <- "tables/gdd_approach"

# Read GDD data
gdd_orig <- readRDS(file=file.path(gdddir, "GDD_2018_intakes_national_for_analysis.Rds"))

# Read population data
pop_orig <- readRDS(file.path(popdir, "WB_1960_2020_population_size_by_country_agesex.Rds"))

# Read HDI
hdi_orig <- readRDS(file.path(hdidir, "UNDP_2020_human_development_index.Rds")) %>%
  mutate(hdi_catg=as.character(hdi_catg))

# Get ARs
ars_orig <- nutriR::nrvs
dris_orig <- nutriR::dris

# Get zinc AR info
zinc_orig <- readRDS("data/wessells_brown/wessells_brown_2012_zinc_absorption.Rds") %>%
  select(iso3, estimated_fractional_absorption) %>%
  rename(zinc_frac=estimated_fractional_absorption)
zinc_ars_orig <- readRDS("data/wessells_brown/zinc_ars_absorption.Rds") %>%
  rename(age_range=age,
         zinc_frac=frac,
         zinc_ar_mg=ar_mg)

# Get iron AR info
iron_key <- readRDS("data/gdd/processed/GDD_total_protein_avg.Rds") %>%
  select(iso3, supply_med_cap) %>%
  rename(protein_g=supply_med_cap)
iron_ars <- readRDS("data/gdd/processed/iron_ars_protein.Rds") %>%
  rename(age_range=age,
         protein_g=protein,
         iron_ar_mg=ar_mg)


# Format GDD data
################################################################################

# Inspect population age/sex key
agesex_key_pop <- pop_orig %>%
  select(sex, age) %>%
  unique() %>%
  arrange(sex, age)

# Inspect GDD age/sex key
agesex_key_gdd <- gdd_orig %>%
  select(sex, age_range) %>%
  unique() %>%
  arrange(sex, age_range)

# Based on this, you'll have to do the following formatting:
# (1) Rename age as age range in the pop key

# Format population data
pop <- pop_orig %>%
  rename(age_range=age) %>%
  filter(year==2018)
sum(pop$npeople, na.rm=T) / 1e9
n_distinct(pop$iso3)


# Expand GDD data
################################################################################

# Which countries have population data but not GDD data?
isos_gdd <- sort(unique(gdd_orig$iso3))
isos2match <- pop_orig %>%
  select(iso3, country) %>%
  unique() %>%
  filter(!iso3 %in% isos_gdd)
write.csv(isos2match, file=file.path(tabledir, "TableSX_countries_without_gdd_data.csv"), row.names=F)

# Read GDD country match key
gdd_match_key <- readxl::read_excel(file.path(tabledir, "TableS3_countries_without_gdd_data.xlsx"), skip=1) %>%
  # Rename
  setNames(c("iso1", "country1", "iso2", "country2"))

# Loop through key and create data to add
x <- 1
gdd_add <- purrr::map_df(1:nrow(gdd_match_key), function(x){

  # ISOs
  iso_do <- gdd_match_key$iso1[x]
  cntry_do <- gdd_match_key$country1[x]
  # continent_do <- countrycode::countrycode(cntry_do, "country.name", "continent")
  iso_borrow <- gdd_match_key$iso2[x]

  # Get data update data
  gdd_out <- gdd_orig %>%
    # Borrowed data
    filter(iso3==iso_borrow) %>%
    # Overwrite borrowed country
    mutate(iso3=iso_do,
           country=cntry_do) %>%
    # Add GDD data type
    mutate(gdd_type="Borrowed")

})

# Merge data
gdd <- bind_rows(gdd_orig, gdd_add) %>%
  # Add country used for GDD data
  left_join(gdd_match_key %>% select(iso1, iso2, country2), by=c("iso3"="iso1")) %>%
  rename(gdd_iso3=iso2, gdd_country=country2) %>%
  # Mark whether GDD borrowed or reported
  mutate(gdd_type=ifelse(is.na(gdd_type), "Reported", gdd_type),
         gdd_iso3=ifelse(gdd_type=="Reported", iso3, gdd_iso3),
         gdd_country=ifelse(gdd_type=="Reported", country, gdd_country)) %>%
  # Arrange
  select(continent:gdd_type, gdd_iso3, gdd_country, everything()) %>%
  arrange(continent, iso3, nutrient_type, nutrient, sex, age_range)

# Check ISOs again
isos_gdd <- sort(unique(gdd$iso3))
isos2match <- pop_orig %>%
  select(iso3, country) %>%
  unique() %>%
  filter(!iso3 %in% isos_gdd)

# Inspect
freeR::complete(gdd)


# Summarize GDD supplies to match population data
################################################################################

# Average GDD supply by right age groups
gdd_harmonized <- gdd %>%
  # Update sexes
  mutate(sex=recode(sex, "Male"="Males", "Female"="Females")) %>%
  # Update nutrient names
  mutate(nutrient=recode(nutrient,
                         "Vitamin B3"="Niacin",
                         "Vitamin B2"="Riboflavin",
                         "Vitamin B1"="Thiamin")) %>%
  # Update nutrient units
  mutate(units=recode(units, "µg"="ug")) %>%
  # Add harmonized age group
  mutate(age_range=as.character(age_range),
         age_range_use=recode_factor(age_range,
                                      "0-11 mo."="0-4",
                                      "12-23 mo."="0-4",
                                      "2-5 years"="0-4",
                                      "6-10 years"="5-9",
                                      "11-14 years"="10-14",
                                      "15-19 years"="15-19",
                                      "20-24 years"="20-24",
                                      "25-29 years"="25-29",
                                      "30-34 years"="30-34",
                                      "35-39 years"="35-39",
                                      "40-44 years"="40-44",
                                      "45-49 years"="45-49",
                                      "50-54 years"="50-54",
                                      "55-59 years"="55-59",
                                      "60-64 years"="60-64",
                                      "65-69 years"="65-69",
                                      "70-74 years"="70-74",
                                      "75-79 years"="75-79",
                                      "80-84 years"="80+",
                                      "85-89 years"="80+",
                                      "90-94 years"="80+",
                                      "95+ years"="80+")) %>%
  # Summarize by harmonized age group
  group_by(nutrient_type, nutrient, units, continent, country, iso3,  gdd_type, gdd_iso3, gdd_country, sex, age_range_use) %>%
  summarize(supply_med=mean(supply_med)) %>%
  ungroup() %>%
  # Rename
  rename(age_range=age_range_use)

# Inspect
str(gdd_harmonized)
freeR::complete(gdd_harmonized)


# Format ARs
################################################################################

# Nutrient names
sort(unique(ars_orig$nutrient))
sort(unique(gdd_harmonized$nutrient))

# Format ARs
ars <- ars_orig %>%
  # Reduce to ARs and eliminate stafes
  filter(nrv_type=="Average requirement" & !stage%in%c("Lactation", "Pregnancy")) %>%
  # Simplify
  select(nutrient, units, source, sex, age_group, nrv, nrv_note) %>%
  # Rename
  rename(age_range=age_group, ar=nrv, ar_note=nrv_note) %>%
  # Format a few nutrients
  mutate(nutrient=recode(nutrient,
                         "Vitamin A"="Vitamin A (RAE)",
                         "Vitamin B-6"="Vitamin B6",
                         "Vitamin B-12"="Vitamin B12")) %>%
  # Convert copper from micrograms to milligrams
  mutate(ar=ifelse(nutrient=="Copper", measurements::conv_unit(ar, from="ug", to="mg"), ar),
         units=case_when(nutrient=="Copper" ~ "mg",
                         units=="ug RAE" ~ "µg RAE",
                         units=="ug DFE" ~ "µg DFE",
                         T ~ units)) %>%
  # Add AR CV
  mutate(ar_cv=ifelse(nutrient=="Vitamin B12", 0.25, 0.10)) %>%
  select(nutrient:ar, ar_cv, ar_note, everything())

# Split children into males/females
ars_mf <- ars %>%
  filter(sex %in% c("Males", "Females"))
ars_child <- ars  %>%
  filter(!sex %in% c("Males", "Females"))
ars_child_m <- ars_child %>% mutate(sex="Males")
ars_child_f <- ars_child %>% mutate(sex="Females")
ars_use <- bind_rows(ars_mf, ars_child_m, ars_child_f) %>%
  arrange(nutrient, sex, age_range) %>%
  select(-ar_note) %>%
  rename(ar_source=source, ar_units=units)


# Build data
################################################################################

# Merge data
data <- gdd_harmonized %>%
  # Adjust calcium intakes
  mutate(supply_med=ifelse(nutrient=="Calcium", supply_med+1.7*42, supply_med)) %>%
  # Add population data
  left_join(pop %>% select(-c(year, country)), by=c("iso3", "sex", "age_range")) %>%
  # Add HDI
  left_join(hdi_orig %>% select(-country), by="iso3") %>%
  # Fill in HDI gaps (add HDI of country providing GDD data)
  left_join(hdi_orig %>% select(iso3, hdi_catg) %>% rename(hdi_catg2=hdi_catg), by=c("gdd_iso3"="iso3")) %>%
  mutate(hdi_catg=ifelse(!is.na(hdi_catg), hdi_catg, hdi_catg2)) %>%
  select(-hdi_catg2) %>%
  # Assign an HDI to countries missing an HDI category
  mutate(hdi_catg=case_when(country=="French Polynesia" ~ "Medium",
                            country=="New Caledonia" ~ "Medium",
                            country=="Taiwan" ~ "Very high",
                            T ~ hdi_catg)) %>%
  # Add zinc absorpiton
  mutate(zinc_iso3=recode(gdd_iso3, "SSD"="CAF")) %>%
  left_join(zinc_orig, by=c("zinc_iso3"="iso3")) %>%
  # Add ARs
  # Recode age range for matching to ARs
  # Requires converting factor age groups back to character age groups
  mutate(age_range=as.character(age_range)) %>%
  mutate(age_range_ar=recode(age_range,
                              "0-4"="1-3 y",
                              "5-9"="7-10 y",
                              "10-14"="11-14 y",
                              "15-19"="15-17 y",
                              "20-24"="18-24 y",
                              "25-29"="25-50 y",
                              "30-34"="25-50 y",
                              "35-39"="25-50 y",
                              "40-44"="25-50 y",
                              "45-49"="25-50 y",
                              "50-54"="51-70 y",
                              "55-59"="51-70 y",
                              "60-64"="51-70 y",
                              "65-69"="51-70 y",
                              "70-74"=">70 y",
                              "75-79"=">70 y",
                              "80+"=">70 y")) %>%
  # Recode nutrient for matching to ARs
  # mutate(nutrient_ar=case_when(nutrient=="Iron" & hdi_catg=="Low" ~ "Iron (low absorption)",
  #                              nutrient=="Iron" & hdi_catg=="Medium" ~ "Iron (moderate absorption)",
  #                              nutrient=="Iron" & hdi_catg %in% c("High", "Very high") ~ "Iron (high absorption)",
  #                              nutrient=="Zinc" & hdi_catg=="Low" ~ "Zinc (unrefined diet)",
  #                              nutrient=="Zinc" & hdi_catg=="Medium" ~ "Zinc (semi-unrefined diet)",
  #                              nutrient=="Zinc" & hdi_catg=="High" ~ "Zinc (semi-refined diet)",
  #                              nutrient=="Zinc" & hdi_catg=="Very high" ~ "Zinc (refined diet)",
  #                              T ~ nutrient)) %>%
  # Fix iron and zinc age groups
  mutate(age_range_ar=ifelse(nutrient=="Zinc" & age_range %in% c("25-29", "30-34", "35-39", "40-44", "45-49"), "25-50 y", age_range_ar),
         age_range_ar=ifelse(nutrient=="Iron" & age_range %in% c("25-29", "30-34", "35-39", "40-44", "45-49"), "25-50 y", age_range_ar)) %>%
  # Add ARs from Allen
  left_join(ars_use, by=c("nutrient_ar"="nutrient", "sex", "age_range_ar"="age_range")) %>%
  # Add zinc ARs
  left_join(zinc_ars_orig, by=c("sex", "age_range", "zinc_frac")) %>%
  # Finalize Zinc ARs
  mutate(ar_source=ifelse(nutrient=="Zinc", "EFSA", ar_source),
         ar_units=ifelse(nutrient=="Zinc", "mg", ar_units),
         ar=ifelse(nutrient=="Zinc", zinc_ar_mg, ar),
         ar_cv=ifelse(nutrient=="Zinc", 0.1, ar_cv)) %>%
  select(-zinc_ar_mg) %>%
  # Refactor age range
  mutate(age_range=factor(age_range, levels=levels(gdd_harmonized$age_range)))

# Inspect
freeR::complete(data)

# Which nutrients are missing AR?
data %>% filter(is.na(ar)) %>% pull(nutrient) %>% unique()

# Build country key
################################################################################

# Country key
cntry_key <- data %>%
  # Reduce to single nutrient (so population doesn't get double counted)
  filter(nutrient=="Calcium") %>%
  # Summarize
  group_by(continent, country, iso3, hdi, hdi_catg) %>%
  summarize(npeople=sum(npeople),
            gdd_yn=unique(gdd_type)) %>%
  ungroup()

# Check number of people
sum(cntry_key$npeople, na.rm=T) / 1e9
sum(pop$npeople, na.rm=T) / 1e9 - sum(cntry_key$npeople, na.rm=T) / 1e9

# Check
freeR::which_duplicated(cntry_key$country)
freeR::which_duplicated(cntry_key$iso3)

# Export
write.csv(cntry_key, file=file.path(outdir, "country_key_gdd.csv"), row.names=F)


# Check data
################################################################################

# The following ISOs in GDD do not have population data
data %>%
  filter(is.na(npeople)) %>%
  pull(iso3) %>%
  unique() %>% sort()

# The following ISOs in GDD do not have a human development index (but I updated the catg)
data %>%
  filter(is.na(hdi)) %>%
  pull(iso3) %>%
  unique() %>% sort()

# Check AR coverage
ar_coverage <- data %>%
  select(nutrient_ar, sex, age_range, ar) %>%
  unique()

# Plot AR coverage check
ggplot(ar_coverage, aes(x=age_range, y=nutrient_ar, fill=ar)) +
  facet_wrap(~sex) +
  geom_tile() +
  labs(x="Age range (yr)", y="") +
  scale_fill_continuous(name="AR", na.value="grey90") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Check AR units
unit_key <- data %>%
  select(nutrient, nutrient_ar, units, ar_units) %>%
  unique()
unit_key_check <- unit_key %>%
  filter(!is.na(ar_units) & ar_units!=units)


# Remove nutrients without ARs
################################################################################

# Nutrients w/out ARs
nutrients_wo_ars <- c("Total omega-6 fatty acids", "Seafood omega-3 fatty acids",
                      "Saturated fat", "Potassium", "Plant omega-3 fatty acids", "Monounsaturated fatty acids")

# Remove nutrients without ARs
data_out <- data %>%
  filter(!nutrient %in% nutrients_wo_ars)

# Nutrients included
nutrients_in <- sort(unique(data_out$nutrient))
nutrients_in

# Any not with dist info?
nutrients_w_shape <- nutriR::dists_full$nutrient %>% unique() %>% sort()
nutrients_in[!nutrients_in %in% nutrients_w_shape]


# Export data
################################################################################

# Export data
saveRDS(data_out, file=file.path(outdir, "2018_subnational_nutrient_intake_estimates.Rds"))



