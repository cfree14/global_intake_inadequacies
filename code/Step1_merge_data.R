

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
popdir <- "data/population"
genusdir <- "data/genus/processed"
hdidir <- "data/human_development_index"
outdir <- "output"
plotdir <- "figures"
tabledir <- "tables"

# Read GENuS data
genus_orig_full <- readRDS(file.path(genusdir, "GENUS_1961_2011_country_agesex_nutrient_intakes.Rds"))

# Read population data
pop_orig <- readRDS(file.path(popdir, "WB_1960_2020_population_size_by_country_agesex.Rds"))

# Read HDI
hdi_orig <- readRDS(file.path(hdidir, "UNDP_2020_human_development_index.Rds")) %>%
  mutate(hdi_catg=as.character(hdi_catg))

# Get ARs
ars_orig <- nutriR::nrvs


# Format GENUS data
################################################################################

# Format GENUS data
genus_orig <- genus_orig_full %>%
  # Reduce to 2011
  filter(year==2011)

# Inspect population age/sex key
agesex_key_pop <- pop_orig %>%
  select(sex, age) %>%
  unique() %>%
  arrange(sex, age)

# Inspect GENUS age/sex key
agesex_key_genus <- genus_orig %>%
  select(sex, age_range) %>%
  unique() %>%
  arrange(sex, age_range)

# Based on this, you'll have to do the following formatting:
# (1) Rename age as age range in the pop key
# (2) Duplicate the children rows in GENUS and turn into male and female rows
# (3) Also, add continent to GENUS data

# Format population data
pop <- pop_orig %>%
  rename(age_range=age) %>%
  filter(year==2011)
sum(pop$npeople, na.rm=T) / 1e9

# Format GENUS data
# (1) Separate children and non-children data; (2) duplicate children into males/females; (3) merge and sort
genus_child <- genus_orig %>%
  filter(sex=="Children")
genus_mf <- genus_orig %>%
  filter(sex!="Children")
genus_child_m <- genus_child %>%
  mutate(sex="Males")
genus_child_f <- genus_child %>%
  mutate(sex="Females")
genus1 <- bind_rows(genus_mf, genus_child_m, genus_child_f) %>%
  # Add GENUS data type
  mutate(genus_type="Reported") %>%
  # Arrange
  select(continent, iso3, country, genus_type, everything()) %>%
  arrange(continent, iso3, nutrient, sex, age_range, year)


# Expand GENUS data
################################################################################

# Which countries have population data but not GENuS data?
isos_genus <- sort(unique(genus1$iso3))
isos2match <- pop_orig %>%
  select(iso3, country) %>%
  unique() %>%
  filter(!iso3 %in% isos_genus)
write.csv(isos2match, file=file.path(tabledir, "TableSX_countries_without_genus_data.csv"), row.names=F)

# Read GENUS country match key
genus_match_key <- readxl::read_excel(file.path(tabledir, "TableSX_countries_without_genus_data.xlsx"), skip=1) %>%
  # Rename
  setNames(c("iso1", "country1", "iso2", "country2"))

# Loop through key and create data to add
x <- 1
genus_add <- purrr::map_df(1:nrow(genus_match_key), function(x){

  # ISOs
  iso_do <- genus_match_key$iso1[x]
  cntry_do <- genus_match_key$country1[x]
  # continent_do <- countrycode::countrycode(cntry_do, "country.name", "continent")
  iso_borrow <- genus_match_key$iso2[x]

  # Get data update data
  genus_out <- genus1 %>%
    # Borrowed data
    filter(iso3==iso_borrow) %>%
    # Overwrite borrowed country
    mutate(iso3=iso_do,
           country=cntry_do) %>%
    # Add GENUS data type
    mutate(genus_type="Borrowed")

})

# Merge data
genus <- bind_rows(genus1, genus_add) %>%
  # Add country used for GENuS data
  left_join(genus_match_key %>% select(iso1, iso2, country2), by=c("iso3"="iso1")) %>%
  rename(genus_iso3=iso2, genus_country=country2) %>%
  mutate(genus_iso3=ifelse(genus_type=="Reported", iso3, genus_iso3),
         genus_country=ifelse(genus_type=="Reported", country, genus_country)) %>%
  # Arrange
  select(continent:genus_type, genus_iso3, genus_country, everything()) %>%
  arrange(continent, iso3, nutrient, sex, age_range, year)

# Check ISOs again
isos_genus <- sort(unique(genus$iso3))
isos2match <- pop_orig %>%
  select(iso3, country) %>%
  unique() %>%
  filter(!iso3 %in% isos_genus)

# Inspect
freeR::complete(genus)


# Format ARs
################################################################################

# Format ARs
ars <- ars_orig %>%
  # Reduce to ARs and eliminate stafes
  filter(nrv_type=="Average requirement" & !stage%in%c("Lactation", "Pregnancy")) %>%
  # Simplify
  select(nutrient, units, source, sex, age_group, nrv, nrv_note) %>%
  # Rename
  rename(age_range=age_group, ar=nrv, ar_note=nrv_note) %>%
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
data <- genus %>%
  # Add population data
  left_join(pop %>% select(-country), by=c("iso3", "year", "sex", "age_range")) %>%
  # Add HDI
  left_join(hdi_orig %>% select(-country), by="iso3") %>%
  # Fill in HDI gaps (add HDI of country providing GENUS data)
  left_join(hdi_orig %>% select(iso3, hdi_catg) %>% rename(hdi_catg2=hdi_catg), by=c("genus_iso3"="iso3")) %>%
  mutate(hdi_catg=ifelse(!is.na(hdi_catg), hdi_catg, hdi_catg2)) %>%
  select(-hdi_catg2) %>%
  # Assign an HDI to countries missing an HDI category
  mutate(hdi_catg=case_when(country=="French Polynesia" ~ "Medium",
                            country=="New Caledonia" ~ "Medium",
                            T ~ hdi_catg)) %>%
  # Add ARs
  # Recode nutrient for matching to ARs
  mutate(nutrient_ar=case_when(nutrient=="Iron" & hdi_catg=="Low" ~ "Iron (low absorption)",
                                nutrient=="Iron" & hdi_catg=="Medium" ~ "Iron (moderate absorption)",
                                nutrient=="Iron" & hdi_catg %in% c("High", "Very high") ~ "Iron (high absorption)",
                                nutrient=="Zinc" & hdi_catg=="Low" ~ "Zinc (unrefined diet)",
                                nutrient=="Zinc" & hdi_catg=="Medium" ~ "Zinc (semi-unrefined diet)",
                                nutrient=="Zinc" & hdi_catg=="High" ~ "Zinc (semi-refined diet)",
                                nutrient=="Zinc" & hdi_catg=="Very high" ~ "Zinc (refined diet)",
                                T ~ nutrient)) %>%
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
  # Fix iron and zinc age groups
  mutate(age_range_ar=ifelse(nutrient=="Zinc" & age_range %in% c("25-29", "30-34", "35-39", "40-44", "45-49"), "25-50 y", age_range_ar),
         age_range_ar=ifelse(nutrient=="Iron" & age_range %in% c("25-29", "30-34", "35-39", "40-44", "45-49"), "25-50 y", age_range_ar)) %>%
  # Add AR
  left_join(ars_use, by=c("nutrient_ar"="nutrient", "sex", "age_range_ar"="age_range")) %>%
  # Refactor age range
  mutate(age_range=factor(age_range, levels=levels(genus$age_range)))

# Inspect
freeR::complete(data)


# Build country key
################################################################################

# Country key
cntry_key <- data %>%
  # Reduce to single nutrient (so population doesn't get double counted)
  filter(nutrient=="Calcium") %>%
  # Summarize
  group_by(continent, country, iso3, hdi, hdi_catg) %>%
  summarize(npeople=sum(npeople),
            genus_yn=unique(genus_type)) %>%
  ungroup()

# Check number of people
sum(cntry_key$npeople, na.rm=T) / 1e9
sum(pop$npeople, na.rm=T) / 1e9 - sum(cntry_key$npeople, na.rm=T) / 1e9

# Export
write.csv(cntry_key, file=file.path(outdir, "country_key.csv"), row.names=F)


# Check data
################################################################################

# The following ISOs in GENUS do not have population data
data %>%
  filter(is.na(npeople)) %>%
  pull(iso3) %>%
  unique() %>% sort()

# The following ISOs in GENUS do not have a human development index (but I updated the catg)
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
  select(nutrient, nutrient_ar, units_short, ar_units) %>%
  unique()
unit_key_check <- unit_key %>%
  filter(!is.na(ar_units) & ar_units!=units_short)


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "2011_subnational_nutrient_intake_estimates.Rds"))



