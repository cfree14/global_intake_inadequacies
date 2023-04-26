

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
outdir <- "data/world/processed"
plotdir <- "figures"


# Build world
################################################################################

# Projections
wgs84 <- sf::st_crs("+proj=longlat +datum=WGS84")

# Get country data
world_sm_orig <- rnaturalearth::ne_countries(scale="small", returnclass = "sf") %>% sf::st_transform(wgs84)
world_lg_orig <- rnaturalearth::ne_countries(scale="large", returnclass = "sf") %>% sf::st_transform(wgs84)
world_tiny_orig <- rnaturalearth::ne_countries(type="tiny_countries", returnclass="sf") %>% sf::st_transform(wgs84)

# Format world (small) - for plotting
world_sm <- world_sm_orig %>%
  # Simplify
  select(iso3_orig=su_a3,
         country_orig=subunit) %>%
  # Look up ISO3 and country
  mutate(country_corr=countrycode::countrycode(country_orig, "country.name", "country.name"),
         iso3_corr=countrycode::countrycode(country_corr, "country.name", "iso3c")) %>%
  # Pick
  mutate(country=ifelse(!is.na(country_corr), country_corr, country_orig),
         iso3=ifelse(!is.na(iso3_corr), iso3_corr, iso3_orig)) %>%
  # Simplify
  select(country_orig, iso3_orig, country, iso3, everything()) %>%
  # Fix Cyprus
  mutate(iso3=ifelse(country_orig=="Northern Cyprus", iso3_orig, iso3),
         country=ifelse(country_orig=="Northern Cyprus", country_orig, country)) %>%
  # Simplify
  select(iso3, country, geometry)

# Check
freeR::which_duplicated(world_sm$iso3)
freeR::which_duplicated(world_sm$country)

# Format world (large) - for centroids
world_lg <- world_lg_orig %>%
  # Simplify
  select(iso3_orig=su_a3,
         country_orig=subunit) %>%
  # Look up ISO3 and country
  mutate(country_corr=countrycode::countrycode(country_orig, "country.name", "country.name"),
         iso3_corr=countrycode::countrycode(country_corr, "country.name", "iso3c")) %>%
  # Pick
  mutate(country=ifelse(!is.na(country_corr), country_corr, country_orig),
         iso3=ifelse(!is.na(iso3_corr), iso3_corr, iso3_orig)) %>%
  # Add area
  mutate(area_sqkm=sf::st_area(.) %>% as.numeric() %>% measurements::conv_unit(., from="m2", to="km2")) %>%
  # Simplify
  select(country_orig, iso3_orig, country, iso3, everything()) %>%
  # Fix Cyprus
  mutate(iso3=ifelse(country_orig %in% c("Northern Cyprus", "Cyprus No Mans Area", "US Naval Base Guantanamo Bay"), iso3_orig, iso3),
         country=ifelse(country_orig %in% c("Northern Cyprus", "Cyprus No Mans Area", "US Naval Base Guantanamo Bay"), country_orig, country)) %>%
  # Simplify
  select(iso3, country, area_sqkm, geometry)

# Check
freeR::which_duplicated(world_lg$iso3)
freeR::which_duplicated(world_lg$country)

# Format tiny countries - for plotting and centroids
world_tiny <- world_tiny_orig  %>%
  # Simplify
  select(iso3=su_a3,
         country_orig=subunit) %>%
  # Correct country
  mutate(country=countrycode::countrycode(iso3, "iso3c", "country.name"),
         country=ifelse(is.na(country), country_orig, country)) %>%
  # Add lat/long
  mutate(long_dd = sf::st_coordinates(.)[,1],
         lat_dd = sf::st_coordinates(.)[,2]) %>%
  # Simplify
  select(iso3, country, long_dd, lat_dd, geometry)

# Check
freeR::which_duplicated(world_tiny$iso3)
freeR::which_duplicated(world_tiny$country)


# Centroids - large
world_lg_centroids <- world_lg %>%
  sf::st_make_valid() %>%
  sf::st_centroid(of_largest_polygon = T) %>%
  # Add lat/long
  mutate(long_dd = sf::st_coordinates(.)[,1],
         lat_dd = sf::st_coordinates(.)[,2]) %>%
  # Drop geo and simplify
  sf::st_drop_geometry() %>%
  select(iso3, country, long_dd, lat_dd, area_sqkm)

# Centroids - tiny
world_tiny_centroids <- world_tiny %>%
  sf::st_drop_geometry() %>%
  select(iso3, country, long_dd, lat_dd) %>%
  mutate(area_sqkm=0)

# Centroids all
world_centroids <- bind_rows(world_lg_centroids, world_tiny_centroids %>% filter(!iso3 %in% world_lg_centroids$iso3))
freeR::which_duplicated(world_centroids$iso3)
freeR::which_duplicated(world_centroids$iso3)


# Export data
################################################################################

# Export data
saveRDS(world_lg, file=file.path(outdir, "world_large.Rds"))
saveRDS(world_sm, file=file.path(outdir, "world_small.Rds"))
saveRDS(world_centroids, file=file.path(outdir, "world_centroids.Rds"))




