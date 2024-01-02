

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
datadir <- "data/world/raw/guf_adm_ign_shp"
plotdir <- "figures"

# Projections
wgs84 <- sf::st_crs("+proj=longlat +datum=WGS84")

# Read data
# https://data.humdata.org/dataset/cod-ab-guf
data_orig <- sf::st_read(file.path(datadir, "guf_admbnda_adm0_ign.shp")) %>%
  sf::st_transform(wgs84)
plot(data_orig)

# Export
saveRDS(data_orig, file="data/world/processed/french_guiana.Rds")

