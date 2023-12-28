

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
outdir <- "data/gdd/processed"
plotdir <- "figures/gdd_approach"
tabledir <- "tables"
gisdir <- "data/world/processed"

# Read world data
world_lg_orig <- readRDS(file=file.path(gisdir, "world_large.Rds"))
world_sm_orig <- readRDS(file=file.path(gisdir, "world_small.Rds"))
world_centers_orig <- readRDS(file=file.path(gisdir, "world_centroids.Rds"))



# Build data
################################################################################

# Data
world_sm <- world_sm_orig %>%
  sf::st_as_sf() %>%
  # Add subregion
  mutate(region=countrycode::countrycode(iso3, "iso3c", "un.region.name")) %>%
  mutate(subregion=countrycode::countrycode(iso3, "iso3c", "un.regionsub.name")) %>%
  mutate(subregion=case_when(iso3=="CHI" ~ "Western Europe",
                             iso3=="XKX" ~ "Southern Europe",
                             iso3=="TWN" ~ "Eastern Asia",
                             iso3=="ATA" ~ "", # Antarctica
                             iso3=="CYN" ~ "Western Asia", # Northern Cyprus
                             iso3=="KOS" ~ "Southern Europe", # Kosovo
                             iso3=="SOL" ~ "Melanesia", # Solomon Islands
                             T ~ subregion)) %>%
  mutate(subregion=recode(subregion,
                          "Latin America and the Caribbean"="Latin America / Caribbean",
                          "Australia and New Zealand"="Australia / New Zealand"))

# Centers
world_centers <- world_centers_orig %>%
  # Filter to small
  filter(area_sqkm<=25000) %>%
  # Add subregion
  mutate(region=countrycode::countrycode(iso3, "iso3c", "un.region.name")) %>%
  mutate(subregion=countrycode::countrycode(iso3, "iso3c", "un.regionsub.name")) %>%
  mutate(subregion=case_when(iso3=="CHI" ~ "Western Europe",
                             iso3=="XKX" ~ "Southern Europe",
                             iso3=="TWN" ~ "Eastern Asia",
                             iso3=="ATC" ~ "Australia and New Zealand", # Ashmore and Cartier Islands
                             iso3=="BAC" ~ "Sub-Saharan Africa", # Ascension
                             iso3=="BJN" ~ "Latin America and the Caribbean", # Bajo Nuevo Bank (Petrel Is.)
                             iso3=="CLP" ~ "Latin America and the Caribbean", # Clipperton Island
                             iso3=="CNM" ~ "Western Asia", # Cyprus No Mans Area
                             iso3=="CSI" ~ "Australia and New Zealand", # Coral Sea Islands
                             iso3=="CYN" ~ "Western Asia", # Northern Cyprus
                             iso3=="ESB" ~ "Western Asia", # Dhekelia Sovereign Base Area
                             iso3=="ESC" ~ "Southern Europe", # Canary Islands
                             iso3=="IOA" ~ "South-eastern Asia", # Indian Ocean Territories
                             iso3=="KAB" ~ "Central Asia", # Baykonur Cosmodrome
                             iso3=="KAS" ~ "Southern Asia", # Siachen Glacier
                             iso3=="KOS" ~ "Southern Europe", # Kosovo
                             iso3=="NJM" ~ "Northern Europe", # Jan Mayen
                             iso3=="PAZ" ~ "Southern Europe", # Azores
                             iso3=="PGA" ~ "South-eastern Asia", # Spratly Islands
                             iso3=="PMD" ~ "Southern Europe", # Madeira
                             iso3=="SCR" ~ "South-eastern Asia", # Scarborough Reef
                             iso3=="SER" ~ "Latin America and the Caribbean", # Serranilla Bank
                             iso3=="USG" ~ "Latin America and the Caribbean", # US Naval Base Guantanamo Bay
                             iso3=="WSB" ~ "Western Asia", # Akrotiri Sovereign Base Area
                             T ~ subregion)) %>%
  mutate(subregion=recode(subregion,
                          "Latin America and the Caribbean"="Latin America / Caribbean",
                          "Australia and New Zealand"="Australia / New Zealand"))


# Plot data
################################################################################

# Theme
my_theme <-  theme(legend.text=element_text(size=5),
                   legend.title=element_text(size=6),
                   # axis.text=element_text(size=6),
                   # axis.title=element_text(size=8),
                   # axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.text=element_blank(),
                   axis.ticks = element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.margin = margin(-8,0,-5,0),
                   legend.key.size = unit(0.5, "cm"),
                   legend.position = "bottom",
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot population
g <- ggplot(world_sm, aes(fill=subregion)) +
  geom_sf(lwd=0.2, color="grey30") +
  geom_point(data=world_centers, mapping=aes(x=long_dd, y=lat_dd, fill=subregion),
             color="grey30", size=1.8, pch=21) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_discrete(name="UN subregion", na.translate = F) +
  # Crop
  coord_sf(ylim=c(-52, 80), expand = T) +
  # Theme
  theme_bw() + my_theme
g


# Export
ggsave(g, filename=file.path(plotdir, "FigS12_subregion_map.png"),
       width=6.5, height=3.75, units="in", dpi=600)


