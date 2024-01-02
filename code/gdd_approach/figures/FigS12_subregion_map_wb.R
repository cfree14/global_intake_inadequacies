

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
fg_orig <- readRDS(file=file.path(gisdir, "french_guiana.Rds"))
world_lg_orig <- readRDS(file=file.path(gisdir, "world_large.Rds"))
world_sm_orig <- readRDS(file=file.path(gisdir, "world_small.Rds"))
world_centers_orig <- readRDS(file=file.path(gisdir, "world_centroids.Rds"))

# Read region key
region_key_orig <- readxl::read_excel("data/wb_regions/Country_Region_WorldBank.xlsx")

# Format key
region_key <- region_key_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(iso3=country_code,
         country=table_name) %>%
  # Filter
  filter(!is.na(region))



# Build data
################################################################################

# FG
fg <- fg_orig %>%
  mutate(country="French Guiana",
         iso3="GUF",
         region="Latin Amercia & Caribbean") %>%
  select(iso3, country, region, geometry)

# Data
world_sm <- world_sm_orig %>%
  sf::st_as_sf() %>%
  # Add region
  left_join(region_key %>% select(iso3, region)) %>%
  # Fill in missing regions
  mutate(region=case_when(country=="Antarctica" ~ NA,
                          country=="French Southern Territories" ~ NA,
                          country=="Northern Cyprus" ~ "Europe & Central Asia",
                          country=="Falkland Islands" ~ "Latin Amercia & Caribbean",
                          country=="Kosovo" ~  "Europe & Central Asia",
                          country=="Western Sahara" ~ "Middle East & North Africa",
                          country=="Somaliland" ~ "Sub-Saharan Africa",
                          country=="Taiwan" ~ "East Asia & Pacific",
                          T ~ region))

# Centers
world_centers <- world_centers_orig %>%
  # Filter to small
  # filter(area_sqkm<=25000) %>%
  # Add region
  left_join(region_key %>% select(iso3, region)) %>%
  # Fill in missing regions
  mutate(region=case_when(country=="Antarctica" ~ NA,
                          country=="French Southern Territories" ~ NA,
                          country=="Northern Cyprus" ~ "Europe & Central Asia",
                          country=="Falkland Islands" ~ "Latin Amercia & Caribbean",
                          country=="Kosovo" ~  "Europe & Central Asia",
                          country=="Western Sahara" ~ "Middle East & North Africa",
                          country=="Somaliland" ~ "Sub-Saharan Africa",
                          country=="Taiwan" ~ "East Asia & Pacific",
                          T ~ region))

# Make key
region_key_out <- bind_rows(fg %>% sf::st_drop_geometry(),
                            world_centers %>% sf::st_drop_geometry()) %>%
  arrange(iso3) %>%
  # Select
  select(iso3, country, region) %>%
  filter(!is.na(region))

# Export
saveRDS(region_key_out, file = file.path("data/wb_regions/region_key.Rds"))


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
g <- ggplot(world_sm, aes(fill=region)) +
  geom_sf(lwd=0.2, color="grey30") +
  geom_sf(data=fg, lwd=0.2, color="grey30") +
  # geom_point(data=world_centers, mapping=aes(x=long_dd, y=lat_dd, fill=region),
  #            color="grey30", size=1.8, pch=21) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_discrete(name="World Bank\nregion", na.translate = F) +
  # Crop
  coord_sf(ylim=c(-52, 80), expand = T) +
  # ThemeR
  theme_bw() + my_theme
g


# Export
ggsave(g, filename=file.path(plotdir, "FigS12_subregion_map.png"),
       width=6.5, height=3.25, units="in", dpi=600)


