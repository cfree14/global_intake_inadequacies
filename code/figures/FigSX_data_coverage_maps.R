

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
outdir <- "output"
plotdir <- "figures"
tabledir <- "tables"

# Read data
data <- read.csv(file=file.path(outdir, "country_key.csv"), as.is=T)

# Projections
wgs84 <- sf::st_crs("+proj=longlat +datum=WGS84")

# Build country data
################################################################################

# Get country data
world_sm_orig <- rnaturalearth::ne_countries(scale="small", returnclass = "sf") %>% sf::st_transform(wgs84)
world_lg_orig <- rnaturalearth::ne_countries(scale="large", returnclass = "sf") %>% sf::st_transform(wgs84)
world_tiny_orig <- rnaturalearth::ne_countries(type="tiny_countries", returnclass="sf") %>% sf::st_transform(wgs84)

# Format world (small) - for plotting
world_sm <- world_sm_orig %>%
  select(brk_a3) %>%
  rename(iso3=brk_a3) %>%
  # Add metadata
  left_join(data, by="iso3") %>%
  mutate(genus_yn=ifelse(is.na(genus_yn), F, genus_yn)) %>%
  # Formay HDI catg
  mutate(hdi_catg=factor(hdi_catg, levels=c("Low", "Medium", "High", "Very high")))

# Format world (large) - for centroids
world_lg <- world_lg_orig %>%
  select(brk_a3) %>%
  rename(iso3=brk_a3) %>%
  # Calculate area
  mutate(area_sqkm=sf::st_area(.) %>% as.numeric() %>% measurements::conv_unit(., from="m2", to="km2"))

# Format tiny countries - for plotting and centroids
world_tiny <- world_tiny_orig  %>%
  select(brk_a3) %>%
  rename(iso3=brk_a3) %>%
  # Add metadata
  left_join(data, by="iso3") %>%
  # Formay HDI catg
  mutate(hdi_catg=factor(hdi_catg, levels=c("Low", "Medium", "High", "Very high"))) %>%
  # Add lat/long
  mutate(long_dd = sf::st_coordinates(.)[,1],
         lat_dd = sf::st_coordinates(.)[,2])

# Centroids - large
world_lg_centroids <- world_lg %>%
  sf::st_make_valid() %>%
  sf::st_centroid() %>%
  # Add lat/long
  mutate(long_dd = sf::st_coordinates(.)[,1],
         lat_dd = sf::st_coordinates(.)[,2]) %>%
  # Drop geo and simplify
  sf::st_drop_geometry() %>%
  select(iso3, long_dd, lat_dd, area_sqkm)

# Centroids - tiny
world_tiny_centroids <- world_tiny %>%
  sf::st_drop_geometry() %>%
  select(iso3, long_dd, lat_dd) %>%
  mutate(area_sqkm=0)

# Centroids all
world_centroids <- bind_rows(world_lg_centroids, world_tiny_centroids %>% filter(!iso3 %in% world_lg_centroids$iso3))
freeR::which_duplicated(world_centroids$iso3)


# Build GENUS match key
################################################################################

# Read GENUS country match key
genus_match_key <- readxl::read_excel(file.path(tabledir, "TableSX_countries_without_genus_data.xlsx"), skip=1) %>%
  # Rename
  setNames(c("iso1", "country1", "iso2", "country2")) %>%
  # Add centroids 1
  left_join(world_centroids %>% select(-area_sqkm), by=c("iso1"="iso3")) %>%
  rename(lat1=lat_dd, long1=long_dd) %>%
  # Add centroids 1
  left_join(world_centroids %>% select(-area_sqkm), by=c("iso2"="iso3")) %>%
  rename(lat2=lat_dd, long2=long_dd)



# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot GENuS coverage
g <- ggplot(world_sm , aes(fill=genus_yn)) +
  geom_sf(lwd=0.1, color="grey30") +
  geom_sf(data=world_tiny, color="grey30", size=1.5, pch=21) +
  # Plot links
  geom_segment(data=genus_match_key, mapping=aes(x=long1, xend=long2, y=lat1, yend=lat2), inherit.aes = F, lwd=0.4) +
  # Legend
  scale_fill_manual(name="GENuS data?", values=c("red", "grey90")) +
  # Crop
  coord_sf(ylim=c(-52, 80)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_genus_data.png"),
       width=6.5, height=2.75, units="in", dpi=600)


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot population
g1 <- ggplot(world, aes(fill=npeople)) +
  geom_sf(lwd=0.2, color="grey30") +
  geom_sf(data=world_tiny, color="grey30", size=1.5, pch=21) +
  # Legend
  scale_fill_gradientn(name="Population", colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       trans="log10") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(ylim=c(-52, 80)) +
  # Theme
  theme_bw() + my_theme
g1

# Plot HDI index
g2 <- ggplot(world, aes(fill=hdi)) +
  geom_sf(lwd=0.2, color="grey30") +
  geom_sf(data=world_tiny, color="grey30", size=1.5, pch=21) +
  # Legend
  scale_fill_gradientn(name="HDI index", colors=RColorBrewer::brewer.pal(9, "Spectral")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(ylim=c(-52, 80)) +
  # Theme
  theme_bw() + my_theme
g2

# Plot HDI category
g3 <- ggplot(world, aes(fill=hdi_catg)) +
  geom_sf(lwd=0.2, color="grey30") +
  geom_sf(data=world_tiny, color="grey30", size=1.5, pch=21, show.legend = F) +
  # Legend
  scale_fill_ordinal(name="HDI category") +
  # Crop
  coord_sf(ylim=c(-52, 80)) +
  # Theme
  theme_bw() + my_theme
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, ncol=1)

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_population_and_hdi.png"),
       width=6.5, height=7, units="in", dpi=600)




