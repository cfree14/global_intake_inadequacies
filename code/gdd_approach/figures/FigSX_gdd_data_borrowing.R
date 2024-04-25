

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
outdir <- "output"
plotdir <- "figures/gdd_approach"
tabledir <- "tables/gdd_approach"
gisdir <- "data/world/processed"

# Read data
data <- read.csv(file=file.path(outdir, "country_key_gdd.csv"), as.is=T) %>%
  mutate(gdd_yn1=recode_factor(gdd_yn,
                               "Borrowed"="No",
                               "Reported"="Yes"))

# Read world data
world_lg_orig <- readRDS(file=file.path(gisdir, "world_large.Rds"))
world_sm_orig <- readRDS(file=file.path(gisdir, "world_small.Rds")) %>% sf::st_as_sf()
world_centers_orig <- readRDS(file=file.path(gisdir, "world_centroids.Rds"))


# Build data
################################################################################

# Read GDD country match key
gdd_match_key <- readxl::read_excel(file.path(tabledir, "TableSX_countries_without_gdd_data.xlsx"), skip=1) %>%
  # Rename
  setNames(c("iso1", "country1", "iso2", "country2")) %>%
  # Add centroids 1
  left_join(world_centers_orig %>% select(-area_sqkm), by=c("iso1"="iso3")) %>%
  rename(lat1=lat_dd, long1=long_dd) %>%
  # Add centroids 1
  left_join(world_centers_orig %>% select(-area_sqkm), by=c("iso2"="iso3")) %>%
  rename(lat2=lat_dd, long2=long_dd)

# Add population info
world_sm <- world_sm_orig %>%
  left_join(data %>% select(iso3, gdd_yn, gdd_yn1))
world_centers <- world_centers_orig %>%
  left_join(data %>% select(iso3, gdd_yn, gdd_yn1)) %>%
  filter(area_sqkm<=25000 & !is.na(gdd_yn))


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_blank(),
                   axis.title=element_blank(),
                   axis.ticks = element_blank(),
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


# Plot gdd coverage
g <- ggplot(world_sm, aes(fill=gdd_yn1)) +
  geom_sf(lwd=0.1, color="grey30") +
  # geom_sf(data=world_tiny, color="grey30", size=1.5, pch=21) +
  # Plot links
  # geom_segment(data=gdd_match_key, mapping=aes(x=long1, xend=long2, y=lat1, yend=lat2), inherit.aes = F, linewidth=0.4) +
  # Plot nodes
  # geom_point(data=gdd_match_key, mapping=aes(x=long1, y=lat1), color="darkred", inherit.aes = F, size=0.25) +
  # geom_point(data=gdd_match_key, mapping=aes(x=long2, y=lat2), color="black", inherit.aes = F, size=0.25) +
  # Legend
  scale_fill_manual(name="GDD data", values=c("red", "grey90"), na.value = "grey30") +
  # Crop
  coord_sf(ylim=c(-52, 80)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_gdd_data_borrowing.png"),
       width=6.5, height=2.5, units="in", dpi=600)




