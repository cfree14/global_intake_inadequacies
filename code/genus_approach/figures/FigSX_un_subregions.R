

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
outdir <- "output"
gisdir <- "data/world/processed"
plotdir <- "figures"

# Read world data
world_sm_orig <- readRDS(file=file.path(gisdir, "world_small.Rds"))
world_centers_orig <- readRDS(file=file.path(gisdir, "world_centroids.Rds"))



# Build data
################################################################################

# Build data
world_sm <- world_sm_orig %>%
  # Add region
  mutate(region=countrycode::countrycode(iso3, "iso3c", "un.regionsub.name"))
world_centers <- world_centers_orig %>%
  # Add region
  mutate(region=countrycode::countrycode(iso3, "iso3c", "un.regionsub.name")) %>%
  # Filter
  filter(area_sqkm<=25000)


# Plot data
################################################################################

# Setup theme
theme1 <- theme(axis.text=element_blank(),
                axis.title=element_blank(),
                legend.text=element_text(size=5),
                legend.title=element_text(size=6),
                strip.text=element_text(size=6, hjust=0.5, face="bold"),
                # Gridlines
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                # Legend
                legend.position = "bottom",
                legend.key.size = unit(0.5, "cm"),
                legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  # Plot data
  geom_sf(data=world_sm, mapping=aes(fill=region), lwd=0.1) +
  geom_point(data=world_centers, mapping=aes(x=long_dd, y=lat_dd, fill=region), pch=21, size=0.9, inherit.aes = F, stroke=0.2) +
  # Crop
  coord_sf(ylim=c(-52, 78)) +
  # Legend
  scale_fill_discrete(name="") +
  # Theme
  theme_bw() + theme1
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_un_subregions.png"),
       width=6.5, height=3.7, units="in", dpi=600)



