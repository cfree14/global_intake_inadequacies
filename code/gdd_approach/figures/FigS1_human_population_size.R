

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
tabledir <- "tables"
gisdir <- "data/world/processed"

# Read data
data <- read.csv(file=file.path(outdir, "country_key_gdd.csv"), as.is=T)

# Read world data
world_lg_orig <- readRDS(file=file.path(gisdir, "world_large.Rds"))
world_sm_orig <- readRDS(file=file.path(gisdir, "world_small.Rds"))
world_centers_orig <- readRDS(file=file.path(gisdir, "world_centroids.Rds"))


# Build data
################################################################################

# Add population info
world_sm <- world_sm_orig %>%
  left_join(data %>% select(iso3, npeople))
world_centers <- world_centers_orig %>%
  left_join(data %>% select(iso3, npeople)) %>%
  filter(area_sqkm<=25000 & !is.na(npeople))


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


# Plot population
g <- ggplot(world_sm, aes(fill=npeople)) +
  geom_sf(lwd=0.2, color="grey30") +
  geom_point(data=world_centers, mapping=aes(x=long_dd, y=lat_dd, fill=npeople),
             color="grey30", size=1.5, pch=21) +
  # Legend
  scale_fill_gradientn(name="Population size", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                       trans="log10", na.value="grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(ylim=c(-52, 80)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS1_human_population_size.png"),
       width=6.5, height=2.5, units="in", dpi=600)




