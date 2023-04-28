

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(GENuS)
library(tidyverse)

# Directories
datadir <- "data/wessells_brown"
gisdir <- "data/world/processed"
plotdir <- "figures/gdd_approach"

# Export
data <- readRDS(file=file.path(datadir, "wessells_brown_2012_zinc_absorption.Rds"))

# Read world data
world_lg_orig <- readRDS(file=file.path(gisdir, "world_large.Rds"))
world_sm_orig <- readRDS(file=file.path(gisdir, "world_small.Rds"))
world_centers_orig <- readRDS(file=file.path(gisdir, "world_centroids.Rds"))

# Add population info
world_sm <- world_sm_orig %>%
  left_join(data %>% select(iso3, phytate_mg))
world_centers <- world_centers_orig %>%
  left_join(data %>% select(iso3, phytate_mg)) %>%
  filter(area_sqkm<=25000 & !is.na(phytate_mg))


# Plot data
################################################################################

# Theme
base_theme <-  theme(axis.text=element_text(size=6),
                     axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=5),
                   legend.title=element_text(size=7),
                   plot.tag=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot phytate intake
g1 <- ggplot(data, aes(x=phytate_mg)) +
  # Reference lines
  geom_vline(xintercept = c(300, 600, 900, 1200), linetype="dotted", color="grey60", lwd=0.5) +
  # Density
  geom_density() +
  # Labels
  labs(x="Phytate intake (mg/d)", y="Density", tag="A") +
  scale_x_continuous(lim=c(0, NA)) +
  # Theme
  theme_bw() + base_theme
g1

# Plot population
g2 <- ggplot(world_sm, aes(fill=phytate_mg)) +
  geom_sf(lwd=0.1, color="grey30") +
  geom_point(data=world_centers, mapping=aes(x=long_dd, y=lat_dd, fill=phytate_mg),
             color="grey30", size=1.5, pch=21) +
  # Labels
  labs(x="", y="", tag="B") +
  # Legend
  scale_fill_gradientn(name="Phytate\nintake (mg/d)",
                       colors=RColorBrewer::brewer.pal(9, "Blues"),  na.value="grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(ylim=c(-52, 80)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.15, 0.35),
        legend.key.size=unit(0.3, "cm"),
        axis.text=element_blank(),
        axis.ticks = element_blank())
g2

# Arrange
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.3, 0.7))
g


# Export
ggsave(g, filename=file.path(plotdir, "FigS9_phytate_map.png"),
       width=6.5, height=2, units="in", dpi=600)


