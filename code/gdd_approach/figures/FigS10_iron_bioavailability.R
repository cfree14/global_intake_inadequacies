

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

# Read data
asf_orig <- readRDS(file="data/gdd/processed/GDD_animal_foods_avg.Rds") %>% ungroup()
phytate_orig <- readRDS(file="data/wessells_brown/wessells_brown_2012_zinc_absorption.Rds") %>% ungroup()

# Read world data
world_lg_orig <- readRDS(file=file.path(gisdir, "world_large.Rds"))
world_sm_orig <- readRDS(file=file.path(gisdir, "world_small.Rds"))
world_centers_orig <- readRDS(file=file.path(gisdir, "world_centroids.Rds"))



# Build data
################################################################################

# Build data
data <- phytate_orig %>%
  # Simplify
  select(country, iso3, phytate_mg) %>%
  # Add animal intakes
  left_join(asf_orig %>% select(iso3, supply_med), by="iso3") %>%
  # Rename
  rename(asf_g=supply_med) %>%
  # Reduce
  na.omit() %>%
  # Add South sudan
  bind_rows(.,
            tibble(country="South Sudan",
                   iso3="SSD",
                   phytate_mg=1548, # from Central African Republic (not Sudan b/c forested)
                   asf_g=78.40906)) %>%
  # Scale
  mutate(phytate_q=scales::rescale(phytate_mg, to=c(1,0)),
         asf_q=scales::rescale(asf_g, to=c(0,1)),
         mean_q=(phytate_q+asf_q)/2,
         bioavailability=scales::rescale(mean_q, to=c(5,16)),
         bioavailability_cap=pmin(bioavailability, 12))

# Export data
saveRDS(data, file=file.path(outdir, "iron_bioavailability.Rds"))



# Visualize data
################################################################################

# Data
world_sm <- world_sm_orig %>%
  sf::st_as_sf() %>%
  left_join(data %>% select(iso3, bioavailability, bioavailability_cap), by="iso3")
world_centers <- world_centers_orig %>%
  left_join(data %>% select(iso3, bioavailability, bioavailability_cap)) %>%
  filter(area_sqkm<=25000 & !is.na(bioavailability))

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   plot.tag = element_text(size=8),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot bioavailability
g1 <- ggplot(data, aes(x=bioavailability)) +
  geom_density() +
  # Reference line
  # geom_vline(xintercept = c(12), linetype="dotted") +
  # Labels
  labs(x="Iron absorption (%)", y="Density", tag="A") +
  # Axes
  scale_x_continuous(breaks=seq(0,16,2), lim=c(0,16)) +
  # Theme
  theme_bw() + my_theme
g1

# Plot population
g2 <- ggplot(world_sm, aes(fill=bioavailability)) +
  geom_sf(lwd=0.2, color="grey30") +
  geom_point(data=world_centers, mapping=aes(x=long_dd, y=lat_dd, fill=bioavailability),
             color="grey30", size=1.5, pch=21) +
  # Labels
  labs(x="", y="", tag="B") +
  # Legend
  scale_fill_gradientn(name="Iron absorption (%)",
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                       na.value="grey80") +
                       # breaks=seq(6,12, 2),
                       # labels=c(seq(6,10,2), "≥12")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", title.position="top")) +
  # Crop
  coord_sf(ylim=c(-52, 80), expand = T) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.key.size = unit(0.5, "cm"),
        legend.direction="horizontal",
        legend.position = c(0.15, 0.16),
        axis.text=element_blank(),
        axis.ticks = element_blank())
g2

# Arrange
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.25, 0.75))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS10_iron_bioavailability.png"),
       width=6.5, height=2.25, units="in", dpi=600)


