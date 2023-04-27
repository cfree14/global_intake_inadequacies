

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

# Get data
data_orig <- readxl::read_excel(file.path(datadir, "Wessells_Brown_2012_Table_S2.xls"))


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  # Simplify
  select(country, year, estimated_fractional_absorption) %>%
  # Format year
  mutate(year=gsub('b', "", year) %>% as.numeric()) %>%
  # Add ISO3
  mutate(iso3=countrycode::countrycode(country, "country.name", "iso3c")) %>%
  # Get most recent
  group_by(country) %>%
  arrange(country, desc(year)) %>%
  slice(1) %>%
  ungroup()

# Export
saveRDS(data, file=file.path(datadir, "wessells_brown_2012_zinc_absorption.Rds"))




# Visualize data
################################################################################

# Read world data
world_lg_orig <- readRDS(file=file.path(gisdir, "world_large.Rds"))
world_sm_orig <- readRDS(file=file.path(gisdir, "world_small.Rds"))
world_centers_orig <- readRDS(file=file.path(gisdir, "world_centroids.Rds"))

# Add population info
world_sm <- world_sm_orig %>%
  left_join(data %>% select(iso3, estimated_fractional_absorption))
world_centers <- world_centers_orig %>%
  left_join(data %>% select(iso3, estimated_fractional_absorption)) %>%
  filter(area_sqkm<=25000 & !is.na(estimated_fractional_absorption))


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
g <- ggplot(world_sm, aes(fill=estimated_fractional_absorption)) +
  geom_sf(lwd=0.2, color="grey30") +
  geom_point(data=world_centers, mapping=aes(x=long_dd, y=lat_dd, fill=estimated_fractional_absorption),
             color="grey30", size=1.5, pch=21) +
  # Legend
  scale_fill_gradientn(name="Estimated fractional absorption", colors=RColorBrewer::brewer.pal(9, "Blues"),  na.value="grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(ylim=c(-52, 80)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(datadir, "wessells_brown_estimated_fractional_absorption.png"),
       width=6.5, height=2, units="in", dpi=600)










