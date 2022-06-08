

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

# Read data
data_orig <- readRDS(file.path(outdir, "2011_subnational_nutrient_intake_inadequacy_estimates_simple.Rds"))

# Read world data
world_lg <- readRDS(file=file.path(gisdir, "world_large.Rds"))
world_sm <- readRDS(file=file.path(gisdir, "world_small.Rds"))
world_centers <- readRDS(file=file.path(gisdir, "world_centroids.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>%
  group_by(nutrient, continent, iso3, country) %>%
  summarize(npeople=sum(npeople),
            ndeficient=sum(ndeficient)) %>%
  mutate(pdeficient=ndeficient/npeople) %>%
  ungroup()

# Add to spatial data
data_sf <- world_sm %>%
  select(-country) %>%
  left_join(data, by="iso3") %>%
  mutate(nutrient="Calcium") # Won't work with more nutrients

# Create points for small countries
data_pts <- world_centers %>%
  select(-country) %>%
  left_join(data, by="iso3") %>%
  mutate(nutrient="Calcium") %>% # Won't work with more nutrients
  filter(area_sqkm<=25000 & !is.na(pdeficient))

# Build labels
stat_labels <- data %>%
  group_by(nutrient) %>%
  summarize(npeople=sum(npeople, na.rm=T),
            ndeficient=sum(ndeficient, na.rm=T)) %>%
  mutate(pdeficient=ndeficient/npeople,
         label=paste0(round(ndeficient/1e9, 2), " billion people\n", round(pdeficient*100,1), "% of global population")) %>%
  ungroup()


# Plot data
################################################################################

# Setup theme
theme1 <- theme(axis.text=element_blank(),
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

# Plot data
g <- ggplot() +
  # Facet
  facet_wrap(~nutrient) +
  # Plot data
  geom_sf(data=data_sf, mapping=aes(fill=pdeficient), lwd=0.2) +
  geom_point(data=data_pts, mapping=aes(x=long_dd, y=lat_dd, fill=pdeficient), pch=21, size=1.5, inherit.aes = F) +
  # Add label
  geom_text(data=stat_labels, mapping=aes(x=-160, y=-40, label=label), size=2.4, hjust=0, inherit.aes=F) +
  # Legend
  scale_fill_gradientn(name="% inadequate",
                       labels=scales::percent, lim=c(0,1),
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                       na.value="grey90") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(ylim=c(-52, 80)) +
  # Theme
  theme_bw() + theme1
g


