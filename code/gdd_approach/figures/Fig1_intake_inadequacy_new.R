

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(ggplot2)
library(tidyverse)
library(countrycode)

# Directories
outdir <- "output"
gisdir <- "data/world/processed"
plotdir <- "figures/gdd_approach"

# Read data
data_orig <- readRDS(file.path(outdir, "2018_subnational_nutrient_intake_inadequacy_estimates_simple.Rds"))

# Read world data
world_lg <- readRDS(file=file.path(gisdir, "world_large.Rds"))
world_sm <- readRDS(file=file.path(gisdir, "world_small.Rds")) %>% sf::st_as_sf()
world_centers <- readRDS(file=file.path(gisdir, "world_centroids.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>%
  # Remove Vitmin D
  filter(nutrient!="Vitamin D") %>%
  # Calculate stats
  group_by(nutrient, continent, iso3, country) %>%
  summarize(npeople=sum(npeople, na.rm=T),
            ndeficient=sum(ndeficient, na.rm=T)) %>%
  mutate(pdeficient=ndeficient/npeople) %>%
  ungroup()

# Build labels
stat_labels <- data %>%
  group_by(nutrient) %>%
  summarize(npeople=sum(npeople, na.rm=T),
            ndeficient=sum(ndeficient, na.rm=T)) %>%
  mutate(pdeficient=ndeficient/npeople,
         subtitle=paste0(round(ndeficient/1e9, 2), " billion people, ", round(pdeficient*100,1), "% of world"),
         nutrient_label=paste0(nutrient, "\n", subtitle)) %>%
  ungroup() %>%
  arrange(desc(pdeficient))

# Add labels
data1 <- data %>%
  left_join(stat_labels %>% select(nutrient, nutrient_label))

# Nutrients
nutrients <- stat_labels$nutrient_label

# Add to spatial data
x <- nutrients[1]
data_sf <- purrr::map_df(nutrients, function(x){
  out <- world_sm %>%
    select(-country) %>%
    left_join(data1 %>% filter(nutrient_label==x), by="iso3") %>%
    mutate(nutrient=x)
}) %>% mutate(nutrient=factor(nutrient, levels=nutrients))


# Create points for small countries
data_pts <- purrr::map_df(nutrients, function(x){
  out <- world_centers %>%
    select(-country) %>%
    left_join(data1 %>% filter(nutrient_label==x), by="iso3") %>%
    mutate(nutrient=x) %>%
    filter(area_sqkm<=25000 & !is.na(pdeficient))
}) %>% mutate(nutrient=factor(nutrient, levels=nutrients))

levels(data_pts$nutrient)
levels(data_sf$nutrient)

# Plot data
################################################################################

# Setup theme
theme1 <- theme(axis.text=element_blank(),
                axis.title=element_blank(),
                legend.text=element_text(size=5),
                legend.title=element_text(size=6),
                strip.text=element_text(size=6, hjust=0.5), # face="bold"
                # Borders/axes
                strip.background=element_blank(),
                axis.line.x = element_blank(),
                axis.line.y = element_blank(),
                axis.ticks = element_blank(),
                panel.border = element_blank(),
                # Gridlines
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                # Legend
                legend.position = c(0.88, 0.1),
                legend.direction = "horizontal",
                legend.key.size = unit(0.4, "cm"),
                legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  # Facet
  facet_wrap(~nutrient, ncol=4) +
  # Plot data
  geom_sf(data=data_sf, mapping=aes(fill=pdeficient), lwd=0.1) +
  geom_point(data=data_pts, mapping=aes(x=long_dd, y=lat_dd, fill=pdeficient), pch=21, size=0.9, inherit.aes = F, stroke=0.2) +
  # Legend
  scale_fill_gradientn(name="% inadequate",
                       labels=scales::percent, lim=c(0,1),
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                       na.value="grey90") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",
                               title.position = "top", title.hjust = 0.5)) +
  # Crop
  coord_sf(ylim=c(-52, 78)) +
  # Theme
  theme_bw() + theme1
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig1_intake_inadequacy_gdd_new.png"),
       width=6.5, height=3.75, units="in", dpi=600)



