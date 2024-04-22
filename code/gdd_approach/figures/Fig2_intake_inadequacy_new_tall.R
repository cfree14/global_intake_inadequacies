

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
  # Remove GDD borrowed
  filter(gdd_type=="Reported") %>%
  # Remove Vitamin D
  filter(nutrient!="Vitamin D") %>%
  # Calculate stats
  group_by(nutrient, continent, iso3, country) %>%
  summarize(npeople=sum(npeople, na.rm=T),
            ndeficient=sum(ndeficient, na.rm=T)) %>%
  mutate(pdeficient=ndeficient/npeople) %>%
  ungroup()

# Summarize studided pop
npeople_tot <- data %>%
  filter(nutrient=="Calcium") %>%
  pull(npeople) %>% sum()
npeople_tot / 1e9

# Build labels
stat_labels <- data %>%
  group_by(nutrient) %>%
  summarize(ndeficient=sum(ndeficient, na.rm=T)) %>%
  mutate(pdeficient=ndeficient/npeople_tot,
         subtitle=paste0(round(ndeficient/1e9, 2), " bllion people, ", round(pdeficient*100,1), "% of world"),
         nutrient_label1=paste0(nutrient, "\n", subtitle),
         nutrient_label2=paste0(nutrient, " | ", round(ndeficient/1e9, 1), " billion (", round(pdeficient*100,0), "%)")) %>%
  ungroup() %>%
  arrange(desc(pdeficient))

# Add labels
data1 <- data %>%
  left_join(stat_labels %>% select(nutrient, nutrient_label1, nutrient_label2), by="nutrient")

# Nutrients
nutrients <- stat_labels$nutrient

# Add to spatial data
x <- nutrients[1]
data_sf <- purrr::map_df(nutrients, function(x){
  out <- world_sm %>%
    select(-country) %>%
    # Add nutrient info
    left_join(data1 %>% filter(nutrient==x), by="iso3") %>%
    # Fill in missing values
    mutate(nutrient=na.omit(unique(nutrient)),
           nutrient_label1=na.omit(unique(nutrient_label1)),
           nutrient_label2=na.omit(unique(nutrient_label2)))
}) %>% mutate(nutrient_label1=factor(nutrient_label1, levels=stat_labels$nutrient_label1),
              nutrient_label2=factor(nutrient_label2, levels=stat_labels$nutrient_label2))


# Create points for small countries
data_pts <- purrr::map_df(nutrients, function(x){
  out <- world_centers %>%
    select(-country) %>%
    left_join(data1 %>% filter(nutrient==x), by="iso3") %>%
    # Fill in missing values
    mutate(nutrient=na.omit(unique(nutrient)),
           nutrient_label1=na.omit(unique(nutrient_label1)),
           nutrient_label2=na.omit(unique(nutrient_label2))) %>%
  # Filter to tiny places with data
    filter(area_sqkm<=25000 & !is.na(pdeficient))
}) %>% mutate(nutrient_label1=factor(nutrient_label1, levels=stat_labels$nutrient_label1),
              nutrient_label2=factor(nutrient_label2, levels=stat_labels$nutrient_label2))

levels(data_pts$nutrient_label2)
levels(data_sf$nutrient_label2)


# Plot data
################################################################################

# Setup theme
theme1 <- theme(axis.text=element_blank(),
                axis.title=element_blank(),
                legend.text=element_text(size=5),
                legend.title=element_text(size=6),
                strip.text=element_text(size=6, hjust=0), # face="bold"
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
                legend.position = "bottom",
                legend.direction = "horizontal",
                legend.key.size = unit(0.4, "cm"),
                legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  # Facet
  facet_wrap(~nutrient_label2, ncol=3) +
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
  theme_bw() + theme1 +
  theme(panel.spacing = unit(-0.2, "lines"),
        legend.margin = margin(-8,0,-4,0))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig2_intake_inadequacy_gdd_new_tall.png"),
       width=6.5, height=5.5, units="in", dpi=600) # 6 in when double line legend
ggsave(g, filename=file.path(plotdir, "Fig2_intake_inadequacy_gdd_new_tall.pdf"),
       width=6.5, height=5.75, units="in", dpi=600)


