

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
datadir <- "data/gdd/processed"
plotdir <- "figures/gdd_approach"
tabledir <- "tables"
gisdir <- "data/world/processed"

# Read data
data <- readRDS(file=file.path(datadir, "GDD_2018_intakes_national.Rds"))

# Read world data
world_lg_orig <- readRDS(file=file.path(gisdir, "world_large.Rds"))
world_sm_orig <- readRDS(file=file.path(gisdir, "world_small.Rds"))
world_centers_orig <- readRDS(file=file.path(gisdir, "world_centroids.Rds"))


# Build data
################################################################################

# Number of countries
n_distinct(data$iso3)

# Number age groups
(n_distinct(data$age_range) - 1) * 2

# Inspect dietary factors
diet_factors <- data %>%
  select(factor_type, factor, factor_units) %>% unique()
table(diet_factors$factor_type)


# Animal proteins
animal_proteins <- c("Eggs", "Total processed meats", "Unprocessed red meats", "Total seafoods")

# Build data
sdata <- data %>%
  # Reduce to animal protein intakes averaged across all categories
  filter(factor %in% animal_proteins & residence=="All residences" & education=="All education levels" & sex=="Both sexes" & age_range=="All ages") %>%
  # Summarize
  group_by(region, iso3, country) %>%
  summarize(supply_med=sum(supply_med, na.rm=T),
            supply_med_cap=pmin(supply_med, 250))

# Plot supply distribution
ggplot(sdata, aes(x=supply_med)) +
  geom_density() +
  # Labels
  labs(x="Animal foods intake (g)") +
  # Theme
  theme_bw()


# Export data
saveRDS(sdata, file=file.path(datadir, "GDD_animal_foods_avg.Rds"))


# Visualize data
################################################################################

# Data
world_sm <- world_sm_orig %>%
  sf::st_as_sf() %>%
  left_join(sdata %>% select(-country), by="iso3")
world_centers <- world_centers_orig %>%
  left_join(sdata %>% select(iso3, supply_med, supply_med_cap)) %>%
  filter(area_sqkm<=25000 & !is.na(supply_med))

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


# Plot age
g1 <- ggplot(sdata, aes(x=supply_med)) +
  geom_density() +
  # Reference line
  geom_vline(xintercept = c(250), linetype="dotted") +
  # Labels
  labs(x="ASF supply (g/day)", y="Density", tag="A") +
  # Axes
  scale_x_continuous(breaks=seq(0,600,100)) +
  # Theme
  theme_bw() + my_theme
g1

# Plot population
g2 <- ggplot(world_sm, aes(fill=supply_med_cap)) +
  geom_sf(lwd=0.2, color="grey30") +
  geom_point(data=world_centers, mapping=aes(x=long_dd, y=lat_dd, fill=supply_med_cap),
             color="grey30", size=1.5, pch=21) +
  # Labels
  labs(x="", y="", tag="B") +
  # Legend
  scale_fill_gradientn(name="ASF intake (g/day)", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), na.value="grey80",#) +
                       breaks=seq(0, 250, 50), labels=c(seq(0, 200, 50), "≥250")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", title.position="top")) +
  # Crop
  coord_sf(ylim=c(-52, 80), expand = T) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.key.size = unit(0.5, "cm"),
        legend.direction="horizontal",
        legend.position = c(0.13, 0.16),
        axis.text=element_blank(),
        axis.ticks = element_blank())
g2

# Arrange
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.25, 0.75))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS9_gdd_animal_foods.png"),
       width=6.5, height=2.25, units="in", dpi=600)


