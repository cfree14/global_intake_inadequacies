

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
plotdir <- "figures/gdd_approach"
tabledir <- "tables/gdd_approach"

# Read data
data_orig <- readRDS(file.path(outdir, "2018_subnational_nutrient_intake_inadequacy_estimates_simple.Rds"))


# Build data
################################################################################

# Nutrient order
nutrient_key <- read.csv(file=file.path(tabledir, "TableS1_global_inadequacies.csv"), as.is=T) %>%
  arrange(desc(pdeficient))

# Build data
data <- data_orig %>%
  # Remove Vitamin D
  filter(nutrient!="Vitamin D") %>%
  # Add region
  mutate(region=countrycode::countrycode(iso3, "iso3c", "un.regionsub.name")) %>%
  mutate(region=case_when(iso3=="CHI" ~ "Western Europe",
                          iso3=="XKX" ~ "Southern Europe",
                          iso3=="TWN" ~ "Eastern Asia",
                          T ~ region)) %>%
  mutate(region=recode(region,
                       "Latin America and the Caribbean"="Latin America / Caribbean",
                       "Australia and New Zealand"="Australia / New Zealand")) %>%
  # Summarize
  group_by(nutrient, region, sex, age_range) %>%
  summarize(npeople=sum(npeople, na.rm=T),
           ndeficient=sum(ndeficient, na.rm=T)) %>%
  mutate(pdeficient=ndeficient/npeople) %>%
  ungroup() %>%
  # Add short region
  mutate(region_short=recode(region,
                             "Australia / New Zealand"="NZ/Australia",
                             "Central Asia"="C Asia",
                             "Eastern Asia"="E Asia",
                             "Eastern Europe"="E Europe",
                             "Latin America / Caribbean"="Lat. America",
                             # "Melanesia"="",
                             # "Micronesia"="",
                             "Northern Africa"="N Africa",
                             "Northern America"="N America",
                             "Northern Europe"="N Europe",
                             # "Polynesia"="",
                             "South-eastern Asia"="SE Asia",
                             "Southern Asia"="S Asia",
                             "Southern Europe"="S Europe",
                             "Sub-Saharan Africa"="SS Africa",
                             "Western Asia"="W Asia",
                             "Western Europe"="W Europe")) %>%
  # Add region order
  group_by(nutrient, region) %>%
  mutate(pdeficient_region=sum(ndeficient, na.rm=T)/sum(npeople, na.rm=T)) %>%
  ungroup()

# Region order
region_order <- data %>%
  group_by(region_short) %>%
  summarize(pdeficient=median(pdeficient)) %>%
  arrange(pdeficient)

# Order data
data_ordered <- data %>%
  # Order nutrient
  mutate(nutrient=factor(nutrient, levels=nutrient_key$nutrient))

# Break into two
nutrients <- nutrient_key$nutrient
data1 <- data_ordered %>% filter(nutrient %in% nutrients[1:8])
data2 <- data_ordered %>% filter(nutrient %in% nutrients[9:15])


# Plot data
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=5),
                  axis.text.x=element_text(size=4.75, angle = 90, vjust = 0.5, hjust=1),
                  axis.title=element_text(size=6),
                  axis.title.y=element_blank(),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=7),
                  strip.text=element_text(size=6),
                  # Facet spacing
                  panel.spacing = unit(0.1, "lines"),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g1 <- ggplot(data1, aes(x=age_range,
                        y=tidytext::reorder_within(region_short, pdeficient_region, nutrient),
                        fill=pdeficient)) +
  facet_grid(nutrient~sex, scales = "free") +
  geom_tile() +
  # Labels
  labs(x="Age range (yr)", y="") +
  tidytext::scale_y_reordered() +
  # Legend
  scale_fill_gradientn(name="% inadequate",
                       lim=c(0,1),
                       labels=scales::percent,
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g1

# Plot
g2 <- ggplot(data2, aes(x=age_range,
                        y=tidytext::reorder_within(region_short, pdeficient_region, nutrient),
                        fill=pdeficient)) +
  facet_grid(nutrient~sex, scales="free") +
  geom_tile() +
  # Labels
  labs(x="Age range (yr)", y="") +
  tidytext::scale_y_reordered() +
  # Legend
  scale_fill_gradientn(name="% inadequate",
                       lim=c(0,1),
                       labels=scales::percent,
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",
                               title.position = "top", title.hjust = 0.5)) +
  # Theme
  theme_bw() + my_theme +
  theme(#axis.text.y=element_blank(),
        legend.position = "top")
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, ncol=2, widths=c(0.5, 0.5))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig2_intake_inadequacy_agesex.png"),
       width=6.5, height=7.75, units="in", dpi=600)

