

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
plotdir <- "figures"
bealdir <- "data/beal_etal_2017/raw"
datadir <- "output"

# Read data
beal_orig <- readRDS(file=file.path(bealdir, "Beal_1961_2011_inadequate_nutrient_intake_by_country.Rds"))
data_orig <- readRDS(file=file.path(datadir, "2011_subnational_nutrient_intake_inadequacy_estimates_simple.Rds"))


# Build data
################################################################################

# Format Beal
beal <- beal_orig %>%
  # Filter
  filter(year==2011 & fortification==0) %>%
  # Simplify
  select(iso3, nutrient, pinadequate) %>%
  # Rename
  rename(pdeficient_beal=pinadequate) %>%
  # Scale
  mutate(pdeficient_beal=pdeficient_beal/100) %>%
  # Format
  mutate(nutrient=recode(nutrient,
                         "Thiamin"="Thiamine",
                         "Vitamin A"="Vitamin A (RAE)"))

# Build data
data <- data_orig %>%
  # Calculate number deificient
  group_by(continent, iso3, country, nutrient) %>%
  summarize(npeople=sum(npeople, na.rm=T),
            ndeficient=sum(ndeficient, na.rm=T)) %>%
  ungroup() %>%
  # Calculate proportion deificient
  mutate(pdeficient=ndeficient/npeople) %>%
  # Add Beal proportion deficient
  left_join(beal, by=c('iso3', "nutrient"))


# Plot data
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=8),
                  legend.text=element_text(size=7),
                  legend.title=element_text(size=8),
                  strip.text=element_text(size=8),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.position="bottom",
                  legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=pdeficient, y=pdeficient_beal, color=continent)) +
  facet_wrap(~ nutrient) +
  geom_point() +
  # Labels
  labs(x="% inadequate intake\n(present paper)",
       y="% inadequate intake\n(Beal et al. 2017)") +
  scale_color_discrete(name="") +
  # Lines
  geom_abline(slope=1) +
  # Axes
  scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  # Theme
  theme_bw() + my_theme
g

# Export data
ggsave(g, filename=file.path(plotdir, "FigSX_inadequacy_comparisons.png"),
       width=6.5, height=6.5, units="in", dpi=600)


