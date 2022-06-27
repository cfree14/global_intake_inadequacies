

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

# Read data
data_orig <- readRDS(file.path(outdir, "2018_subnational_nutrient_intake_inadequacy_estimates_simple.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>%
  # Simplify
  select(nutrient, units, sex, age_range, ar, ar_source) %>%
  unique() %>%
  # Remove iron and zinc
  filter(!grepl("Zinc|Iron", nutrient) & nutrient!="Vitamin D") %>%
  # Add label
  mutate(label=paste0(nutrient, " (", units, ")")) %>%
  mutate(label=recode(label,
                      "Vitamin A (RAE) (µg RAE)"="Vitamin A (µg RAE)"))

# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   axis.title=element_text(size=8),
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
g <- ggplot(data, aes(x=age_range, y=ar, color=sex, group=sex, linetype=ar_source)) +
  facet_wrap(~label, scales="free_y", ncol=4) +
  geom_line() +
  # Limits
  lims(y=c(0,NA)) +
  # Labels
  labs(y="Average requirement", x="Age range (yr)") +
  # Legend
  scale_color_discrete(name="Sex") +
  scale_linetype(name="Source") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.box = "horizontal",
        legend.position=c(0.8, 0.05))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS6_average_requirements.png"),
       width=6.5, height=6.5, units="in", dpi=600)




