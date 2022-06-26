

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
plotdir <- "figures"
bealdir <- "data/beal_etal_2017/processed"
datadir <- "output"

# Read data
beal_cvs <- readRDS(file=file.path(bealdir, "Beal_nutrient_cvs.Rds")) %>%
  mutate(dist_type=factor(dist_type, levels=c("normal", "lognormal")))
data_orig <- nutriR::dists_full


# Build data
################################################################################

# Format data
data <- data_orig %>%
  filter(nutrient %in% beal_cvs$nutrient)


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
                  legend.position=c(0.75, 0.1),
                  legend.box = "horizontal",
                  legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=cv, fill=best_dist)) +
  facet_wrap(~nutrient, scales="free") +
  geom_histogram() +
  # Plot Beal line
  geom_vline(data=beal_cvs, mapping=aes(xintercept=cv, linetype=dist_type)) +
  # Labels
  labs(x="CV of usual intake distribution", y="Number of age-sex groups") +
  scale_fill_discrete(name="Intake distribution type\nfrom Passarelli et al. (2022)") +
  scale_linetype_discrete(name="Intake distribution type\nfrom Beal et al. (2017)") +
  # Limits
  lims(x=c(0, NA)) +
  # Theme
  theme_bw() + my_theme
g

# Export data
ggsave(g, filename=file.path(plotdir, "FigSX_beal_cv_comparison.png"),
       width=6.5, height=6.5, units="in", dpi=600)


