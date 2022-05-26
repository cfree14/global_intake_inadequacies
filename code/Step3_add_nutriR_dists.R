

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
outdir <- "output"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file=file.path(outdir, "1961_2011_subnational_nutrient_intake_estimates.Rds"))

# Get distributions
dists_orig <- nutriR::dists_full

# To-do list
# Add continent to data - nice for factoirng plot
# 1) Function to shift distribution to match medians (I think current is for means)
# 2) Figure out procedure for adding reference dist id
# 3) Have figure show where the shape parameters were imputed from

# Merge
################################################################################

# Nutrient coverage
nutrients_genus <- sort(unique(data_orig$nutrient))
nutrients_dist <- sort(unique(dists_orig$nutrient))
nutrients_genus[!nutrients_genus %in% nutrients_dist]

# Format dists
dists <- dists_orig %>%
  # Rename
  rename(age_range=age_group) %>%
  # Add distribution code
  mutate(dist_id=paste(nutrient, iso3, sex, age_range, sep="-")) %>%
  # Simplify
  select(dist_id, best_dist, g_shape, g_rate, g_mu, ln_meanlog, ln_sdlog, ln_mu)

# Format data
data <- data_orig %>%
  # Rename nutrients to match nutriR
  mutate(nutrient=recode(nutrient, "Thiamin"="Thiamine", "Vitamin A"="Vitamin A (RAE)"),
         nutrient_ar=recode(nutrient_ar, "Thiamin"="Thiamine", "Vitamin A"="Vitamin A (RAE)")) %>%
  # Reduce to nutrients in nutriR
  filter(nutrient %in% nutrients_dist) %>%
  # Add distribution code
  mutate(dist_id=paste(nutrient, iso3, sex, age_range, sep="-")) %>%
  # Mark whether shape is known or must be imputed
  mutate(shape_status=ifelse(dist_id %in% dists$dist_id, "known", "imputed"))
  # Add distribution info


# Impute order
# Known
# Nearest age within sex
# From other sex within country
# From nearest country


# Inspect impute rate
################################################################################

# Plot imputation rate
plot_impute_rate <- function(data, nutrient="Calcium"){

  # Subset data
  nutrient_do <- nutrient
  sdata <- data %>%
    filter(nutrient==nutrient_do)

  # Theme
  theme1 <- theme(axis.text=element_text(size=6),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  axis.title=element_text(size=7),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=7),
                  strip.text=element_text(size=8),
                  plot.title=element_text(size=9),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.background = element_rect(fill=alpha('blue', 0)))

  # Plot data
  g <- ggplot(sdata, aes(x=age_range, y=iso3, fill=shape_status)) +
    facet_wrap(~sex) +
    geom_raster() +
    # Labels
    labs(x="Age range (yr)", y="Country ISO3 code", title=nutrient_do) +
    # Legend
    scale_fill_manual(name="Shape info", values=c("grey80", "grey30")) +
    # Theme
    theme_bw() + theme1
  g

  # Return
  return(g)

}




