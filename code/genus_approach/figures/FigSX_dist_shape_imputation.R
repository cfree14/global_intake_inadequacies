

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
data_orig <- readRDS(file.path(outdir, "2011_subnational_nutrient_intake_inadequacy_estimates_full.Rds"))




# Helper function
################################################################################

# Helper function
data_do <- data_orig; nutrient <- "Copper"
plot_impute_rate <- function(data_do, nutrient){

  # Subset data
  nutrient_do <- nutrient
  sdata <- data_do %>%
    filter(nutrient==nutrient_do) %>%
    select(continent, iso3, age_range, sex, shape_status, shape_source) %>%
    unique()

  # Theme
  theme1 <- theme(axis.text=element_text(size=5),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  axis.text.y = element_text(size=4),
                  axis.title=element_text(size=6),
                  plot.title=element_text(size=6),
                  legend.text=element_text(size=5),
                  legend.title=element_text(size=6),
                  strip.text=element_text(size=6),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.background = element_rect(fill=alpha('blue', 0)))

  # Plot data
  g <- ggplot(sdata, aes(x=age_range, y=iso3, fill=shape_source)) +
    facet_grid(continent~sex, scales="free_y", space="free_y") +
    geom_raster() +
    # Labels
    labs(x="Age range (yr)", y="Country ISO3 code", title=nutrient_do) +
    # Legend
    # scale_fill_manual(name="Shape info", values=c("grey80", "grey30")) + # for known/imputed
    scale_fill_manual(name="Shape info", values=c("grey30", "lightblue", "lightgreen", "grey95"), drop=F) +
    # Theme
    theme_bw() + theme1
  g

  # Export figure
  figname <- paste0("FigSX_dist_impute_rate_", tolower(nutrient_do) %>% gsub(" ", "_", .), ".png")
  ggsave(g, filename=file.path(plotdir, figname),
         width=6.5, height=8, units="in", dpi=600)

  # Return
  return(g)

}

# Make plots
################################################################################

# Loop through nutrients
nutrients <- sort(unique(data_orig$nutrient))
for(i in nutrients){
  plot_impute_rate(data_orig, i)
}



# Synthetic plot
################################################################################


# Summmarize
stats <- data_orig %>%
  group_by(nutrient) %>%
  summarize(ncountries=n_distinct(iso3[shape_source=="Known"]),
            n_known=sum(shape_source=="Known"),
            p_known=n_known / n()) %>%
  ungroup() %>%
  arrange(desc(n_countries))  %>%
  mutate(nutrient=factor(nutrient, levels=nutrient))


# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   axis.title.y=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   plot.tag=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g1 <- ggplot(stats, aes(x=ncountries, y=nutrient)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of countries contributing\ndistribution shape information", y="", tag="A") +
  # Theme
  theme_bw() + my_theme
g1

# Plot
g2 <- ggplot(stats, aes(x=p_known, y=nutrient)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of subnational groups\nwith known distribution shapes", y="", tag="B") +
  scale_x_continuous(labels=scales::percent) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_blank())
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.55, 0.45))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_dist_shape_imputation_rate.png"),
       width=6.5, height=2.5, units="in", dpi=600)





