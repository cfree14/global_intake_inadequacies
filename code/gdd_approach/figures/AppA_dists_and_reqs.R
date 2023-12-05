

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(ggplus)

# Directories
outdir <- "output"
plotdir <- "figures/gdd_approach"

# Read data
data_orig <- readRDS(file.path(outdir, "2018_subnational_nutrient_intake_inadequacy_estimates_full.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>%
  # Add label
  mutate(cntry_sex_label=paste0(country, "\n(", tolower(sex), ")")) %>%
  # Simplify
  select(continent, iso3, country, cntry_sex_label, nutrient, units, sex, age_range,
         supply_med, ar, ar_source, ar_cv,
         npeople, sev, ndeficient,
         best_dist, g_shape_shift, g_rate_shift, ln_meanlog_shift, ln_sdlog_shift) %>%
  # Rename
  rename(supply50=supply_med, shape=g_shape_shift, rate=g_rate_shift, meanlog=ln_meanlog_shift, sdlog=ln_sdlog_shift) %>%
  # Calculate 95% for intake
  rowwise() %>%
  mutate(supply025=ifelse(best_dist=="gamma", qgamma(p=0.025, shape=shape, rate=rate), qlnorm(p=0.025, meanlog=meanlog, sdlog=sdlog)),
         supply25=ifelse(best_dist=="gamma", qgamma(p=0.25, shape=shape, rate=rate), qlnorm(p=0.25, meanlog=meanlog, sdlog=sdlog)),
         supply75=ifelse(best_dist=="gamma", qgamma(p=0.75, shape=shape, rate=rate), qlnorm(p=0.75, meanlog=meanlog, sdlog=sdlog)),
         supply975=ifelse(best_dist=="gamma", qgamma(p=0.975, shape=shape, rate=rate), qlnorm(p=0.975, meanlog=meanlog, sdlog=sdlog))) %>%
  ungroup() %>%
  # Remove Vitamin D
  filter(nutrient!="Vitamin D")


# Plot data function
################################################################################

nutrient <- "Iron"
plot_data <- function(nutrient){

  # Nutrient
  nutrient_do <- nutrient

  # Countries
  countries <- sort(unique(data$country))

  # Filter data
  sdata <- data %>%
    # Reduce
    filter(nutrient==nutrient_do)

  # Units
  units <- sdata$units %>% unique()
  yaxis_label <- paste0("Usual intake (", units, ")")

  # Setup theme
  my_theme <-  theme(axis.text=element_text(size=6),
                     axis.text.y = element_text(angle = 90, hjust = 0.5),
                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                     axis.title=element_text(size=7),
                     legend.text=element_text(size=6),
                     legend.title=element_text(size=7),
                     strip.text=element_text(size=7),
                     plot.title=element_text(size=8),
                     # Gridlines
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.position="top",
                     legend.key.size = unit(0.5, "cm"),
                     legend.background = element_rect(fill=alpha('blue', 0)))

  # Plot data
  g <- ggplot(sdata) +
    # facet_wrap(~cntry_sex_label, ncol=4, scale="free_y") +
    # Plot CI
    geom_segment(mapping=aes(x=age_range, xend=age_range, y=supply025, yend=supply975, color=sev/100), linewidth=0.75) +
    geom_segment(mapping=aes(x=age_range, xend=age_range, y=supply25, yend=supply75, color=sev/100), linewidth=1.75) +
    # Plot point
    geom_point(mapping=aes(x=age_range, y=supply50, fill=sev/100), size=3, pch=21) +
    # Reference
    geom_line(mapping=aes(x=age_range, y=ar, group=sex), inherit.aes = F, linewidth=1.3) +
    # Labels
    labs(x="Age range (yr)", y=yaxis_label, title=nutrient_do) +
    # Limits
    lims(y=c(0, NA)) +
    # Legend
    scale_color_gradientn(name="% inadequate", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                          lim=c(0,1), labels=scales::percent, guide="none") +
    scale_fill_gradientn(name="% inadequate", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                         lim=c(0,1), labels=scales::percent) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() + my_theme

  # Export
  filename <- paste0("AppendixA_intakes_reqs_", tolower(nutrient_do), ".pdf")
  pdf(file.path(plotdir, filename), width=8.5, height=11)
  gg10 <- facet_multiple(plot=g, facets=c("cntry_sex_label"), ncol = 4, nrow = 5, scales="free_y")
  dev.off()


  # Plot data
  # ggsave(g, filename=file.path(plotdir, "AppendixX_iron_intakes.png"),
  #        width=8.5, height=11.5, units="in", dpi=600)


}

# Plot data
################################################################################

# Loop
nutrients <- sort(unique(data$nutrient))
for(i in 1:length(nutrients)){
  plot_data(nutrients[i])
}

# Examples
plot_data("Iron")
plot_data("Calcium")
