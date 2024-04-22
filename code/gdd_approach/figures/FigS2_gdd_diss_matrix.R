

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
load(file=file.path(outdir, "GDD_country_dissimilarity_for_figure.Rdata"))


# Plot data
################################################################################

# Plot dissimilarity
g <- ggplot(diff_df, aes(x=iso1, y=iso2, fill=diss)) +
  geom_raster(alpha=0.7) +
  # Lines
  geom_hline(yintercept = cntry_key_seps$iso3) +
  geom_vline(xintercept = cntry_key_seps$iso3) +
  geom_text(data=cntry_key_seps, mapping=aes(x=iso3, y=iso3, label=continent), inherit.aes = F,
            hjust=-0.1, vjust=-0.5) +
  # Labels
  labs(x="", y="") +
  scale_fill_gradientn(name="Euclidean\ndistance",
                       colors=RColorBrewer::brewer.pal(n=9, name="Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text=element_text(size=3),
        axis.title=element_text(size=6),
        legend.text=element_text(size=5),
        legend.title=element_text(size=6),
        # Gridlines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        # Legend
        legend.position="bottom")
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigS2_gdd_diss_matrix.png"),
       width=6.5, height=6.5, units="in", dpi=600)
