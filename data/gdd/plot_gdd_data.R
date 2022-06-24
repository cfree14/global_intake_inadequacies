

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(GENuS)
library(tidyverse)

# Directories
indir <- "data/gdd/raw/GDD_FinalEstimates_01102022"
outdir <- "data/gdd/processed"
plotdir <- "data/gdd/figures"
gisdir <- "data/world/processed"

# Read data
data_orig <- readRDS(data, file=file.path(outdir, "GDD_2018_intakes_national_for_analysis.Rds"))

# Read world data
world_lg <- readRDS(file=file.path(gisdir, "world_large.Rds"))
world_sm <- readRDS(file=file.path(gisdir, "world_small.Rds"))
world_centers <- readRDS(file=file.path(gisdir, "world_centroids.Rds"))


# Plotting function
################################################################################

# Plotting functon
nutrient <- "Calcium"
plot_map <- function(nutrient="Calcium"){

  # Subset data
  nutrient_do <- nutrient
  sdata <- data_orig %>%
    # Reduce
    filter(nutrient==nutrient_do) %>%
    # Summarize
    group_by(iso3, units) %>%
    summarize(supply_avg=mean(supply_med)) %>%
    ungroup()

  # Extract units
  units <- unique(sdata$units)

  # Build labels
  plot_title <- paste0(nutrient_do, " (", units, ")")
  legend_title <- paste0("Supply (", units, ")")

  # Spatialize data
  sdata_sf <- world_sm %>%
    left_join(sdata)

  sdata_sf_sm <- world_centers %>%
    left_join(sdata) %>%
    filter(area_sqkm<=25000 & !is.na(supply_avg))

  # Setup theme
  my_theme <-  theme(axis.text=element_blank(),
                     axis.title=element_blank(),
                     axis.ticks = element_blank(),
                     legend.text=element_text(size=6),
                     legend.title=element_text(size=7),
                     plot.title=element_text(size=10),
                     # Gridlines
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.background = element_rect(fill=alpha('blue', 0)))

  # Plot data
  g <- ggplot(sdata_sf, aes(fill=supply_avg)) +
    geom_sf(col="grey30", lwd=0.1) +
    geom_point(data=sdata_sf_sm, mapping=aes(x=long_dd, y=lat_dd, fill=supply_avg), pch=21) +
    # Crop
    coord_sf(ylim=c(-60, 80), expand = F) +
    # Labels
    labs(title=plot_title) +
    # Legend
    scale_fill_gradientn(name=legend_title,
                         colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                         na.value="grey90") +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() + my_theme
  g
  # return(g)

  # Export
  figout <- paste0("FigSX_gdd_subnational_map_", tolower(nutrient_do) %>% gsub(" ", "_", .), ".png")
  ggsave(g, filename=file.path(plotdir, figout),
         width=6.5, height=2.75, units="in", dpi=600)

}

plot_data <- function(nutrient="Calcium"){

  # Subset data
  nutrient_do <- nutrient
  sdata <- data_orig %>%
    # Reduce
    filter(nutrient==nutrient_do)

  # Extract units
  units <- unique(sdata$units)

  # Build labels
  plot_title <- paste0(nutrient_do, " (", units, ")")
  legend_title <- paste0("Supply (", units, ")")

  # Setup theme
  my_theme <-  theme(axis.text=element_text(size=5),
                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                     axis.title=element_text(size=7),
                     legend.text=element_text(size=6),
                     legend.title=element_text(size=7),
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
  g <- ggplot(sdata, aes(x=age_range, y=iso3, fill=supply_med)) +
    facet_grid(continent~sex, scales="free_y", space="free_y") +
    geom_tile() +
    # Labels
    labs(x="Age range", y="Country", title=plot_title) +
    # Legend
    scale_fill_gradientn(name=legend_title, colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() + my_theme
  # print(g)
  # return(g)

  # Export
  figout <- paste0("FigSX_gdd_subnational_", tolower(nutrient_do) %>% gsub(" ", "_", .), ".pdf")
  ggsave(g, filename=file.path(plotdir, figout),
         width=6.5, height=12.5, units="in", dpi=600)

}

# Build plots
################################################################################

nutrients <- sort(unique(data_orig$nutrient))
for(i in nutrients){
  plot_data(nutrient=i)
}

for(i in nutrients){
  plot_map(nutrient=i)
}



