
# Plot coverage
# data <- data; nutrient <- "Calcium"
plot_inadequacies_map <- function(data, nutrient, base_theme){

  # Build data
  nutrient_do <- nutrient
  sdata <- data %>%
    # Filter
    filter(nutrient==nutrient_do ) %>%
    # Calculate stats
    group_by(nutrient, continent, iso3, country) %>%
    summarize(npeople=sum(npeople, na.rm=T),
              ndeficient=sum(ndeficient, na.rm=T)) %>%
    mutate(pdeficient=ndeficient/npeople) %>%
    ungroup()

  # Add to spatial data
  data_sf <- world_sm %>%
      select(-country) %>%
      left_join(sdata, by="iso3")


  # Create points for small countries
  data_pts <-world_centers %>%
      select(-country) %>%
      left_join(sdata, by="iso3") %>%
      filter(area_sqkm<=25000 & !is.na(pdeficient))

  # Plot data
  ################################################################################

  # Plot data
  g <- ggplot() +
    # Plot data
    geom_sf(data=data_sf, mapping=aes(fill=pdeficient), lwd=0.1) +
    geom_point(data=data_pts, mapping=aes(x=long_dd, y=lat_dd, fill=pdeficient), pch=21, size=3, inherit.aes = F, stroke=0.2) +
    # Labels
    labs(x="", y="", title=nutrient_do) +
    # Legend
    scale_fill_gradientn(name="% inadequate",
                         labels=scales::percent, lim=c(0,1),
                         colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                         na.value="grey90") +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",
                                 title.position = "top", title.hjust = 0.5)) +
    # Crop
    coord_sf(ylim=c(-52, 78)) +
    # Theme
    theme_bw() + base_theme +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  g

}
