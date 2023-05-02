
# Plot coverage
# data <- data; country <- "Ghana"
plot_inadequacies <- function(data, country, base_theme){

  # Country
  country_do <- country

  # Subset data
  sdata <- data %>%
    filter(country==country_do)

  # Order
  nutr_key <- sdata %>%
    group_by(nutrient) %>%
    summarize(ndeficient=sum(ndeficient, na.rm=T)) %>%
    ungroup() %>%
    arrange(desc(ndeficient))
  sdata <- sdata %>%
    mutate(nutrient=factor(nutrient, levels=nutr_key$nutrient))

  # Plot data
  g <- ggplot(sdata, aes(x=age_range, y=nutrient, fill=sev)) +
    facet_wrap(~sex) +
    geom_raster() +
    # Labels
    labs(x="Age range", y="") +
    # Legend
    scale_fill_gradientn(name="% inadequate", lim=c(0,100), colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() + base_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g

}
