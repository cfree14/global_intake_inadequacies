
# Plot coverage
# data <- data; country <- "Ghana"
plot_inadequacies_v2 <- function(data, country, base_theme){

  # Country
  country_do <- country

  # Subset data
  sdata <- data %>%
    filter(country==country_do)

  # Subset data
  sdata2 <- sdata %>%
    # Simplify
    select(nutrient, sex, age_range, sev) %>%
    # Spread
    spread(key="sex", value="sev") %>%
    # Calc diff
    mutate(diff=Males-Females)

  # Nutrient order
  nutr_key <- sdata %>%
    group_by(nutrient) %>%
    summarize(ndeficient=sum(ndeficient, na.rm=T)) %>%
    ungroup() %>%
    arrange(desc(ndeficient))

  # Apply nutrient order
  sdata_ordered <- sdata %>%
    mutate(nutrient=factor(nutrient, levels=nutr_key$nutrient))
  sdata2_ordered <- sdata2 %>%
    mutate(nutrient=factor(nutrient, levels=nutr_key$nutrient))

  # Plot data
  g1 <- ggplot(sdata_ordered, aes(x=age_range, y=nutrient, fill=sev)) +
    facet_wrap(~sex) +
    geom_raster() +
    # Labels
    labs(x="Age range (yrs)", y="", tag="A") +
    # Legend
    scale_fill_gradientn(name=" \n% inadequate", lim=c(0,100), colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", title.position = "top", title.hjust = 0.5)) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position="bottom",
          legend.key.size = unit(0.8, "cm"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g1

  # Plot data
  g2 <- ggplot(sdata2_ordered, aes(x=age_range, y=nutrient, fill=diff)) +
    # Raster
    geom_raster() +
    # Labels
    labs(x="Age range (yrs)", y="", tag="B", title=" ") +
    # Legend
    scale_fill_gradient2(name="Δ Inadequacies (%)\n(males - females)",
                         midpoint=0, mid="white", low="darkred", high="navy") +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", title.position = "top", title.hjust = 0.5)) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position="bottom",
          legend.key.size = unit(0.8, "cm"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g2

  # Merge
  g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.66, 0.34))
  g

}
