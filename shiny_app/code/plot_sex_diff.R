
# Plot coverage
# data <- data; nutrient <- "Calcium"; country <- "Ghana"
plot_sex_diff <- function(data, country, base_theme){

  # Country
  country_do <- country

  # Subset data
  sdata <- data %>%
    # Reduce
    filter(country==country_do) %>%
    # Simplify
    select(nutrient, sex, age_range, sev) %>%
    # Spread
    spread(key="sex", value="sev") %>%
    # Calc diff
    mutate(diff=Males-Females)


  # Plot data
  g <- ggplot(sdata, aes(x=age_range, y=nutrient, fill=diff)) +
    # Raster
    geom_raster() +
    # Labels
    labs(x="Age range (yr)", y="") +
    # Legend
    scale_fill_gradient2(name="Difference in inadequacies between males and females\n(blue/red=more deficiencies in males/females, respectively)",
                         midpoint=0, mid="white", low="darkred", high="navy") +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", title.position = "top")) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position="bottom",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g

}
