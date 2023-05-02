
# Plot coverage
# data <- data; nutrient <- "Calcium"; country <- "Ghana"
plot_sex_diff <- function(data, country, base_theme){

  # Country
  country_do <- country

  # Subset data
  sdata <- data %>%
    filter(country==country_do) %>%
    # Format nutrient title
    mutate(nutrient=recode(nutrient,
                           "Vitamin A (RAE)"="Vitamin A"),
           nutrient=paste0(nutrient, " (", units, ")"))

  # Get units for y-axis
  units <- sdata$units %>% unique()
  yaxis_label <- paste0("Usual intake (", units, ")")

  # Plot data
  g <- ggplot() +
    facet_grid(nutrient~sex, scale="free_y") +
    # Plot CI
    geom_segment(data=sdata, mapping=aes(x=age_range, xend=age_range, y=supply025, yend=supply975, color=sev/100), lwd=1) +
    geom_segment(data=sdata, mapping=aes(x=age_range, xend=age_range, y=supply25, yend=supply75, color=sev/100), lwd=2.5) +
    # Plot point
    geom_point(data=sdata, mapping=aes(x=age_range, y=supply50, fill=sev/100), size=5, pch=21) +
    # Reference
    geom_line(data=sdata, mapping=aes(x=age_range, y=ar, group=sex), inherit.aes = F) +
    # Labels
    labs(x="Age range (yr)", y="Usual intake", title=country_do) +
    lims(y=c(0, NA)) +
    # Legend
    scale_color_gradientn(name="% inadequate", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                         lim=c(0,1), labels=scales::percent, guide="none") +
    scale_fill_gradientn(name="% inadequate", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                         lim=c(0,1), labels=scales::percent) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position="top",
          legend.key.size = unit(1.2, "cm"),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g


}
