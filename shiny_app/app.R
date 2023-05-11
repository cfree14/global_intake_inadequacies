
# Clear
rm(list = ls())
options(dplyr.summarise.inform=F)

# Setup
################################################################################

# Packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(RColorBrewer)

# Directories
datadir <- "data" # for actual app
codedir <- "code"  # for actual app
# datadir <- "shiny_app/data" # when testing
# codedir <- "shiny_app/code" # when testing

# Source code
sapply(list.files(codedir), function(x) source(file.path(codedir, x)))

# Read data
data <- readRDS(file.path(datadir, "2018_subnational_nutrient_intake_inadequacy_estimates.Rds"))

# Read world data
world_lg <- readRDS(file=file.path(datadir, "world_large.Rds"))
world_sm <- readRDS(file=file.path(datadir, "world_small.Rds")) %>% sf::st_as_sf()
world_centers <- readRDS(file=file.path(datadir, "world_centroids.Rds"))



# Parameters
################################################################################

# Parameters
countries <- sort(unique(data$country))
ncountries <- length(countries)
nutrients <- sort(unique(data$nutrient))

# Base theme
base_theme <- theme(axis.text=element_text(size=12),
                    axis.title=element_text(size=14),
                    legend.text=element_text(size=12),
                    legend.title=element_text(size=14),
                    strip.text=element_text(size=14),
                    plot.subtitle=element_text(size=12),
                    plot.title=element_text(size=14),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"))


# User interface
################################################################################

# User interface
ui <- navbarPage("Subnational nutrient intake inadequacies",

  # Explore by nutrient
  tabPanel("Explore by nutrient",

    # Select by nutrient
    selectInput(inputId = "nutrient", label = "Select a nutrient:",
               choices = nutrients,  multiple = F, selected="Calcium"),
    br(),

    # Inadequacies map
    plotOutput(outputId = "plot_inadequacies_map", width=800, height=250),
    br(),

    # Select by country
    selectInput(inputId = "country", label = "Select a country:",
                choices = countries,  multiple = F, selected="Afghanistan"),
    br(),

    # Plot intakes and requirements
    h3("Subnational intakes, requirements, and inadequacies"),
    p("The figure below shows usual intake distributions (the colored points and lines) by age-sex group relative to their average requirement (the black line). The color of the usual intake distribution lines indicates the prevalence of inadequate intakes. The point represents the median usual intake based on the Global Dietary Database (GDD). The thick line represents the inner 50% of the intake distribution and the thin line represents the inner 95% of the intake distribution. The shape of the distribution around the GDD median is based on Passarelli et al. (2022). The black line shows the average requirement defined by Allen et al. (2020)."),
    plotOutput(outputId = "plot_intakes", width=800, height=450),
    br()

  ),

  # Explore by country
  tabPanel("Explore by country",

    # Select by country
    selectInput(inputId = "country2", label = "Select a country:",
               choices = countries,  multiple = F, selected="Afghanistan"),
    br(),

    # Plot inadequacies
    h3("Inadequacies by nutrient"),
    p("The figure below shows the prevalence of inadequate intakes by age-sex group for the selected country. Micronutrients are ordered from fewest intake inadequacies (top) to most intake intake inadequacies (bottom)."),
    plotOutput(outputId = "plot_inadequacies", width=1000, height=450),
    br(),

    # Plot intakes and requirements
    h3("Difference in inadequacies between sexes"),
    p("The figure below shows the difference in the prevalence of inadequate intakes between males and females in the same age group. Blue colors indicate age groups in which males have more intake inadequacies than females and red colors initiate age groups in which females have more intake inadequacies than males."),
    plotOutput(outputId = "plot_sex_diff", width=600, height=400),
    br(),

    # Plot intakes and requirements
    h3("Subnational intakes, requirements, and inadequacies"),
    p("The figure below shows usual intake distributions (the colored points and lines) by age-sex group relative to their average requirement (the black line). The color of the usual intake distribution lines indicates the prevalence of inadequate intakes. The point represents the median usual intake based on the Global Dietary Database (GDD). The thick line represents the inner 50% of the intake distribution and the thin line represents the inner 95% of the intake distribution. The shape of the distribution around the GDD median is based on Passarelli et al. (2022). The black line shows the average requirement defined by Allen et al. (2020)."),
    plotOutput(outputId = "plot_intakes_country", width=800, height=4000),
    br()

  ),

  # Explore by country
  tabPanel("References",

    h3("References"),
    p("1. Allen LH, Carriquiry AL, Murphy SP (2020) Perspective: Proposed harmonized nutrient reference values for populations. Advances in Nutrition 11: 469–83."),
    p("2. Global Dietary Database (2022) The GDD Prediction Model. Available at: https://www.globaldietarydatabase.org/gdd-prediction-model"),
    p("3. Passarelli S, Free CM, Allen LH, Batis C, Beal T, Biltoft-Jensen AP, Bromage S, Cao L, Castellanos-Guitiérrez A, Christensen T, Crispim SP, Dekkers A, De Ridder K, Kronsteiner-Gicevic S, Lee C, Li Y, Moursi M, Moyersoen I, Schmidhuber J,  Shepon A, Viana DF, Golden CD (2022) Estimating national and subnational nutrient intake distributions of global diets. The American Journal of Clinical Nutrition 116(2): 551-560."),
    p("4. Passarelli S, Free CM,  Shepon A, Beal T, Batis C, Golden CD. Global estimates of national and subnational nutrient intake inadequacies. Near submission to The Lancet.")

  )

)


# Server
################################################################################

# Server
server <- function(input, output, session){

  # Plot inadequacies - map
  output$plot_inadequacies_map <- renderPlot({
    g <- plot_inadequacies_map(data = data,
                               world_sm = world_sm,
                               world_centers = world_centers,
                               nutrient = input$nutrient,
                               base_theme = base_theme)
    g
  })

  # Plot intakes
  output$plot_intakes <- renderPlot({
    g <- plot_intakes(data = data,
                      nutrient = input$nutrient,
                      country = input$country,
                      base_theme = base_theme)
    g
  })

  # Plot inadequacies - country
  output$plot_inadequacies <- renderPlot({
    g <- plot_inadequacies(data = data,
                           country = input$country2,
                           base_theme = base_theme)
    g
  })

  # Plot inadequacies sex diff - country
  output$plot_sex_diff <- renderPlot({
    g <- plot_sex_diff(data = data,
                       country = input$country2,
                       base_theme = base_theme)
    g
  })

  # Plot intakes - country
  output$plot_intakes_country <- renderPlot({
    g <- plot_intakes_country(data = data,
                              country = input$country2,
                              base_theme = base_theme)
    g
  })

}

shinyApp(ui = ui, server = server)
