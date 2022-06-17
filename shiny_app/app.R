
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
data <- readRDS(file.path(datadir, "2011_subnational_nutrient_intake_inadequacy_estimates.Rds"))


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

    # Select by country
    selectInput(inputId = "country", label = "Select a country:",
                choices = countries,  multiple = F, selected="Afghanistan"),
    br(),

    # Plot intakes and requirements
    h3("Subnational intakes, requirements, and inadequacies"),
    plotOutput(outputId = "plot_intakes", width=800, height=450),
    br()



  ),

  # Explore by country
  tabPanel("Explore by country",


  )

)


# Server
################################################################################

# Server
server <- function(input, output, session){

  # Plot intakes
  output$plot_intakes <- renderPlot({
    g <- plot_intakes(data = data,
                      nutrient = input$nutrient,
                      country = input$country,
                      base_theme = base_theme)
    g

  })

}

shinyApp(ui = ui, server = server)
