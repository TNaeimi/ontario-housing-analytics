###########################################################
# Load packages for data wrangling, dashboard development,
# interactive visualization, and Bootstrap styling
###########################################################
library(tidyverse)
library(shiny)
library(plotly)
library(bslib)

dataFolder = "./Data/Raw/"
###########################################################
# Define data source location and import housing dataset
###########################################################


housing_supply_and_rental <- read.csv(
  paste0(dataFolder, "housing-supply-price-rental.csv")
)
###########################################################
# Prepare dashboard dataset
#
# - Focus on selected Ontario municipalities
# - Create a date field for time-series visualization
###########################################################

clean_housing_supply_and_rental <-
  housing_supply_and_rental %>%
  filter(
    region %in% c(
      "guelph", "hamilton", "ottawa",
      "oshawa", "windsor", "sudbury",
      "peterborough", "brantford",
      "toronto", "thunder_bay",
      "barrie", "kingston"
    )
  ) %>%
  mutate(
    Date = as.Date(paste0(year, "-01-01"))
  )
              
###########################################################
# User Interface
#
# Provides:
# - Housing indicator selection
# - Dynamic chart title
# - Interactive Plotly visualization
###########################################################

housing_dash <- clean_housing_supply_and_rental

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  div(
    style = "margin-bottom: 20px;",
    h1("Ontario Housing Monitoring Dashboard", style = "font-weight: bold; color: #000000; "),
    h4(
      "Interactive monitoring of housing market indicators 1990-2016", 
      style = "color: #6c757d; font-weight: 400;"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        "var",
        "Select Housing Indicator:",
        choices = names(housing_dash)[!names(housing_dash) %in% c("X","year","other","res_building_permit_amount","vacancy_rate_seniors",
                                                                  "labour_participation_rate",
                                                                  "unemployment_rate",
                                                                  "disposable_income_change",
                                                                  "region",
                                                                  "Date")]
      )
    ),
    mainPanel(
      width = 9,
      card(
        card_header(textOutput("card_title")),
        plotlyOutput("plot", height = "500px")
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$card_title <- renderText({
    paste0("HOUSING TREND: YEAR VS ", toupper(input$var))
  })
  
  output$plot <- renderPlotly({
    
    plot_ly(
      data = housing_dash,
      x = ~Date,
      y = ~get(input$var), 
      type = "bar",
      marker = list(
        color = 'purple', 
        cornerradius = 5,
        line = list(color = '#ffffff', width = 1)
      ),
      hovertemplate = paste0(
        "<b>Date:</b> %{x}<br>",
        "<b>Value:</b> %{y:,.0f}<extra></extra>" 
      )
    ) %>%
      layout(
        margin = list(t = 20, r = 20, b = 40, l = 50),
        font = list(family = "Inter, sans-serif", size = 13),
        xaxis = list(
          title = FALSE, 
          showgrid = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          title = list(text = input$var, font = list(size = 14, color = "#7f8c8d")),
          gridcolor = "#f0f0f0", 
          zeroline = FALSE
        ),
        config = list(displayModeBar = FALSE) 
      )
    
  })
  
}

shinyApp(ui, server)

