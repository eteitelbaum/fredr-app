# Load packages
library(shiny)
library(fredr)
library(dplyr)
library(ggplot2)

# Set FRED API key
#fredr_set_key("YOUR FRED API KEY")
fred_api_key = Sys.getenv("fred_api_key")
fredr_set_key(fred_api_key) 

# Assign FRED series to objects
cci <- "CSCICP03USM665S"
bci <- "BSCICP03USM665S"
cli <- "USALOLITONOSTSAM"
unemp_rate <- "UNRATE"
growth <- "A191RL1Q225SBEA"

# set start and end date
start_date <- as.Date("1970-01-01")
end_date <- as.Date(Sys.Date())

# Create list of named values for the input selection
vars <- c(
          "Consumer Confidence" = cci,
          "Business Confidence" = bci,
          "Composite Indicator" = cli,
          "Unemployment Rate" = unemp_rate,
          "Growth Rate" = growth
          )

# Load helper script
source("helper.R")

# Define the UI for our FRED line chart

ui <- fluidPage(
  
  # Application title
  titlePanel("FRED Data App"),
  
  # Create the dropdown menu
  
  fluidRow(
    
    # The dropdown menu panel will take 4 columns
    column(4, wellPanel(
      selectInput("indicator", "Indicator", vars)
    ),
    helpText("Select an indicator, choose a date range and view the trend.
             The grey bars represent economic ressions.
             The data for this app comes from the St. Louis Fed's FRED databse.
             The consumer confidence, business confidence and lead composite 
             indicators are OECD data downloaded through FRED.")
    ),
    
    # The remaining 8 columns will be occupied by the plot
    column(8,
           plotOutput("lineChart"),
           sliderInput(
             "range",
             "",
             min = start_date,
             max = end_date,
             value = c(start_date, end_date),
             width = "100%"
           )
           )
  )
)

# Define the server function

server <- function(input, output) {
  
  # Download data from FRED with reactive function.
  # Only updates when user selects new indicator
  fred_indicator <- reactive({
    fredr(series_id = input$indicator,
          observation_start = start_date,
          observation_end = end_date)
    
  })
  
  # Filter data according to selected years
  # Only updates when user selects new date range
  fred_data <- reactive({
    fred_indicator() |>
      filter(between(date, input$range[1], input$range[2]))
  })
  
  # Render line chart
  output$lineChart <- renderPlot({
    
    glimpse(fred_data())
    #print(fred_data(), n = 100)
    
    # Build plot with ggplot2
    # ggplot(fred_data(), aes(x = date, y = value)) +
    #   geom_line(color = "navyblue") +
    #   labs(
    #     x = "",
    #     y = names(vars[which(vars == input$indicator)])
    #   )+
    #   theme_minimal() +
    #   # add recession shading
    #   add_rec_shade(st_date = input$range[1],
    #                 ed_date = input$range[2],
    #                 shade_color = "darkgrey",
    #                 y_min = min(fred_data()$value),
    #                 y_max = max(fred_data()$value))
    })
}

shinyApp(ui = ui, server = server)