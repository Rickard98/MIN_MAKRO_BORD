# Load required libraries
library(shiny)
library(ggplot2)
library(stringr)
library(knitr)
library(dplyr)
library(tidyr)
library(purrr)
library(rsconnect)
library(shinydashboard)  # Load shinydashboard
library(zoo)

# Load your data
Monthly_data <- readRDS("data/Monthly_data_all.R")  # Replace "your_data.csv" with your file path
Quarterly_data <- readRDS("data/Quarterly_data_all.R")  # Replace "your_quarterly_data.csv" with your file path




# Filter out rows with time values smaller than 2019-01-01
Monthly_data <- Monthly_data %>%
  filter(time >= as.Date("2019-01-01"))

Quarterly_data <- Quarterly_data %>%
  filter(time >= as.Date("2019-01-01"))

# Define UI for application
ui <- dashboardPage(
  skin = "blue", # Set the dahsboard collor
  dashboardHeader(title = "Makro Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Select Dataset", icon = icon("database"),
               selectInput("dataset", "Select Dataset:", choices = c("Monthly", "Quarterly")),
               conditionalPanel(
                 condition = "input.dataset == 'Monthly'",
                 selectInput("country_monthly", "Select Country:", choices = unique(Monthly_data$geo)),
                 selectInput("variable_monthly", "Select Variable:", choices = c("Infaltion CPI", "Change in energy price",
                                                                                 "Change in food price", "Unemployment data"))
               ),
               conditionalPanel(
                 condition = "input.dataset == 'Quarterly'",
                 selectInput("country_quarterly", "Select Country:", choices = unique(Quarterly_data$geo)),
                 selectInput("variable_quarterly", "Select Variable:", choices = c("Total aggregated Final consumption", "Change in GDP", "Gross fixed capital formation",
                                                                                   "Government debt to gdp", "Government deficit", "Current account", "Direct investment", "Employees Compensation",
                                                                                   "Government expenditure", "Change in Housing prices", "Totlat employment", "Net international investment",
                                                                                   "Nominal unit labour cost", "Labour productivity"))
               )
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Custom CSS to apply olive color to the box header and body */
        .box.box-solid.box-custom>.box-header {
          color: #fff;
          background-color: #3d9970; /* Olive color */
        }
        .box.box-solid.box-custom {
          border: 1px solid #3d9970; /* Olive border */
        }
        .box.box-solid.box-custom>.box-body {
          background-color: #f2f2f2; /* Customize the box body color */
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = "Time Series Plot", 
                  solidHeader = TRUE, 
                  width = 12,
                  plotOutput("time_series_plot"),
                  class = "box-custom"  # Apply custom class for the CSS
                )
              ),
              fluidRow(
                box(title = "Note about the data", width = 12, background = "light-blue", 
                HTML("The figures above are compiled from various data sources provided by Eurostat (https://ec.europa.eu/eurostat).<br>
                HCPI (inflation): Harmonized Consumer Price Index (all items and annual rate of change). Change in energy and food prices are also HCPI. <br>
                Unemployment data: Percentage of population in the labour force (seasonally adjusted data). <br>
                Gross domestic product (GDP): GDP at market prices (seasonally adjusted and chain linked volumes, annualized percentage change on previous period). <br>
                Change in Housing prices: House price index (2015 = 100), total types of houses, and annual rate of change. <br>
                Final consumption expenditure households: Final consumption expenditure of households and non-profit institutions serving households, Current prices, million units of national currency")
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$time_series_plot <- renderPlot({
    # Determine which dataset is selected
    if (input$dataset == "Monthly") {
      # Filter Monthly data based on selected country
      data <- Monthly_data[Monthly_data$geo == input$country_monthly, ]
      
      # Create the plot
      ggplot(data, aes(x = time, y = get(input$variable_monthly))) + 
        geom_line(color = "blue") +
        geom_point(color = "red") +  # Add points on the line
        geom_text(aes(label = round(get(input$variable_monthly), 2)), vjust = -0.5, size = 5) +  # Add data labels
        labs(title = paste("Time Series Plot of", input$variable_monthly),
             x = "Time", y = input$variable_monthly) +
        theme_minimal()
      
    } else {
      # Filter Quarterly data based on selected country
      data <- Quarterly_data[Quarterly_data$geo == input$country_quarterly, ]

      # Apply seasonal adjustment if the selected variable requires it
      if (input$variable_quarterly %in% c("Total aggregated Final consumption", "Gross fixed capital formation", "Government expenditure")) {
        window_size <- 4  # Adjust based on your data's seasonality (e.g., 4 for quarterly data)
        data[[input$variable_quarterly]] <- rollmean(data[[input$variable_quarterly]], k = window_size, fill = NA, align = "center")
      }
      
      # Create the plot
      ggplot(data, aes(x = time, y = get(input$variable_quarterly))) + 
        geom_line(color = "blue") +
        geom_point(color = "red") +  # Add points on the line
        geom_text(aes(label = round(get(input$variable_quarterly), 2)), vjust = -0.5, size = 5) +  # Add data labels
        labs(title = paste("Time Series Plot of", input$variable_quarterly),
             x = "Time", y = input$variable_quarterly) +
        theme_minimal()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
