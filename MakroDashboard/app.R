# Load required libraries
library(shiny)
library(ggplot2)
library(stringr)
library(knitr)
library(dplyr)
library(tidyr)
library(purrr)



# Load your data
Monthly_data <- readRDS("data/Monthly_data_all.R")  # Replace "your_data.csv" with your file path
Quarterly_data <- readRDS("data/Quarterly_data_all.R")  # Replace "your_quarterly_data.csv" with your file path

# Filter out rows with time values smaller than 2019-01-01
Monthly_data <- Monthly_data %>%
  filter(time >= as.Date("2019-01-01"))

Quarterly_data <- Quarterly_data %>%
  filter(time >= as.Date("2019-01-01"))

# Define UI for application
ui <- fluidPage(
  titlePanel("Makro Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Select Dataset:", choices = c("Monthly", "Quarterly")),
      conditionalPanel(
        condition = "input.dataset == 'Monthly'",
        selectInput("country_monthly", "Select Country:", choices = unique(Monthly_data$geo)),
        selectInput("variable_monthly", "Select Variable:", choices = c("CPI", "Energy_price", "food_price", "unemployment_data"))
      ),
      conditionalPanel(
        condition = "input.dataset == 'Quarterly'",
        selectInput("country_quarterly", "Select Country:", choices = unique(Quarterly_data$geo)),
        selectInput("variable_quarterly", "Select Variable:", choices = c("Final_consumption", "GDP_change", "GFCF", "government_debt", "government_deficit",
                                                                          "Government_expenditure", "House_price"))
      )
    ),
    
    mainPanel(
      plotOutput("time_series_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$time_series_plot <- renderPlot({
    if (input$dataset == "Monthly") {
      # Filter Monthly data based on selected country
      data <- Monthly_data[Monthly_data$geo == input$country_monthly, ]
      
      # Plot selected variable over time
      ggplot(data, aes(x = time, y = get(input$variable_monthly))) + 
        geom_line() +
        labs(title = paste("Time Series Plot of", input$variable_monthly),
             x = "Time", y = input$variable_monthly)
    } else {
      # Filter Quarterly data based on selected country
      data <- Quarterly_data[Quarterly_data$geo == input$country_quarterly, ]
      
      # Plot selected variable over time
      ggplot(data, aes(x = time, y = get(input$variable_quarterly))) + 
        geom_line() +
        labs(title = paste("Time Series Plot of", input$variable_quarterly),
             x = "Time", y = input$variable_quarterly)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
