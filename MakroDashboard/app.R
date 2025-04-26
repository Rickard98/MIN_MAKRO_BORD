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

all_years <- sort(unique(c(
  as.numeric(format(Monthly_data$time, "%Y")),
  as.numeric(format(Quarterly_data$time, "%Y"))
)))



# Define UI for application
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Makro Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Select Dataset", icon = icon("database"),
               
               selectInput("dataset", "Select Dataset:", choices = c("Monthly", "Quarterly")),
               
               sliderInput("year_range", "Select Year Range:",
                           min = min(all_years),
                           max = max(all_years),
                           value = c(min(all_years), max(all_years)),
                           sep = "", step = 1),
               
               conditionalPanel(
                 condition = "input.dataset == 'Monthly'",
                 selectInput("country_monthly", "Select Country:", choices = unique(Monthly_data$geo)),
                 selectInput("variable_monthly", "Select Variable:", choices = c("Infaltion CPI", "Change in energy price",
                                                                                 "Change in food price", "Unemployment data"))
               ),
               conditionalPanel(
                 condition = "input.dataset == 'Quarterly'",
                 selectInput("country_quarterly", "Select Country:", choices = unique(Quarterly_data$geo)),
                 selectInput("variable_quarterly", "Select Variable:", choices = c("Change in GDP", "Gross fixed capital formation", "Final consumption expenditure households",
                                                                                   "Government debt to gdp", "Government deficit", "Government expenditure", "Current account",
                                                                                   "Direct investment", "Net international investment", "Change in Housing prices",
                                                                                   "Totlat employment", "Employees Compensation","Nominal unit labour cost", "Labour productivity"))
               )
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .box.box-solid.box-custom>.box-header {
          color: #fff;
          background-color: #3d9970;
        }
        .box.box-solid.box-custom {
          border: 1px solid #3d9970;
        }
        .box.box-solid.box-custom>.box-body {
          background-color: #f2f2f2;
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
                  br(),
                  downloadButton("download_filtered_data", "Download the desplayed countires data"),
                  class = "box-custom"
                )
              ),
              fluidRow(
                box(title = "Note about the data", width = 12, background = "light-blue", 
                    HTML("The figures above are compiled from various data sources provided by Eurostat (https://ec.europa.eu/eurostat).<br>
                    HCPI (inflation): Harmonized Consumer Price Index (all items and annual rate of change). Change in energy and food prices are also HCPI. <br>
                    Unemployment data: Percentage of population in the labour force (seasonally adjusted data). <br>
                    Gross domestic product (GDP): GDP at market prices (seasonally adjusted and chain linked volumes, annualized percentage change on previous period). <br>
                    Change in Housing prices: House price index (2015 = 100), total types of houses, and Quaterly rate of change. <br>
                    Final consumption expenditure households: Final consumption expenditure of households and non-profit institutions serving households, Current prices, million units of national currency. <br>
                    The unit labour cost (ULC) is defined as the ratio of labour costs to labour productivity. <br>
                    Compensation of employees (at current prices) is defined as the total remuneration, in cash or in kind, payable by an employer to an employee in return for work done by the latter during the accounting period. <br>
                    Labour productivity is expressed in terms of percentage change compared to same period in previous year. ")
                )
              )
      )
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  # Create a reactive filtered dataset
  filtered_data <- reactive({
    year_start <- input$year_range[1]
    year_end <- input$year_range[2]
    
    if (input$dataset == "Monthly") {
      Monthly_data %>%
        filter(geo == input$country_monthly,
               format(time, "%Y") >= year_start & format(time, "%Y") <= year_end)
    } else {
      Quarterly_data %>%
        filter(geo == input$country_quarterly,
               format(time, "%Y") >= year_start & format(time, "%Y") <= year_end)
    }
  })
  
  # Plot
  output$time_series_plot <- renderPlot({
    data <- filtered_data()
    
    if (input$dataset == "Monthly") {
      ggplot(data, aes(x = time, y = get(input$variable_monthly))) + 
        geom_line(color = "blue") +
        geom_point(color = "red") +
        geom_text(aes(label = round(get(input$variable_monthly), 2)), vjust = -0.5, size = 5) +
        labs(title = paste("Time Series Plot of", input$variable_monthly),
             x = "Time", y = input$variable_monthly) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, face = "bold"),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text = element_text(size = 12)
        )
    } else {
      ggplot(data, aes(x = time, y = get(input$variable_quarterly))) + 
        geom_line(color = "blue") +
        geom_point(color = "red") +
        geom_text(aes(label = round(get(input$variable_quarterly), 2)), vjust = -0.5, size = 5) +
        labs(title = paste("Time Series Plot of", input$variable_quarterly),
             x = "Time", y = input$variable_quarterly) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, face = "bold"),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text = element_text(size = 12)
        )
    }
  })
  
  # Download handler
  output$download_filtered_data <- downloadHandler(
    filename = function() {
      paste0("Filtered_", input$dataset, "_data.csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}


# Run the application 
shinyApp(ui = ui, server = server)
