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
Monthly_data <- select(Monthly_data, -coicop)

Quarterly_data <- readRDS("data/Quarterly_data_all.R")  # Replace "your_quarterly_data.csv" with your file path

PredictionsMon <- read.csv("data/ML_predictions/forecasted_features_with_dates_montlhy.csv")  
PredictionsQa <- read.csv("data/ML_predictions/forecasted_features_with_dates_Q.csv")  

FeaturesM <- read.csv("data/ML feature importance/top_5_feature_importances_with_model_names_monthly.csv")
FeaturesQ <- read.csv("data/ML feature importance/top_5_feature_importances_with_model_names_quaterly.csv")

source("script/Funktions script.R")

##############################
### Prediction data
##############################
PredictionsM_processed <- Fix_Monthlt_predictions_df(PredictionsMon, Monthly_data)
PredictionsM_processed <- subset(PredictionsM_processed, time >= "2024-01-01")

PredictionsQ_processed <- Fix_Quaterly_predictions_df(PredictionsQa, Quarterly_data)
PredictionsQ_processed <- subset(PredictionsQ_processed, time >= "2024-01-01")

### Features

### 

all_years <- sort(unique(c(
  as.numeric(format(Monthly_data$time, "%Y")),
  as.numeric(format(Quarterly_data$time, "%Y"))
)))



# Define UI for application
# Define UI for application
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Makro Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Predictions", tabName = "predictions", icon = icon("chart-line")),  # <-- New tab added
      menuItem("Select dashboard specifics", icon = icon("database"),
               
               selectInput("dataset", "Select Dataset:", choices = c("Monthly", "Quarterly")),

               sliderInput("year_range", "Select Year Range:",
                           min = min(all_years),
                           max = max(all_years),
                           value = c(min(all_years), max(all_years)),
                           sep = "", step = 1),
               
               conditionalPanel(
                 condition = "input.dataset == 'Monthly'",
                 selectizeInput("country_monthly", "Select Country:", choices = unique(Monthly_data$geo),
                                multiple = TRUE, options = list(maxItems = 10)),
                 selectizeInput("variable_monthly", "Select Variables:", 
                                choices = c("Infaltion CPI", "Change in energy price",
                                            "Change in food price", "Unemployment data"),
                                multiple = TRUE, options = list(maxItems = 2))
               ),
               conditionalPanel(
                 condition = "input.dataset == 'Quarterly'",
                 selectizeInput("country_quarterly", "Select Country:", choices = unique(Quarterly_data$geo),
                                multiple = TRUE, options = list(maxItems = 10)),
                 selectizeInput("variable_quarterly", "Select Variable:", choices = c("Change in GDP", "Gross fixed capital formation", "Final consumption expenditure households",
                                                                                      "Government debt to gdp", "Government deficit", "Government expenditure", "Current account",
                                                                                      "Direct investment", "Net international investment", "Change in Housing prices",
                                                                                      "Totlat employment", "Employees Compensation","Nominal unit labour cost", "Labour productivity"),
                                multiple = TRUE, options = list(maxItems = 2))
               ),
               # New input for choosing between PredictionsM and PredictionsQ 
               selectInput("prediction_data", "Select Prediction Data:", 
                           choices = c("Monthly predictions", "Quarterly predictions")),
               
               # New geo filter input for filtering PredictionsM_processed or PredictionsQ
               selectizeInput("geo_filter", "Select Geo (Country):", 
                              choices = unique(PredictionsM_processed$geo), 
                              selected = NULL, 
                              multiple = TRUE),
               
               # Variable selector for the plot
               conditionalPanel(
                 condition = "input.prediction_data == 'Monthly predictions'",
                 selectizeInput("variable_predictionsM", "Select Variable to Plot:", 
                                choices = c("Infaltion CPI", "Change in energy price",
                                            "Change in food price", "Unemployment data"),
               )),
               conditionalPanel(
                 condition = "input.prediction_data == 'Quarterly predictions'",
                 selectizeInput("variable_predictionsQ", "Select Variable to Plot:", 
                                choices = c("Change in GDP", "Gross fixed capital formation", "Final consumption expenditure households",
                                            "Government debt to gdp", "Government expenditure", "Current account",
                                            "Direct investment", "Net international investment", "Change in Housing prices",
                                            "Totlat employment", "Employees Compensation","Nominal unit labour cost", "Labour productivity"),
                                )
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
      # Dashboard tab
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
                box(
                  title = "Correlation Between Selected Variables", 
                  solidHeader = TRUE, 
                  width = 12,
                  verbatimTextOutput("correlation_output"),
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
      ),
      
      # New Predictions tab
      tabItem(tabName = "predictions",
              fluidRow(
                box(
                  title = "Forecasted Trends", 
                  solidHeader = TRUE, 
                  width = 12,
                  plotOutput("prediction_plot"),
                  class = "box-custom"
                )
              ),
              fluidRow(
                box(
                  title = "Select Dataset and Model", 
                  solidHeader = TRUE, 
                  width = 4,
                  selectInput("feature_data_source", "Choose dataset:", choices = c("Monthly", "Quarterly")),
                  uiOutput("model_selector"),
                  class = "box-custom"
                ),
                box(
                  title = "Feature Importance", 
                  solidHeader = TRUE, 
                  width = 8,
                  plotOutput("Feature_plot"),
                  class = "box-custom"
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Dynamically adjust maxItems for country selector
  observeEvent(input$variable_monthly, {
    if (input$dataset == "Monthly") {
      max_items <- if (length(input$variable_monthly) == 1) 2 else 1
      selected <- input$country_monthly
      
      # Trim excess selections if needed
      if (!is.null(selected) && length(selected) > max_items) {
        updateSelectizeInput(session, "country_monthly",
                             selected = head(selected, max_items))
      }
    }
  })
  
  observeEvent(input$variable_quarterly, {
    if (input$dataset == "Quarterly") {
      max_items <- if (length(input$variable_quarterly) == 1) 2 else 1
      selected <- input$country_quarterly
      
      if (!is.null(selected) && length(selected) > max_items) {
        updateSelectizeInput(session, "country_quarterly",
                             selected = head(selected, max_items))
      }
    }
  })
  
  # Reactive filtered data
  filtered_data <- reactive({
    year_start <- input$year_range[1]
    year_end <- input$year_range[2]
    
    if (input$dataset == "Monthly") {
      Monthly_data %>%
        filter(geo %in% input$country_monthly,
               format(time, "%Y") >= year_start & format(time, "%Y") <= year_end)
    } else {
      Quarterly_data %>%
        filter(geo %in% input$country_quarterly,
               format(time, "%Y") >= year_start & format(time, "%Y") <= year_end)
    }
  })
  
  
  #########
  # The acctual data
  #########
  
  
  
  
  # Plot output
  output$time_series_plot <- renderPlot({
    data <- filtered_data()
    
    # Determine selected variables
    vars <- if (input$dataset == "Monthly") input$variable_monthly else input$variable_quarterly
    
    if (length(vars) == 0) {
      return(NULL)
    }
    
    if (length(vars) == 1) {
      ggplot(data, aes(x = time, y = get(vars[1]), color = geo)) +
        geom_line() +
        geom_point() +
        geom_text(aes(label = round(get(vars[1]), 2), color = geo), vjust = -0.5, size = 5) +  # Color the text the same as the line
        labs(title = paste("Time Series Plot of", vars[1]),
             x = "Time", y = vars[1]) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, face = "bold"),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text = element_text(size = 12)
        )
    } else if (length(vars) == 2) {
      scale_factor <- max(data[[vars[1]]], na.rm = TRUE) / max(data[[vars[2]]], na.rm = TRUE)
      
      ggplot(data, aes(x = time)) +
        geom_line(aes(y = get(vars[1]), color = geo)) +
        geom_line(aes(y = get(vars[2]) * scale_factor, linetype = geo), color = "#F2AA84") +
        scale_y_continuous(
          name = vars[1],
          sec.axis = sec_axis(~./scale_factor, name = vars[2])
        ) +
        labs(title = paste("Time Series Plot of", vars[1], "and", vars[2]),
             x = "Time") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, face = "bold"),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text = element_text(size = 12),
          axis.title.y.right = element_text(color = "#F2AA84"),
        #  axis.title.y.left = element_text(color = "#0F9ED5")
        )
    }
  })
  
  ##############
  # Predictions
  ##############
  
  # Dynamic plot for prediction data
  output$prediction_plot <- renderPlot({
    if (input$prediction_data == "Monthly predictions") {
      vars <- input$variable_predictionsM
      data <- PredictionsM_processed %>% filter(geo %in% input$geo_filter)
      
      if (length(vars) == 0) {
        return(NULL)
      }
      
      # Identify the last observation per group
      last_obs <- data %>%
        group_by(geo) %>%
        filter(time == max(time)) %>%
        ungroup()
      
      ggplot(data, aes(x = time)) +
        geom_line(aes(y = get(vars[1]), color = geo)) +
        geom_point(data = last_obs, aes(y = get(vars[1])), color = "blue", size = 3) +
        labs(
          title = paste("Forecasted Trends for", vars[1]),
          x = "Time",
          y = vars[1]
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          axis.title.x = element_text(size = 16), 
          axis.title.y = element_text(size = 16),  
          axis.text.x = element_text(size = 14),   
          axis.text.y = element_text(size = 14)
        )
      
    } else if (input$prediction_data == "Quarterly predictions") {
      vars <- input$variable_predictionsQ
      data <- PredictionsQ_processed %>% filter(geo %in% input$geo_filter)
      
      if (length(vars) == 0) {
        return(NULL)
      }
      
      # Identify the last observation per group
      last_obs <- data %>%
        group_by(geo) %>%
        filter(time == max(time)) %>%
        ungroup()
      
      ggplot(data, aes(x = time)) +
        geom_line(aes(y = get(vars[1]), color = geo)) +
        geom_point(data = last_obs, aes(y = get(vars[1])), color = "blue", size = 3) +
        labs(
          title = paste("Forecasted Trends for", vars[1]),
          x = "Time",
          y = vars[1]
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          axis.title.x = element_text(size = 16), 
          axis.title.y = element_text(size = 16),  
          axis.text.x = element_text(size = 14),   
          axis.text.y = element_text(size = 14)
        )
    }
  })
  
  ##############
  # Feature imporance
  ##############
  
  # Provide UI for model selection based on chosen dataset
  output$model_selector <- renderUI({
    models <- if (input$feature_data_source == "Monthly") {
      unique(FeaturesM$Model)
    } else {
      unique(FeaturesQ$Model)
    }
    
    selectInput("selected_model", "Choose model:", choices = models)
  })
  
  # Render the feature importance barplot
  output$Feature_plot <- renderPlot({
    req(input$selected_model, input$feature_data_source)
    
    df <- if (input$feature_data_source == "Monthly") FeaturesM else FeaturesQ
    
    df_filtered <- df %>%
      filter(Model == input$selected_model) %>%
      arrange(desc(Importance))
    
    ggplot(df_filtered, aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(
        title = paste("Feature Importance:", input$selected_model),
        x = "Feature", y = "Importance"
      ) +
      theme_minimal()+
      theme(
        legend.position = "bottom",
        axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(size = 16),  
        axis.text.x = element_text(size = 14),   
        axis.text.y = element_text(size = 14)
      )
    
  })
  
  ##############
  # Correlation between variables
  ##############
  
  output$correlation_output <- renderText({
    data <- filtered_data()
    vars <- if (input$dataset == "Monthly") input$variable_monthly else input$variable_quarterly
    
    if (length(vars) == 2) {
      # Remove rows with NA before computing correlation
      data <- data %>% select(all_of(vars)) %>% na.omit()
      
      corr <- cor(data[[vars[1]]], data[[vars[2]]])
      paste0("Correlation between '", vars[1], "' and '", vars[2], "' is: ", round(corr, 3))
    } else {
      "Select exactly two variables to compute correlation."
    }
  })
  
  # Download filtered data
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
