
Fix_Monthlt_predictions_df  <- function(data1, data2){
  
  Monthly_data <- readRDS("data/Monthly_data_all.R")  # Replace "your_data.csv" with your file path
  

PredictionsM <- data1  
PredictionsM$geo <- "Finland" 

PredictionsM <- PredictionsM %>% 
  rename(time = Date)%>%
  rename(`Infaltion CPI` = Infaltion.CPI_Finland_target) %>%
  rename(`Change in energy price` = Change.in.energy.price_Finland_target) %>%
  rename(`Change in food price` = Change.in.food.price_Finland_target) %>%
  rename(`Unemployment data` = Unemployment.data_Finland_target)

PredictionsM <- select(PredictionsM, geo, time, `Infaltion CPI`,`Change in energy price`, `Change in food price`,
                       `Unemployment data`)

# Get the last 'Unemployment data' from data2 for Finland
last_unemp <- Monthly_data %>%
  filter(geo == "Finland") %>%
  arrange(desc(time)) %>%
  slice(2) %>%
  pull(`Unemployment data`)

# Adjust the new unemployment value
PredictionsM$`Unemployment data` <- last_unemp * ((PredictionsM$`Unemployment data`/100) + 1)

PredictionsM_final <- subset(data2, geo == "Finland")
PredictionsM_final <- rbind(PredictionsM_final,PredictionsM)


return(PredictionsM_final)
}

#################
### Fix quaterly data
##################

old_names <- c(
  "Current.account_Finland_target",
  "Direct.investment_Finland_target",
  "Employees.Compensation_Finland_target",
  "Totlat.employment_Finland_target",
  "Final.consumption.expenditure.households_Finland_target",
  "Change.in.GDP_Finland_target",
  "Gross.fixed.capital.formation_Finland_target",
  "Government.debt.to.gdp_Finland_target",
  "Government.expenditure_Finland_target",
  "Change.in.Housing.prices_Finland_target",
  "Labour.productivity_Finland_target",
  "Net.international.investment_Finland_target",
  "Nominal.unit.labour.cost_Finland_target",
  "geo",
  "Date"
)

new_names <- c(
  "Current account",
  "Direct investment",
  "Employees Compensation",
  "Totlat employment",
  "Final consumption expenditure households",
  "Change in GDP",
  "Gross fixed capital formation",
  "Government debt to gdp",
  "Government expenditure",
  "Change in Housing prices",
  "Labour productivity",
  "Net international investment",
  "Nominal unit labour cost",
  "geo",
  "time"
)

# Quarterly_data <- readRDS("MakroDashboard/data/Quarterly_data_all.R")  # Replace "your_quarterly_data.csv" with your file path
# PredictionsQ <- read.csv("MakroDashboard/data/ML_predictions/forecasted_features_with_dates_Q.csv")
# data_Q <- select(Quarterly_data, -`Government deficit`)

Fix_Quaterly_predictions_df  <- function(data1, data2){
  
  PredictionsQ <- data1
  
  PredictionsQ$geo <- "Finland" 
  data_Q <- select(data2, -`Government deficit`)
  
  #Rename and reorder df1
  names(PredictionsQ) <- new_names[match(names(PredictionsQ), old_names)]
  PredictionsQ <- PredictionsQ[, names(data_Q)]
  
  ####
  
  variables <- c("Direct investment", "Employees Compensation", "Final consumption expenditure households", 
                 "Totlat employment", "Gross fixed capital formation", "Government debt to gdp", 
                 "Government expenditure")
  
  for (variable in variables) {
    
    last_obs <- data_Q %>%
      filter(geo == "Finland") %>%
      arrange(desc(time)) %>%
      slice(2) %>%
      pull(!!sym(variable))
    
    if (length(last_obs) == 0) next  # Skip if no second-to-last observation
    
    PredictionsQ[[variable]] <- last_obs * ((PredictionsQ[[variable]] / 100) + 1)
  }
  
  #################
  
  
  
  PredictionsQ_finland <- subset(data_Q, geo == "Finland")
  PredictionsQ_final <- rbind(PredictionsQ_finland,PredictionsQ)
  
  return(PredictionsQ_final)
  
  
}