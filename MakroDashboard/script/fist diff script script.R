
Monthly_data <- readRDS("MakroDashboard/data/Monthly_data_all.R")

variables <- c("Unemployment data")

for (variable in variables) {
Monthly_data <- Monthly_data %>%
  group_by(geo) %>%
  mutate(!!variable := c(NA, diff(!!sym(variable)) / head(!!sym(variable), -1) * 100)) %>%
  ungroup() 
}

Monthly_data$`Unemployment data` <- round(Monthly_data$`Unemployment data`, digits = 4) 



Monthly_data <- select(Monthly_data, -coicop)

Monthly_wide <- Monthly_data %>%
  pivot_wider(
    id_cols = time,
    names_from = geo,
    values_from = c(`Infaltion CPI`, `Change in energy price`, `Change in food price`, `Unemployment data`)
  )


write.csv(Monthly_wide, "MakroDashboard/data/ML data//Monthly_wide_diff.csv")


####Quarter

Quarterly_data <- readRDS("MakroDashboard/data/Quarterly_data_all.R")
Quarterly_data <- select(Quarterly_data,-`Government deficit`)


variables <- c("Direct investment", "Employees Compensation", "Final consumption expenditure households", 
               "Totlat employment", "Gross fixed capital formation", "Government debt to gdp", 
               "Government expenditure")

for (variable in variables) {
  Quarterly_data <- Quarterly_data %>%
    group_by(geo) %>%
    mutate(!!variable := c(NA, diff(!!sym(variable)) / head(!!sym(variable), -1) * 100)) %>%
    ungroup()
}

# Let's say Quarterly2 is your starting data
Quarterly_wide <- Quarterly_data %>%
  pivot_wider(
    id_cols = time,
    names_from = geo,
    values_from = c(`Current account`, `Direct investment`, `Employees Compensation`, `Totlat employment`,
                    `Final consumption expenditure households`, `Change in GDP`, `Gross fixed capital formation`,
                    `Government debt to gdp`, `Government expenditure`, 
                    `Change in Housing prices`, `Labour productivity`, `Net international investment`,
                    `Nominal unit labour cost`) 
  )





write.csv(Quarterly_wide, "MakroDashboard/data/ML data//Quarterly_wide_diff.csv")


