pacman::p_load("stringr", "pbapply","knitr" ,"openxlsx", "dplyr", "tidyr", "purrr", "zoo")

###Quarterly 
# Directory containing the Excel files
directory <- "MakroDashboard/data/quarterly/"

# Get a list of all Excel files in the directory
R_files <- list.files(path = directory, pattern = "\\.R$", full.names = TRUE)

# Create an empty list to store the data frames
data_list <- list()

# Loop through each Excel file, read it, and store it in the list
for (file in R_files) {
  data <- readRDS(file)
  data_list[[basename(file)]] <- data
}


Quarterly_data <- reduce(data_list, left_join, by = c("geo", "time"))

Quarterly_data <- Quarterly_data %>%
  rename(`Final consumption expenditure households` = Final_consumption) %>%
  rename(`Change in GDP` = GDP_change) %>%
  rename(`Gross fixed capital formation` = GFCF) %>%
  rename(`Government debt to gdp` = government_debt) %>%
  rename(`Government expenditure` = Government_expenditure) %>%
  rename(`Change in Housing prices` = House_price) %>%
  rename(`Government deficit` = government_deficit) %>%
  rename(`Current account` = Current_account) %>%
  rename(`Direct investment` = Direct_investment) %>%
  rename(`Employees Compensation` = Employees_Compensation) %>%
  rename(`Net international investment` = Net_international_investment) %>%
  rename(`Nominal unit labour cost` = Nominal_unit_labour_cost) %>%
  rename(`Labour productivity` = labour_productivity) %>%
  rename(`Totlat employment` = Employment)
  



Quarterly_data <- Quarterly_data %>% 
  mutate(geo = case_when(
    geo == "DE" ~ "Germany",
    geo == "DK" ~ "Denmark",
    geo == "ES" ~ "Spain",
    geo == "FI" ~ "Finland",
    geo == "FR" ~ "France",
    geo == "IT" ~ "Italy",
    geo == "NL" ~ "Netherlands",
    geo == "SE" ~ "Sweden",
    TRUE ~ geo # This keeps other values unchanged
  ))
  

saveRDS(Quarterly_data, "MakroDashboard/data/Quarterly_data_all.R")

###Monthly 
# Directory containing the Excel files
directory <- "MakroDashboard/data/Monthly/"

# Get a list of all Excel files in the directory
R_files <- list.files(path = directory, pattern = "\\.R$", full.names = TRUE)

# Create an empty list to store the data frames
data_list <- list()

# Loop through each Excel file, read it, and store it in the list
for (file in R_files) {
  data <- readRDS(file)
  data_list[[basename(file)]] <- data
}


Monthly_data <- reduce(data_list, left_join, by = c("geo", "time"))

Monthly_data <- select(Monthly_data, -coicop.x, -coicop.y)


Monthly_data <- Monthly_data %>%
  rename(`Change in energy price` = Energy_price) %>%
  rename(`Change in food price` = food_price) %>%
  rename(`Infaltion CPI` = CPI) %>%
  rename(`Unemployment data` = unemployment_data)

Monthly_data <- Monthly_data %>% 
  mutate(geo = case_when(
    geo == "DE" ~ "Germany",
    geo == "DK" ~ "Denmark",
    geo == "ES" ~ "Spain",
    geo == "FI" ~ "Finland",
    geo == "FR" ~ "France",
    geo == "IT" ~ "Italy",
    geo == "NL" ~ "Netherlands",
    geo == "SE" ~ "Sweden",
    TRUE ~ geo # This keeps other values unchanged
  ))

saveRDS(Monthly_data, "MakroDashboard/data/Monthly_data_all.R")

###########################################################################








