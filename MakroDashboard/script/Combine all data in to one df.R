pacman::p_load("stringr","lubridate", "pbapply","knitr" ,"openxlsx", "dplyr", "tidyr", "purrr", "zoo")

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
### Make the LM data
###########################################################################

Monthly_data <- readRDS("MakroDashboard/data/Monthly_data_all.R")
Quarterly_data <- readRDS("MakroDashboard/data/Quarterly_data_all.R")


earliest_time_per_geo <- Quarterly_data %>%
  group_by(geo) %>%
  summarise(earliest_time = min(time, na.rm = TRUE), .groups = "drop")


Monthly_data_filtered <- Monthly_data %>%
  left_join(earliest_time_per_geo, by = "geo") %>%
  filter(time >= earliest_time)


################
##Quarterly
##############

Quarterly2 <- merge(Quarterly_data, Monthly_data, by = c("geo", "time"), all.x = T)
Quarterly2 <- select(Quarterly_data, -`Government deficit`)


# Let's say Quarterly2 is your starting data
Quarterly2_wide <- Quarterly2 %>%
  pivot_wider(
    id_cols = time,
    names_from = geo,
    values_from = c(`Current account`, `Direct investment`, `Employees Compensation`, `Totlat employment`,
                    `Final consumption expenditure households`, `Change in GDP`, `Gross fixed capital formation`,
                    `Government debt to gdp`, `Government expenditure`, 
                    `Change in Housing prices`, `Labour productivity`, `Net international investment`,
                    `Nominal unit labour cost`) #`Infaltion CPI`, `Change in energy price`, 
                    #`Change in food price`, `Unemployment data`)
  )

write.csv(Quarterly2_wide, "MakroDashboard/data/ML data//Quarterly_wide2.csv")


################
##Monthly
##############

Monthly_data <- select(Monthly_data, -coicop)

Monthly_wide <- Monthly_data %>%
  pivot_wider(
    id_cols = time,
    names_from = geo,
    values_from = c(`Infaltion CPI`, `Change in energy price`, `Change in food price`, `Unemployment data`)
  )

write.csv(Monthly_wide, "MakroDashboard/data/ML data/Monthly_wide.csv")

## Ide 2

Monthly_data2 <- merge(Monthly_data_filtered, Quarterly_data, by = c("geo", "time"), all.x = T)