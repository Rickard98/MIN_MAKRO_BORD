
pacman::p_load("stringr", "pbapply","knitr" ,"openxlsx", "dplyr", "tidyr", "eurostat", "httr", "jsonlite")

library(eurostat)
############################################################
# Fetch total unemployment data for select countries
############################################################

unemployment_data_fi_total_pcact <- get_eurostat("une_rt_m", 
                                                 filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), 
                                                                sex = "T", 
                                                                unit = "PC_ACT", 
                                                                age = "TOTAL", 
                                                                s_adj = "SA" ), 
                                                 time_format="date")


unemployment_data_fi_total_pcact <- na.omit(unemployment_data_fi_total_pcact)
unemployment_data_fi_total_pcact <- select(unemployment_data_fi_total_pcact, geo, time, values )

unemployment_data_fi_total_pcact <- unemployment_data_fi_total_pcact %>% rename( unemployment_data = values)

saveRDS(unemployment_data_fi_total_pcact, "MakroDashboard/data/Monthly/unemployment_data.R")


#################################################
# Fetch CPI data for select countries
#####################################################


CPI   <- get_eurostat("prc_hicp_manr",
                        filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), coicop = "CP00"),
                        time_format = "date")

CPI <- select(CPI, geo, time, coicop, values)
CPI <- CPI %>% rename(CPI = values)


saveRDS(CPI, "MakroDashboard/data/Monthly/CPI.R")


#################################################
# Fetch Energy infaltion data for select countries
#####################################################
Energy_price <- get_eurostat("prc_hicp_manr", 
                            filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), coicop = "NRG"), 
                            time_format="date")

Energy_price <- select(Energy_price, geo, time, coicop, values)
Energy_price <- na.omit(Energy_price)
Energy_price <- Energy_price %>% rename(Energy_price = values)

saveRDS(Energy_price, "MakroDashboard/data/Monthly/Energy_price.R")

#################################################
# Fetch food infaltion data for select countries
#####################################################


food_price <- get_eurostat("prc_hicp_manr", 
                             filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), coicop = "CP011"), 
                             time_format="date")

food_price <- select(food_price, geo, time, coicop, values)
food_price <- na.omit(food_price)
food_price <- food_price %>% rename(food_price = values)

saveRDS(food_price, "MakroDashboard/data/Monthly/food_price.R")



#################################
################################
#######Quarterly data 
#################################
#################################



#################################################
# Fetch GDP Growth data for select countries
#####################################################

GDP <- get_eurostat("namq_10_gdp", 
                    filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), unit = "CLV_PCH_PRE", s_adj = "SCA", na_item = "B1GQ"), 
                    time_format="date")

GDP <- select(GDP, geo, time, values)

GDP<- na.omit(GDP)
GDP <- GDP %>% rename(GDP_change = values)

saveRDS(GDP, "MakroDashboard/data/quarterly/GDP.R")


#################################################
# Fetch House price data for select countries
#####################################################

House_price <- get_eurostat("prc_hpi_q", 
                            filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), purchase = "TOTAL", unit = "RCH_Q"), 
                            time_format="date")
House_price <- select(House_price, geo, time, values)

House_price <- na.omit(House_price)
House_price <- House_price %>% rename(House_price = values)

saveRDS(House_price, "MakroDashboard/data/quarterly/House_price.R")


#################################################
# Final consumption expenditure of households and non-profit institutions serving households - quarterly data
#####################################################

Final_consumption <- get_eurostat("tipsho41", 
                           filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), unit = "CP_MNAC" ), 
                           time_format="date")

Final_consumption <- select(Final_consumption, geo, time, values)
Final_consumption <- na.omit(Final_consumption)
Final_consumption <- Final_consumption %>% rename(Final_consumption = values)

saveRDS(Final_consumption, "MakroDashboard/data/quarterly/Final_consumption.R")

#################################################
# Fetch Government expenditure data for select countries
#####################################################
#CP_MEUR

Government_expenditure <- get_eurostat("namq_10_gdp", 
                                  filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"),na_item = "P3_S13", s_adj = "NSA", unit = "CP_MEUR" ), 
                                  time_format="date")

Government_expenditure <- select(Government_expenditure, geo, time, values)
Government_expenditure <- na.omit(Government_expenditure)
Government_expenditure <- Government_expenditure %>% rename(Government_expenditure = values)

saveRDS(Government_expenditure, "MakroDashboard/data/quarterly/Government_expenditure.R")

#################################################
# Fetch Gross fixed capital formation data for select countries
#####################################################
#CP_MEUR

GFCF <- get_eurostat("namq_10_gdp", 
                                  filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"),na_item = "P51G", s_adj = "NSA", unit = "CP_MEUR" ), 
                                  time_format="date")

GFCF <- select(GFCF, geo, time, values)
GFCF <- na.omit(GFCF)
GFCF <- GFCF %>% rename(GFCF = values)

saveRDS(GFCF, "MakroDashboard/data/quarterly/GFCF.R")


#################################################
# Fetch Quarterly government debt data for select countries
#####################################################

government_debt <- get_eurostat("gov_10q_ggdebt", 
                                  filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"),unit = "PC_GDP",
                                                 na_item = "GD",sector = "S13"), 
                                  time_format="date")

government_debt <- select(government_debt, geo, time, values)
government_debt <- na.omit(government_debt)
government_debt <- government_debt %>% rename(government_debt = values)

saveRDS(government_debt, "MakroDashboard/data/quarterly/government_debt.R")



#################################################
# Fetch General government deficit data for select countries
#####################################################


government_deficit <- get_eurostat("teina205", 
                                  filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), unit = "PC_GDP_NSA" ), 
                                  time_format="date")

government_deficit <- select(government_deficit, geo, time, values)
government_deficit <- na.omit(government_deficit)
government_deficit <- government_deficit %>% rename(government_deficit = values)

saveRDS(government_deficit, "MakroDashboard/data/quarterly/government_deficit.R")



###New data


#################################################
# Net international investment position - quarterly data, % of GDP
#####################################################

Net_international_investment <- get_eurostat("tipsii40", 
                                   filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), unit = "PC_GDP" ), 
                                   time_format="date")

Net_international_investment <- select(Net_international_investment, geo, time, values)
Net_international_investment <- na.omit(Net_international_investment)
Net_international_investment <- Net_international_investment %>% rename(Net_international_investment = values)

saveRDS(Net_international_investment, "MakroDashboard/data/quarterly/Net_international_investment.R")


#################################################
# Direct investment - quarterly data, million units of national currency
#####################################################

Direct_investment <- get_eurostat("tipsii42", time_format = "date")

Direct_investment <- get_eurostat("tipsii42", 
                                             filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), stk_flow = "A_LE" ), 
                                             time_format="date")

Direct_investment <- select(Direct_investment, geo, time, values)
Direct_investment <- na.omit(Direct_investment)
Direct_investment <- Direct_investment %>% rename(Direct_investment = values)

saveRDS(Direct_investment, "MakroDashboard/data/quarterly/Direct_investment.R")


#################################################
# Current account, main components, net balance - quarterly data, % of GDP
#####################################################

Current_account <- get_eurostat("tipsbp41", time_format = "date")

Current_account <- get_eurostat("tipsbp41", 
                                  filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), bop_item = "CA" ), 
                                  time_format="date")


Current_account <- select(Current_account, geo, time, values)
Current_account <- na.omit(Current_account)
Current_account <- Current_account %>% rename(Current_account = values)

saveRDS(Current_account, "MakroDashboard/data/quarterly/Current_account.R")


#################################################
#Real labour productivity per person employed - quarterly data
#####################################################

labour_productivity <- get_eurostat("tipsna71", 
                                filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), unit = "PCH_SM" ), 
                                time_format="date")


labour_productivity <- select(labour_productivity, geo, time, values)
labour_productivity <- na.omit(labour_productivity)
labour_productivity <- labour_productivity %>% rename(labour_productivity = values)

saveRDS(labour_productivity, "MakroDashboard/data/quarterly/labour_productivity.R")

#################################################
#Compensation of employees - quarterly data, million units of national currency
#####################################################

Employees_Compensation <- get_eurostat("tipslm14", 
                                    filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), s_adj = "NSA" ), 
                                    time_format="date")


Employees_Compensation <- select(Employees_Compensation, geo, time, values)
Employees_Compensation <- na.omit(Employees_Compensation)
Employees_Compensation <- Employees_Compensation %>% rename(Employees_Compensation = values)

saveRDS(Employees_Compensation, "MakroDashboard/data/quarterly/Employees_Compensation.R")


#################################################
#Employment, domestic concept - quarterly data, thousands persons, not seasonally adjusted
#####################################################


Employment <- get_eurostat("tipsna61", 
                                       filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), na_item = "EMP_DC" ), 
                                       time_format="date")


Employment <- select(Employment, geo, time, values)
Employment <- na.omit(Employment)
Employment <- Employment %>% rename(Employment = values)

saveRDS(Employment, "MakroDashboard/data/quarterly/Employment.R")



#################################################
#Nominal unit labour cost (NULC) - quarterly data
#####################################################

Nominal_unit_labour_cost <- get_eurostat("tipslm40", time_format = "date")


Nominal_unit_labour_cost <- get_eurostat("tipslm40", 
                           filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), unit = "PCH_SM" ), 
                           time_format="date")


Nominal_unit_labour_cost <- select(Nominal_unit_labour_cost, geo, time, values)
Nominal_unit_labour_cost <- na.omit(Nominal_unit_labour_cost)
Nominal_unit_labour_cost <- Nominal_unit_labour_cost %>% rename(Nominal_unit_labour_cost = values)

saveRDS(Nominal_unit_labour_cost, "MakroDashboard/data/quarterly/Nominal_unit_labour_cost.R")

#################################
################################
#######OECD data  
#################################
#################################


