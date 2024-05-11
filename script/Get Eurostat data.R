
pacman::p_load("stringr", "pbapply","knitr" ,"openxlsx", "dplyr", "tidyr", "eurostat")

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

saveRDS(unemployment_data_fi_total_pcact, "data/Monthly/unemployment_data.R")


#################################################
# Fetch CPI data for select countries
#####################################################


CPI   <- get_eurostat("prc_hicp_manr",
                        filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), coicop = "CP00"),
                        time_format = "date")

CPI <- select(CPI, geo, time, coicop, values)
CPI <- CPI %>% rename(CPI = values)


saveRDS(CPI, "data/Monthly/CPI.R")


#################################################
# Fetch Energy infaltion data for select countries
#####################################################


Energy_price <- get_eurostat("prc_hicp_manr", 
                            filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), coicop = "NRG"), 
                            time_format="date")

Energy_price <- select(Energy_price, geo, time, coicop, values)
Energy_price <- na.omit(Energy_price)
Energy_price <- Energy_price %>% rename(Energy_price = values)

saveRDS(Energy_price, "data/Monthly/Energy_price.R")

#################################################
# Fetch food infaltion data for select countries
#####################################################


food_price <- get_eurostat("prc_hicp_manr", 
                             filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), coicop = "CP011"), 
                             time_format="date")

food_price <- select(food_price, geo, time, coicop, values)
food_price <- na.omit(food_price)
food_price <- food_price %>% rename(food_price = values)

saveRDS(food_price, "data/Monthly/food_price.R")

#######Quarterly data 





#################################################
# Fetch GDP Growth data for select countries
#####################################################

GDP <- get_eurostat("namq_10_gdp", 
                    filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), unit = "CLV_PCH_PRE", s_adj = "SCA", na_item = "B1GQ"), 
                    time_format="date")

GDP <- select(GDP, geo, time, values)

GDP<- na.omit(GDP)
GDP <- GDP %>% rename(GDP_change = values)

saveRDS(GDP, "data/quarterly/GDP.R")


#################################################
# Fetch House price data for select countries
#####################################################

House_price <- get_eurostat("prc_hpi_q", 
                            filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), purchase = "TOTAL", unit = "RCH_A"), 
                            time_format="date")
House_price <- select(House_price, geo, time, values)

House_price <- na.omit(House_price)
House_price <- House_price %>% rename(House_price = values)

saveRDS(House_price, "data/quarterly/House_price.R")


#################################################
# Fetch Final consumption data for select countries
#####################################################

Final_consumption <- get_eurostat("namq_10_gdp", 
                           filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"),na_item = "P3", s_adj = "NSA", unit = "CP_MEUR" ), 
                           time_format="date")

Final_consumption <- select(Final_consumption, geo, time, values)
Final_consumption <- na.omit(Final_consumption)
Final_consumption <- Final_consumption %>% rename(Final_consumption = values)

saveRDS(Final_consumption, "data/quarterly/Final_consumption.R")

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

saveRDS(Government_expenditure, "data/quarterly/Government_expenditure.R")

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

saveRDS(GFCF, "data/quarterly/GFCF.R")


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

saveRDS(government_debt, "data/quarterly/government_debt.R")



#################################################
# Fetch General government deficit data for select countries
#####################################################


government_deficit <- get_eurostat("teina205", 
                                  filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), unit = "PC_GDP_NSA" ), 
                                  time_format="date")

government_deficit <- select(government_deficit, geo, time, values)
government_deficit <- na.omit(government_deficit)
government_deficit <- government_deficit %>% rename(government_deficit = values)

saveRDS(government_deficit, "data/quarterly/government_deficit.R")



