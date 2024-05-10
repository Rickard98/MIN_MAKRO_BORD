install.packages("eurostat")

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

write.xlsx(unemployment_data_fi_total_pcact, "data/unemployment_data.xlsx")


#################################################
# Fetch CPI data for select countries
#####################################################


CPI   <- get_eurostat("prc_hicp_manr",
                        filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), coicop = "CP00"),
                        time_format = "date")

CPI <- select(CPI, geo, time, coicop, values)

write.xlsx(CPI, "data/CPI.xlsx")


#################################################
# Fetch GDP Growth data for select countries
#####################################################

GDP <- get_eurostat("namq_10_gdp", 
                                  filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), unit = "CLV_PCH_PRE", s_adj = "SCA", na_item = "B1GQ"), 
                                  time_format="date")

GDP <- select(GDP, geo, time, unit, values)

GDP<- na.omit(GDP)

write.xlsx(GDP, "data/GDP.xlsx")

#################################################
# Fetch House price data for select countries
#####################################################

House_price <- get_eurostat("prc_hpi_q", 
                    filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), purchase = "TOTAL", unit = "RCH_A"), 
                    time_format="date")
House_price <- select(House_price, geo, time, values)

House_price <- na.omit(House_price)
write.xlsx(House_price, "data/House_price.xlsx")

#################################################
# Fetch Energy infaltion data for select countries
#####################################################


Energy_price <- get_eurostat("prc_hicp_manr", 
                            filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), coicop = "NRG"), 
                            time_format="date")

Energy_price <- select(Energy_price, geo, time, coicop, values)
Energy_price <- na.omit(Energy_price)

write.xlsx(Energy_price, "data/Energy_price.xlsx")


#################################################
# Fetch food infaltion data for select countries
#####################################################


food_price <- get_eurostat("prc_hicp_manr", 
                             filters = list(geo = c("FI", "SE", "DK", "DE", "FR", "NL", "IT", "ES"), coicop = "CP011"), 
                             time_format="date")

food_price <- select(food_price, geo, time, coicop, values)
food_price <- na.omit(food_price)

write.xlsx(Energy_price, "data/food_price.xlsx")


