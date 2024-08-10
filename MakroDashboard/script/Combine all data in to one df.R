pacman::p_load("stringr", "pbapply","knitr" ,"openxlsx", "dplyr", "tidyr", "purrr")


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

saveRDS(Monthly_data, "MakroDashboard/data/Monthly_data_all.R")


names(Monthly_data)

