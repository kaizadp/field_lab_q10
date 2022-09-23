# FIELD VS. LAB GHG Q10
# PATEL ET AL. 2022
# 
# This script contains functions needed to download data from Google Sheets
# and clean the data for further processing and analysis.
# kfp 2022
# 
# ############################# #

source("code/0-packages.R")
library(googlesheets4)


# download from Google Drive ----------------------------------------------

load_CH4_files_from_googlesheets = function(gsheets_path_CH4){
  ch4_data = 
    googlesheets4::read_sheet(gsheets_path_CH4)
  
  ch4_data %>% 
    mutate_all(as.character) %>% 
    mutate(across(.fns = ~replace(., . == "NULL", NA))) %>%   
    mutate(Q10 = as.numeric(Q10))
  
}

load_CO2_files_from_googlesheets = function(gsheets_path_CH4){
  co2_data = 
    googlesheets4::read_sheet(gsheets_path_CO2)
  
  co2_data %>% 
    mutate_all(as.character) %>% 
    mutate(across(.fns = ~replace(., . == "NULL", NA))) %>%   
    mutate(Q10 = as.numeric(Q10))
  
}


# set the GSheets path
gsheets_path_CH4 = "1GIq2R8afh2p8tULEDuPmS2y-hSUTp8_wXTde2Wz5q8k"
gsheets_path_CO2 = "1hgpGLFuVAhnV7mfCThyXu2ApFLeC7sIR91cQ_I9TP7c"

# run the functions
gsheets_data_CH4 = load_CH4_files_from_googlesheets(gsheets_path_CH4)
gsheets_data_CO2 = load_CO2_files_from_googlesheets(gsheets_path_CO2)

# export the files
gsheets_data_CH4 %>% write.csv(GSHEETS_LOCAL_PATH_CH4, row.names = FALSE, na = "")
gsheets_data_CO2 %>% write.csv(GSHEETS_LOCAL_PATH_CO2, row.names = FALSE, na = "")
