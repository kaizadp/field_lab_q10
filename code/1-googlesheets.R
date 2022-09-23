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
load_N_files_from_googlesheets = function(gsheets_path_N){
  n_data = 
    googlesheets4::read_sheet(gsheets_path_N) 
  
  n_data %>% 
    mutate_all(as.character) %>% 
    mutate(across(.fns = ~replace(., . == "NULL", NA))) %>% 
    mutate(Q10 = as.numeric(Q10))
}

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


gsheets_path_N = "1uBhsn7s6LJTl0CLYl7IPGQ6dM0tPnFiMY0iShcmIUX4"
gsheets_path_CH4 = "1GIq2R8afh2p8tULEDuPmS2y-hSUTp8_wXTde2Wz5q8k"
gsheets_path_CO2 = "1hgpGLFuVAhnV7mfCThyXu2ApFLeC7sIR91cQ_I9TP7c"


gsheets_data_N = load_N_files_from_googlesheets(gsheets_path_N)
gsheets_data_CH4 = load_CH4_files_from_googlesheets(gsheets_path_CH4)
gsheets_data_CO2 = load_CO2_files_from_googlesheets(gsheets_path_CO2)

gsheets_data_N %>% write.csv(GSHEETS_LOCAL_PATH_N, row.names = FALSE, na = "")
gsheets_data_CH4 %>% write.csv(GSHEETS_LOCAL_PATH_CH4, row.names = FALSE, na = "")
gsheets_data_CO2 %>% write.csv(GSHEETS_LOCAL_PATH_CO2, row.names = FALSE, na = "")
