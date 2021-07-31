## Comparing field vs. lab Q10
## download data from google drive

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

gsheets_path_N = "1uBhsn7s6LJTl0CLYl7IPGQ6dM0tPnFiMY0iShcmIUX4"
gsheets_path_CH4 = "1GIq2R8afh2p8tULEDuPmS2y-hSUTp8_wXTde2Wz5q8k"


gsheets_data_N = load_N_files_from_googlesheets(gsheets_path_N)
gsheets_data_CH4 = load_CH4_files_from_googlesheets(gsheets_path_CH4)