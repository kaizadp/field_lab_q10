## Comparing field vs. lab Q10
## download data from google drive

source("code/0-packages.R")
library(googlesheets4)


# download from Google Drive ----------------------------------------------

n_data = 
  googlesheets4::read_sheet("1uBhsn7s6LJTl0CLYl7IPGQ6dM0tPnFiMY0iShcmIUX4") 

n_data2 = 
  n_data %>% 
  mutate_all(as.character) %>% 
  mutate(across(.fns = ~replace(., . == "NULL", NA))) %>% 
  mutate(Q10 = as.numeric(Q10))


ch4_data = 
  googlesheets4::read_sheet("1GIq2R8afh2p8tULEDuPmS2y-hSUTp8_wXTde2Wz5q8k")

ch4_data2 = 
  ch4_data %>% 
  mutate_all(as.character) %>% 
  mutate(across(.fns = ~replace(., . == "NULL", NA))) %>%   
  mutate(Q10 = as.numeric(Q10))


# save files locally ------------------------------------------------------

n_data2 %>% write.csv("data/data_from_papers/N_data.csv", row.names = FALSE, na = "")
ch4_data2 %>% write.csv("data/data_from_papers/CH4_data.csv", row.names = FALSE, na = "")
