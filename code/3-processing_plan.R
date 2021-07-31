source("code/0-packages.R")
source("code/1-googlesheets.R")
source("code/2-data_processing_functions.R")


# SET PATHS ---------------------------------------------------------------
GSHEETS_LOCAL_PATH_N = "data/2-data_from_papers-GoogleDrive/N_data.csv"
GSHEETS_LOCAL_PATH_CH4 = "data/2-data_from_papers-GoogleDrive/CH4_data.csv"


#
# PROCESSING PLAN ---------------------------------------------------------
processing_plan = drake_plan(
  # GoogleSheets: save locally as csv ----
  gsheets_data_N %>% write.csv(GSHEETS_LOCAL_PATH_N, row.names = FALSE, na = ""),
  gsheets_data_CH4 %>% write.csv(GSHEETS_LOCAL_PATH_CH4, row.names = FALSE, na = ""),
  
  # Process data from papers ----
  N_data_from_papers = import_N_data_from_papers(GSHEETS_LOCAL_PATH_N),
  CH4_data_from_papers = import_CH4_data_from_papers(GSHEETS_LOCAL_PATH_CH4),
  
  # export these data
  ## perhaps delete this later (?)
  write.csv(N_data_from_papers, "data/3-data_from_papers-cleaned/N_data.csv", row.names = FALSE, na = ""),
  write.csv(CH4_data_from_papers, "data/3-data_from_papers-cleaned/CH4_data.csv", row.names = FALSE, na = ""),
  
  # combine the data ----
  combined_data = bind_rows(N_data_from_papers, CH4_data_from_papers),

  )


make(processing_plan)

loadd(N_data_from_papers, CH4_data_from_papers)

x = 
