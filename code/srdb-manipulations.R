
SRDB_FILE_PATH_DATA = "data/CO2/SRDB_V5_1827/data/srdb-data-V5.csv"
SRDB_FILE_PATH_EQ = "data/CO2/SRDB_V5_1827/data/srdb-equations-V5.csv"
SRDB_FILE_PATH_STUDIES = "data/CO2/SRDB_V5_1827/data/srdb-studies-V5.csv"
SIDB_FILE_PATH = "data/CO2/sidb.RData"

# PART 1: SRDB (Soil Respiration DataBase) --------------------------------

get_srdb_manipulations_table <- function(){
  srdb_v5_data <- read.csv(SRDB_FILE_PATH_DATA)
  #srdb_v5_equations <- read.csv(SRDB_FILE_PATH_EQ)
  #srdb_v5_studies <- read.csv(SRDB_FILE_PATH_STUDIES)
  #srdb_v5_rh_list = read.csv("data/CO2/SRDB_v5_rh_studies.csv")
  
  manipulations <-
    srdb_v5_data %>% 
    dplyr::select(Manipulation) %>% 
    distinct() %>% 
    knitr::kable()
  