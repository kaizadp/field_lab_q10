library(tidyverse)
theme_set(theme_bw())


clean_srdb_dataset = function(){
  srdb_v5_data = read.csv("data/SRDB_V5_1827/data/srdb-data-V5.csv")
  srdb_v5_equations = read.csv("data/SRDB_V5_1827/data/srdb-equations-V5.csv")
  
  sites =
    srdb_v5_data %>% 
    dplyr::select(Record_number, Study_number, Entry_date, Duplicate_record, 
                  Latitude, Longitude, Elevation, Biome, Ecosystem_type, 
                  Manipulation, Manipulation_level, Meas_method) %>% 
    filter(Manipulation == "None"|Manipulation_level %in% c("None", "NONE", "none"))
  
  r10 = 
    srdb_v5_equations %>% 
    dplyr::select(Record_number, R10)
  
  
  process_q10_data = function(srdb_v5_equations){
    q10_1 = 
      srdb_v5_equations %>% 
      dplyr::select(Record_number, Q10_0_10, Q10_5_15, Q10_10_20, Q10_0_20) %>% 
      pivot_longer(-Record_number, names_to = "temp_range", values_to = "Q10") %>% 
      mutate(temp_range = str_remove(temp_range, "Q10_")) 
    
    q10_other1 = 
      srdb_v5_equations %>% 
      dplyr::select(Record_number, starts_with("Q10_other1")) %>% 
      drop_na() %>% 
      # round temperature ranges to the nearest 5
      mutate(Q10_other1_temp_min = round(Q10_other1_temp_min/5)*5,
             Q10_other1_temp_max = round(Q10_other1_temp_max/5)*5,
             temp_range = paste0(Q10_other1_temp_min, "_", Q10_other1_temp_max)) %>% 
      rename(Q10 = Q10_other1) %>% 
      mutate(Q10 = round(Q10, 2)) %>% 
      dplyr::select(Record_number, temp_range, Q10)
    
    q10_other2 = 
      srdb_v5_equations %>% 
      dplyr::select(Record_number, starts_with("Q10_other2")) %>% 
      drop_na() %>% 
      mutate(temp_range = paste0(Q10_other2_temp_min, "_", Q10_other2_temp_max)) %>% 
      rename(Q10 = Q10_other2) %>% 
      mutate(Q10 = round(Q10, 2)) %>% 
      dplyr::select(Record_number, temp_range, Q10)
    
    bind_rows(q10_1, q10_other1, q10_other2) %>% filter(Q10 <= 500)
    
  }
  q10 = process_q10_data(srdb_v5_equations)
  
  r10_sites = sites %>% left_join(r10) %>% filter(!is.na(Q10))
  q10_sites = sites %>% left_join(q10) %>% filter(!is.na(Q10))

  list(r10_sites = r10_sites,
       q10_sites = q10_sites)
  
}
