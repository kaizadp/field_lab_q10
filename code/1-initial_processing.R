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

do_q10_exploration = function(){
  srdb_v5_equations = read.csv("data/SRDB_V5_1827/data/srdb-equations-V5.csv")
  
  
  temp_effect = 
    srdb_v5_equations %>% 
    dplyr::select(Temp_effect, Q10_5_15, Q10_0_20, Q10_0_10) %>% 
    pivot_longer(-Temp_effect)
  
  temp_effect %>% 
    ggplot(aes(x = name, y = value, color = Temp_effect, group = Temp_effect))+
    geom_point(position = position_dodge(width = 0.5))
  
}

import_individual_studies = function(){
  
  filePaths_field <- list.files(path = "data/cleaned_for_analysis/field",pattern = "*.csv", full.names = TRUE)
  filePaths_lab <- list.files(path = "data/cleaned_for_analysis/lab",pattern = "*.csv", full.names = TRUE)
  
  field_data <- do.call(bind_rows, lapply(filePaths_field, function(path) {
    df <- read.csv(path, header=TRUE)
    df})) %>% 
    mutate(Incubation = if_else(is.na(Incubation), "field", Incubation),
           Depth_cm = as.character(Depth_cm))
  
  lab_data <- do.call(bind_rows, lapply(filePaths_lab, function(path) {
    df <- read.csv(path, header=TRUE)
    df})) %>% 
    mutate(Incubation = "lab")
  
  combined = 
    bind_rows(lab_data, field_data) %>% 
    dplyr::select(Source, Incubation, Record_number, Duplicate_record,
                  Site_name, Latitude_deg, Longitude_deg, Latitude, Longitude,
                  Species, Soil_type, Ecosystem_type, 
                  Temp_range, Temp_mean, Temp_min, Temp_max, Q10, R10)
  
# next:
#  - fix lat/longtitude
#  - fix temp ranges/temp_mean
  
  
  
}

## misc code ----
more_data = import_individual_studies()

more_data_srdb = more_data %>% 
  bind_rows(srdb_q10 %>% 
              mutate(Record_number = as.character(Record_number),
                                Incubation = "field",
                                Source = "SRDB") %>% 
              rename(Temp_range = temp_range))

more_data2 = 
  more_data_srdb %>% 
  dplyr::select(Source, Incubation, Q10, Temp_range, Temp_mean, Latitude, Longitude) %>% 
  mutate(Temp_range2 = Temp_range) %>% 
  separate(Temp_range2, sep = "_", into = c("Temp_min", "Temp_max")) %>% 
  mutate(Temp_min = as.numeric(Temp_min),
         Temp_max = as.numeric(Temp_max),
         Temp_mean = if_else(is.na(Temp_mean), (Temp_min+Temp_max)/2, Temp_mean))



more_data2 %>% 
  ggplot(aes(x = Temp_mean, y = Q10, color = Incubation))+
  geom_point()+
  geom_smooth()

more_data2 %>% 
  ggplot(aes(y = Latitude, x = Q10, color = Incubation))+
  geom_point()

hamdi = read_csv("data/cleaned_for_analysis/lab/Hamdi2013.csv")

hamdi2 = 
  hamdi %>% 
  dplyr::select(latitude_deg) %>% 
  mutate(latitude_deg = if_else(grepl('"', latitude_deg), 
                                      latitude_deg, 
                                      str_replace_all(latitude_deg, "'", "'00")),
         latitude_deg = if_else(grepl("'", latitude_deg), 
                                latitude_deg, 
                                str_replace_all(latitude_deg, "_", "_00'00")),
         ) %>% 
  rowwise() %>% 
  mutate(
         
         
         latitude_deg = str_replace_all(latitude_deg, "_", " "),
         latitude_deg = str_replace_all(latitude_deg, "'", " "),
         latitude_deg = str_replace_all(latitude_deg, '"', " "),
         latitude_deg = str_remove(latitude_deg, "N"),
         lat = measurements::conv_unit(latitude_deg, from = "deg_min_sec", to = "dec_deg")
         )



# SIDb --------------------------------------------------------------------

# import_sidb_data = function(){
  
  library(sidb)
  library(data.table)
  
  # use the `sidb` package to load the data and then
  # flatten the large list into a list with two nested lists 
  load("data/sidb.RData")
  sidb_flat = flatterSIDb(sidb)
  
  # convert the two lists into dataframes
  # vars needs data.table::rbindlist because it is a different format 
  sidb_timeseries = dplyr::bind_rows(sidb_flat$timeseries)
  sidb_vars = rbindlist(sidb_flat$vars, fill = TRUE)
  
  ## next steps:
  # - combine data with metadata, using ID - done
  # - exclude time > 350 d? - done
  # - exclude 13C/14C data - done
  # - exclude glucose additions - done
  # - exclude data with only a single temperature level
  
  clean_sidb_data = function(sidb_vars, sidb_timeseries){
    sidb_vars_clean = 
      sidb_vars %>% 
      filter(!units %in% c("permille", "percentC14Remaining")) %>% 
      filter(is.na(elevatedCO2) | elevatedCO2 == "control") %>% 
      filter(is.na(glucose)) %>% 
      filter(is.na(cellulose) | cellulose == "control")
    
    sidb_timeseries_clean = 
      sidb_timeseries %>% 
      filter(time <= 370) %>% 
      left_join(sidb_vars_clean %>% dplyr::select(ID, temperature, units, citationKey)) %>% 
      drop_na() 
    
      # keep only data with multiple temperature levels
      temp_count = 
        sidb_timeseries_clean %>% 
        distinct(citationKey, temperature) %>%
        group_by(citationKey) %>% 
        dplyr::mutate(n = n())
      
      sidb_timeseries_clean2 = 
        sidb_timeseries_clean %>% 
        left_join(temp_count) %>% 
        filter(n > 1) %>% 
        dplyr::select(-n)
    
    
    list(sidb_vars_clean = sidb_vars_clean,
         sidb_timeseries_clean2 = sidb_timeseries_clean2)
  }
  sidb_vars_clean = clean_sidb_data(sidb_vars, sidb_timeseries)$sidb_vars_clean
  sidb_timeseries_clean = clean_sidb_data(sidb_vars, sidb_timeseries)$sidb_timeseries_clean2
  
  calculate_sidb_q10_r10 = function(sidb_timeseries_clean){
    # using equations from Meyer et al. 2018. https://doi.org/10.1002/2017GB005644
    fit_q10_parameters = function(sidb_timeseries_clean){
      curve.nls = nls(response ~ a*exp(temperature * b),
                        start=list(a=5,
                                   b=2),
                        data = sidb_timeseries_clean)
      coef1 = coef(curve.nls) %>% as.data.frame() 
      coef_transpose = coef1 %>% transpose() %>% `colnames<-`(rownames(coef1))
      coef_transpose
    }
    
    a =   
      sidb_timeseries_clean %>% 
      group_by(citationKey, time) %>% 
      do(fit_q10_parameters(.))
    
  }

