SRDB_FILE_PATH_DATA = "data/CO2/SRDB_V5_1827/data/srdb-data-V5.csv"
SRDB_FILE_PATH_EQ = "data/CO2/SRDB_V5_1827/data/srdb-equations-V5.csv"
SRDB_FILE_PATH_STUDIES = "data/CO2/SRDB_V5_1827/data/srdb-studies-V5.csv"
SIDB_FILE_PATH = "data/CO2/sidb.RData"

# PART 1: SRDB (Soil Respiration DataBase) --------------------------------

clean_srdb_dataset <- function(){
  srdb_v5_data <- read.csv(SRDB_FILE_PATH_DATA)
  srdb_v5_equations <- read.csv(SRDB_FILE_PATH_EQ)
  srdb_v5_studies <- read.csv(SRDB_FILE_PATH_STUDIES)
  srdb_v5_rh_list = read.csv("data/CO2/SRDB_v5_rh_studies.csv")
  
  sites <-
    srdb_v5_data %>% 
    dplyr::select(Record_number, Study_number, Entry_date, Duplicate_record, 
                  Latitude, Longitude, Elevation, Biome, Ecosystem_type, Meas_method,
                  Manipulation, Manipulation_level, Meas_method, Soil_drainage, RC_annual) %>% 
    filter(Manipulation == "None"|Manipulation_level %in% c("None", "NONE", "none")) %>% 
    left_join(srdb_v5_rh_list)
  
  r10 <-
    srdb_v5_equations %>% 
    dplyr::select(Record_number, R10)
  
  studies_subset <- 
    srdb_v5_studies %>% 
    dplyr::select(Study_number, Authors, Title, PubYear) %>% 
    mutate(StudyName = paste0(Authors, "_", Title, "_", PubYear)) %>% 
    dplyr::select(Study_number, StudyName) 
    
  process_q10_data = function(srdb_v5_equations){
    q10_1 <-
      srdb_v5_equations %>% 
      dplyr::select(Record_number, Q10_0_10, Q10_5_15, Q10_10_20, Q10_0_20) %>% 
      pivot_longer(-Record_number, names_to = "Temp_range", values_to = "Q10") %>% 
      mutate(Temp_range = str_remove(Temp_range, "Q10_")) 
    
    q10_other1 <- 
      srdb_v5_equations %>% 
      dplyr::select(Record_number, starts_with("Q10_other1")) %>% 
      drop_na() %>% 
      # round temperature ranges to the nearest 5
      mutate(Q10_other1_temp_min = round(Q10_other1_temp_min/5)*5,
             Q10_other1_temp_max = round(Q10_other1_temp_max/5)*5,
             Temp_range = paste0(Q10_other1_temp_min, "_", Q10_other1_temp_max)) %>% 
      rename(Q10 = Q10_other1) %>% 
      mutate(Q10 = round(Q10, 2)) %>% 
      dplyr::select(Record_number, Temp_range, Q10)
    
    q10_other2 <- 
      srdb_v5_equations %>% 
      dplyr::select(Record_number, starts_with("Q10_other2")) %>% 
      drop_na() %>% 
      mutate(Temp_range = paste0(Q10_other2_temp_min, "_", Q10_other2_temp_max)) %>% 
      rename(Q10 = Q10_other2) %>% 
      mutate(Q10 = round(Q10, 2)) %>% 
      dplyr::select(Record_number, Temp_range, Q10)
    
    bind_rows(q10_1, q10_other1, q10_other2) %>% filter(Q10 <= 500) 
    
  }
  q10 <- process_q10_data(srdb_v5_equations)
  
  r10_sites <- sites %>% left_join(r10) %>% filter(!is.na(R10))
  q10_sites <- sites %>% left_join(q10) %>% filter(!is.na(Q10)) %>% 
    mutate(Source = "SRDB",
           Species = "CO2",
           Incubation = "field",
           SRDB_study_ID = paste0("SRDB-", Study_number),
           StudyName = paste0("SRDB-", Study_number)) %>% 
    rename(SRDB_record_number = Record_number,
           SRDB_study_number = Study_number) %>% 
    #left_join(studies_subset) %>% 
    force()
  
  list(r10_sites = r10_sites,
       q10_sites = q10_sites)
  
}

do_q10_exploration <- function(){
  srdb_v5_equations = read.csv("data/SRDB_V5_1827/data/srdb-equations-V5.csv")
  
  
  temp_effect <-
    srdb_v5_equations %>% 
    dplyr::select(Temp_effect, Q10_5_15, Q10_0_20, Q10_0_10) %>% 
    pivot_longer(-Temp_effect)
  
  temp_effect %>% 
    ggplot(aes(x = name, y = value, color = Temp_effect, group = Temp_effect))+
    geom_point(position = position_dodge(width = 0.5))
  
}


#
# PART 2: SIDb (Soil Incubation Database) ---------------------------------

## next steps:
# - combine data with metadata, using ID - done
# - exclude time > 350 d? - done
# - exclude 13C/14C data - done
# - exclude glucose additions - done
# - exclude data with only a single temperature level

clean_sidb_data <- function(sidb_vars, sidb_timeseries){
  sidb_vars_clean <- 
    sidb_vars %>% 
    filter(!units %in% c("permille", "percentC14Remaining")) %>% 
    filter(is.na(elevatedCO2) | elevatedCO2 == "control") %>% 
    filter(is.na(glucose)) %>% 
    filter(is.na(cellulose) | cellulose == "control")
  
  sidb_timeseries_clean <- 
    sidb_timeseries %>% 
    filter(time <= 370) %>% 
    left_join(sidb_vars_clean %>% dplyr::select(ID, temperature, units, citationKey)) %>% 
    drop_na() 
  
  # keep only data with multiple temperature levels
  temp_count <-
    sidb_timeseries_clean %>% 
    distinct(citationKey, temperature) %>%
    group_by(citationKey) %>% 
    dplyr::mutate(n = n())
  
  sidb_timeseries_clean2 <- 
    sidb_timeseries_clean %>% 
    left_join(temp_count) %>% 
    filter(n > 1) %>% 
    dplyr::select(-n)
  
  
  list(sidb_vars_clean = sidb_vars_clean,
       sidb_timeseries_clean2 = sidb_timeseries_clean2)
}

calculate_sidb_q10_r10 <- function(sidb_timeseries_clean, sidb_vars){
  # using equations from Meyer et al. 2018. https://doi.org/10.1002/2017GB005644
  fit_q10_parameters <- function(x){
    #browser()#    tryCatch()
    coefs <- data.frame(n = nrow(x), err = NA_character_)
    tryCatch({
      # Estimate the a and b parameters using a linear model...
      m <- lm(log(response) ~ temperature, data = x)
      a_start <- exp(coef(m)[1])
      b_start <- coef(m)[2]
      
      # ...and then fit the nonlinear model using these starting values
      curve.nls <- nls(response ~ a * exp(temperature * b),
                       start = list(a = a_start, b = b_start),
                       data = x)
      coefs <- cbind(coefs, data.frame(as.list(coef(curve.nls))))
    },
    error = function(e) {
      coefs$err <<- e$message
    }
    )  # end of tryCatch
    
    coefs    
  }
  #  sidb_timeseries_clean %>% 
  #    group_by(citationKey, units) %>% 
  #    do(fit_q10_parameters(.))
  
  
  
  sidb_temps <-
    sidb_timeseries_clean %>% 
    group_by(citationKey) %>% 
    dplyr::summarise(temp_range_min = min(temperature),
                     temp_range_max = max(temperature))
  
  sidb_latlon = 
    sidb_vars %>% 
    dplyr::select(citationKey, latitude, longitude) %>% 
    group_by(citationKey) %>% 
    dplyr::summarise(Latitude = mean(latitude),
                     Longitude = mean(longitude))
  
  sidb_q10_calculated <-   
    sidb_timeseries_clean %>% 
    group_by(citationKey, units) %>% 
    do(fit_q10_parameters(.)) %>% 
    left_join(sidb_temps) %>% 
    mutate(r10 = a * exp(b * 10),
           r0 = a * exp(b * 0),
           r5 = a * exp(b * 5),
           r15 = a * exp(b * 15),
           r20 = a * exp(b * 20),
           r25 = a * exp(b * 25),
           r35 = a * exp(b * 35),
           r40 = a * exp(b * 45)) %>% 
    mutate(q10_5_15 = case_when(temp_range_min < 15 ~ (r5+r15)/r5),
           q10_15_25 = case_when(temp_range_min < 25 ~ (r15+r25)/r15),
           q10_25_35 = case_when(temp_range_min < 35 ~ (r25+r35)/r25),
           q10_35_40 = case_when(temp_range_min < 40 & temp_range_max > 35 ~ (r35+r40)/r35)) %>% 
    dplyr::select(citationKey, units, n, temp_range_min, temp_range_max, a, b, r10, starts_with("q10")) %>% 
    force()
  
  sidb_q10_clean <-
    sidb_q10_calculated %>% 
    ungroup() %>% 
    dplyr::select(citationKey, starts_with("Q10")) %>% 
    pivot_longer(-citationKey, values_to = "Q10", names_to = "Temp_range") %>% 
    drop_na() %>% 
    mutate(Temp_range = str_remove(Temp_range, "q10_"),
           Source = "SIDb",
           Incubation = "lab",
           Species = "CO2") %>% 
    left_join(sidb_latlon) %>% 
    rename(StudyName = citationKey)
  
#  sidb_studies = 
#    sidb_q10_clean1 %>% 
#    distinct(StudyName) %>% 
#    arrange(StudyName) %>% 
#    rownames_to_column("Study_ID") %>% 
  
  list(sidb_q10_calculated = sidb_q10_calculated,
       sidb_q10_clean = sidb_q10_clean)
}


#
# PART 3: Data from papers ------------------------------------------------
import_CH4_data_from_papers = function(filePaths_CH4){
  #field_data_CH4 <-
  read_csv(filePaths_CH4, col_types = cols(Temp_range = col_character(),
                                           Latitude = col_character(),
                                           Longitude = col_character(),
                                           Sample = col_character())) %>% 
    #bind_rows() %>% 
 #   mutate(
 #     # clean up f-ing latitude/longitude
 #     Latitude = str_replace(Latitude, " N", "N"),
 #     Latitude = str_replace(Latitude, " S", "S"),
 #     Longitude = str_replace(Longitude, " E", "E"),
 #     Longitude = str_replace(Longitude, " W", "W"),
 #     
 #     Latitude = str_replace(Latitude, " N", "N"),
 #     Latitude = str_replace(Latitude, " S", "S"),
 #     Longitude = str_replace(Longitude, " E", "E"),
 #     Longitude = str_replace(Longitude, " W", "W"),
 #     
 #     Latitude  = str_replace(Latitude, "′′", '"'),
 #     Longitude  = str_replace(Longitude, "′′", '"'),
 #     
 #     Latitude  = str_replace(Latitude, "″", '"'),
 #     Longitude  = str_replace(Longitude, "″", '"'),
 #     
 #     Latitude  = str_replace(Latitude, "′", "'"),
 #     Longitude  = str_replace(Longitude, "′", "'")
 #   ) %>% 
    filter_all(any_vars(!is.na(.)))
  
}

import_CO2_data_from_papers = function(filePaths_CO2){
  #field_data_CH4 <-
  read_csv(filePaths_CO2, col_types = cols(Temp_range = col_character(),
                                           Latitude = col_character(),
                                           Longitude = col_character(),
                                           Sample = col_character(),
                                           Temp_flag = col_character())) %>% 
    #bind_rows() %>% 
 #   mutate(
 #     # clean up f-ing latitude/longitude
 #     Latitude = str_replace(Latitude, " N", "N"),
 #     Latitude = str_replace(Latitude, " S", "S"),
 #     Longitude = str_replace(Longitude, " E", "E"),
 #     Longitude = str_replace(Longitude, " W", "W"),
 #     
 #     Latitude = str_replace(Latitude, " N", "N"),
 #     Latitude = str_replace(Latitude, " S", "S"),
 #     Longitude = str_replace(Longitude, " E", "E"),
 #     Longitude = str_replace(Longitude, " W", "W"),
 #     
 #     Latitude  = str_replace(Latitude, "′′", '"'),
 #     Longitude  = str_replace(Longitude, "′′", '"'),
 #     
 #     Latitude  = str_replace(Latitude, "″", '"'),
 #     Longitude  = str_replace(Longitude, "″", '"'),
 #     
 #     Latitude  = str_replace(Latitude, "′", "'"),
 #     Longitude  = str_replace(Longitude, "′", "'")
 #   ) %>% 
    filter_all(any_vars(!is.na(.)))
  
}


create_study_matrix = function(combined_data_cleaned){
  combined_data_cleaned %>% 
    dplyr::select(Species, StudyName, DOI) %>% 
    distinct() %>% 
    mutate(Species2 = Species) %>% 
    pivot_wider(names_from = "Species2", values_from = "Species") %>% 
    dplyr::select(StudyName, DOI, CO2, N2O, CH4)
  
}


#
# PART 4: cleaning --------------------------------------------------------

clean_lat_lon = function(all_data){
  all_data %>% 
    #dplyr::select(Latitude, Longitude) %>% 
    
    # first, clean up ----
  mutate(
    # clean up f-ing latitude/longitude
    Latitude = str_replace(Latitude, " N", "N"),
    Latitude = str_replace(Latitude, " S", "S"),
    Longitude = str_replace(Longitude, " E", "E"),
    Longitude = str_replace(Longitude, " W", "W"),
    
    Latitude = str_replace(Latitude, " N", "N"),
    Latitude = str_replace(Latitude, " S", "S"),
    Longitude = str_replace(Longitude, " E", "E"),
    Longitude = str_replace(Longitude, " W", "W"),
    
    Latitude  = str_replace(Latitude, "′′", '"'),
    Longitude  = str_replace(Longitude, "′′", '"'),
    
    Latitude  = str_replace(Latitude, "″", '"'),
    Longitude  = str_replace(Longitude, "″", '"'),
    
    Latitude  = str_replace(Latitude, "′", "'"),
    Longitude  = str_replace(Longitude, "′", "'")
    ) %>% 
    
    # LATITUDE ----
    # a. remove any spaces, so they don't fuck up the analysis later
    # assign negative values to S hemisphere
    # and then delete N/S from the Latitude column
    mutate(Latitude = str_remove(Latitude, " "),
           Latitude = if_else(grepl("S", Latitude), 
                              paste0("-", Latitude), 
                              Latitude),
           Latitude = str_remove(Latitude, "[A-Z]")) %>% 
    
    # b. check if the Latitude is already in decimal form
    # and separate into different columns
    # if the minutes symbol is present, then it needs to be converted
    mutate(latitude_dec = if_else(grepl("'", Latitude), 
                                  NA_character_,
                                  Latitude),
           latitude_deg = if_else(grepl("'", Latitude), 
                                  Latitude,
                                  NA_character_),
           # the latitude_dec may have some straggler _, remove
           latitude_dec = parse_number(latitude_dec)) %>% 
    
    # c. check the Latitude column for completeness of DD MM SS
    # if seconds are not reported, assume 00 sec
    mutate(latitude_deg = if_else(grepl('"', latitude_deg), 
                                  latitude_deg, 
                                  str_replace_all(latitude_deg, "'", "'00"))) %>% 
    
    # d. replace all degree, minute, second symbols with spaces
    mutate(
      latitude_deg = str_replace_all(latitude_deg, "_", " "),
      latitude_deg = str_replace_all(latitude_deg, "'", " "),
      latitude_deg = str_replace_all(latitude_deg, '"', " ")) %>% 
    
    # e. finally, do the conversion
    # then merge the latitude_dec and lat columns and format
    rowwise() %>% 
    mutate(lat = measurements::conv_unit(latitude_deg, 
                                         from = "deg_min_sec", 
                                         to = "dec_deg")) %>% 
    mutate(Latitude_corrected = case_when(!is.na(latitude_dec) ~ latitude_dec,
                                          !is.na(lat) ~ as.numeric(lat)),
           Latitude_corrected = round(Latitude_corrected, 3)) %>% 
    
    # LONGITUDE ----
  # a. remove any spaces, so they don't fuck up the analysis later
  # assign negative values to S hemisphere
  # and then delete N/S from the Longitude column
  mutate(Longitude = str_remove(Longitude, " "),
         Longitude = if_else(grepl("W", Longitude), 
                             paste0("-", Longitude), 
                             Longitude),
         Longitude = str_remove(Longitude, "[A-Z]")) %>% 
    
    # b. check if the Longitude is already in decimal form
    # and separate into different columns
    # if the minutes symbol is present, then it needs to be converted
    mutate(longitude_dec = if_else(grepl("'", Longitude), 
                                   NA_character_,
                                   Longitude),
           longitude_deg = if_else(grepl("'", Longitude), 
                                   Longitude,
                                   NA_character_),
           # the longitude_dec may have some straggler _, remove
           longitude_dec = parse_number(longitude_dec)) %>% 
    
    # c. check the Longitude column for completeness of DD MM SS
    # if seconds are not reported, assume 00 sec
    mutate(longitude_deg = if_else(grepl('"', longitude_deg), 
                                   longitude_deg, 
                                   str_replace_all(longitude_deg, "'", "'00"))) %>% 
    
    # d. replace all degree, minute, second symbols with spaces
    mutate(
      longitude_deg = str_replace_all(longitude_deg, "_", " "),
      longitude_deg = str_replace_all(longitude_deg, "'", " "),
      longitude_deg = str_replace_all(longitude_deg, '"', " ")) %>% 
    
    # e. finally, do the conversion
    # then merge the longitude_dec and lat columns and format
    rowwise() %>% 
    mutate(lon = measurements::conv_unit(longitude_deg, 
                                         from = "deg_min_sec", 
                                         to = "dec_deg")) %>% 
    mutate(Longitude_corrected = case_when(!is.na(longitude_dec) ~ longitude_dec,
                                           !is.na(lon) ~ as.numeric(lon)),
           Longitude_corrected = round(Longitude_corrected, 3)) %>% 
    
    dplyr::select(-c("Latitude", "Longitude", "latitude_dec", "latitude_deg", "lat",
                    "longitude_dec", "longitude_deg", "lon")) %>% 
    rename(Latitude = Latitude_corrected,
           Longitude = Longitude_corrected)
  
}

clean_temp_range2 <- function(combined_data){
  #defunct, don't use
  temp_ranges <- 
    combined_data %>% 
    dplyr::select(Temp_range) %>% 
    distinct() %>% 
    mutate(Temp_range2 = Temp_range) %>% 
    separate(Temp_range2, sep = "_", into = c("Temp_min", "Temp_max")) %>% 
    mutate(Temp_max = as.integer(Temp_max),
           Temp_min = as.integer(Temp_min),
           Temp_diff = Temp_max - Temp_min) %>% 
    filter(Temp_diff <= 10) %>% 
    mutate(Temp_range_new = 
             case_when(Temp_max <= 0 ~ "< 0",
                       Temp_min >= 0 & Temp_max <= 5 ~ "0_5",
                       Temp_min >= 5 & Temp_max <= 15 ~ "5_15",
                       Temp_min >= 15 & Temp_max <= 25 ~ "15_25",
                       Temp_min >= 25 ~ "> 25"))
  
  # 
    combined_data %>% 
    left_join(temp_ranges %>% dplyr::select(Temp_range, Temp_range_new)) %>% 
 #   mutate(Temp_range_new = if_else(is.na(Temp_range),
 #                                   case_when(Temp_mean < 0 ~ "< 0",
 #                                             Temp_mean >= 0 & Temp_mean < 5 ~ "0_5",
 #                                             Temp_mean >= 5 & Temp_mean < 15 ~ "5_15",
 #                                             Temp_mean >= 15 & Temp_mean <= 25 ~ "15_25",
 #                                             Temp_mean >= 25 ~ "> 25"),
 #                                   Temp_range_new)) %>% 
 #    filter(!is.na(Temp_range_new)) %>% 
 #    filter(!is.na(Q10)) %>% 
      rename(Temp_range_old = Temp_range,
             Temp_range = Temp_range_new) %>% 
      mutate(Temp_range = factor(Temp_range, 
                                 levels = c("< 0", "0_5", "5_15", "15_25", "> 25")))
}

clean_temp_range <- function(all_data){
  # new version
  temp_ranges <- 
    all_data %>% 
    dplyr::select(Temp_range) %>% 
    distinct() %>% 
    mutate(Temp_range2 = Temp_range) %>% 
    separate(Temp_range2, sep = "_", into = c("Temp_min", "Temp_max")) %>% 
    mutate(Temp_max = as.integer(Temp_max),
           Temp_min = as.integer(Temp_min),
           #Temp_diff = Temp_max - Temp_min
           ) %>% 
    mutate(Temp_min2 = round(Temp_min/5)*5,
           Temp_max2 = round(Temp_max/5)*5,
           Temp_range_rounded = if_else(!is.na(Temp_min2) & !is.na(Temp_max2), paste0(Temp_min2, "_", Temp_max2), NA_character_)) %>% 
    #separate(Temp_range_2, sep = "_", into = c("Temp_min", "Temp_max")) %>% 
    mutate(Temp_diff = Temp_max2 - Temp_min2) %>% 
    
    rownames_to_column("rownum") %>% 
  
  
  
#    filter(Temp_diff <= 10) %>% 
    mutate(Temp_range_new = 
             case_when(Temp_max2 <= 0 ~ "< 0",
                       Temp_min2 >= 0 & Temp_max2 <= 5 ~ "0_5",
                       Temp_min2 >= 5 & Temp_max2 <= 15 ~ "5_15",
                       Temp_min2 >= 15 & Temp_max2 <= 25 ~ "15_25",
                       Temp_min2 >= 25 ~ "> 25")) %>% 
    dplyr::select(Temp_range, Temp_range_rounded, Temp_range_new, Temp_diff)
  
  # 
  #x = 
  all_data %>% 
    left_join(temp_ranges) %>% 
    #   mutate(Temp_range_new = if_else(is.na(Temp_range),
    #                                   case_when(Temp_mean < 0 ~ "< 0",
    #                                             Temp_mean >= 0 & Temp_mean < 5 ~ "0_5",
    #                                             Temp_mean >= 5 & Temp_mean < 15 ~ "5_15",
    #                                             Temp_mean >= 15 & Temp_mean <= 25 ~ "15_25",
    #                                             Temp_mean >= 25 ~ "> 25"),
    #                                   Temp_range_new)) %>% 
    #    filter(!is.na(Temp_range_new)) %>% 
    #    filter(!is.na(Q10)) %>% 
    rename(Temp_range_old = Temp_range,
           Temp_range = Temp_range_new) %>% 
    mutate(Temp_range = factor(Temp_range, 
                               levels = c("< 0", "0_5", "5_15", "15_25", "> 25")))
}


combine_all_q10_studies = function(combined_data, srdb_q10, sidb_q10_clean){
  bind_rows(combined_data, srdb_q10, sidb_q10_clean) %>% 
    mutate_all(na_if,"") %>% 
    filter(is.na(Duplicate_record)) %>% 
    filter(Incubation %in% c("lab", "field")) %>% 
    mutate(Manipulation = tolower(Manipulation),
           Manipulation_level = tolower(Manipulation_level)) %>%
    filter(Manipulation_level %in% c("none", "control", NA)) %>% 
    filter(Manipulation %in% c("control", "none", NA)) %>% 
    mutate(Species = factor(Species, levels = c("CO2", "N2O", "CH4"))) %>% 
    dplyr::select(Species, 
                  starts_with("Temp"),
                  starts_with("Q10"),
                  Sample, Incubation, Latitude, Longitude, Soil_drainage, RC_annual,
                  Ecosystem_type, Meas_method,
                  Source, StudyName, DOI,
                  starts_with("notes"), Respiration_type,
                  SRDB_record_number, SRDB_study_number) %>% 
    mutate(Q10 = as.numeric(Q10),
           Q10 = round(Q10, 2),
           Ecosystem_type = tools::toTitleCase(Ecosystem_type))
}




# setting study-ID and study-number
assign_study_numbers = function(all_data){
  all_data1 = 
    all_data %>% 
    arrange(Species, Incubation, StudyName) %>% 
    rownames_to_column("Q10_record_number")
  
  studies =   
    all_data1 %>% 
    dplyr::select(Q10_record_number, Species, Incubation, StudyName, DOI) %>% 
    arrange(Q10_record_number) %>% 
    distinct(StudyName, DOI) %>% 
    rownames_to_column("Q10_study_ID") %>% 
    mutate(#Q10_study_ID = str_pad(Q10_study_ID, 3, pad = "0"),
      Q10_study_ID = as.numeric(Q10_study_ID),
      DOI = as.character(DOI))
  
  all_data1 %>% 
    left_join(studies)
  
}

subset_combined_dataset = function(dat){
  # subset only data needed
  study_data = 
    dat %>% 
    dplyr::select(Q10_study_ID, SRDB_record_number, Source, StudyName, DOI) %>% 
    distinct() %>% 
    mutate(Q10_study_ID = as.numeric(Q10_study_ID)) %>% 
    arrange(Q10_study_ID)
  # subset study info
  
  data_Q10 = 
    dat %>% 
    dplyr::select(Q10_record_number, 
                  Q10_study_ID,
                  Species,
                  starts_with("Q10"),
                  Incubation,
                  starts_with("Temp"),
                  Respiration_type,
                  notes) %>% 
    mutate(Respiration_type = if_else(Incubation == "lab", "heterotrophic", Respiration_type))
  
  sample_metadata = 
    dat %>% 
    dplyr::select(Q10_record_number, Q10_study_ID, 
                  Latitude, Longitude, Soil_drainage, RC_annual,
                  Ecosystem_type, Meas_method,
                  SRDB_record_number, Source, StudyName, DOI) %>% 
    mutate(Q10_study_ID = as.numeric(Q10_study_ID),
           Q10_record_number = as.numeric(Q10_record_number)) %>% 
    arrange(Q10_record_number)
  
  list(study_data = study_data,
       data_Q10 = data_Q10,
       sample_metadata = sample_metadata)
}

#


assign_climate_biome = function(dat){
  
  UDel_summarized_climate = read.csv("data/geographic_databases/UDel_summarized_climate.csv")
  KoeppenGeigerASCII = readxl::read_xlsx("data/geographic_databases/KoeppenGeigerASCII.xlsx")
  
  dat_mat_map = 
    dat %>% 
    mutate(Latitude2 = round(Latitude*2)/2,
           Longitude2 = round(Longitude*2)/2,
           Lat_dif = ifelse(Latitude2 - Latitude >=0, 0.25, -0.25),
           Lon_dif = ifelse(Longitude2 - Longitude >=0, 0.25, -0.25),
           Latitude2 = Latitude2 - Lat_dif,
           Longitude2 = Longitude2 - Lon_dif) %>% 
    dplyr::select(-Lat_dif, -Lon_dif) %>% 
    left_join(UDel_summarized_climate, by=c("Latitude2"="Latitude", "Longitude2"="Longitude")) %>% 
    left_join(KoeppenGeigerASCII, by=c("Latitude2"="Latitude", "Longitude2"="Longitude")) %>% 
    mutate(ClimateTypes = case_when(grepl("A", ClimateTypes) ~ "equatorial",
                                      grepl("B", ClimateTypes) ~ "arid",
                                      grepl("C", ClimateTypes) ~ "temperate",
                                      grepl("D", ClimateTypes) ~ "snow",
                                      grepl("E", ClimateTypes) ~ "polar")) %>% 
    dplyr::select(-Latitude2, -Longitude2)
    
  
}



#


##################
##################
##################

# COMBINE DATASETS --------------------------------------------------------

combine_all_q10_studies_OLD <- function(indiv_studies, srdb_q10, sidb_q10_clean){
  # combine the available data ----
  q10_combined <- 
    indiv_studies %>% 
    dplyr::select(Source, Incubation, Latitude, Longitude, Temp_range, Temp_mean, Q10) %>% 
    bind_rows(srdb_q10 %>% 
                mutate(Source = "SRDB", Incubation = "field") %>% 
                rename(Temp_range = temp_range)) %>% 
    bind_rows(sidb_q10_clean)
  
  # clean the temperature ranges ----
  temp_ranges <- 
    q10_combined %>% 
    dplyr::select(Temp_range) %>% 
    distinct() %>% 
    mutate(Temp_range2 = Temp_range) %>% 
    separate(Temp_range2, sep = "_", into = c("Temp_min", "Temp_max")) %>% 
    mutate(Temp_max = as.integer(Temp_max),
           Temp_min = as.integer(Temp_min),
           Temp_diff = Temp_max - Temp_min) %>% 
    filter(Temp_diff <= 10) %>% 
    mutate(Temp_range_new = 
             case_when(Temp_max <= 0 ~ "< 0",
                       Temp_min >= 0 & Temp_max <= 5 ~ "0_5",
                       Temp_min >= 5 & Temp_max <= 15 ~ "5_15",
                       Temp_min >= 15 & Temp_max <= 25 ~ "15_25",
                       Temp_min >= 25 ~ "> 25"))
  
  # 
  q10_combined_temp <- 
    q10_combined %>% 
    left_join(temp_ranges %>% dplyr::select(Temp_range, Temp_range_new)) %>% 
    mutate(Temp_range_new = if_else(is.na(Temp_range),
                                    case_when(Temp_mean < 0 ~ "< 0",
                                              Temp_mean >= 0 & Temp_mean < 5 ~ "0_5",
                                              Temp_mean >= 5 & Temp_mean < 15 ~ "5_15",
                                              Temp_mean >= 15 & Temp_mean <= 25 ~ "15_25",
                                              Temp_mean >= 25 ~ "> 25"),
                                    Temp_range_new)) %>% 
    filter(!is.na(Temp_range_new)) %>% 
    filter(!is.na(Q10)) %>% 
    mutate(Temp_range_new = factor(Temp_range_new, levels = c("< 0", "0_5", "5_15", "15_25", "> 25")))
  
  q10_combined_temp
}

import_individual_studies_OLD <- function(){
  
  filePaths_field <- list.files(path = "data/CO2/cleaned_for_analysis/field",pattern = "*.csv", full.names = TRUE)
  filePaths_lab <- list.files(path = "data/CO2/cleaned_for_analysis/lab",pattern = "*.csv", full.names = TRUE)
  
  field_data <-
    lapply(filePaths_field, read.csv, stringsAsFactors = FALSE) %>% 
    bind_rows() %>% 
    mutate(Incubation = "field", Depth_cm = as.character(Depth_cm))
  
  lab_data <-
    lapply(filePaths_lab, read.csv, stringsAsFactors = FALSE) %>% 
    bind_rows() %>% 
    mutate(Incubation = "lab", Depth_cm = as.character(Depth_cm))
  
  
  combined <- 
    bind_rows(lab_data, field_data) %>% 
    dplyr::select(Source, Incubation, Record_number, Duplicate_record,
                  Site_name, Latitude_deg, Longitude_deg, Latitude, Longitude,
                  Species, Soil_type, Ecosystem_type, 
                  Temp_range, Temp_mean, Temp_min, Temp_max, Q10, R10) %>% 
    na_if("")
  
  fix_temp_and_latlon <- function(combined){
    # clean temperature ranges ----
    combined2 <- 
      combined %>% 
      mutate(Temp_min = round(Temp_min/5)*5,
             Temp_max = round(Temp_max/5)*5,
             Temp_range = if_else(!is.na(Temp_min), paste0(Temp_min, "_", Temp_max), Temp_range),
             Temp_range2 = Temp_range) %>% 
      separate(Temp_range2, sep = "_", into = c("Temp_min", "Temp_max")) %>% 
      mutate(Temp_max = as.integer(Temp_max),
             Temp_min = as.integer(Temp_min),
             Temp_diff = Temp_max - Temp_min) %>% 
      rownames_to_column("rownum")
    
    # fix latitude/longitude ----
    # subset only the columns needed, we will re-join later
    combined2_latlong <- 
      combined2 %>% 
      dplyr::select(rownum, Latitude, Longitude, Latitude_deg, Longitude_deg) %>% 
      # create a separate column for hemisphere
      # we will use this later to assign negative values for S and W
      mutate(hemisphere = paste0(str_extract(Latitude_deg, "[A-Z]"), str_extract(Longitude_deg, "[A-Z]")),
             hemisphere = str_remove_all(hemisphere, "NA")) %>% 
      # remove all letters, remove unnecessary characters like _ and space
      mutate(latitude_deg = str_remove(Latitude_deg, "N"),
             latitude_deg = str_remove(latitude_deg, "S"),
             latitude_deg = str_remove(latitude_deg, " "),
             latitude_deg = str_replace_all(latitude_deg, "_", " "),
             # some values are already as decimals. we need to extract those
             # decimal format is without ', so use that to extract
             # create a new column for decimal value data (no ')
             # then delete those values from the latitude_deg column
             lat_dec = if_else(grepl("'", latitude_deg), NA_character_, latitude_deg),
             latitude_deg = if_else(grepl("'", latitude_deg), latitude_deg, NA_character_)) %>% 
      # replace ' and " with spaces, for the unit conversion function
      # you NEED to run conv_unit() rowwise, because it treats the entire column as a vector,
      # and numbers may carry over into the next row
      # we want to avoid this
      rowwise() %>% 
      mutate(
        latitude_deg = str_replace_all(latitude_deg, "'", " "),
        latitude_deg = str_replace_all(latitude_deg, '"', " "),
        lat_dec2 = measurements::conv_unit(latitude_deg, from = "deg_min_sec", to = "dec_deg")) %>% 
      # next, combine lat_dec and lat_dec2
      mutate(Latitude_dec = case_when(!is.na(lat_dec) ~ lat_dec,
                                      !is.na(lat_dec2) ~ lat_dec2),
             Latitude_dec = round(as.numeric(Latitude_dec), 3),
             # finally, assign - values for Southern hemisphere       
             Latitude_dec = case_when(grepl("N", hemisphere) ~ Latitude_dec,
                                      grepl("S", hemisphere) ~ Latitude_dec * -1)) %>% 
      # NOW, repeat for longitude
      #
      # remove all letters, remove unnecessary characters like _ and space
      mutate(longitude_deg = str_remove(Longitude_deg, "E"),
             longitude_deg = str_remove(longitude_deg, "W"),
             longitude_deg = str_remove(longitude_deg, " "),
             longitude_deg = str_replace_all(longitude_deg, "_", " "),
             # some values are already as decimals. we need to extract those
             # decimal format is without ', so use that to extract
             # create a new column for decimal value data (no ')
             # then delete those values from the longitude_deg column
             lon_dec = if_else(grepl("'", longitude_deg), NA_character_, longitude_deg),
             longitude_deg = if_else(grepl("'", longitude_deg), longitude_deg, NA_character_)) %>% 
      # replace ' and " with spaces, for the unit conversion function
      # you NEED to run conv_unit() rowwise, because it treats the entire column as a vector,
      # and numbers may carry over into the next row
      # we want to avoid this
      rowwise() %>% 
      mutate(
        longitude_deg = str_replace_all(longitude_deg, "'", " "),
        longitude_deg = str_replace_all(longitude_deg, '"', " "),
        lon_dec2 = measurements::conv_unit(longitude_deg, from = "deg_min_sec", to = "dec_deg")) %>% 
      # next, combine lat_dec and lat_dec2
      mutate(Longitude_dec = case_when(!is.na(lon_dec) ~ lon_dec,
                                       !is.na(lon_dec2) ~ lon_dec2),
             Longitude_dec = round(as.numeric(Longitude_dec), 3),
             # finally, assign - values for Southern hemisphere       
             Longitude_dec = case_when(grepl("E", hemisphere) ~ Longitude_dec,
                                       grepl("W", hemisphere) ~ Longitude_dec * -1)) %>% 
      # then, combine the lat and lon columns
      mutate(Latitude = case_when(!is.na(Latitude) ~ Latitude,
                                  !is.na(Latitude_dec) ~ Latitude_dec),
             Longitude = case_when(!is.na(Longitude) ~ Longitude,
                                   !is.na(Longitude_dec) ~ Longitude_dec)) %>% 
      # finally, subset the necessary columns and then join with the dataset
      dplyr::select(rownum, Latitude, Longitude)
    
    # combine ----
    combined2 %>% 
      dplyr::select(-Latitude, -Longitude, -Latitude_deg, -Longitude_deg) %>% 
      left_join(combined2_latlong)
  }
  fix_temp_and_latlon(combined)
}
import_individual_studies_OLD2 <- function(){
  
  filePaths_papers <- list.files(path = "data/data_from_papers",pattern = "*.csv", full.names = TRUE)
  
  combined <-
    lapply(filePaths_papers, 
           read_csv, col_types = cols(moisture = col_character())) %>% 
    bind_rows()
  
  fix_temp_and_latlon <- function(combined){
    # clean temperature ranges ----
    combined2 <- 
      combined %>% 
      mutate(#Temp_min = round(Temp_min/5)*5,
        #Temp_max = round(Temp_max/5)*5,
        #Temp_range = if_else(!is.na(Temp_min), paste0(Temp_min, "_", Temp_max), Temp_range),
        Temp_range2 = Temp_range) %>% 
      separate(Temp_range2, sep = "_", into = c("Temp_min", "Temp_max")) %>% 
      mutate(Temp_max = as.integer(Temp_max),
             Temp_min = as.integer(Temp_min),
             Temp_diff = Temp_max - Temp_min) %>% 
      rownames_to_column("rownum")
    
    # fix latitude/longitude ----
    # subset only the columns needed, we will re-join later
    combined2_latlong <- 
      combined2 %>% 
      dplyr::select(rownum, Latitude, Longitude) %>% 
      # create a separate column for hemisphere
      # we will use this later to assign negative values for S and W
      mutate(hemisphere = paste0(str_extract(Latitude, "[A-Z]"), str_extract(Longitude, "[A-Z]")),
             hemisphere = str_remove_all(hemisphere, "NA")) %>% 
      # remove all letters, remove unnecessary characters like _ and space
      mutate(latitude_deg = str_remove(Latitude, "N"),
             latitude_deg = str_remove(latitude_deg, "S"),
             latitude_deg = str_remove(latitude_deg, " "),
             latitude_deg = str_replace_all(latitude_deg, "_", " "),
             # some values are already as decimals. we need to extract those
             # decimal format is without ', so use that to extract
             # create a new column for decimal value data (no ')
             # then delete those values from the latitude_deg column
             lat_dec = if_else(grepl("'", latitude_deg), NA_character_, latitude_deg),
             latitude_deg = if_else(grepl("'", latitude_deg), latitude_deg, NA_character_)) %>% 
      # replace ' and " with spaces, for the unit conversion function
      # you NEED to run conv_unit() rowwise, because it treats the entire column as a vector,
      # and numbers may carry over into the next row
      # we want to avoid this
      rowwise() %>% 
      mutate(
        latitude_deg = str_replace_all(latitude_deg, "'", " "),
        latitude_deg = str_replace_all(latitude_deg, '"', " "),
        lat_dec2 = measurements::conv_unit(latitude_deg, from = "deg_min_sec", to = "dec_deg")) %>% 
      # next, combine lat_dec and lat_dec2
      mutate(Latitude_dec = case_when(!is.na(lat_dec) ~ lat_dec,
                                      !is.na(lat_dec2) ~ lat_dec2),
             Latitude_dec = round(as.numeric(Latitude_dec), 3),
             # finally, assign - values for Southern hemisphere       
             Latitude_dec = case_when(grepl("N", hemisphere) ~ Latitude_dec,
                                      grepl("S", hemisphere) ~ Latitude_dec * -1)) %>% 
      # NOW, repeat for longitude
      #
      # remove all letters, remove unnecessary characters like _ and space
      mutate(longitude_deg = str_remove(Longitude, "E"),
             longitude_deg = str_remove(longitude_deg, "W"),
             longitude_deg = str_remove(longitude_deg, " "),
             longitude_deg = str_replace_all(longitude_deg, "_", " "),
             # some values are already as decimals. we need to extract those
             # decimal format is without ', so use that to extract
             # create a new column for decimal value data (no ')
             # then delete those values from the longitude_deg column
             lon_dec = if_else(grepl("'", longitude_deg), NA_character_, longitude_deg),
             longitude_deg = if_else(grepl("'", longitude_deg), longitude_deg, NA_character_)) %>% 
      # replace ' and " with spaces, for the unit conversion function
      # you NEED to run conv_unit() rowwise, because it treats the entire column as a vector,
      # and numbers may carry over into the next row
      # we want to avoid this
      rowwise() %>% 
      mutate(
        longitude_deg = str_replace_all(longitude_deg, "'", " "),
        longitude_deg = str_replace_all(longitude_deg, '"', " "),
        lon_dec2 = measurements::conv_unit(longitude_deg, from = "deg_min_sec", to = "dec_deg")) %>% 
      # next, combine lat_dec and lat_dec2
      mutate(Longitude_dec = case_when(!is.na(lon_dec) ~ lon_dec,
                                       !is.na(lon_dec2) ~ lon_dec2),
             Longitude_dec = round(as.numeric(Longitude_dec), 3),
             # finally, assign - values for Southern hemisphere       
             Longitude_dec = case_when(grepl("E", hemisphere) ~ Longitude_dec,
                                       grepl("W", hemisphere) ~ Longitude_dec * -1),
             #Longitude_dec = as.character(Longitude_dec),
             #Latitude_dec = as.character(Latitude_dec)
      ) %>% 
      # then, combine the lat and lon columns
      ##    mutate(Latitude2 = case_when(!is.na(Latitude) ~ Latitude,
      ##                                !is.na(Latitude_dec) ~ Latitude_dec),
      ##           Longitude2 = case_when(!is.na(Longitude) ~ Longitude,
      ##                                 !is.na(Longitude_dec) ~ Longitude_dec)) %>% 
      # finally, subset the necessary columns and then join with the dataset
      dplyr::select(rownum, Latitude_dec, Longitude_dec) %>% 
      rename(Latitude = Latitude_dec,
             Longitude = Longitude_dec)
    
    # combine ----
    combined2 %>% 
      dplyr::select(-starts_with("Latitude"), -starts_with("Longitude")) %>% 
      left_join(combined2_latlong)
  }
  fix_temp_and_latlon(combined)
}
