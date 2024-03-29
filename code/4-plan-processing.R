# FIELD VS. LAB GHG Q10
# PATEL ET AL. 2022
# 
# This script contains the {drake} plan to import, process, combine, and clean 
# the Q10 data from the different sources
# 
# kfp 2022
# 
# ############################# #


# SET PATHS ---------------------------------------------------------------
GSHEETS_LOCAL_PATH_CH4 = "data/2-data_from_papers-GoogleDrive/CH4_data.csv"
GSHEETS_LOCAL_PATH_CO2 = "data/2-data_from_papers-GoogleDrive/CO2_data.csv"

#
# SOURCE SCRIPTS ----------------------------------------------------------
source("code/0-packages.R")
source("code/1-googlesheets.R")
source("code/2-functions-data_processing.R")

#
# PROCESSING PLAN ---------------------------------------------------------
processing_plan = drake_plan(
  # I. STUDIES FROM PAPERS ---- 
  # GoogleSheets: save locally as csv
  # this is now done outside drake, to avoid fuck ups

  # Process data from papers
  CH4_data_from_papers = import_CH4_data_from_papers(GSHEETS_LOCAL_PATH_CH4) %>% mutate_all(as.character),
  CO2_data_from_papers = import_CO2_data_from_papers(GSHEETS_LOCAL_PATH_CO2) %>% mutate_all(as.character),
  
  # combine the data
  combined_data = bind_rows(CH4_data_from_papers, CO2_data_from_papers) %>% 
    filter(Species %in% c("CO2", "CH4")),
  
  #
  # II. SRDB ----
  srdb_q10 = clean_srdb_dataset()$q10_sites %>% mutate_all(as.character), #%>%  clean_temp_range(.),
  srdb_r10 = clean_srdb_dataset()$r10_sites,

  #
  # III. SIDb ----
  load(SIDB_FILE_PATH),
  sidb_flat = flatterSIDb(sidb),
  sidb_timeseries = dplyr::bind_rows(sidb_flat$timeseries),
  sidb_vars = rbindlist(sidb_flat$vars, fill = TRUE),
  # use the `sidb` package to load the data and then
  # flatten the large list into a list with two nested lists 
  
  # convert the two lists into dataframes
  # vars needs data.table::rbindlist because it is a different format 
  
  sidb_timeseries_clean = clean_sidb_data(sidb_vars, sidb_timeseries)$sidb_timeseries_clean2,
  sidb_q10_calculated = calculate_sidb_q10_r10(sidb_timeseries_clean, sidb_vars)$sidb_q10_calculated,
  sidb_q10_clean = calculate_sidb_q10_r10(sidb_timeseries_clean, sidb_vars)$sidb_q10_clean %>% mutate_all(as.character), 

  #
  # IV. COMBINE ----
  all_data = combine_all_q10_studies(combined_data, srdb_q10, sidb_q10_clean),
  all_data_clean = all_data %>% clean_lat_lon(.) %>% clean_temp_range(.),
  all_data_study_numbers = assign_study_numbers(all_data_clean),
  
  processed_data_q10 = subset_combined_dataset(all_data_study_numbers)$data_Q10,
  study_metadata = subset_combined_dataset(all_data_study_numbers)$study_data,
  sample_metadata = subset_combined_dataset(all_data_study_numbers)$sample_metadata,

  sample_metadata_climate_biome = assign_climate_biome(sample_metadata),
  
  processed_data_q10 %>% write.csv("data/processed/Q10_data.csv", row.names = F, na = ""),
  study_metadata %>% write.csv("data/processed/Q10_study_metadata.csv", row.names = F, na = ""),
  sample_metadata_climate_biome %>% write.csv("data/processed/Q10_sample_metadata.csv", row.names = F, na = "")
  )

#
# MAKE/RUN THE PLAN -------------------------------------------------------
make(processing_plan, lock_cache = FALSE)
