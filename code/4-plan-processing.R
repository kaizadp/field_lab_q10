source("code/0-packages.R")
source("code/1-googlesheets.R")
source("code/2-functions-data_processing")


# SET PATHS ---------------------------------------------------------------
GSHEETS_LOCAL_PATH_N = "data/2-data_from_papers-GoogleDrive/N_data.csv"
GSHEETS_LOCAL_PATH_CH4 = "data/2-data_from_papers-GoogleDrive/CH4_data.csv"


#
# PROCESSING PLAN ---------------------------------------------------------
processing_plan = drake_plan(
  # I. STUDIES FROM PAPERS ---- 
  # GoogleSheets: save locally as csv
  # this is now done outside drake, to avoid fuck ups
  #  gsheets_data_N %>% write.csv(GSHEETS_LOCAL_PATH_N, row.names = FALSE, na = ""),
  #  gsheets_data_CH4 %>% write.csv(GSHEETS_LOCAL_PATH_CH4, row.names = FALSE, na = ""),
  
  # Process data from papers
  N_data_from_papers = import_N_data_from_papers(GSHEETS_LOCAL_PATH_N),
  CH4_data_from_papers = import_CH4_data_from_papers(GSHEETS_LOCAL_PATH_CH4),
  
  # export these data
  ## perhaps delete this later (?)
  ## write.csv(N_data_from_papers, "data/3-data_from_papers-cleaned/N_data.csv", row.names = FALSE, na = ""),
  ## write.csv(CH4_data_from_papers, "data/3-data_from_papers-cleaned/CH4_data.csv", row.names = FALSE, na = ""),
  
  # combine the data
  combined_data = bind_rows(N_data_from_papers, CH4_data_from_papers) %>% 
    filter(Species %in% c("CO2", "N2O", "CH4")),
  
  # fix the latitude-longitude
  combined_data_cleaned = 
    combined_data %>% 
    clean_lat_lon(.) %>% 
    clean_temp_range(.),
  
  #
  # II. SRDB ----
  srdb_q10 = clean_srdb_dataset()$q10_sites %>%  clean_temp_range(.),
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
  
  # sidb_vars_clean2 <- clean_sidb_data(sidb_vars, sidb_timeseries)$sidb_vars_clean,
  sidb_timeseries_clean = clean_sidb_data(sidb_vars, sidb_timeseries)$sidb_timeseries_clean2,
  sidb_q10_calculated = calculate_sidb_q10_r10(sidb_timeseries_clean, sidb_vars)$sidb_q10_calculated,
  sidb_q10_clean = calculate_sidb_q10_r10(sidb_timeseries_clean, sidb_vars)$sidb_q10_clean %>% 
    clean_temp_range(.),
  
  #
  # IV. COMBINE ----
  #all_data = bind_rows(combined_data_cleaned, srdb_q10, sidb_q10_clean),
  all_data = combine_all_q10_studies(combined_data_cleaned, srdb_q10, sidb_q10_clean),
  all_data_study_numbers = assign_study_numbers(all_data),
  
  processed_data = subset_combined_dataset(all_data_study_numbers)$data_subset,
  study_metadata = subset_combined_dataset(all_data_study_numbers)$study_data,
  
  processed_data %>% write.csv("data/processed/Q10_data.csv", row.names = F, na = ""),
  study_metadata %>% write.csv("data/processed/Q10_study_metadata.csv", row.names = F, na = "")
  )


make(processing_plan)

loadd(N_data_from_papers, CH4_data_from_papers, combined_data, combined_data_cleaned)
loadd(srdb_q10, sidb_q10_clean)
loadd(all_data)
loadd(all_data_study_numbers, processed_data, study_metadata)
