# FIELD VS. LAB GHG Q10
# PATEL ET AL. 2022
# 
# This script contains the {drake} plan to analyze the processed Q10 data.
# Output is in the corresponding Rmd/md files.
# 
# kfp 2022
# 
# ############################# #


source("code/0-packages.R")
source("code/3-functions-analysis.R")

# set palettes
pal_biome = PNWColors::pnw_palette("Sailboat", 5)
pal_incubation = soilpalettes::soil_palette("redox2", 3)


analysis_plan = drake_plan(
  
  # import processed Q10 data ----
  Q10 = read.csv("data/processed/Q10_data.csv", na.strings = "") %>% filter(!is.na(Q10)),
  sample_metadata = read.csv("data/processed/Q10_sample_metadata.csv", na.strings = ""),
  Q10_data = left_join(Q10, 
                       sample_metadata %>% 
                         dplyr::select(
                         Q10_record_number, Latitude, Longitude, MAT, MAP, 
                         ClimateTypes, Soil_drainage, RC_annual, Ecosystem_type, Meas_method)) %>% 
    reorder_species_levels(.) %>% reorder_temp_levels(.) %>% reorder_biome_levels(.),
  
  # exploration ----
  
  Q10_map = make_map_all_studies(Q10_data),
  study_summary = compute_study_summary(Q10_data),
  gg_temp_ranges = plot_temperature_ranges(Q10_data),
  gg_temp_ranges_q10 = plot_q10_by_temp_ranges(Q10_data),
  gg_temp_ranges_field_lab = plot_temp_ranges_field_vs_lab(Q10_data),
  gg_mat_map = plot_mat_map(Q10_data),
  
  #
  #summary tables ----
  summary_tables = co2_all_summaries(Q10_data),
  
  #
  # analysis - CO2 ----
  co2_all = compute_co2_all(Q10_data),
  co2_temp_range = compute_co2_temp_range(Q10_data),
  co2_biome = compute_co2_biome(Q10_data),
  co2_ecosystem = compute_co2_ecosystem(Q10_data),
  co2_measurement = compute_co2_measurement(Q10_data),
  
  co2_bootstrapping = compute_co2_bootstrapping(Q10_data),
  Q10_rh_only = rh_analysis(Q10_data),

  
  # analysis - CH4 ----
  ch4_all = compute_ch4(Q10_data),

  #
  # reports ----
  report = rmarkdown::render(
    knitr_in("reports/2-data_analysis.Rmd"),
    output_format = rmarkdown::github_document()), quiet = T,
)

#
# MAKE/RUN THE PLAN -------------------------------------------------------
make(analysis_plan, lock_cache = FALSE)
