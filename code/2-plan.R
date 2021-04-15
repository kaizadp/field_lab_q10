source("code/0-packages.R")
source("code/1-initial_processing.R")

q10_plan = 
  drake_plan(
  
  # Process datasets ----
  # SRDB
  srdb_q10 = clean_srdb_dataset()$q10_sites,
  srdb_r10 = clean_srdb_dataset()$r10_sites,
  
  # data pulled from papers
  indiv_studies =  import_individual_studies(),
  
  # SIDb
  load("data/sidb.RData"),
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
  sidb_q10_clean = calculate_sidb_q10_r10(sidb_timeseries_clean, sidb_vars)$sidb_q10_clean,
  
  # Combine datasets ----
  
  combined_q10 = combine_all_q10_studies(indiv_studies, srdb_q10, sidb_q10_clean),
  
  # Analysis ----
  
  gg_map = make_map_all_studies(combined_q10),
  
  gg_q10 = make_graphs_q10(combined_q10),
  
  # Reports ----
  report = rmarkdown::render(
    knitr_in("reports/1-data_exploration.Rmd"),
    output_format = rmarkdown::github_document()), quiet = T,
  
  report2 = rmarkdown::render(
    knitr_in("reports/2-data_analysis.Rmd"),
    output_format = rmarkdown::github_document())

)


make(q10_plan)
