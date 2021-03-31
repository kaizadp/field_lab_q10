library(drake)
source("code/1-initial_processing.R")

q10_plan = 
  drake_plan(
  
  srdb_q10 = clean_srdb_dataset()$q10_sites,
  srdb_r10 = clean_srdb_dataset()$r10_sites,
  
  indiv_studies =  import_individual_studies(),
  combined_q10 = combine_all_q10_studies(indiv_studies, srdb_q10),
  
  gg_map = make_map_all_studies(combined_q10),
  
  gg_q10 = make_graphs_q10(combined_q10),
  
  report = rmarkdown::render(
    knitr_in("reports/1-data_exploration.Rmd"),
    output_format = rmarkdown::github_document()), quiet = T,
  
  report2 = rmarkdown::render(
    knitr_in("reports/2-data_analysis.Rmd"),
    output_format = rmarkdown::github_document()), quiet = T
)


make(q10_plan)





