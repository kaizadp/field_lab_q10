source("code/0-packages.R")
source("code/3-functions-analysis")


analysis_plan = drake_plan(
  Q10_data = read.csv("data/processed/Q10_data.csv", na.strings = "") %>% 
    reorder_temp_levels(.),
  
  Q10_map = make_map_all_studies(Q10_data),
  Q10_stats = compute_stats_q10(Q10_data),
  Q10_graphs = make_graphs_q10(Q10_data),
  
  study_summary = compute_study_summary(Q10_data),
  
  # Reports ----
  report = rmarkdown::render(
    knitr_in("reports/2-data_analysis.Rmd"),
    output_format = rmarkdown::github_document()), quiet = T,
)


make(analysis_plan)
