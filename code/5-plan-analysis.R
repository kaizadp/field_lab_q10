source("code/0-packages.R")
source("code/3-functions-analysis.R")
source("code/4-functions-q10_fitting.R")


analysis_plan = drake_plan(
  #Q10 from papers ----
  Q10_data = read.csv("data/processed/Q10_data.csv", na.strings = "") %>% 
    reorder_temp_levels(.) %>% 
    mutate(Ecosystem_type = tolower(Ecosystem_type),
           Biome = tolower(Biome)),
  
  Q10_map = make_map_all_studies(Q10_data),
  Q10_stats = compute_stats_q10(Q10_data),
  Q10_graphs = make_graphs_q10(Q10_data),
  Q10_rh_only = compute_rh_only(Q10_data),
  
  study_summary = compute_study_summary(Q10_data),
  
  #
  # fit Q10 ----
  
  q10_calculated = compute_q10(sidb_timeseries_clean),
  gg_q10_calculated = plot_q10_functions(q10_calculated),
  
  #
  # Reports ----
  report = rmarkdown::render(
    knitr_in("reports/2-data_analysis.Rmd"),
    output_format = rmarkdown::github_document()), quiet = T,
)


make(analysis_plan, lock_cache = FALSE)


x = 
  Q10_data %>% 
  distinct(Q10_study_ID, Year, Species) %>% 
  group_by(Species, Year) %>% 
  dplyr::summarise(n = n()) 

x %>% 
  drop_na() %>% 
  ggplot(aes(x = Year, y = n, color = Species))+
  geom_path()


