library(drake)
source("code/1-initial_processing.R")

q10_plan = 
  drake_plan(
  
  srdb_q10 = clean_srdb_dataset()$q10_sites,
  srdb_r10 = clean_srdb_dataset()$r10_sites,
  
  report = rmarkdown::render(
    knitr_in("reports/1-data_exploration.Rmd"),
    output_format = rmarkdown::github_document()), quiet = T
)


make(q10_plan)





