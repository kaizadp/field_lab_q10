library(tidyverse)


devtools::install_github("bpbond/cosore")

library(cosore)

db_info <- csr_database()
tibble::glimpse(db_info)


varner <- csr_dataset("d20190415_VARNER")
sr <- varner$data


ggplot(sr, aes(CSR_TIMESTAMP_BEGIN, CSR_FLUX_CO2, color = CSR_PORT)) +
  geom_point(size = 0.5, alpha = 0.25) +
  coord_cartesian(ylim = c(0, 20))

tibble::glimpse(varner$ports)

doi <- varner$description$CSR_PRIMARY_PUB
print(doi)



dbf_datasets <- subset(db_info, CSR_IGBP == "Deciduous broadleaf forest")$CSR_DATASET
tdf <- csr_table("description", dbf_datasets)

tdf_dat <- csr_table("data", dbf_datasets, quiet = TRUE)

ggplot(tdf_dat, aes(CSR_TIMESTAMP_BEGIN, CSR_FLUX_CO2, color = CSR_DATASET)) + 
  geom_point(size = 0.5, alpha = 0.25) + 
  scale_color_discrete(guide = FALSE) +
  coord_cartesian(ylim = c(0, 20))
