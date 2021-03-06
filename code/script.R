library(tidyverse)

# import N data from individual files ----
filePaths_N <- list.files(path = "data/x-data/x-N",pattern = "*.csv", full.names = TRUE)

field_data_N <-
  lapply(filePaths_N, read_csv, col_types = cols(#moisture = col_double(),
    Latitude = col_character(),
    Longitude = col_character(),
    Sample = col_character(),
    moisture = col_character())) %>%
  bind_rows() %>% 
  rename(Species = N_species) %>% 
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
  )

write.csv(field_data_N, "data/data_from_papers/0_N_data1.csv", row.names = FALSE, na = "")



# import CH4 data from individual files ----


filePaths_CH4 <- list.files(path = "data/x-data/CH4",pattern = "*.csv", full.names = TRUE)

field_data_CH4 <-
  lapply(filePaths_CH4, read_csv, col_types = cols(Temp_range = col_character(),
                                                   Latitude = col_character(),
                                                   Longitude = col_character(),
                                                   Sample = col_character())) %>% 
  bind_rows() %>% 
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
  filter_all(any_vars(!is.na(.)))

write.csv(field_data_CH4, "data/data_from_papers/0_CH4_data1.csv", row.names = FALSE, na = "")

# misc ----

a = read_csv("data/CH4/Jiang2010.csv")


field_data_CH4 %>% 
  ggplot(aes(x = Incubation, y = Q10))+
  geom_point()+
  labs(title = "CH4")


field_data_N %>% 
  filter(!is.na(Q10)) %>% 
  
  ggplot(aes(x = Incubation, y = Q10))+
  geom_point()+
  labs(title = "N")+
  facet_wrap(~N_species)


field_data_N %>% 
  filter(!is.na(Q10)) %>% 
  distinct(Source, StudyName, DOI)

field_data_CH4 %>% 
  filter(!is.na(Q10)) %>% 
  distinct(Source, StudyName, DOI)

indiv_studies %>% 
  filter(!is.na(Q10)) %>% 
  
  ggplot(aes(x = Incubation, y = Q10))+
  geom_point()+
  facet_wrap(~Species)


indiv_studies %>% 
  filter(!is.na(Q10)) %>% 
  distinct(Species, Incubation, Source, StudyName, DOI) %>% 
  group_by(Species, Incubation) %>% 
  dplyr::summarise(n = n())


indiv_studies %>% 
  filter(!is.na(Q10)) %>% 
  group_by(Species, Incubation) %>% 
  dplyr::summarise(n = n())



indiv_studies %>% 
  filter(!is.na(Q10)) %>% 
  filter(Species %in% c("CH4", "N2O")) %>% 
  
  ggplot(aes(x = Incubation, y = Q10))+
  geom_point()+
  facet_wrap(~Species)
