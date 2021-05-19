
# -------------------------------------------------------------------------

# check for Q10 temp effect (positive, negative, etc.)
loadd(srdb_q10)
srdb_v5_equations = read.csv("data/SRDB_V5_1827/data/srdb-equations-V5.csv")


temp_effect = 
  srdb_v5_equations %>% 
  dplyr::select(Temp_effect, Q10_5_15, Q10_0_20, Q10_0_10) %>% 
  pivot_longer(-Temp_effect)

temp_effect %>% 
  ggplot(aes(x = name, y = value, color = Temp_effect, group = Temp_effect))+
  geom_point(position = position_dodge(width = 0.5))


## misc code ----
# more_data <- import_individual_studies()
# 
# more_data_srdb <- more_data %>% 
#   bind_rows(srdb_q10 %>% 
#               mutate(Record_number = as.character(Record_number),
#                      Incubation = "field",
#                      Source = "SRDB") %>% 
#               rename(Temp_range = temp_range))
# 
# more_data2 <- 
#   more_data_srdb %>% 
#   dplyr::select(Source, Incubation, Q10, Temp_range, Temp_mean, Latitude, Longitude) %>% 
#   mutate(Temp_range2 = Temp_range) %>% 
#   separate(Temp_range2, sep = "_", into = c("Temp_min", "Temp_max")) %>% 
#   mutate(Temp_min = as.numeric(Temp_min),
#          Temp_max = as.numeric(Temp_max),
#          Temp_mean = if_else(is.na(Temp_mean), (Temp_min+Temp_max)/2, Temp_mean))
# 
# 
# 
# more_data2 %>% 
#   ggplot(aes(x = Temp_mean, y = Q10, color = Incubation))+
#   geom_point()+
#   geom_smooth()
# 
# more_data2 %>% 
#   ggplot(aes(y = Latitude, x = Q10, color = Incubation))+
#   geom_point()
# 
# hamdi <- read_csv("data/cleaned_for_analysis/lab/Hamdi2013.csv")
# 
# hamdi2 <- 
#   hamdi %>% 
#   dplyr::select(latitude_deg) %>% 
#   mutate(latitude_deg = if_else(grepl('"', latitude_deg), 
#                                 latitude_deg, 
#                                 str_replace_all(latitude_deg, "'", "'00")),
#          latitude_deg = if_else(grepl("'", latitude_deg), 
#                                 latitude_deg, 
#                                 str_replace_all(latitude_deg, "_", "_00'00")),
#   ) %>% 
#   rowwise() %>% 
#   mutate(
#     
#     
#     latitude_deg = str_replace_all(latitude_deg, "_", " "),
#     latitude_deg = str_replace_all(latitude_deg, "'", " "),
#     latitude_deg = str_replace_all(latitude_deg, '"', " "),
#     latitude_deg = str_remove(latitude_deg, "N"),
#     lat = measurements::conv_unit(latitude_deg, from = "deg_min_sec", to = "dec_deg")
#   )
# 
# 
