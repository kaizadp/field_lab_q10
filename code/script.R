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


#
# misc --------------------------------------------------------------------
make_map_all_studies <- function(Q10_data){
  world <- ne_countries(scale = "medium",  returnclass = "sf", type = "countries")
  
  Q10_map_data = 
    Q10_data %>% 
    filter(!is.na(Species)) %>% 
    distinct(Species, Latitude, Longitude, Incubation) %>% 
    drop_na()
  
  gg_facet = 
    world %>% 
    ggplot()+
    geom_sf(color = NA, alpha = 0.7)+
    geom_point(data = Q10_map_data,
               aes(x = Longitude, y = Latitude, 
                   color = Incubation), 
               alpha = 0.5, size = 4)+
    labs(color = "",
         x = "",
         y = "")+
    scale_color_manual(values = pal_incubation)+
    #theme_void()+
    theme_kp()+
    theme(axis.text = element_blank(),
          legend.position = c(0.15, 0.6))+
    facet_wrap(~Species, ncol = 1)+
    guides(colour = guide_legend(nrow = 1))+
    NULL
  
  
  gg_species = 
    world %>% 
    ggplot()+
    geom_sf(color = NA, alpha = 0.7)+
    geom_point(data = Q10_map_data,
               aes(x = Longitude, y = Latitude, 
                   color = Species#, shape = Incubation
               ), 
               alpha = 0.7, size = 3)+
    labs(color = "",
         x = "",
         y = "")+
    scale_color_manual(values = soilpalettes::soil_palette("rendoll", 2))+
    #theme_void()+
    theme_kp()+
    theme(axis.text = element_blank(),
          #legend.position = c(0.15, 0.6)
    )+
    #facet_wrap(~Species, ncol = 1)+
    guides(colour = guide_legend(nrow = 1))+
    NULL
  
  list(gg_facet = gg_facet,
       gg_species = gg_species)
  
}



38.43063628	-76.2268381
41.50148, -83.04611

library(ggplot2)
library(usmap)
test_data <- data.frame(lon = c(-76.2268381, -83.04611), lat = c(38.43063628, 41.50148))
transformed_data <- usmap_transform(test_data)
plot_usmap(color = "grey") + 
  geom_point(data = transformed_data, 
             aes(x = x, y = y), 
             color = "black",
             size = 2.5)+
  annotate("text", label = "Secret River (SR)", x = -1686856, y = 411954.3, size=2, hjust="left")+
  annotate("text", label = "Caribou Poker Creeks Research Watershed\n(CPCRW)", x = -1051020, y = -1842646.7, 
           size=2, hjust="left")+
  #  xlim(1000110, 2239367)+
  # ylim(-635306.6, -3663.3)+
  NULL


#

# misc --------------------------------------------------------------------

library(googlesheets4)


x = read_sheet("1kECUzSJYklneco7Jd8uE_q9okybO7QDtShv3nlG26sA", sheet = "Summary Table",
               skip = 2)


# misc --------------------------------------------------------------------



Q10_data2 = 
  Q10_data %>% 
  separate(Temp_range_rounded, sep = "_", into = c("min", "max")) %>% 
  mutate(min = as.numeric(min),
         max = as.numeric(max)) %>% 
  filter(!is.na(min) & !is.na(max)) %>% 
  mutate(temp_mean = (min + max)/2)


Q10_data2 %>% 
  filter(Species == "CO2") %>% 
  filter(Temp_diff <= 10) %>% 
  ggplot(aes(x = temp_mean, y = Q10, color = Incubation))+
  # geom_violin(aes(group_by(temp_mean)))
  geom_point(position = position_dodge(width = 1),
             size = 2)+
  scale_color_manual(values = pal_incubation)+
  labs(x = "Mean incubation temperature, °C")+
  #  ylim(0, 50)+
  #  scale_y_log10()+
  theme(legend.position = c(0.7, 0.8))+
  NULL




# -------------------------------------------------------------------------

world <- ne_countries(scale = "medium",  returnclass = "sf", type = "countries")

Q10_map_data_q10 = 
  Q10_data %>% 
  filter(!is.na(Species)) %>% 
  distinct(Species, Latitude, Longitude, Incubation, Q10) %>% 
  drop_na()

#gg_facet = 
world %>% 
  ggplot()+
  geom_sf(color = NA, alpha = 0.7)+
  geom_point(data = Q10_map_data_q10 %>% filter(Species == "CO2") %>% filter(Q10 <= 5),
             aes(x = Longitude, y = Latitude, 
                 color = Q10), 
             alpha = 0.5, size = 4)+
  labs(color = "",
       x = "",
       y = "")+
  scale_color_viridis_c()+  
  #scale_color_manual(values = pal_incubation)+
  #theme_void()+
  theme_kp()+
  theme(axis.text = element_blank(),
        #legend.position = c(0.15, 0.6)
  )+
  facet_wrap(~Incubation, ncol = 1)+
  #guides(colour = guide_legend(nrow = 1))+
  NULL


gg_species = 
  world %>% 
  ggplot()+
  geom_sf(color = NA, alpha = 0.7)+
  geom_point(data = Q10_map_data,
             aes(x = Longitude, y = Latitude, 
                 color = Species#, shape = Incubation
             ), 
             alpha = 0.7, size = 3)+
  labs(color = "",
       x = "",
       y = "")+
  scale_color_manual(values = soilpalettes::soil_palette("rendoll", 2))+
  #theme_void()+
  theme_kp()+
  theme(axis.text = element_blank(),
        #legend.position = c(0.15, 0.6)
  )+
  #facet_wrap(~Species, ncol = 1)+
  guides(colour = guide_legend(nrow = 1))+
  NULL  



# -------------------------------------------------------------------------

Q10_CO2_data %>% 
  filter(Species == "CO2" & !is.na(ClimateTypes)) %>%
  #filter(Q10 < 300) %>% 
  ggplot(aes(x = Incubation, y = Q10, color = Incubation, group = Incubation))+
  geom_jitter(width = 0.2, size = 1)+
  facet_wrap(~ClimateTypes, ncol = 5)+
  scale_color_manual(values = pal_incubation)+
  labs(x = "")+
  scale_y_log10()+
  
  NULL


#



# misc --------------------------------------------------------------------


ggplot()+
  
  ggdist::stat_halfeye(data = bootstrap_long,
                       aes(x = Incubation, y = Q10), 
                       size = 1, alpha = 0.5,
                       fill = "grey",
                       position = position_nudge(x = 0.3), 
                       width = 0.3)+
  
  geom_jitter(data = bootstrap_long,
              aes(x = Incubation, y = Q10), 
              width = 0.1, color = "grey", alpha = 0.5)+
  
  
  geom_jitter(data = Q10_CO2_data %>% filter(Temp_range %in% c("5_15", "15_25", "> 25")),
              aes(x = Incubation, y = Q10, color = Incubation),
              width = 0.2, size = 1)+
  
  
  
  facet_wrap(~Temp_range #, strip.position = "bottom"
  )+
  labs(title = "CO2",
       subtitle = "only > 5 C included",
       caption = "grey circles = bootstrapped",
       x = "")+
  scale_color_manual(values = pal_incubation)+
  scale_fill_manual(values = pal_incubation)+
  theme(legend.position = "none")+
  #    ylim(0,20)+
  NULL



bootstrap_long %>% 
  ggplot(aes(x = Incubation, y = Q10, color = Incubation))+
  ggdist::stat_halfeye(aes(fill = Incubation), 
                       size = 1, alpha = 0.5,
                       position = position_nudge(x = 0.2), width = 0.5)+
  geom_jitter(aes(color = Incubation), width = 0.1, )+
  geom_text(aes(x = 1.5, y = 8), label = "*", color = "black", size = 10)+
  facet_wrap(~Temp_range #, strip.position = "bottom"
  )+
  labs(title = "CO2",
       subtitle = "bootstrapped",
       x = "")+
  scale_color_manual(values = pal_incubation)+
  scale_fill_manual(values = pal_incubation)+
  theme(legend.position = "none")+
  ylim(0, 20)+
  NULL
