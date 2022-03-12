
Q10_data = read.csv("data/processed/Q10_data.csv", na.strings = "") %>% 
  reorder_temp_levels(.) %>% 
  mutate(Ecosystem_type = tolower(Ecosystem_type),
         Biome = tolower(Biome))



plot_temperature_ranges = function(Q10_data){
  Q10_data_temps = 
    Q10_data %>% 
    distinct(Temp_range_old, Species, Incubation) %>% 
    separate(Temp_range_old, sep = "_", into = c("temp_start", "temp_stop")) %>% 
    mutate(temp_start = as.numeric(temp_start),
           temp_stop = as.numeric(temp_stop))

  Q10_data_temps %>% 
    filter(!is.na(Species) & !is.na(Incubation)) %>% 
    arrange(temp_start, temp_stop) %>% 
    mutate(rownames_to_column(., "y")) %>% 
    ggplot(aes(y = y))+
    geom_point(aes(x = temp_start), color = "red")+
    geom_point(aes(x = temp_stop), color = "black")+
    geom_segment(aes(x = temp_start, xend = temp_stop, yend = y))+
    theme_bw()+
    scale_x_continuous(minor_breaks = seq(-20, 50, 5))+
    theme(axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.line.y = element_blank())+
    labs(title = "all data, all temperature ranges")+
    facet_grid(Species ~ Incubation)
}

plot_mat_map = function(Q10_data){
  Q10_data %>% 
    filter(!is.na(Species)) %>% 
    ggplot(aes(x = MAT_C, y = MAP_mm))+
    geom_point(aes(color = Species), size = 2)+
    theme_classic()
  
}





UDel_summarized_climate = read.csv("UDel_summarized_climate.csv")
UDel_summarized_climate %>% 
  ggplot(aes(x = Longitude, y = Latitude, color = MAT))+
  geom_point()


Q10_MAT = 
  Q10_data %>% 
  mutate(Latitude2 = round(Latitude*2)/2,
         Longitude2 = round(Longitude*2)/2,
         Lat_dif = ifelse(Latitude2 - Latitude >=0, 0.25, -0.25),
         Lon_dif = ifelse(Longitude2 - Longitude >=0, 0.25, -0.25),
         Latitude2 = Latitude2 - Lat_dif,
         Longitude2 = Longitude2 - Lon_dif) %>% 
  dplyr::select(-Lat_dif, -Lon_dif) %>% 
  left_join(UDel_summarized_climate, by=c("Latitude2"="Latitude", "Longitude2"="Longitude")) %>% 
  left_join(KoeppenGeigerASCII, by=c("Latitude2"="Latitude", "Longitude2"="Longitude"))
  
  

Q10_MAT %>% 
  filter(!is.na(Species)) %>% 
  filter(Species %in% c("CO2", "CH4")) %>% 
  ggplot(aes(x = MAT, y = MAP))+
  geom_point(aes(color = ClimateTypes_2), size = 2)+
  facet_wrap(~Species)+
  theme_classic()



KoeppenGeigerASCII = readxl::read_xlsx("KoeppenGeigerASCII.xlsx")
KoeppenGeigerASCII %>% 
  ggplot(aes(x = Longitude, y = Latitude, color = ClimateTypes))+
  geom_point()

KoeppenGeigerASCII = 
  KoeppenGeigerASCII %>% 
  mutate(ClimateTypes_2 = case_when(grepl("A", ClimateTypes) ~ "equatorial",
                                    grepl("B", ClimateTypes) ~ "arid",
                                    grepl("C", ClimateTypes) ~ "temperate",
                                    grepl("D", ClimateTypes) ~ "snow",
                                    grepl("E", ClimateTypes) ~ "polar"))


KoeppenGeigerASCII %>% 
  ggplot(aes(x = Longitude, y = Latitude, color = ClimateTypes_2))+
  geom_point()


KoeppenGeigerASCII_2 = 
  KoeppenGeigerASCII %>% 
  left_join(UDel_summarized_climate)

KoeppenGeigerASCII_2 %>% 
  ggplot(aes(x = MAT, y = MAP))+
  geom_point(aes(color = ClimateTypes_2))

nonsnow = 
  Q10_MAT %>% 
  filter(Species == "CO2" & ClimateTypes_2 != "snow") %>%
  filter(Q10 < 300) %>% 
  ggplot(aes(x = Incubation, y = Q10, color = Incubation, group = Incubation))+
  geom_jitter(width = 0.2, size = 1)+
  facet_wrap(~ClimateTypes_2, ncol = 4)

snow = 
  Q10_MAT %>% 
  filter(Species == "CO2" & ClimateTypes_2 == "snow") %>%
  ggplot(aes(x = Incubation, y = Q10, color = Incubation, group = Incubation))+
  geom_jitter(width = 0.2, size = 1)+
  facet_wrap(~ClimateTypes_2, ncol = 4)

library(patchwork)


nonsnow + snow + 
  plot_layout(widths = c(4, 1),
              guides = "collect") &
  theme(legend.position = "top") &
  labs(x = "")
