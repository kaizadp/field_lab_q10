# FIELD VS. LAB GHG Q10
# PATEL ET AL. 2022
# 
# This script contains functions used for data exploration of the compiled Q10 data
# kfp 2022
# 
# ############################# #

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


