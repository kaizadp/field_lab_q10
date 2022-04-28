

# high values ----
high_values_co2_30 = 
  Q10_data %>% 
  filter(Q10 > 30 & Species == "CO2")


high_values_co2_30 %>% 
  ggplot(aes(x = Incubation, y = Q10, color = Temp_range_rounded))+
  geom_jitter(width = 0.3, size = 3)+
  facet_wrap(~ClimateTypes)+
  ylim(0, 350)+
  labs(x = "",
       color = "incubation temp,  C",
       title = "CO2, high Q10 values only (> 30)",
       subtitle = "color = incubation temp,  C")+
  NULL


high_values_co2_10 = 
  Q10_data %>% 
  filter(Q10 > 10 & Species == "CO2")


high_values_co2_10 %>% 
  ggplot(aes(x = Incubation, y = Q10, color = Temp_range_rounded))+
  geom_jitter(width = 0.3, size = 3)+
  facet_wrap(~ClimateTypes)+
  ylim(0, 350)+
  labs(x = "",
       color = "incubation temp,  C",
       title = "CO2, high Q10 values only (> 30)",
       subtitle = "color = incubation temp,  C")+
  NULL


high_values_co2_20 = 
  Q10_data %>% 
  filter(Q10 > 20 & Species == "CO2")


high_values_co2_20 %>% 
  ggplot(aes(x = Incubation, y = Q10, color = Temp_range_rounded))+
  geom_jitter(width = 0.3, size = 3)+
  facet_wrap(~ClimateTypes)+
  ylim(0, 350)+
  labs(x = "",
       color = "incubation temp,  C",
       title = "CO2, high Q10 values only (> 30)",
       subtitle = "color = incubation temp,  C")+
  NULL




#
# srdb ----

loadd(srdb_q10)

srdb_q10_2 = 
  srdb_q10 %>% 
  mutate(Temp_range2 = Temp_range) %>% 
  separate(Temp_range2, sep = "_", into = c("Temp_min", "Temp_max")) %>% 
  mutate(Q10 = as.numeric(Q10),
         Temp_min = as.numeric(Temp_min),
         Temp_max = as.numeric(Temp_max)) 

srdb_q10_2 %>% 
  mutate(Q10 = as.numeric(Q10)) %>% 
  ggplot(aes(x = Temp_max,  y = Temp_min))+
  geom_jitter(aes(color = Q10),
             size =2, width = 0.2, height = 0.2)+
  scale_color_gradientn(trans = "log10", colors = PNWColors::pnw_palette("Bay"))


srdb_q10_2 %>% 
  filter(Q10 < 30) %>% 
  filter(Temp_min != Temp_max) %>% 
  ggplot()+
  geom_segment(aes(x = Temp_min, xend = Temp_max, 
                   y = Q10, yend = Q10,
                   color = Q10),
               size = 1)+
scale_color_gradientn(trans = "log10", colors = rev(PNWColors::pnw_palette("Bay")),
                      )



# biome ----

compute_co2_biome = function(Q10_data){

  Q10_CO2_data = 
    Q10_data %>% 
    filter(!is.na(Q10)) %>% 
    filter(Species == "CO2" & !is.na(Temp_range))
  
  
  Q10_CO2_data %>% 
    filter(Species == "CO2" & !is.na(ClimateTypes)) %>%
    #filter(Q10 < 300) %>% 
    ggplot(aes(x = ClimateTypes, y = Q10, fill = Incubation, group = interaction(ClimateTypes, Incubation)))+
    geom_violin(position = position_dodge(width = 0.5))+
    scale_fill_manual(values = pal_incubation)+
    scale_y_log10()+
    labs(x = "")+
    NULL
    
  
  Q10_CO2_data %>% 
    filter(Species == "CO2" & !is.na(ClimateTypes)) %>%
    ggplot(aes(x = Incubation, y = Q10, fill = Incubation))+
    ggdist::stat_halfeye(aes(fill = Incubation), 
                         size = 1, alpha = 0.5,
                         position = position_nudge(x = 0.2), width = 0.5)+
    geom_jitter(aes(color = Incubation), width = 0.1, )+
    facet_wrap(~ClimateTypes, ncol = 5 #, strip.position = "bottom"
    )+
    labs(title = "CO2",
         x = "")+
    scale_color_manual(values = pal_incubation)+
    scale_fill_manual(values = pal_incubation)+
    theme(legend.position = "none",
          panel.grid = element_blank())+
    scale_y_log10()+
    NULL



  Q10_CO2_data %>% 
    filter(Species == "CO2" & !is.na(ClimateTypes)) %>%
    #filter(Q10 < 300) %>% 
    ggplot(aes(x = ClimateTypes, y = Q10, fill = Incubation, color = Incubation, 
               group = interaction(ClimateTypes, Incubation)))+
    ggdist::stat_halfeye(aes(fill = Incubation), 
                         size = 1, alpha = 0.8, 
                         position = position_nudge(x = 0.2), width = 0.5,
                         show.legend = FALSE)+
    geom_jitter(aes(color = Incubation, shape = Incubation), 
                size = 2, stroke = 1,
                position = position_dodge(width = 0.2))+
    scale_fill_manual(values = pal_incubation)+
    scale_color_manual(values = pal_incubation)+
    scale_shape_manual(values = c(16, 1))+
    scale_y_log10()+
    labs(x = "")+
    NULL
  
  
  
  
  
  # stats ----
  
  #co2_aov_biome = 
  Q10_data %>% 
    filter(!Temp_range %in% c("< 0", "0_5")) %>% # because no lab data for < 0
    group_by(ClimateTypes) %>% 
    do(fit_aov(.)) %>% 
    mutate(p_value = round(p_value,5))
  
  
  Q10_data %>% 
    filter(!Temp_range %in% c("< 0", "0_5")) %>% # because no lab data for < 0
    group_by(ClimateTypes, Incubation) %>% 
    dplyr::summarise(mean = mean(Q10)) %>% 
    pivot_wider(names_from = Incubation, values_from = mean)
  
  # graphs ----
  nonsnow = 
    
  snow = 
    Q10_CO2_data %>% 
    filter(Species == "CO2" & ClimateTypes == "snow") %>%
    ggplot(aes(x = Incubation, y = Q10, color = Incubation, group = Incubation))+
    geom_jitter(width = 0.2, size = 1)+
    facet_wrap(~ClimateTypes, ncol = 4)+
    scale_y_log10()+
    scale_color_manual(values = pal_incubation)+
    labs(y = "")+
    NULL
  
  combined = 
    nonsnow + snow + 
    plot_layout(widths = c(4, 1),
                guides = "collect") &
    theme(legend.position = "none") &
    labs(x = "")
  
  list(combined = combined)
}



srdb_q10_2 %>% 
  ggplot(aes(x = Soil_drainage, y = Q10))+
  ggdist::stat_halfeye(aes(fill = Incubation), 
                       size = 1, alpha = 0.8, 
                       position = position_nudge(x = 0.2), width = 0.5,
                       point_interval = "median_qi",
                       show.legend = FALSE)+
  geom_jitter(aes(color = Incubation, shape = Incubation), 
              size = 2, stroke = 1, width = 0.1)+
  scale_y_log10()



# variation ----

x =  var.test(Q10_data %>% filter(Species == "CO2" & Incubation == "lab") %>% pull(Q10),
         Q10_data %>% filter(Species == "CO2" & Incubation == "field") %>% pull(Q10))$`p.value` 

Q10_data %>% 
  group_by(Species, Incubation) %>% 
  dplyr::summarise(n = n(),
                   sd = sd(Q10, na.rm = TRUE),
                   mean = mean(Q10, na.rm = TRUE),
                   cv = sd/mean)


compute_ftest = function(dat){
  var.test(dat %>% filter(Species == "CO2" & Incubation == "lab") %>% pull(Q10),
           dat %>% filter(Species == "CO2" & Incubation == "field") %>% pull(Q10))$`p.value` %>% 
    as_tibble() %>% 
    mutate(value = round(value, 4))
  
}


Q10_data %>% filter(Q10 <= 20) %>% 
  group_by(ClimateTypes) %>% do(compute_ftest(.))


Q10_data %>% 
  filter(Species == "CO2" & Q10 <= 30) %>% 
  group_by(ClimateTypes, Incubation) %>% 
  dplyr::summarise(cv = sd(Q10, na.rm = T)/mean(Q10, na.rm = T))


Q10_data %>% filter(ClimateTypes  == "arid") %>% do(compute_ftest(.))

#
#  biome vs. temp ----

Q10_data %>% 
  filter(Species == "CO2") %>% 
  ggplot(aes(x = ClimateTypes, y = Temp_range))+
  geom_jitter(width = 0.2, height = 0.2, aes(color = Incubation))

Q10_data %>% 
  filter(Species == "CO2") %>% 
  ggplot(aes(x = ClimateTypes, y = Temp_range_rounded))+
  geom_jitter(width = 0.2, height = 0.2)

Q10_data %>% 
#  group_by(ClimateTypes, Temp_range_rounded) %>% 
  separate(Temp_range_rounded, sep = "_", into = c("Temp_min", "Temp_max")) %>% 
  mutate(Temp_min = as.numeric(Temp_min),
         Temp_max = as.numeric(Temp_max)) %>% 
  ggplot()+
  geom_segment(aes(x = Temp_min, xend = Temp_max,  y = Q10, yend = Q10,
                   color = Incubation),
               size = 1)+
#  facet_wrap(~ClimateTypes,  ncol = 6)+
  facet_grid(Incubation ~ ClimateTypes)+
  scale_y_log10()+
  labs(x = "Incubation Temp Range (min-to-max)")



temps = 
  Q10_data %>% 
  #  group_by(ClimateTypes, Temp_range_rounded) %>% 
  separate(Temp_range_rounded, sep = "_", into = c("Temp_min", "Temp_max")) %>% 
  mutate(Temp_min = as.numeric(Temp_min),
         Temp_max = as.numeric(Temp_max))

temps %>%  
  ggplot()+
  geom_smooth(aes(x = Temp_min, y = Q10))+
  geom_smooth(aes(x = Temp_max, y = Q10))+
  geom_point(aes(x = Temp_min, y = Q10))+
  facet_wrap(~ClimateTypes,  ncol = 6)+
  scale_y_log10()


Q10_data %>% 
  #  group_by(ClimateTypes, Temp_range_rounded) %>% 
  separate(Temp_range_old, sep = "_", into = c("Temp_min", "Temp_max")) %>% 
  mutate(Temp_min = as.numeric(Temp_min),
         Temp_max = as.numeric(Temp_max)) %>% 
  ggplot()+
  geom_density(aes(Temp_min), fill = "red", color = "red", alpha = 0.6)+
  geom_density(aes(Temp_max), fill = "blue", color = "blue", alpha = 0.6)+
#  facet_wrap(~ClimateTypes,  ncol = 6)+
  facet_grid(Incubation ~ ClimateTypes)



ggplot(Q10_data, aes(Q10)) +
  geom_density()

temps_minmax = 
  temps %>% 
  group_by(ClimateTypes, Incubation) %>% 
  dplyr::summarise(min = min(Temp_min, na.rm = TRUE),
                   max = max(Temp_max, na.rm = TRUE)) 


ggplot()+
  geom_rect(data = temps_minmax %>% filter(Incubation == "field"),
            aes(xmin = min, xmax = max, ymin = 1, ymax = 1.5), color = "red", fill = "red", alpha = 0.2)+
  geom_rect(data = temps_minmax %>% filter(Incubation == "lab"),
            aes(xmin = min, xmax = max, ymin = 0.45, ymax = 0.95), color = "blue", fill = "blue", alpha = 0.2)+
  annotate("text", label = "field", x = 10, y = 1.2)+
  annotate("text", label = "lab", x = 10, y = 0.7)+
  ylim(0,2)+
  facet_grid(. ~ ClimateTypes)+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y  = element_blank(),
        panel.grid.minor.y  = element_blank())+
  labs(x = "Temperature Range, C",
       y = "")




# determine field limits ----

field_limits = 
  Q10_data %>% 
  filter(Incubation == "field" & Species == "CO2") %>% 
  separate(Temp_range_rounded, sep = "_", into = c("Temp_min", "Temp_max")) %>% 
  group_by(Species, ClimateTypes) %>% 
  dplyr::summarise(temp_min_min = min(Temp_min, na.rm = TRUE),
                   temp_max_max = max(Temp_max, na.rm = TRUE))
