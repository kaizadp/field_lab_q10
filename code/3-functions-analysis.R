
# packages for map
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

reorder_temp_levels = function(dat){
  dat %>% 
    mutate(Temp_range = factor(Temp_range, levels = c("< 0", "0_5", "5_15", "15_25", "> 25")))
}

make_map_all_studies <- function(Q10_data){
  world <- ne_countries(scale = "medium",  returnclass = "sf", type = "countries")
  
  Q10_map_data = 
    Q10_data %>% 
    distinct(Species, Latitude, Longitude, Incubation)
  
  world %>% 
    ggplot()+
    geom_sf(color = NA, alpha = 0.7)+
    geom_point(data = Q10_map_data,
               aes(x = Longitude, y = Latitude, 
                   color = Incubation), 
               alpha = 0.5, size = 1)+
    labs(color = "")+
    theme_void()+
    theme(legend.position = "top")+
    facet_wrap(~Species)
}



compute_stats_q10 <- function(Q10_data){
  
  fit_aov <- function(dat){
    a = aov(Q10 ~ Incubation, data = dat)
    
    broom::tidy(a) %>% 
      filter(term == "Incubation") %>% 
      rename(p_value = `p.value`) %>% 
      #    mutate(label = case_when(p_value <= 0.05 ~ "*")) %>% 
      #    dplyr::select(p_value) %>% 
      force()
  }
  
  # CO2 ----
  
  co2_aov_temp = 
    Q10_data %>% 
    #filter(!is.na(Q10)) %>% 
    filter(Species == "CO2" & !is.na(Temp_range)) %>% 
    filter(!Temp_range %in% c("< 0", "0_5")) %>% # because no lab data for < 0
    group_by(Temp_range) %>% 
    do(fit_aov(.))
  
  co2_aov_all = 
    Q10_data %>% 
    # filter(!is.na(Q10)) %>% 
    filter(Species == "CO2" & !is.na(Temp_range)) %>% 
    do(fit_aov(.))
  
  co2_lme_all = 
    Q10_data %>% 
    filter(!is.na(Q10)) %>% 
    filter(Species == "CO2" & !is.na(Temp_range)) %$%
    nlme::lme(Q10 ~ Incubation, random = ~1|Temp_range) %>% anova(.)
  
  # N2O ----
  n2o_aov_all = 
    Q10_data %>% 
    # filter(!is.na(Q10)) %>% 
    filter(Species == "N2O") %>% 
    do(fit_aov(.))
  
  # CH4 ----
  ch4_aov_all = 
    Q10_data %>% 
    # filter(!is.na(Q10)) %>% 
    filter(Species == "CH4") %>% 
    do(fit_aov(.))
  
  # list ----
  list(co2_aov_temp = co2_aov_temp,
       co2_aov_all = co2_aov_all,
       co2_lme_all = co2_lme_all,
       n2o_aov_all = n2o_aov_all,
       ch4_aov_all = ch4_aov_all)
}

make_graphs_q10 <- function(Q10_data){
  
  Q10_CO2_data = 
    Q10_data %>% 
    filter(!is.na(Q10)) %>% 
    filter(Species == "CO2" & !is.na(Temp_range))
  
  (resp_q10_temp <- 
      Q10_CO2_data %>% 
      ggplot(aes(x = Temp_range, y = Q10, color = Incubation))+
      #      geom_jitter(width = 0.2, )+
      geom_point(position = position_dodge(width = 0.4))+
      labs(title = "CO2")+
      ylim(0,20))
  
  (resp_q10_temp_jitter <- 
      Q10_CO2_data %>% 
      filter(Temp_range %in% c("5_15", "15_25", "> 25")) %>% 
      ggplot(aes(x = Incubation, y = Q10, color = Incubation))+
      geom_jitter(width = 0.2, size = 1)+
      facet_wrap(~Temp_range, strip.position = "bottom")+
      labs(title = "CO2",
           subtitle = "only > 5 C included")+
      ylim(0,20))
  
  resp_q10_latitude = 
    Q10_CO2_data %>% 
    ggplot(aes(x = Q10, y = Latitude, color = Incubation))+
    geom_point(position = position_dodge(width = 0.4))+
    facet_wrap(~Temp_range, scales = "free_x")+
    labs(title = "CO2 by latitude")+
    theme_bw()
  
  resp_biome = 
    Q10_data %>% 
    filter(Species == "CO2") %>% 
    ggplot(aes(x = Biome, y = Q10, color = Incubation))+
    labs(title = "CO2")+
    geom_point(position = position_dodge(width = 0.3))
  
  resp_ecosystem = 
    Q10_data %>% 
    filter(Species == "CO2") %>% 
    ggplot(aes(x = Incubation, y = Q10, color = Incubation))+
    geom_point(position = position_dodge(width = 0.3))+
    labs(title = "CO2")+
    facet_wrap(~Ecosystem_type)
  
  N2O_incubation = 
    Q10_data %>% 
    filter(Species == "N2O") %>% 
    ggplot(aes(x = Incubation, y = Q10, color = Temp_range))+
    #      geom_jitter(width = 0.2, )+
    geom_point(position = position_dodge(width = 0.4))+
    labs(title = "N2O")
  ylim(0,20)
  
  CH4_incubation = 
    Q10_data %>% 
    filter(Species == "CH4") %>% 
    ggplot(aes(x = Incubation, y = Q10, color = Temp_range))+
    geom_point(position = position_dodge(width = 0.4))+
    labs(title = "CH4")
  ylim(0,20)
  
  
  list(resp_q10_temp = resp_q10_temp,
       resp_q10_temp_jitter = resp_q10_temp_jitter,
       resp_q10_latitude = resp_q10_latitude,
       resp_biome = resp_biome,
       resp_ecosystem = resp_ecosystem,
       N2O_incubation = N2O_incubation,
       CH4_incubation = CH4_incubation)
}

compute_study_summary = function(Q10_data){
  datapoints = 
    Q10_data %>% 
    filter(!is.na(Q10)) %>% 
    group_by(Species, Incubation) %>% 
    dplyr::summarise(n = n()) %>% 
    pivot_wider(names_from = "Incubation", values_from = "n")
  
  study_counts = 
    Q10_data %>% 
    distinct(Q10_study_ID, Species, Incubation) %>% 
    group_by(Species, Incubation) %>% 
    dplyr::summarise(n = n()) %>% 
    pivot_wider(names_from = "Incubation", values_from = "n")
  
  list(datapoints = datapoints,
       study_counts = study_counts)
}

compute_rh_only = function(Q10_data){
  Q10_data_rh_only = 
    Q10_data %>% 
    filter(Respiration_type == "heterotrophic")
  
  library(magrittr)
  rh_aov = 
    Q10_data_rh_only %>% 
    filter(Temp_range == "5_15") %$% 
    aov(Q10 ~ Incubation) %>% 
    summary(.)
  
  rh_summary = 
    Q10_data_rh_only %>% 
    group_by(Temp_range, Incubation) %>% 
    dplyr::summarise(n = n())
  
  rh_jitter_plot = 
    Q10_data_rh_only %>% 
    filter(Temp_range %in% c("5_15", "15_25", "> 25")) %>% 
    ggplot(aes(x = Incubation, y = Q10, color = Incubation))+
    geom_jitter(width = 0.2, size = 1)+
    facet_wrap(~Temp_range, strip.position = "bottom")+
    labs(title = "CO2",
         subtitle = "heterotrophic respiration only")+
    ylim(0,20)
  
  list(rh_aov = rh_aov,
       rh_summary = rh_summary,
       rh_jitter_plot = rh_jitter_plot)
}
