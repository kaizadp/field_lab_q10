
# packages for map
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)


make_map_all_studies <- function(Q10_data){
  world <- ne_countries(scale = "medium",  returnclass = "sf", type = "countries")
  
  Q10_map_data = 
    Q10_data %>% 
    distinct(Species, Latitude, Longitude)
  
  world %>% 
    ggplot()+
    geom_sf(color = NA, alpha = 0.7)+
    geom_point(data = Q10_map_data,
               aes(x = Longitude, y = Latitude), 
               alpha = 0.5, size = 1)+
    labs(color = "")+
    theme_void()+
    theme(legend.position = "top")+
    facet_wrap(~Species)
}

compute_stats_q10 <- function(Q10_data){
  # LME ----
  l <- nlme::lme(Q10 ~ Incubation, random = ~1|Incubation, data = Q10_data) 
  anova(l)
  # NaNs produced
  
  # ANOVA/HSD ----
  fit_aov <- function(Q10_data){
    a = aov(Q10 ~ Incubation, data = Q10_data)
    # h = agricolae::HSD.test(a, "Incubation")
    # h$groups %>% 
    #   rownames_to_column("Incubation")
    
    broom::tidy(a) %>% 
      filter(term == "Incubation") %>% 
      rename(p_value = `p.value`) %>% 
      mutate(label = case_when(p_value <= 0.05 ~ "*")) %>% 
      dplyr::select(label)
  }
  
  Q10_data %>% 
    group_by(Temp_range) %>% 
    do(fit_aov(.))
  # 5-15: lab > field
  # 15-25, > 25: field > lab
}

make_graphs_q10 <- function(Q10_data){
  
  resp_q10_temp <- 
    Q10_data %>% 
    ggplot(aes(x = Temp_range_new, y = Q10, color = Incubation))+
    geom_point(position = position_dodge(width = 0.4))
  
  resp_q10_latitude = 
    Q10_data %>% 
    ggplot(aes(x = Q10, y = Latitude, color = Incubation))+
    geom_point(position = position_dodge(width = 0.4))+
    facet_wrap(~Temp_range_new, scales = "free_x")+theme_bw()
  
  list(resp_q10_temp = resp_q10_temp,
       resp_q10_latitude = resp_q10_latitude)
}


compute_stats_q10_CO2 <- function(Q10_data){

  fit_aov <- function(dat){
    a = aov(Q10 ~ Incubation, data = dat)
    
    broom::tidy(a) %>% 
      filter(term == "Incubation") %>% 
      rename(p_value = `p.value`) %>% 
  #    mutate(label = case_when(p_value <= 0.05 ~ "*")) %>% 
  #    dplyr::select(p_value) %>% 
      force()
  }
  
 Q10_data %>% 
    filter(Species == "CO2" & !is.na(Temp_range)) %>% 
    filter(!Temp_range %in% c("< 0", "0_5")) %>% # because no lab data for < 0
    group_by(Temp_range) %>% 
    do(fit_aov(.))

}
