
# packages for map
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)


# general functions -------------------------------------------------------

reorder_species_levels = function(dat){
  dat %>% 
    mutate(Species = factor(Species, levels = c("CO2", "CH4")))
  
}
reorder_temp_levels = function(dat){
  dat %>% 
    mutate(Temp_range = factor(Temp_range, levels = c("< 0", "0_5", "5_15", "15_25", "> 25")))
  
}
reorder_biome_levels = function(dat){
  dat %>% 
    mutate(ClimateTypes = factor(ClimateTypes, levels = c("equatorial", "arid", "temperate", "snow", "polar")))
  
}

recode_temp_levels = function(dat){
  dat %>% 
    mutate(Temp_range = recode(Temp_range, "5_15" = "5-15 °C", 
                             "15_25" = "15-25 °C",  "> 25" = ">25 °C"))
}

fit_aov <- function(dat){
  a = aov(Q10 ~ Incubation, data = dat)
  
  broom::tidy(a) %>% 
    filter(term == "Incubation") %>% 
    rename(p_value = `p.value`) %>% 
    #    mutate(label = case_when(p_value <= 0.05 ~ "*")) %>% 
    #    dplyr::select(p_value) %>% 
    force()
}

#

# exploration -------------------------------------------------------------

plot_temperature_ranges = function(Q10_data){
  
  Q10_data_temps = 
    Q10_data %>% 
    filter(Species == "CO2") %>% 
#    group_by(Temp_range_rounded, Species, Incubation) %>% 
    group_by(Temp_range_rounded, Species) %>% 
    #distinct(Temp_range_old, Species, Incubation) %>% 
    dplyr::summarise(n = n()) %>% 
    separate(Temp_range_rounded, sep = "_", into = c("temp_start", "temp_stop")) %>% 
    mutate(temp_start = as.numeric(temp_start),
           temp_stop = as.numeric(temp_stop)) %>% 
    drop_na() %>% 
#    filter(!is.na(Species) & !is.na(Incubation)) %>%
    arrange(temp_start, temp_stop) %>% 
    force()
  
  set.seed(42)
  rows <- sample(nrow(Q10_data_temps))
  Q10_data_temps = Q10_data_temps[rows,]
  
  Q10_data_temps %>% 
    mutate(rownames_to_column(., "y")) %>% 
    ggplot(aes(y = n, color = y))+
    
    geom_segment(aes(x = temp_start, xend = temp_stop, yend = n), size = 1)+
    geom_point(aes(x = temp_start), shape = 21, fill = "white", size = 2, stroke = 1)+
    geom_point(aes(x = temp_stop), size = 2, stroke = 1)+
    
    # annotate box for high frequency
    annotate("rect", xmin = -20, xmax = 55, ymin = 300, ymax = 1800,
             color = "grey20", fill = "orange1", alpha = 0.1, linetype = "dashed")+
    annotate("text", label = "most commonly used bins", x = 35, y = 1500, color = "grey20")+
    
    # annotate curves for min/max
    annotate("curve", x = -5, xend = -1, y = 500, yend = 750, curvature = -0.3,
             arrow = arrow(angle = -30, length = unit(3, "mm")), color = "grey20")+
    annotate("curve", x = 25, xend = 21, y = 500, yend = 750, curvature = 0.3,
             arrow = arrow(angle = -30, length = unit(3, "mm")), color = "grey20")+
    annotate("text", label = "min temp", x = -5, y = 450, color = "grey20")+
    annotate("text", label = "max temp", x = 25, y = 450, color = "grey20")+
    
    scale_x_continuous(minor_breaks = seq(-20, 50, 5))+
    theme(axis.ticks = element_blank(),
      legend.position = "none")+
    labs(#title = "all data, all temperature ranges",
      x = "
      Incubation temperature range (°C)",
      y = "Frequency")+
    scale_y_log10()+
    NULL
  
}

plot_q10_by_temp_ranges = function(Q10_data){
  
  Q10_data %>% 
    filter(!is.na(ClimateTypes)) %>% 
    separate(Temp_range_rounded, sep = "_", into = c("Temp_min", "Temp_max")) %>% 
    mutate(Temp_min = as.numeric(Temp_min),
           Temp_max = as.numeric(Temp_max)) %>% 
    ggplot()+
    geom_segment(aes(x = Temp_min, xend = Temp_max,  y = Q10, yend = Q10,
                     color = Incubation), size = 1)+
    scale_y_log10()+
    scale_color_manual(values = pal_incubation)+
    labs(x = "
         Incubation temperature range (°C)",
         y = expression(bold("Q"[10])))+
    facet_grid(Incubation ~ ClimateTypes)+
    theme(legend.position = "none")+
    NULL

}

plot_temp_ranges_field_vs_lab = function(Q10_data){
  
  temps = 
    Q10_data %>% 
    #  group_by(ClimateTypes, Temp_range_rounded) %>% 
    separate(Temp_range_rounded, sep = "_", into = c("Temp_min", "Temp_max")) %>% 
    mutate(Temp_min = as.numeric(Temp_min),
           Temp_max = as.numeric(Temp_max))
  
  temps_minmax = 
    temps %>% 
    group_by(ClimateTypes, Incubation) %>% 
    dplyr::summarise(xmin = min(Temp_min, na.rm = TRUE),
                     xmax = max(Temp_max, na.rm = TRUE)) %>% 
    mutate(ymin = case_when(Incubation == "field" ~ 1,
                            Incubation == "lab" ~ 0.45),
           ymax = case_when(Incubation == "field" ~ 1.5,
                            Incubation == "lab" ~ 0.95)) %>% 
    ungroup() %>% 
    dplyr::mutate(label_x = (xmin + xmax)/2,
                  label_y = (ymin + ymax)/2)
  
  
  temps_minmax %>% 
    filter(!is.na(ClimateTypes)) %>% 
    ggplot()+
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                  color = Incubation, fill = Incubation),
              alpha = 0.2, size = 1)+
    geom_text(aes(label = Incubation, x = label_x, y = label_y), size = 5)+
    
    
    scale_color_manual(values = pal_incubation)+
    scale_fill_manual(values = pal_incubation)+
    
    ylim(0,2)+
    facet_grid(. ~ ClimateTypes)+
    
    labs(#title = "Experiment temperature ranges for field vs. lab",
         x = "Temperature Range, °C",
         y = "")+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y  = element_blank(),
        panel.grid.minor.y  = element_blank(),
        legend.position = "none")+
    NULL
  
  
}

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

plot_mat_map = function(Q10_data){
  
  KoeppenGeigerASCII = readxl::read_xlsx("data/geographic_databases/KoeppenGeigerASCII.xlsx")
  # KG climate zone map of all the globe
  map_climate_regions_all = 
    KoeppenGeigerASCII %>% 
    mutate(ClimateTypes = case_when(grepl("A", ClimateTypes) ~ "equatorial",
                                    grepl("B", ClimateTypes) ~ "arid",
                                    grepl("C", ClimateTypes) ~ "temperate",
                                    grepl("D", ClimateTypes) ~ "snow",
                                    grepl("E", ClimateTypes) ~ "polar")) %>% 
    reorder_biome_levels() %>% 
    ggplot(aes(x = Longitude, y = Latitude, color = ClimateTypes))+
    geom_point()+
    scale_color_viridis_d(option = "turbo", direction = -1, na.translate = F)+
    labs(color = "",
         x = "",
         y = "")+
    theme_kp()+
    theme(axis.text = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank())+
    NULL
  
  
  # plot MAT/MAP distribution
  gg_data_mat_map = 
    Q10_data %>% 
    filter(!is.na(Species)) %>% 
    ggplot(aes(x = MAT, y = MAP/10))+
    geom_point(aes(color = ClimateTypes), size = 1)+
    labs(x = "
         Mean annual temperature (°C)",
         y = "Mean annual precipitation (cm)
         ")+
  #  facet_wrap(~Species, ncol = 1)+
  #  scale_color_manual(values = pal_biome, na.translate = F)+
    scale_color_viridis_d(option = "turbo", direction = -1, na.translate = F)+
    theme_kp()+
    theme(legend.position = c(0.25, 0.7),
          #legend.text = element_text(size = 10),
    )+
    guides(color=guide_legend(override.aes=list(size=2)))+
    NULL

  list(map_climate_regions_all = map_climate_regions_all,
       gg_data_mat_map = gg_data_mat_map)
    
}



# analysis ----------------------------------------------------------------

co2_all_summaries = function(Q10_data){
  
  summary_overall = 
    Q10_data %>% 
    group_by(Species, Incubation) %>% 
    dplyr::summarise(mean = mean(Q10, na.rm = TRUE),
                     median = median(Q10, na.rm = TRUE),
                     perc_75 = quantile(Q10, 0.75, na.rm = TRUE),
                     perc_99 = quantile(Q10, 0.99, na.rm = TRUE),
                     min = min(Q10, na.rm = TRUE),
                     max = max(Q10, na.rm = TRUE)) %>% 
    pivot_longer(-c(Species, Incubation)) %>% 
    pivot_wider(names_from = "Incubation", values_from = "value")
  
  summary_biome = 
    Q10_data %>% 
    filter(Species == "CO2") %>% 
    group_by(Species, Incubation, ClimateTypes) %>% 
    dplyr::summarise(mean = mean(Q10, na.rm = TRUE),
                     median = median(Q10, na.rm = TRUE)) %>% 
    pivot_longer(-c(Species, Incubation, ClimateTypes)) %>% 
    pivot_wider(names_from = "Incubation", values_from = "value") %>% 
    arrange(Species, name, ClimateTypes)
  
  summary_temp = 
    Q10_data %>% 
    filter(Species == "CO2") %>% 
    group_by(Species, Incubation, Temp_range) %>% 
    dplyr::summarise(mean = mean(Q10, na.rm = TRUE),
                     median = median(Q10, na.rm = TRUE)) %>% 
    pivot_longer(-c(Species, Incubation, Temp_range)) %>% 
    pivot_wider(names_from = "Incubation", values_from = "value") %>% 
    arrange(Species, name, Temp_range)

  
  list(summary_overall = summary_overall,
       summary_biome = summary_biome,
       summary_temp = summary_temp)
}

compute_co2_all = function(Q10_data){
  ## comparing field vs. lab for all CO2 data, irrespective of incubation temperatures 
  
  # all data - all temperatures ----

  Q10_CO2_data = 
    Q10_data %>% 
    filter(Species == "CO2")
  
  all_summary = 
    Q10_CO2_data %>% 
    group_by(Incubation) %>% 
    dplyr::summarise(mean = mean(Q10, na.rm = TRUE),
                     median = median(Q10, na.rm = TRUE))
  
  aov_all = 
    Q10_CO2_data %>% 
    do(fit_aov(.))
  
  gg_jitter_all <- 
    Q10_CO2_data %>% 
    ggplot(aes(x = Incubation, y = Q10, color = Incubation))+
    geom_jitter(width = 0.2)+
    scale_y_log10()+
    scale_color_manual(values = pal_incubation)+
    labs(title = "CO2 - all data, all temperatures",
         x = "")+
    # ylim(0,20))
    NULL
  
  co2_sample_size = 
    Q10_CO2_data %>% 
    group_by(Incubation) %>% 
    dplyr::summarise(n = n())
  
  gg_raincloud_all = 
    Q10_CO2_data %>% 
    ggplot(aes(x = Incubation, y = Q10, color = Incubation))+
    ggdist::stat_halfeye(aes(fill = Incubation), 
                         size = 1, alpha = 0.5,
                         position = position_nudge(x = 0.2), width = 0.5, 
                         #slab_color = "black"
    )+
    geom_jitter(aes(color = Incubation), width = 0.1, )+
    geom_text(aes(x = 1.5, y = 200), label = "*", color = "black", size = 10)+
    geom_text(data = co2_sample_size, aes(y = 1000, label = paste("n = ", n)), color = "black")+
    #ylim(30, 300)+
    scale_y_log10()+
    scale_color_manual(values = pal_incubation)+
    scale_fill_manual(values = pal_incubation)+
    labs(#title = "CO2 - all temperatures",
      x = "",
      y = expression(bold("Q"[10] * " for CO"[2])))+
    theme(legend.position = "none")+
    NULL
  
  # by incubation temperature
  Q10_data2 = 
    Q10_data %>% 
    separate(Temp_range_rounded, sep = "_", into = c("min", "max")) %>% 
    mutate(min = as.numeric(min),
           max = as.numeric(max)) %>% 
    filter(!is.na(min) & !is.na(max)) %>% 
    mutate(temp_mean = (min + max)/2)
  
  gg_temp_scatter = 
    Q10_data2 %>% 
    filter(Species == "CO2") %>% 
    filter(Temp_diff <= 10) %>% 
    ggplot(aes(x = min, y = Q10, color = Incubation))+
    # geom_violin(aes(group_by(temp_mean)))
    geom_point(position = position_dodge(width = 1.5),
               size = 2)+
    geom_smooth(method = "loess", se = FALSE)+
    scale_color_manual(values = pal_incubation)+
    labs(x = "Min. incubation temperature, °C",
         y = expression(bold("Q"[10] * " for CO"[2])))+
    #  ylim(0, 50)+
    #  scale_y_log10()+
    theme(legend.position = c(0.7, 0.8))+
    NULL

  # all data - only temperature ranges <= 10 C ----
 aov_tenC = 
    Q10_data %>% 
    filter(Species == "CO2" & Temp_diff <= 10) %>% 
    do(fit_aov(.))
  
 gg_jitter_tenC = 
   Q10_data %>% 
   filter(Species == "CO2" & Temp_diff <= 10) %>% 
   ggplot(aes(x = Incubation, y = Q10, color = Incubation))+
   #geom_jitter(width = 0.2, )+
   geom_density()+
   #scale_y_log10()+
   scale_color_manual(values = pal_incubation)+
   labs(title = "CO2 - all data, any 10C interval only",
        x = "")+
   # ylim(0,20))
   NULL
 
 
 gg_raincloud_tenC = 
   Q10_data %>% 
   filter(Species == "CO2" & Temp_diff <= 10) %>% 
   ggplot(aes(x = Incubation, y = Q10, color = Incubation))+
   ggdist::stat_halfeye(aes(fill = Incubation), 
                        size = 1, alpha = 0.5,
                        position = position_nudge(x = 0.2), width = 0.5, 
                        #slab_color = "black"
   )+
   geom_jitter(aes(color = Incubation), width = 0.1, )+
   geom_text(aes(x = 1.5, y = 200), label = "*", color = "black", size = 10)+
   #ylim(30, 300)+
   scale_y_log10()+
   scale_color_manual(values = pal_incubation)+
   scale_fill_manual(values = pal_incubation)+
   labs(title = "CO2 - all data, any 10C interval only",
        x = "")+
   theme(legend.position = "none")+
   NULL
    
 
 # list ----
 list(aov_all = aov_all,
      gg_jitter_all = gg_jitter_all,
      gg_raincloud_all = gg_raincloud_all,
      gg_temp_scatter = gg_temp_scatter,
      aov_tenC = aov_tenC,
      gg_jitter_tenC = gg_jitter_tenC,
      gg_raincloud_tenC = gg_raincloud_tenC
      )
  
}

compute_co2_extreme = function(Q10_data){
  
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
}


compute_co2_temp_range = function(Q10_data){
  
  Q10_CO2_data = 
    Q10_data %>% 
    filter(!is.na(Q10)) %>% 
    filter(Species == "CO2" & !is.na(Temp_range))
  
  # stats ----
  co2_aov_temp = 
    Q10_CO2_data %>% 
    filter(!Temp_range %in% c("< 0", "0_5")) %>% # because no lab data for < 0
    group_by(Temp_range) %>% 
    do(fit_aov(.))
  
  co2_aov_all = 
    Q10_CO2_data %>% 
    do(fit_aov(.))
  
  co2_lme_all = 
    Q10_CO2_data %$%
    nlme::lme(Q10 ~ Incubation, random = ~1|Temp_range) %>% anova(.)
  
  #
  # graphs ----
  resp_q10_temp <- 
    Q10_CO2_data %>% 
    ggplot(aes(x = Temp_range, y = Q10, color = Incubation))+
    #      geom_jitter(width = 0.2, )+
    geom_point(position = position_dodge(width = 0.4))+
    labs(title = "CO2")+
    scale_color_manual(values = pal_incubation)+
    #  ylim(0,20))+
    NULL
  
  (resp_q10_temp_jitter <- 
      Q10_CO2_data %>% 
      filter(Temp_range %in% c("5_15", "15_25", "> 25")) %>% 
      ggplot(aes(x = Incubation, y = Q10, color = Incubation))+
      geom_jitter(width = 0.2, size = 1)+
      facet_wrap(~Temp_range #, strip.position = "bottom"
      )+
      labs(title = "CO2",
           subtitle = "only > 5 C included")+
      scale_color_manual(values = pal_incubation)+
      theme(legend.position = "none")+
      #    ylim(0,20)+
      NULL)
  
  co2_temp_sample_size =
    Q10_CO2_data %>% 
    filter(Temp_range %in% c("5_15", "15_25", "> 25")) %>% 
    group_by(Temp_range, Incubation) %>% 
    dplyr::summarise(n = n()) %>% 
    recode_temp_levels() 
  
  
  (resp_q10_temp_raincloud <- 
      Q10_CO2_data %>% 
      filter(Temp_range %in% c("5_15", "15_25", "> 25")) %>% 
      recode_temp_levels() %>% 
      ggplot(aes(x = Incubation, y = Q10, color = Incubation))+
      ggdist::stat_halfeye(aes(fill = Incubation), 
                           size = 1, alpha = 0.5,
                           position = position_nudge(x = 0.2), width = 0.5)+
      geom_jitter(aes(color = Incubation), width = 0.1, )+
      geom_text(aes(x = 1.5, y = 18), label = "*", color = "black", size = 10)+
      geom_text(data = co2_temp_sample_size, aes(y = 22, label = paste("n =", n)), color = "black")+
      facet_wrap(~Temp_range #, strip.position = "bottom"
      )+
      labs(#title = expression("CO"[2] * " Q"[10] * " values by incubation temperature"),
           #subtitle = "",
           x = "",
           y = expression(bold("Q"[10])))+
      scale_color_manual(values = pal_incubation)+
      scale_fill_manual(values = pal_incubation)+
      theme(legend.position = "none",
            panel.grid = element_blank())+
    #  ylim(0,30)+
      NULL)
  
  list(co2_aov_temp = co2_aov_temp,
       co2_aov_all = co2_aov_all,
       co2_lme_all = co2_lme_all,
       resp_q10_temp = resp_q10_temp,
       resp_q10_temp_jitter = resp_q10_temp_jitter,
       resp_q10_temp_raincloud = resp_q10_temp_raincloud)
  
}

compute_co2_biome = function(Q10_data){
  library(patchwork)
  
  Q10_CO2_data = 
    Q10_data %>% 
    filter(!is.na(Q10)) %>% 
    filter(Species == "CO2" & !is.na(Temp_range))
  
  # stats ----
  
  co2_aov_biome = 
    Q10_data %>% 
    filter(!Temp_range %in% c("< 0", "0_5")) %>% # because no lab data for < 0
    group_by(ClimateTypes) %>% 
    do(fit_aov(.)) %>% 
    mutate(p_value = round(p_value,5),
           asterisk = case_when(p_value <= 0.05 ~ "*"))
  
  
  Q10_data %>% 
    filter(!Temp_range %in% c("< 0", "0_5")) %>% # because no lab data for < 0
    group_by(ClimateTypes, Incubation) %>% 
    dplyr::summarise(mean = mean(Q10)) %>% 
    pivot_wider(names_from = Incubation, values_from = mean)
  
  # graphs ----
          ## nonsnow = 
          ##   Q10_CO2_data %>% 
          ##   filter(Species == "CO2" & ClimateTypes != "snow") %>%
          ##   filter(Q10 < 300) %>% 
          ##   ggplot(aes(x = Incubation, y = Q10, color = Incubation, group = Incubation))+
          ##   geom_jitter(width = 0.2, size = 1)+
          ##   facet_wrap(~ClimateTypes, ncol = 4)+
          ##   scale_color_manual(values = pal_incubation)+
          ##   NULL
          ## 
          ## snow = 
          ##   Q10_CO2_data %>% 
          ##   filter(Species == "CO2" & ClimateTypes == "snow") %>%
          ##   ggplot(aes(x = Incubation, y = Q10, color = Incubation, group = Incubation))+
          ##   geom_jitter(width = 0.2, size = 1)+
          ##   facet_wrap(~ClimateTypes, ncol = 4)+
          ##   scale_y_log10()+
          ##   scale_color_manual(values = pal_incubation)+
          ##   labs(y = "")+
          ##   NULL
          ##   
          ## combined = 
          ##   nonsnow + snow + 
          ##   plot_layout(widths = c(4, 1),
          ##               guides = "collect") &
          ##   theme(legend.position = "none") &
          ##   labs(x = "")
  
  #
  # graphs 2 ----
  gg_biome_raincloud = 
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
     geom_text(data = co2_aov_biome %>% mutate(Incubation = "lab",
                                               y = case_when(ClimateTypes == "arid" ~ 15, ClimateTypes == "snow" ~ 500)), 
               aes(x = ClimateTypes, y = y, label = asterisk),
               color = "black", size = 10)+
     scale_y_log10()+
    labs(x = "",
         y = expression(bold("Q"[10])))+
     theme(legend.position = c(0.1, 0.85))+
    NULL
  
  list(#combined = combined,
       gg_biome_raincloud = gg_biome_raincloud)
}


compute_co2_bootstrapping = function(Q10_data){
  
  Q10_CO2_data = 
    Q10_data %>% 
    filter(!is.na(Q10)) %>% 
    filter(Species == "CO2" & !is.na(Temp_range))
  
  # bootstrapping ----
  
  RESAMPLE <- function(n, x) {
    mean(sample(x, n, replace = TRUE))
  }
  
  q10_5_15_field = Q10_CO2_data %>% filter(Temp_range == "5_15" & Incubation == "field") %>% pull(Q10)
  q10_5_15_lab = Q10_CO2_data %>% filter(Temp_range == "5_15" & Incubation == "lab") %>% pull(Q10)
  q10_15_25_field = Q10_CO2_data %>% filter(Temp_range == "15_25" & Incubation == "field") %>% pull(Q10)
  q10_15_25_lab = Q10_CO2_data %>% filter(Temp_range == "15_25" & Incubation == "lab") %>% pull(Q10)
  q10_25_field = Q10_CO2_data %>% filter(Temp_range == "> 25" & Incubation == "field") %>% pull(Q10)
  q10_25_lab = Q10_CO2_data %>% filter(Temp_range == "> 25" & Incubation == "lab") %>% pull(Q10)
  
  # repeat how many times
  N_SAMPLES <- 10000 # repeat how many times 
  sample_size <- 10 # how many samples take every time 
  
  set.seed(123456) 
  bootstrap_out <- tibble(
    q10_5_15_field_boots = sapply(rep(sample_size, N_SAMPLES), RESAMPLE, q10_5_15_field),
    q10_5_15_lab_boots = sapply(rep(sample_size, N_SAMPLES), RESAMPLE, q10_5_15_lab),
    q10_15_25_field_boots = sapply(rep(sample_size, N_SAMPLES), RESAMPLE, q10_15_25_field),
    q10_15_25_lab_boots = sapply(rep(sample_size, N_SAMPLES), RESAMPLE, q10_15_25_lab),
    q10_25_field_boots = sapply(rep(sample_size, N_SAMPLES), RESAMPLE, q10_25_field),
    q10_25_lab_boots = sapply(rep(sample_size, N_SAMPLES), RESAMPLE, q10_25_lab)
  )
  
  bootstrap_long = 
    bootstrap_out %>% 
    pivot_longer(cols = dplyr::everything(), values_to = "Q10", names_to = "names") %>% 
    mutate(Temp_range = case_when(grepl("5_15", names) ~ "5_15",
                                  grepl("15_25", names) ~ "15_25",
                                  grepl("q10_25", names) ~ "> 25"),
           Incubation = case_when(grepl("field", names) ~ "field",
                                  grepl("lab", names) ~ "lab"),
           Species = "CO2") %>% 
    reorder_temp_levels(.)
  
  # stats ----
  ## bootstrap aov
  boot_aov = 
    bootstrap_long %>% 
    group_by(Temp_range) %>% 
    do(fit_aov(.))
  
  ## wilcoxon?
  
  bootstrap_summary = 
    bootstrap_long %>% 
    group_by(Species, Incubation, Temp_range) %>% 
    dplyr::summarise(median = median(Q10),
                     mean = mean(Q10))
  
  
  
  # graphs ----
  gg_boot_jitter = 
    bootstrap_long %>% 
    ggplot(aes(x = Incubation, y = Q10, color = Incubation))+
    geom_jitter(width = 0.2, size = 1)+
    facet_wrap(~Temp_range, strip.position = "bottom")+
    labs(title = "CO2",
         subtitle = "bootstrapped")+
    scale_color_manual(values = pal_incubation)+
    theme(legend.position = "none")
  
  gg_boot_raincloud = 
    bootstrap_long %>% 
    recode_temp_levels() %>% 
    ggplot(aes(x = Incubation, y = Q10, color = Incubation))+
    ggdist::stat_halfeye(aes(fill = Incubation), 
                         size = 1, alpha = 0.5,
                         position = position_nudge(x = 0.2), width = 0.5)+
    geom_jitter(aes(color = Incubation), width = 0.1, )+
    geom_text(aes(x = 1.5, y = 8), label = "*", color = "black", size = 10)+
    facet_wrap(~Temp_range #, strip.position = "bottom"
               )+
    labs(subtitle = "bootstrapped",
         x = "",
         y = expression(bold("Q"[10])))+
    scale_color_manual(values = pal_incubation)+
    scale_fill_manual(values = pal_incubation)+
    theme(legend.position = "none",
          panel.grid = element_blank())+
    ylim(0, 20)+
    NULL
  
  gg_boot_density = 
    bootstrap_long %>% 
    ggplot(aes(x = Q10, fill = Incubation))+
    geom_density(alpha = 0.5)+    scale_fill_manual(values = pal_incubation)+
    facet_wrap(~Temp_range)+
    theme(legend.position = "none")
  
  
  #
  # old code from JJ ----
  
 ##  # Bootstrap
 ##  # https://www.youtube.com/watch?v=m_osNICFFLY  # this is a youtube link to learn more about bootstrap, and you can find more learning material from google
 ##  # Bootstrap function
 ##  RESAMPLE <- function(n, x) {
 ##    mean(sample(x, n, replace = TRUE))
 ##  }
 ##  
 ##  # let's do an toy example
 ##  set.seed(123456) 
 ##  x1 = rnorm(n = 100, mean = 0, sd = 0.5)
 ##  x2 = rnorm(n = 1000, mean = 1.5, sd = 0.5)
 ##  
 ##  
 ##  # repeat how many times
 ##  N_SAMPLES <- 10000 # repeat how many times 
 ##  sample_size <- 10 # how many samples take every time 
 ##  
 ##  library(dplyr)
 ##  bootstrap_out <- tibble(
 ##    x1_boots = sapply(rep(sample_size, N_SAMPLES), RESAMPLE, x1),
 ##    x2_boots = sapply(rep(sample_size, N_SAMPLES), RESAMPLE, x2)
 ##  )
 ##  
 ##  
 ##  mean(bootstrap_out$x1_boots)
 ##  sd(bootstrap_out$x1_boots)
 ##  mean(bootstrap_out$x2_boots)
 ##  sd(bootstrap_out$x2_boots)
 ##  
 ##  # wilcox.test
 ##  wilcox.test(bootstrap_out$x1_boots,
 ##              bootstrap_out$x2_boots,
 ##              mu = 0, paired = TRUE, alternative = "two.sided", conf.level = 0.95)
 ##  
 ##  library(tidyr)
 ##  library(ggplot2)
 ##  
 ##  bootstrap_out %>%
 ##    tidyr::gather(Type) %>% 
 ##    ggplot(aes(value, fill = Type)) +
 ##    # theme_cowplot() +
 ##    geom_density(stat = "density", alpha = 0.65) +
 ##    theme(legend.position = c(0.50, 0.75)) 
  
  
  
  
  
  
  # output ----
  list(bootstrap_long = bootstrap_long,
       boot_aov = boot_aov,
       gg_boot_jitter = gg_boot_jitter,
       gg_boot_density = gg_boot_density,
       gg_boot_raincloud = gg_boot_raincloud
       )
}

compute_rh_only = function(Q10_data){
  ## OLD, DEFUNCT
  
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
    filter(Temp_range %in% c("5_15")) %>% 
    ggplot(aes(x = Incubation, y = Q10, color = Incubation))+
    geom_jitter(width = 0.2, size = 1)+
    facet_wrap(~Temp_range)+
    labs(title = "CO2",
         subtitle = "heterotrophic respiration only")+
    scale_color_manual(values = pal_incubation)+
    ylim(0,20)+
    theme(legend.position = "none")+
    NULL
  
  rh_raincloud_plot = 
    Q10_data_rh_only %>% 
    filter(Temp_range %in% c("5_15")) %>% 
    ggplot(aes(x = Incubation, y = Q10, color = Incubation))+
    ggdist::stat_halfeye(aes(fill = Incubation), 
                         size = 1, alpha = 0.5,
                         position = position_nudge(x = 0.2), width = 0.5)+
    geom_jitter(aes(color = Incubation), width = 0.1, )+
    facet_wrap(~Temp_range)+
    labs(title = "CO2",
         subtitle = "heterotrophic respiration only",
         x = "")+
    scale_color_manual(values = pal_incubation)+
    scale_fill_manual(values = pal_incubation)+
    ylim(0,20)+
    theme(legend.position = "none")+
    NULL
  
  rh_raincloud_biomes = 
    Q10_data_rh_only %>% 
    filter(Temp_range %in% c("5_15")) %>% 
    filter(ClimateTypes %in% c("temperate", "snow")) %>% 
    ggplot(aes(x = Incubation, y = Q10, color = Incubation))+
    ggdist::stat_halfeye(aes(fill = Incubation), 
                         size = 1, alpha = 0.5,
                         position = position_nudge(x = 0.2), width = 0.5)+
    geom_jitter(aes(color = Incubation), width = 0.1, )+
    facet_wrap(~Temp_range)+
    labs(title = "CO2",
         subtitle = "heterotrophic respiration only",
         caption = "graph excludes Q10 > 12",
         x = "")+
    scale_color_manual(values = pal_incubation)+
    scale_fill_manual(values = pal_incubation)+
    facet_wrap(~Temp_range + ClimateTypes)+
    ylim(0,11.5)+
    theme(legend.position = "none")+
    NULL
  
  list(rh_aov = rh_aov,
       rh_summary = rh_summary,
       rh_jitter_plot = rh_jitter_plot,
       rh_raincloud_plot = rh_raincloud_plot,
       rh_raincloud_biomes = rh_raincloud_biomes)
  

  
}

rh_analysis = function(Q10_data){
  
  Q10_data_rh = 
    Q10_data %>% 
    mutate(Rh_group = cut(RC_annual, c(-0.1, 0.5, 1.0), labels = c("Field: Rh-dominated", "Field: Ra-dominated")),
           Rh_group = as.character(Rh_group),
           Rh_group = case_when(!is.na(Rh_group) ~ Rh_group,
                                Incubation == "lab" ~ "Lab: Rh only")) %>% 
    filter(!is.na(Rh_group)) %>% 
    filter(Q10 <= 10)

  means = Q10_data_rh %>% group_by(Rh_group) %>% dplyr::summarise(mean = mean(Q10),
                                                                  n = n())
  
  aov_rh_lab = 
    Q10_data_rh %>% filter(grepl("Rh", Rh_group)) %$% aov(Q10 ~ Rh_group) %>% summary()
  aov_rh_ra_field = 
    Q10_data_rh %>% filter(grepl("Field", Rh_group)) %$% aov(Q10 ~ Rh_group) %>% summary()
  
  gg_q10_rh = 
    Q10_data_rh %>% 
    filter(Q10 <= 10) %>% 
    ggplot(aes(x = Q10, color = Rh_group, fill = Rh_group))+
    geom_density(alpha = 0.1, size = 1)+
    geom_vline(data = means, aes(xintercept = mean, color = Rh_group), linetype = "dashed")+
    scale_fill_manual(values = PNWColors::pnw_palette("Bay", 3))+
    scale_color_manual(values = PNWColors::pnw_palette("Bay", 3))+
    labs(x = expression(bold("Q"[10])))+
    theme(legend.position = c(0.8, 0.8))+
    NULL
    
  list(means = means,
       aov_rh_lab = aov_rh_lab,
       aov_rh_ra_field = aov_rh_ra_field,
       gg_q10_rh = gg_q10_rh)
}


compute_stats_q10 <- function(Q10_data){
  # OLD, DEFUNCT
  
  
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
  
 #  # N2O ----
 #  n2o_aov_all = 
 #    Q10_data %>% 
 #    # filter(!is.na(Q10)) %>% 
 #    filter(Species == "N2O") %>% 
 #    do(fit_aov(.))
  
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
       #n2o_aov_all = n2o_aov_all,
       ch4_aov_all = ch4_aov_all)
}

make_graphs_q10 <- function(Q10_data){
  # OLD, DEFUNCT
  
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
  datapoints_co2 = 
    Q10_data %>% 
    filter(!is.na(Q10) & Species == "CO2") %>% 
    group_by(Species, Incubation, ClimateTypes) %>% 
    dplyr::summarise(n = n()) %>% 
    pivot_wider(names_from = "Incubation", values_from = "n")
  
  study_counts_co2 = 
    Q10_data %>% 
    filter(Species == "CO2") %>% 
    distinct(Q10_study_ID, Species, Incubation, ClimateTypes) %>% 
    group_by(Species, Incubation, ClimateTypes) %>% 
    dplyr::summarise(n = n()) %>% 
    pivot_wider(names_from = "Incubation", values_from = "n")
  
  total_datapoints = 
    Q10_data %>% 
    filter(!is.na(Q10)) %>% 
    group_by(Species, Incubation) %>% 
    dplyr::summarise(n = n()) %>% 
    pivot_wider(names_from = "Incubation", values_from = "n")
  
 total_studies = 
    Q10_data %>% 
    distinct(Q10_study_ID, Species, Incubation) %>% 
    group_by(Species, Incubation) %>% 
    dplyr::summarise(n = n()) %>% 
    pivot_wider(names_from = "Incubation", values_from = "n")
  
  list(datapoints_co2 = datapoints_co2,
       total_datapoints = total_datapoints,
       study_counts_co2 = study_counts_co2,
       total_studies = total_studies
       )
}



compute_ch4 = function(Q10_data){
  Q10_CH4_data = 
    Q10_data %>% 
    filter(Species == "CH4") %>% 
    filter(!is.na(Incubation))
  
  ch4_aov = 
    Q10_CH4_data %>% 
    do(fit_aov(.))
  
  gg_ch4_jitter = 
    Q10_CH4_data %>% 
    ggplot(aes(x = Incubation, y = Q10, color = Incubation))+
    geom_jitter(width = 0.2, size = 1)+
    labs(title = "CH4",
         x = "")+
    scale_color_manual(values = pal_incubation)+
    theme(legend.position = "none")+
    NULL
  
  ch4_sample_size = 
    Q10_CH4_data %>% 
    group_by(Incubation) %>% 
    dplyr::summarise(n = n())
  
  gg_ch4_raincloud = 
    Q10_CH4_data %>% 
    ggplot(aes(x = Incubation, y = Q10, color = Incubation))+
    ggdist::stat_halfeye(aes(fill = Incubation), 
                         size = 1, alpha = 0.5,
                         position = position_nudge(x = 0.2), width = 0.5)+
    geom_jitter(aes(color = Incubation), width = 0.1, )+
    geom_text(data = ch4_sample_size, aes(y = 95, label = paste("n =", n)), color = "black")+
    labs(#title = "CH4",
         x = "",
         y = expression(bold("Q"[10] * " for CH"[4])))+
    scale_color_manual(values = pal_incubation)+
    scale_fill_manual(values = pal_incubation)+
    theme(legend.position = "none",
          panel.grid.major.x = element_blank())+
    NULL
  
  list(ch4_aov = ch4_aov,
       gg_ch4_jitter = gg_ch4_jitter,
       gg_ch4_raincloud = gg_ch4_raincloud)
}






