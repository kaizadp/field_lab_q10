
# packages for map
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)


# general functions -------------------------------------------------------

reorder_temp_levels = function(dat){
  dat %>% 
    mutate(Temp_range = factor(Temp_range, levels = c("< 0", "0_5", "5_15", "15_25", "> 25"))) %>% 
    mutate(Species = factor(Species, levels = c("CO2", "CH4")))
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

make_map_all_studies <- function(Q10_data){
  world <- ne_countries(scale = "medium",  returnclass = "sf", type = "countries")
  
  Q10_map_data = 
    Q10_data %>% 
    filter(!is.na(Species)) %>% 
    distinct(Species, Latitude, Longitude, Incubation) %>% 
    drop_na()
  
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
    facet_wrap(~Species, ncol = 1)+
    NULL
}

plot_mat_map = function(Q10_data){
  
  Q10_data %>% 
    filter(!is.na(Species)) %>% 
    ggplot(aes(x = MAT, y = MAP))+
    geom_point(aes(color = ClimateTypes), size = 2)+    
    facet_wrap(~Species, ncol = 1)+
    theme_bw()+
    NULL
  
}



# analysis ----------------------------------------------------------------

compute_co2_all = function(Q10_data){
  ## comparing field vs. lab for all CO2 data, irrespective of incubation temperatures 
  
  # all data - all temperatures ----
# co2_aov_all = 
    Q10_data %>% 
    filter(Species == "CO2") %>% 
    do(fit_aov(.))
  
  #  (resp_q10_temp <- 
  Q10_data %>% 
    filter(Species == "CO2") %>% 
    ggplot(aes(x = Incubation, y = Q10, color = Incubation))+
    geom_jitter(width = 0.2, )+
    scale_y_log10()+
    labs(title = "CO2 - all data, all temperatures")+
    # ylim(0,20))
    NULL

  # all data - only temperature ranges <= 10 C
# co2_aov_tenC = 
    Q10_data %>% 
    filter(Species == "CO2" & Temp_diff <= 10) %>% 
    do(fit_aov(.))
  
    #  (resp_q10_temp <- 
    Q10_data %>% 
      filter(Species == "CO2" & Temp_diff <= 10) %>% 
      ggplot(aes(x = Incubation, y = Q10, color = Incubation))+
      geom_jitter(width = 0.2, )+
      scale_y_log10()+
      labs(title = "CO2 - all data, any 10C interval only")+
      # ylim(0,20))
      NULL
    
  
}

compute_co2_temp_range = function(Q10_data){
  
  # stats ----
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
  
  #
  # graphs ----
  
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
  
  
  nonsnow = 
    Q10_CO2_data %>% 
    filter(Species == "CO2" & ClimateTypes != "snow") %>%
    filter(Q10 < 300) %>% 
    ggplot(aes(x = Incubation, y = Q10, color = Incubation, group = Incubation))+
    geom_jitter(width = 0.2, size = 1)+
    facet_wrap(~ClimateTypes, ncol = 4)
  
  snow = 
    Q10_CO2_data %>% 
    filter(Species == "CO2" & ClimateTypes == "snow") %>%
    ggplot(aes(x = Incubation, y = Q10, color = Incubation, group = Incubation))+
    geom_jitter(width = 0.2, size = 1)+
    facet_wrap(~ClimateTypes, ncol = 4)+
    scale_y_log10()
  
  library(patchwork)
  
  
  nonsnow + snow + 
    plot_layout(widths = c(4, 1),
                guides = "collect") &
    theme(legend.position = "top") &
    labs(x = "")
  
  
  
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
  
  # graphs ----
  gg_boot_jitter = 
    bootstrap_long %>% 
    ggplot(aes(x = Incubation, y = Q10, color = Incubation))+
    geom_jitter(width = 0.2, size = 1)+
    facet_wrap(~Temp_range, strip.position = "bottom")+
    labs(title = "CO2",
         subtitle = "bootstrapped")+
    theme(legend.position = "none")
  
  gg_boot_density = 
    bootstrap_long %>% 
    ggplot(aes(x = Q10, fill = Incubation))+
    geom_density(alpha = 0.5)+
    facet_wrap(~Temp_range)
  
  
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
       gg_boot_density = gg_boot_density
       )
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









