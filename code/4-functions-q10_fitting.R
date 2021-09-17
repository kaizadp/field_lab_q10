## ERL Q10 review

## FITTING Q10 FUNCTIONS TO RESPIRATION DATA
## 2021-09-06: testing on SIDB first, then moving on to other datasets

######################## #
######################## #

library(tidyverse)
library(data.table)
library(sidb)


# SIDb (Soil Incubation Database) ---------------------------------
SIDB_FILE_PATH = "data/CO2/sidb.RData"

# function to clean SIDB data
clean_sidb_data <- function(sidb_vars, sidb_timeseries){
  sidb_vars_clean <- 
    sidb_vars %>% 
    filter(!units %in% c("permille", "percentC14Remaining")) %>% 
    filter(is.na(elevatedCO2) | elevatedCO2 == "control") %>% 
    filter(is.na(glucose)) %>% 
    filter(is.na(cellulose) | cellulose == "control")
  
  sidb_timeseries_clean <- 
    sidb_timeseries %>% 
    filter(time <= 370) %>% 
    left_join(sidb_vars_clean %>% dplyr::select(ID, temperature, units, citationKey)) %>% 
    drop_na() 
  
  # keep only data with multiple temperature levels
  temp_count <-
    sidb_timeseries_clean %>% 
    distinct(citationKey, temperature) %>%
    group_by(citationKey) %>% 
    dplyr::mutate(n = n())
  
  sidb_timeseries_clean2 <- 
    sidb_timeseries_clean %>% 
    left_join(temp_count) %>% 
    filter(n > 1) %>% 
    dplyr::select(-n)
  
  
  list(sidb_vars_clean = sidb_vars_clean,
       sidb_timeseries_clean2 = sidb_timeseries_clean2)
}

#

# load and process SIDB data
load(SIDB_FILE_PATH)
sidb_flat = flatterSIDb(sidb)
sidb_timeseries = dplyr::bind_rows(sidb_flat$timeseries)
sidb_vars = rbindlist(sidb_flat$vars, fill = TRUE)


sidb_timeseries_clean = clean_sidb_data(sidb_vars, sidb_timeseries)$sidb_timeseries_clean2 %>% 
  filter(response > 0) # remove instances where resp == 0, because (a) that's probably wrong, and (b) it fucks up the model fitting 

# subsetting cleaned data for code creation/testing
sidb_test_data_andrews = sidb_timeseries_clean %>% filter(citationKey == "Andrews2000SBB")
sidb_test_data_barrett = sidb_timeseries_clean %>% filter(citationKey == "Barrett2006")
sidb_test_data_zhang = sidb_timeseries_clean %>% filter(citationKey == "Zhang2007")



# FITTING Q10 PARAMETERS --------------------------------------------------

fit_q10_parameters <- function(dat){
  # equations: https://journals.sagepub.com/doi/pdf/10.1177/1940082920914902
  
  #browser()#    tryCatch()
  coefs <- data.frame(n = nrow(dat), err = NA_character_)
  
  fit_exponential = function(dat){
    # Rs = a * exp(b * T)
    
    tryCatch({
      ## not doing this piece any more, using estimates based on initial tests
      ## # Estimate the a and b parameters using a linear model...
      ## m <- lm(log(response) ~ temperature, data = dat)
      ## a_start <- exp(coef(m)[1])
      ## b_start <- coef(m)[2]
      
      # ...and then fit the nonlinear model using these starting values
      curve.nls <- nls(response ~ a * exp(temperature * b),
                       start = list(a = 1, b = 0.05),
                       data = dat)
      
      # get goodness of fit? correlation
      # https://datascienceplus.com/first-steps-with-non-linear-regression-in-r/
      cor.exponential = cor(dat$response, predict(curve.nls))
      
      # RMSE and R2
      res = residuals(curve.nls)
      rmse = ((sum(res)^2)/nrow(dat))^0.5 %>% round(3)
      rsquared = 1- (((sum(res))^2) / (sum((dat$response - mean(dat$response))^2)))
      
      
      # make dataframe of coefficients, add correlation
      coefs.exponential = data.frame(as.list(coef(curve.nls))) %>% mutate(cor = cor.exponential)
      names(coefs.exponential) = c("a", "b", "gof_cor")
      
      # cbind coefficients with the blank coefs file
      coefs_full_exp <- 
        cbind(coefs, coefs.exponential) %>% 
        mutate(fun = "exponential",
               form = "Rs = a * exp(b * T)",
               gof_rmse = rmse,
               gof_rsquared = rsquared,
               R10 = a * exp(b * 10),
               R00 = a * exp(b * 0),
               R05 = a * exp(b * 5),
               R15 = a * exp(b * 15),
               R25 = a * exp(b * 25),
               R35 = a * exp(b * 35),
               R45 = a * exp(b * 45)
        )
      
    },
    error = function(e) {
      coefs$err <<- e$message
    }
    )  # end of tryCatch
    
    coefs_full_exp
  }
  fit_lloyd_taylor = function(dat){
    # Rs = R10 * exp(308.56 * ((1/56) - (1/(T+46))))
    
    curve.lloyd.taylor = lm(response ~ exp(308.56 * ((1/56) - (1/(temperature + 46)))),
                            data = dat)
    
    # get goodness of fit? correlation
    cor.lloyd = cor(dat$response, predict(curve.lloyd.taylor))

    R10 = coef(curve.lloyd.taylor)[2]
    rsquared.lloyd = summary(curve.lloyd.taylor)$adj.r.squared

    # RMSE and R2
    res = residuals(curve.lloyd.taylor)
    rmse = ((sum(res)^2)/nrow(dat))^0.5 %>% round(3)
    rsquared = 1- (((sum(res))^2) / (sum((dat$response - mean(dat$response))^2)))
        
    coefs_full_lt <-
      coefs %>% 
      mutate(fun = "Lloyd-Taylor",
             form = "Rs = R10 * exp(308.56 * ((1/56) - (1/(T+46))))",
             gof_cor = cor.lloyd,
             adj_rsquared = rsquared.lloyd,
             gof_rmse = rmse,
             gof_rsquared = rsquared,
             R10 = R10,
             R00 = R10 * exp(308.56 * ((1/56) - (1/(0+46)))),
             R05 = R10 * exp(308.56 * ((1/56) - (1/(5+46)))),
             R15 = R10 * exp(308.56 * ((1/56) - (1/(15+46)))),
             R25 = R10 * exp(308.56 * ((1/56) - (1/(25+46)))),
             R35 = R10 * exp(308.56 * ((1/56) - (1/(35+46)))),
             R45 = R10 * exp(308.56 * ((1/56) - (1/(45+46))))
      )
    
    coefs_full_lt
  }
  fit_quadratic = function(dat){
    # Rs  = a + b*T + c*(T^2)

    ## some studies have only two temperature levels
    ## we need > 2 levels (distinct points) for a quadratic fit
    ## so, create a loop to only fit the quadratic function if n-observations > 2
    
    n_obs = 
      dat %>% 
      distinct(citationKey, temperature) %>% 
      group_by(citationKey) %>% 
      dplyr::summarize(n = n()) %>% pull(n)
    
    coefs_full_quad = 
      if(n_obs <= 2){
      data.frame()
    } else {
    curve.quadratic <- lm(response ~ poly(temperature, 2),
                          data = dat)
    
    # get goodness of fit? correlation
    cor.quadratic = cor(dat$response, predict(curve.quadratic))
    
    # RMSE and R2
    res = residuals(curve.quadratic)
    rmse = ((sum(res)^2)/nrow(dat))^0.5 %>% round(3)
    rsquared = 1- (((sum(res))^2) / (sum((dat$response - mean(dat$response))^2)))
    
    # make dataframe of coefficients, add correlation
    coefs.quadratic = data.frame(as.list(coef(curve.quadratic))) %>% mutate(cor = cor.quadratic)
    names(coefs.quadratic) = c("a", "b", "c", "gof_cor")
    
    # cbind coefficients with the blank coefs file
    #coefs_full_quad <- 
      cbind(coefs, coefs.quadratic) %>% 
      mutate(fun = "quadratic",
             form = "Rs  = a + b*T + c*(T^2)",
             gof_rmse = rmse,
             gof_rsquared = rsquared,
             R10 = a + (b*10) + (c*10*10),
             R00 = a + (b*0) + (c*0*0),
             R05 = a + (b*5) + (c*5*5),
             R15 = a + (b*15) + (c*15*15),
             R25 = a + (b*25) + (c*25*25),
             R35 = a + (b*35) + (c*35*35),
             R45 = a + (b*45) + (c*45*45))
    
    }
    coefs_full_quad
  }
  fit_arrhenius = function(dat){
    # Rs = a * exp(-Ea / (8.314 * T))

    tryCatch({
      # Estimate the a and b parameters using a linear model...
      dat = dat %>% mutate(temp_inv = 1/temperature)
      m <- lm(log(response) ~ (temp_inv), data = dat)
      a_start <- exp(coef(m)[1])
      b_start <- coef(m)[2] * (-8.314)
      
      # ...and then fit the nonlinear model using these starting values
      curve.arrhenius = nls(response ~ a * exp(-Ea/(8.314 * temperature)),
                            start = list(a = a_start, Ea = b_start), 
                            data = dat)
    
    # get goodness of fit? correlation
    cor.arrhenius = cor(dat$response, predict(curve.arrhenius))
    
    # RMSE and R2
    res = residuals(curve.arrhenius)
    rmse = ((sum(res)^2)/nrow(dat))^0.5 %>% round(3)
    rsquared = 1- (((sum(res))^2) / (sum((dat$response - mean(dat$response))^2)))
    
    
    # make dataframe of coefficients, add correlation
    coefs.arrhenius = data.frame(as.list(coef(curve.arrhenius))) %>% mutate(cor = cor.arrhenius)
    names(coefs.arrhenius) = c("a", "b", "gof_cor")    
    
    coefs_full_ar <- 
      cbind(coefs, coefs.arrhenius) %>% 
      mutate(fun = "arrhenius",
             form = "Rs = a * exp(-b / (8.314 * T))",
             notes = "b = Ea",
             gof_rmse = rmse,
             gof_rsquared = rsquared,
             R10 = a * exp(-b / (8.314 * 10)),
             R00 = a * exp(-b / (8.314 * 0)),
             R05 = a * exp(-b / (8.314 * 5)),
             R15 = a * exp(-b / (8.314 * 15)),
             R25 = a * exp(-b / (8.314 * 25)),
             R35 = a * exp(-b / (8.314 * 35)),
             R45 = a * exp(-b / (8.314 * 45)))
    },
    error = function(e) {
      coefs$err <<- e$message
    }
    )  # end of tryCatch
    
    coefs_full_ar
  }
  
  coefs_full_combined = 
    bind_rows(fit_quadratic(dat), 
              fit_exponential(dat), fit_lloyd_taylor(dat), 
              fit_arrhenius(dat)
              )
  
  coefs_full_combined
}

# testing on Andrews2000SBB data
coefs_andrews = fit_q10_parameters(sidb_test_data_andrews)
coefs_barrett = fit_q10_parameters(sidb_test_data_barrett)
dat = sidb_test_data_zhang

# run the parameters function, for each study
q10_parameters_combined = 
  sidb_timeseries_clean %>%
  filter(citationKey != "Crow2019b") %>% 
  group_by(citationKey) %>%
  do(fit_q10_parameters(.)) %>% 
  mutate_if(is.numeric, ~round(., 3))

q10_calculated = 
  sidb_timeseries_clean %>%
  
  # first, set temp ranges
  group_by(citationKey) %>% 
  dplyr::summarise(temp_range_min = min(temperature),
                   temp_range_max = max(temperature)) %>% 
  left_join(q10_parameters_combined) %>% 
  
  # next, calculate Q10 for each temp range, as Q10 = Rx + R(x+10)/Rx
  mutate(Q10_5_15 = case_when(temp_range_min < 15 ~ (R05+R15)/R05),
         Q10_15_25 = case_when(temp_range_min < 25 ~ (R15+R25)/R15),
         Q10_25_35 = case_when(temp_range_min < 35 ~ (R25+R35)/R25),
         Q10_35_45 = case_when(temp_range_min < 45 & temp_range_max > 35 ~ (R35+R45)/R35)) %>% 
  dplyr::select(-c(R00, R05, R15, R25, R35, R45)) %>% 
  relocate(citationKey, temp_range_min, temp_range_max, n,
           fun, form, a, b, c)




##############

## plotting the functions

sidb_timeseries_clean_with_Q10 = 
  sidb_timeseries_clean %>% 
  left_join(q10_calculated %>% dplyr::select(citationKey, a, b, c)) %>% 
  filter(citationKey != "Crow2019b") 

sidb_timeseries_clean_with_Q10 %>% 
  ggplot(aes(x = temperature, y = response))+
  geom_point()+
  geom_function(fun = ~ a * exp(.x * b, color = "red"))+
  facet_wrap(~ citationKey, scales = "free_y")
## ^ this does not work

## trying a different way to plot the functions
## create a mock dataframe with temp values 1:40 and merge with the Q10 parameters
## run the functions

x = as.tibble(c(1:40))

q10_params_subset = 
  q10_calculated %>% 
  dplyr::select(citationKey, fun, a, b, c, R10) %>% 
  full_join(x, by = character()) %>% 
  rename(x = value)

q10_params_subset2 = 
  q10_params_subset %>% 
  mutate(y = 
           case_when(fun == "exponential" ~ a * exp(x * b),
                     fun == "lloyd_taylor" ~ R10 * exp(308.56 * ((1/56) - (1/(x+46)))),
                     #fun == "quadratic" ~ a + b*x + c*(x^2),
                     fun == "arrhenius" ~ a * exp(-b / (8.314 * x))))


ggplot()+
  # plot the functions
  geom_line(data = q10_params_subset2, aes(x = x, y = y, color = fun))+
  # plot the respiration data points
  geom_point(data = sidb_timeseries_clean, aes(x = temperature, y = response))+
  # plot the mean respiration per temperature
  geom_point(data = sidb_timeseries_clean %>% 
               group_by(citationKey, temperature) %>% 
               dplyr::summarise(response = mean(response)),
             aes(x = temperature, y = response), color = "blue", shape = 21, size = 5)+
  facet_wrap(~ citationKey, scales = "free_y")
