


KoeppenGeigerASCII = readxl::read_xlsx("KoeppenGeigerASCII.xlsx")
KoeppenGeigerASCII %>% 
  ggplot(aes(x = Longitude, y = Latitude, color = ClimateTypes))+
  geom_point()


UDel_summarized_climate = read.csv("UDel_summarized_climate.csv")
UDel_summarized_climate %>% 
  ggplot(aes(x = Longitude, y = Latitude, color = MAT))+
  geom_point()



# Let's say sdata is what you are working on, and want to get climate classification, MAP and MAT for sdata based on latitude and longitude
# You can use join and get climate classification, MAT and MAP based on latitude and longitude
# however, before join, you need to do some works for your latitude and longitude, because latitude and longitude from the Koeppon and 
# global climate data are in 0.5 resolution (e.g., 0.25, 0.75, ...)
# the code below helps to do this, then use Latutude2 and Longitude2 to link with the Koeppon and Climate dataset

# You can find the description for different categories of climate classification from the link below
# https://en.wikipedia.org/wiki/K%C3%B6ppen_climate_classification

# The raw data from Willmott, C. J. and K. Matsuura (2001) can be find through the link below
# http://climate.geog.udel.edu/~climate/html_pages/download.html

sdata %>% mutate(Latitude2 = round(Latitude*2)/2,
                    Longitude2 = round(Longitude*2)/2,
                    Lat_dif = ifelse(Latitude2 - Latitude >=0, 0.25, -0.25),
                    Lon_dif = ifelse(Longitude2 - Longitude >=0, 0.25, -0.25),
                    Latitude2 = Latitude2 - Lat_dif,
                    Longitude2 = Longitude2 - Lon_dif) %>% 
  dplyr::select(-Lat_dif, -Lon_dif) -> 
  sdata

# Get Ecosystem class, MAT and MAP for srdb data
left_join(sdata, KoeppenGeigerASCII, by=c("Latitude2"="Latitude", "Longitude2"="Longitude")) ->
  sdata

# Get MAT and MAP
left_join(sdata, UDel_summarized_climate, by=c("Latitude2"="Latitude", "Longitude2"="Longitude")) ->
  sdata


# Bootstrap
# https://www.youtube.com/watch?v=m_osNICFFLY  # this is a youtube link to learn more about bootstrap, and you can find more learning material from google
# Bootstrap function
RESAMPLE <- function(n, x) {
  mean(sample(x, n, replace = TRUE))
}

# let's do an toy example
set.seed(123456) 
x1 = rnorm(n = 100, mean = 0, sd = 0.5)
x2 = rnorm(n = 1000, mean = 1.5, sd = 0.5)


# repeat how many times
N_SAMPLES <- 10000 # repeat how many times 
sample_size <- 10 # how many samples take every time 

library(dplyr)
bootstrap_out <- tibble(
  x1_boots = sapply(rep(sample_size, N_SAMPLES), RESAMPLE, x1),
  x2_boots = sapply(rep(sample_size, N_SAMPLES), RESAMPLE, x2)
)


mean(bootstrap_out$x1_boots)
sd(bootstrap_out$x1_boots)
mean(bootstrap_out$x2_boots)
sd(bootstrap_out$x2_boots)

# wilcox.test
wilcox.test(bootstrap_out$x1_boots,
            bootstrap_out$x2_boots,
            mu = 0, paired = TRUE, alternative = "two.sided", conf.level = 0.95)

library(tidyr)
library(ggplot2)

bootstrap_out %>%
  gather(Type) %>% 
  ggplot(aes(value, fill = Type)) +
  # theme_cowplot() +
  geom_density(stat = "density", alpha = 0.65) +
  theme(legend.position = c(0.50, 0.75)) 
  



