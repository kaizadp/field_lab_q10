

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



# determine field limits ----

field_limits = 
  Q10_data %>% 
  filter(Incubation == "field" & Species == "CO2") %>% 
  separate(Temp_range_rounded, sep = "_", into = c("Temp_min", "Temp_max")) %>% 
  group_by(Species, ClimateTypes) %>% 
  dplyr::summarise(temp_min_min = min(Temp_min, na.rm = TRUE),
                   temp_max_max = max(Temp_max, na.rm = TRUE))





# biome plots -------------------------------------------------------------




# Note raster masks 'tidyr::extract' and 'dplyr::select'
library(raster)
library(sp)
library(RColorBrewer)



# Download worldclim data for precip and tmean if necessary, into w10/ folder
precip <- getData("worldclim", path = "wc10/", var = "prec", res = 10, download = !file.exists("wc10/wc10/precip1.hdr"))
tmean <- getData("worldclim", path = "wc10/", var = "tmean", res = 10, download = !file.exists("wc10/wc10/tmean1.hdr"))


UDel_summarized_climate = read.csv("data/geographic_databases/UDel_summarized_climate.csv")




KoeppenGeigerASCII = readxl::read_xlsx("data/geographic_databases/KoeppenGeigerASCII.xlsx")
# KG climate zone map of all the globe
map_climate_regions_all = 
  KoeppenGeigerASCII %>% 
  mutate(ClimateTypes = case_when(grepl("A", ClimateTypes) ~ "equatorial",
                                  grepl("B", ClimateTypes) ~ "arid",
                                  grepl("C", ClimateTypes) ~ "temperate",
                                  grepl("D", ClimateTypes) ~ "snow",
                                  grepl("E", ClimateTypes) ~ "polar"))





# Pull out cosore dataset latitudes and longitudes
Q10_data %>%
  dplyr::select(Q10_record_number, Longitude, Latitude) -> cosore_coords

# Extract cosore location data from worldclim data for precip...
raster::extract(precip, cosore_coords[2:3]) -> precip_coords
apply(precip_coords, 1, sum) -> map_cosore
cbind(cosore_coords, map_cosore) -> map_coords

# ...and tmean
raster::extract(tmean, cosore_coords[2:3]) -> tmean_vals
apply(tmean_vals, 1, mean) -> mat_cosore
cbind(map_coords, mat_cosore)  %>%
  # Temp data is stored in degC * 10, so we need to divide to get back to degC
  mutate(mat_cosore = mat_cosore / 10) -> cosore_points
# Extract global climate space data
raster::as.data.frame(precip, xy = TRUE) %>%
  drop_na() -> precip_global
# Calculate annual sum for precip...
precip_global %>%
  dplyr::select(-x, -y) %>%
  apply(1, sum) -> map_global
raster::as.data.frame(tmean, xy = TRUE) %>%
  drop_na() -> tmean_global
# ...and mean for temperature
tmean_global %>%
  dplyr::select(-x, -y) %>%
  apply(1, mean) -> mat_global
# Create tibble with corresponding coordinates
tibble(x = tmean_global$x, y = tmean_global$y, mat = as.vector(mat_global)) -> mat
tibble(x = precip_global$x, y = precip_global$y, map = as.vector(map_global)) -> map
left_join(map, mat, by = c("x", "y")) %>%
  # Temp data is stored in degC * 10, so we need to divide to get back to degC
  mutate(mat = mat / 10) -> map_mat_global



# Try Mark Tjoelker's suggestion re Whittaker biomes
library(plotbiomes) # devtools::install_github("valentinitnelav/plotbiomes")
p_inset <- whittaker_base_plot() +
  geom_point(data = cosore_points, aes(x = mat_cosore, y = map_cosore / 10),
             color = "black", shape = 4) +
  coord_cartesian(ylim = c(0, 500)) +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.4, "lines"),
        legend.position = c(0.35, 0.75),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))
ggsave_quiet("figures/figure3-climate-inset.png")
# SP's main climate space plot
p <- ggplot() +
  geom_hex(data = map_mat_global,
           aes(x = mat, y = map / 10), bins = 100, na.rm = TRUE) +
  scale_fill_viridis_c(name = "Grid cells", begin = 0.85, end = 0) +
  geom_point(data = cosore_points, aes(x = mat_cosore, y = map_cosore / 10),
             color = "black", shape = 4, size = 1.5, na.rm = TRUE) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 500)) +
  labs(x = "MAT (°C)", y = "MAP (cm)")
print(p)
ggsave_quiet("figures/figure3-climate.pdf")
suppressMessages(library(cowplot, quietly = TRUE))
p_new <- ggdraw() +
  draw_plot(p) +
  draw_plot(p_inset, x = 0.1, y = 0.52, width = 0.4, height = 0.45)
print(p_new)






UDel_summarized_climate = read.csv("data/geographic_databases/UDel_summarized_climate.csv")
KoeppenGeigerASCII = readxl::read_xlsx("data/geographic_databases/KoeppenGeigerASCII.xlsx")

mat_map_biome = 
  #  dat %>% 
  #  mutate(Latitude2 = round(Latitude*2)/2,
  #         Longitude2 = round(Longitude*2)/2,
  #         Lat_dif = ifelse(Latitude2 - Latitude >=0, 0.25, -0.25),
  #         Lon_dif = ifelse(Longitude2 - Longitude >=0, 0.25, -0.25),
  #         Latitude2 = Latitude2 - Lat_dif,
  #         Longitude2 = Longitude2 - Lon_dif) %>% 
  #  dplyr::select(-Lat_dif, -Lon_dif) %>% 
  #  left_join(UDel_summarized_climate, by=c("Latitude2"="Latitude", "Longitude2"="Longitude")) %>% 
  
  UDel_summarized_climate %>% 
  left_join(KoeppenGeigerASCII, by=c("Latitude"="Latitude", "Longitude"="Longitude")) %>% 
  mutate(ClimateTypes = case_when(grepl("A", ClimateTypes) ~ "equatorial",
                                  grepl("B", ClimateTypes) ~ "arid",
                                  grepl("C", ClimateTypes) ~ "temperate",
                                  grepl("D", ClimateTypes) ~ "snow",
                                  grepl("E", ClimateTypes) ~ "polar")) 


hull_data = 
  mat_map_biome %>% 
  group_by(ClimateTypes) %>% 
  slice(chull(MAT, MAP))

hull_data %>% 
  ggplot(aes(x = MAT, y = MAP/10))+
  geom_polygon(aes(fill = ClimateTypes))
geom_point()


mat_map_biome %>% 
  ggplot(aes(x = MAT, y = MAP/10, color = ClimateTypes))+
  geom_point()+
  geom_polygon(data = hull_data, aes(fill = ClimateTypes, color = ClimateTypes), alpha = 0.3)+
  ylim(0, 450)


mat_map_biome %>% 
  ggplot(aes(x = MAT, y = MAP/10, color = ClimateTypes))+
  geom_point()+
  # stat_ellipse()+
  ylim(0, 450)


library(tidyverse)
load("data/Whittaker_biomes.rda")


ggplot2::ggplot() +
  # add biome polygons
  ggplot2::geom_polygon(data = Whittaker_biomes,
                        ggplot2::aes(x    = temp_c,
                                     y    = precp_cm,
                                     fill = biome),
                        # adjust polygon border
                        colour = "gray98",
                        size   = 1) 
# fill the polygons with predefined colors
ggplot2::scale_fill_manual(name   = "Whittaker biomes",
                           breaks = names(color_palette),
                           labels = names(color_palette),
                           values = color_palette) +
  ggplot2::scale_x_continuous(xlabel) +
  ggplot2::scale_y_continuous('Precipitation (cm)')


ggplot2::ggplot() +
  # add biome polygons
  ggplot2::geom_point(data = Whittaker_biomes,
                      ggplot2::aes(x    = temp_c,
                                   y    = precp_cm,
                                   color = biome),
                      # adjust polygon border
                      colour = "gray98",
                      size   = 1) 


Whittaker_biomes2 = 
  Whittaker_biomes %>% 
  mutate(Koppen = case_when(biome == "Boreal forest" ~ "snow",
                            grepl("desert", biome) ~ "arid",
                            grepl("Temperate", biome) ~ "temperate",
                            grepl("Tropical", biome) ~ "equatorial",
                            biome == "Tundra" ~ "polar"
  ))



ggplot2::ggplot() +
  # add biome polygons
  ggplot2::geom_polygon(data = Whittaker_biomes2,
                        ggplot2::aes(x    = temp_c,
                                     y    = precp_cm,
                                     fill = Koppen),
                        # adjust polygon border
                        # colour = "gray98",
                        alpha = 0.5,
                        size   = 1) +
  geom_point(data = Q10_data, aes(x = MAT, y = MAP/10, fill = ClimateTypes), shape =21, color = "black")


# fill the polygons with predefined colors
ggplot2::scale_fill_manual(name   = "Whittaker biomes",
                           breaks = names(color_palette),
                           labels = names(color_palette),
                           values = color_palette) +
  ggplot2::scale_x_continuous(xlabel) +
  ggplot2::scale_y_continuous('Precipitation (cm)') +
  
  
  
  remotes::install_github("garretrc/ggvoronoi", dependencies = TRUE, build_opts = c("--no-resave-data"))
library(ggvoronoi)


set.seed(1)
x <- sample(1:400, size = 100)
y <- sample(1:400, size = 100)
dist <- sqrt((x - 200) ^ 2 + (y - 200) ^ 2)

df <- data.frame(x, y, dist = dist)


ggplot(df, aes(x, y)) +
  stat_voronoi(geom = "path")


ggplot(df, aes(x, y, fill = dist)) +
  geom_voronoi()+
  stat_voronoi(geom = "path") +
  geom_point()


#
# Ra vs Rh plots ----------------------------------------------------------

Q10_data2 = 
  Q10_data %>% 
  mutate(RC_group = cut(RC_annual, c(-0.1, 0.5, 1.0), labels = c("Heterotrophic-dominated", "Autotrophic-dominated")))

# autotrophic vs. heterotrophic

Q10_data_auto = 
  Q10_data2 %>% 
  filter(!is.na(RC_group))

Q10_data_auto %>% 
  ggplot(aes(x = Temp_range, y = Q10, color = RC_group))+
  geom_jitter()

Q10_data_auto %>% 
  rbind(Q10_data %>% filter(Incubation == "lab") %>% mutate(RC_group = "lab")) %>% 
  filter(Q10 <= 10) %>% 
  ggplot(aes(x = Q10, color = RC_group, fill = RC_group))+
  geom_density(alpha = 0.1, size = 1)+
  geom_vline(data = means, aes(xintercept = mean, color = RC_group), linetype = "dashed")+
  theme(legend.position = c(0.8, 0.8))


Q10_data_auto %$% aov(Q10 ~ RC_group) %>% summary()
means = Q10_data_auto %>% group_by(RC_group) %>% dplyr::summarise(mean = mean(Q10))


library(magrittr)


Q10_data_auto %>% 
  ggplot(aes(x = Q10, color = RC_group))+
  geom_density()+
  facet_wrap(~Temp_range_rounded)


#
# getting Rh data from studies ----
SRDB_FILE_PATH_DATA = "data/CO2/SRDB_V5_1827/data/srdb-data-V5.csv"
SRDB_FILE_PATH_EQ = "data/CO2/SRDB_V5_1827/data/srdb-equations-V5.csv"
SRDB_FILE_PATH_STUDIES = "data/CO2/SRDB_V5_1827/data/srdb-studies-V5.csv"


srdb_v5_data <- read.csv(SRDB_FILE_PATH_DATA)
srdb_v5_equations <- read.csv(SRDB_FILE_PATH_EQ)
srdb_v5_studies <- read.csv(SRDB_FILE_PATH_STUDIES)
srdb_v5_rh_list = read.csv("data/CO2/SRDB_v5_rh_studies.csv")

rh_list = 
  srdb_v5_rh_list %>% 
  left_join(srdb_v5_studies)


rh_list %>% write.csv("data/srdb_v5_list_of_rh_studies.csv", row.names = F)


rh_data = read.csv("data/srdb_v5_list_of_rh_studies_with_q10.csv")

rh_data %>% 
  filter(respiration_type %in% c("Rh", "Ra")) %>% 
  ggplot(aes(x = respiration_type, y = Q10))+
  geom_violin()+
  geom_jitter(width = 0.2)+
  NULL

rh_data %>% 
  filter(respiration_type %in% c("Rh", "Ra")) %$%
  aov(Q10 ~ respiration_type) %>% 
  summary()
