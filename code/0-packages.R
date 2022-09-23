# FIELD VS. LAB GHG Q10
# PATEL ET AL. 2022
# 
# This script contains packages needed to run the functions
# and custom ggplot theme.
# Source this in the subsequent scripts to load packages
# kfp 2022
# 
# ############################# #

library(tidyverse)
library(drake)
library(nlme) # for LME
library(rnaturalearth) # for world maps
library(rnaturalearthdata) # for world maps
library(sf) # for world maps
#devtools::install_github('SoilBGC-Datashare/sidb/Rpkg')
library(sidb) # Soil Incubation Database 
library(data.table)


theme_kp <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "top",
          legend.key=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'lines'),
          legend.background = element_rect(colour = NA),
          panel.border = element_rect(color="black",size=1, fill = NA),
          
          plot.title = element_text(hjust = 0, size = 14),
          axis.text = element_text(size = 12, color = "black"),
          axis.title = element_text(size = 14, face = "bold", color = "black"),
          
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour="white", fill="white"), #facet formatting
          panel.spacing.x = unit(1, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=12, face="bold"), #facet labels
          strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
    )
}

theme_set(theme_kp())
