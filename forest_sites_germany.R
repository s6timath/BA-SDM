#packages
library(sf)
library(tidyr)
library(tidyverse)
library(dplyr)
library(data.table)

#load data
forest_sites = read.csv(file="Y:/Home/matheis/data/N2k_habitatsforest_end2021.csv", header = TRUE, sep = ",")

n2k_forest_sites_germany = forest_sites %>% 
  filter(grepl('9110|9130|91D0|9410|9420|9140|9150|9170', habitatcode)) %>% 
  filter(grepl('DE', countrycode))
  
write.csv(n2k_forest_sites_germany, "Y:/Home/matheis/data/n2k_forest_sites_germany_end2021.csv", row.names = FALSE)
