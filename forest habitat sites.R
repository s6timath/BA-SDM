#packages
library(sf)
library(tidyr)
library(tidyverse)
library(dplyr)
library(data.table)

#load data
forest_sites = read.csv(file="Y:/Home/matheis/data/n2k raw data/habitats_Sites.csv", header = TRUE, sep = ",")

#remove unnecessary columns
fscleaned <- subset(forest_sites, select = -c(sitename, objectid, description, type, bioregion, habitat_image_url, country))
fsrest <- subset(forest_sites, select = -c(sitename, habitatcode, objectid, description, type, bioregion, habitat_image_url, country))

#aggregate sitecodes
fshcleaned = setDT(fscleaned)[, .(habitatcode = toString(habitatcode), number_of_habitats = .N), by = sitecode]

fsrest <- subset(forest_sites, select = -c(sitename, habitatcode, objectid, description, type, bioregion, habitat_image_url, country))

#merge
sites_forest_habitats = left_join(fshcleaned, fsrest, by = "sitecode", multiple = "any", relationship = "one-to-many")

#save data  
write.csv(sites_forest_habitats, "Y:/Home/matheis/data/N2k_habitatsforest_end2021.csv", row.names = FALSE)
