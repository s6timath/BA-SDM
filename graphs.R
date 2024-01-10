#loding all needed libraries
library(terra)
library(sf)
library(raster)
library(rgdal)
library(tidyverse)
library(data.table)
library(randomForest)
library(lattice)
library(RColorBrewer)
library(PresenceAbsence)
library(mecofun)
library(dismo)
library(exactextractr)   
library(tidyterra)
library(viridis)

#for  traceability
sessionInfo()

for (package_name in sort(loadedNamespaces())) {
  print(paste(package_name, packageVersion(package_name)))
}


#definitions
pred <- c('precipRange',
  'P10_MeanTempWarmestQuarter',
  'P15_PrecipSeasonality',
  'P4_TempSeasonality')
path= c("/work/matheis/model/prediction/projection/")
crs = c("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80
+units=m +no_defs")
rcp = c('rcp45')
quantile = c('5','50','95')
habitatcodes=c('9110','9130','91D0','9410','9420','9140','9150','9170')

#reference data 
modelpicea <- readRDS('/work/matheis/model/train/model_Picea.abies_block_3_rf.RDS')
modelfagus <- readRDS('/work/matheis/model/train/model_Fagus.sylvatica_block_5_rf.RDS')

picea.abies_ref <- rast('/gpfs1/work/matheis/model/prediction/projection/tif/picea1971-1990.tif')
crs(picea.abies_ref) <- crs

ref_prediction_picea <- read.table('/gpfs1/work/matheis/model/prediction/projection/csv/picea1971-1990.csv', header=T, sep=',')
fagus.sylvatica_ref <- rast('/gpfs1/work/matheis/model/prediction/projection/tif/fagus1971-1990.tif')
crs(fagus.sylvatica_ref) <- crs

ref_prediction_fagus <- read.table('/gpfs1/work/matheis/model/prediction/projection/csv/fagus1971-1990.csv', header=T, sep=',')
roi <- terra::vect("/gpfs1/work/matheis/n2k/n2kselected.shp")
roi
crs(roi) <- crs

#projections
#pro95_fagus <- rast("/gpfs1/work/matheis/model/prediction/projection/tif/Fagus.sylvatica_block_5_rf_2079_2098_quantile95_.tif")
#pro5_fagus <- rast("/gpfs1/work/matheis/model/prediction/projection/tif/Fagus.sylvatica_block_5_rf_2079_2098_quantile5_.tif")
pro50_fagus <- rast("/gpfs1/work/matheis/model/prediction/projection/tif/Fagus.sylvatica_block_5_rf_2079_2098_quantile50_.tif")
print(minmax(pro50_fagus))

#pro95_picea <- rast("/gpfs1/work/matheis/model/prediction/projection/tif/Picea.abies_block_3_rf_2079_2098_quantile95_.tif")
#pro5_picea <- rast("/gpfs1/work/matheis/model/prediction/projection/tif/Picea.abies_block_3_rf_2079_2098_quantile5_.tif")
pro50_picea <- rast("/gpfs1/work/matheis/model/prediction/projection/tif/Picea.abies_block_3_rf_2079_2098_quantile50_.tif")
print(minmax(pro50_picea))

#calculate difference
difffagus <- fagus.sylvatica_ref - pro50_fagus
print(minmax(difffagus))

diffpicea <- picea.abies_ref - pro50_picea
print(minmax(diffpicea))

decreasefagus <- difffagus / fagus.sylvatica_ref
print(minmax(decreasefagus))
decreasepicea <- diffpicea / picea.abies_ref
print(minmax(decreasepicea))

resultshp <- terra::vect("/work/matheis/results/reference.shp")

#create subset for each habitat type
for (i in habitatcodes){
filename <- paste0('/work/matheis/results/subset',i,'full.shp')
roihabitat <- dplyr::filter(roi, grepl(i, habttcd))
writeVector(roihabitat, filename, overwrite=TRUE)
}

#load habitat subsets
habitat9170 <- vect("/work/matheis/results/subset9170full.shp")
crs(habitat9170) <- crs
habitat9170

habitat9150 <- vect("/work/matheis/results/subset9150full.shp")
crs(habitat9150) <- crs

habitat9140 <- vect("/work/matheis/results/subset9140full.shp")
crs(habitat9140) <- crs

habitat9420 <- vect("/work/matheis/results/subset9420full.shp")
crs(habitat9420) <- crs

habitat9410 <- vect("/work/matheis/results/subset9410full.shp")
crs(habitat9410) <- crs

habitat91D0 <- vect("/work/matheis/results/subset91D0full.shp")
crs(habitat91D0) <- crs

habitat9130 <- vect("/work/matheis/results/subset9130full.shp")
crs(habitat9130) <- crs

habitat9110 <- vect("/work/matheis/results/subset9110full.shp")
crs(habitat9110) <- crs


#########all
#fagus
results  <- terra::extract(fagus.sylvatica_ref, 
                                   resultshp, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9110 <- cbind(resultshp, results)
summary(results9110)
png(paste0('/work/matheis/results/allfagusref.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = fagus1971.1990), data=results9110, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted present occurrence probabilities for Fagus sylvatica", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(pro50_fagus,
                                   resultshp, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9110 <- cbind(resultshp, results)
summary(results9110)
png(paste0('/work/matheis/results/allfagusproj.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = Fagus.sylvatica_block_5_rf_2079_2098_quantile50_), data=results9110, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +  
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted future occurrence probabilities for Fagus sylvatica", fill="Occurrence Probability") 
dev.off()

results  <- terra::extract(difffagus, 
                                   resultshp, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE) 
results9110 <- cbind(resultshp, results)
summary(results9110)
png(paste0('/work/matheis/results/allfagusdiff.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = fagus1971.1990), data=results9110, colour = NA) +
  scale_fill_viridis_c(limits = c(-0.25, 0.5), na.value = "transparent", direction = -1) +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Difference between predicted future and present occurrence probabilities for Fagus sylvatica", fill="Difference in Occurrence Probability")
dev.off()

#picea
results  <- terra::extract(picea.abies_ref, 
                                   resultshp, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9110 <- cbind(resultshp, results)
summary(results9110)
png(paste0('/work/matheis/results/allpicearef.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = picea1971.1990), data=results9110, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted present occurrence probabilities for Picea abies", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(pro50_picea,
                                   resultshp, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9110 <- cbind(resultshp, results)
summary(results9110)
png(paste0('/work/matheis/results/allpiceaproj.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = Picea.abies_block_3_rf_2079_2098_quantile50_), data=results9110, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted future occurrence occurrence probabilities for Picea abies", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(diffpicea, 
                                   resultshp, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE) 
results9110 <- cbind(resultshp, results)
summary(results9110)
png(paste0('/work/matheis/results/allpiceadiff.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = picea1971.1990), data=results9110, colour = NA) +
  scale_fill_viridis_c(limits = c(-0.25, 0.5), na.value = "transparent", direction = -1) +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Difference between predicted future and present occurrence probabilities for Picea abies", fill="Difference in Occurrence Probability")
dev.off()



#############
#habitats####
#############
##9110
#fagus
results  <- terra::extract(fagus.sylvatica_ref, 
                                   habitat9110, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9110 <- cbind(habitat9110, results)
summary(results9110)
png(paste0('/work/matheis/results/9110fagusref.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = fagus1971.1990), data=results9110, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted present occurrence probabilities in Habitat 9110 for Fagus sylvatica", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(pro50_fagus,
                                   habitat9110, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9110 <- cbind(habitat9110, results)
summary(results9110)
png(paste0('/work/matheis/results/9110fagusproj.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = Fagus.sylvatica_block_5_rf_2079_2098_quantile50_), data=results9110, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +  
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted future occurrence probabilities in Habitat 9110 for Fagus sylvatica", fill="Occurrence Probability") 
dev.off()

results  <- terra::extract(difffagus, 
                                   habitat9110, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE) 
results9110 <- cbind(habitat9110, results)
summary(results9110)
png(paste0('/work/matheis/results/9110fagusdiff.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = fagus1971.1990), data=results9110, colour = NA) +
  scale_fill_viridis_c(limits = c(-0.25, 0.5), na.value = "transparent", direction = -1) +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Difference between predicted future and present occurrence probabilities in Habitat 9110 for Fagus sylvatica", fill="Difference in Occurrence Probability")
dev.off()

#picea
results  <- terra::extract(picea.abies_ref, 
                                   habitat9110, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9110 <- cbind(habitat9110, results)
summary(results9110)
png(paste0('/work/matheis/results/9110picearef.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = picea1971.1990), data=results9110, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted present occurrence probabilities in Habitat 9110 for Picea abies", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(pro50_picea,
                                   habitat9110, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9110 <- cbind(habitat9110, results)
summary(results9110)
png(paste0('/work/matheis/results/9110piceaproj.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = Picea.abies_block_3_rf_2079_2098_quantile50_), data=results9110, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted future occurrence occurrence probabilities in Habitat 9110 for Picea abies", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(diffpicea, 
                                   habitat9110, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE) 
results9110 <- cbind(habitat9110, results)
summary(results9110)
png(paste0('/work/matheis/results/9110piceadiff.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = picea1971.1990), data=results9110, colour = NA) +
  scale_fill_viridis_c(limits = c(-0.25, 0.5), na.value = "transparent", direction = -1) +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Difference between predicted future and present occurrence probabilities in Habitat 9110 for Picea abies", fill="Difference in Occurrence Probability")
dev.off()

#9130
#fagus
results  <- terra::extract(fagus.sylvatica_ref, 
                                   habitat9130, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9130 <- cbind(habitat9130, results)
summary(results9130)
png(paste0('/work/matheis/results/9130fagusref.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = fagus1971.1990), data=results9130, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted present occurrence probabilities in Habitat 9130 for Fagus sylvatica", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(pro50_fagus,
                                   habitat9130, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9130 <- cbind(habitat9130, results)
summary(results9130)
png(paste0('/work/matheis/results/9130fagusproj.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = Fagus.sylvatica_block_5_rf_2079_2098_quantile50_), data=results9130, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +  
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted future occurrence probabilities in Habitat 9130 for Fagus sylvatica", fill="Occurrence Probability") 
dev.off()

results  <- terra::extract(difffagus, 
                                   habitat9130, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE) 
results9130 <- cbind(habitat9130, results)
summary(results9130)
png(paste0('/work/matheis/results/9130fagusdiff.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = fagus1971.1990), data=results9130, colour = NA) +
  scale_fill_viridis_c(limits = c(-0.25, 0.5), na.value = "transparent", direction = -1) +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Difference between predicted future and present occurrence probabilities in Habitat 9130 for Fagus sylvatica", fill="Difference in Occurrence Probability")
dev.off()

#picea
results  <- terra::extract(picea.abies_ref, 
                                   habitat9130, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9130 <- cbind(habitat9130, results)
summary(results9130)
png(paste0('/work/matheis/results/9130picearef.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = picea1971.1990), data=results9130, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted present occurrence probabilities in Habitat 9130 for Picea abies", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(pro50_picea,
                                   habitat9130, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9130 <- cbind(habitat9130, results)
summary(results9130)
png(paste0('/work/matheis/results/9130piceaproj.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = Picea.abies_block_3_rf_2079_2098_quantile50_), data=results9130, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted future occurrence probabilities in Habitat 9130 for Picea abies", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(diffpicea, 
                                   habitat9130, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE) 
results9130 <- cbind(habitat9130, results)
summary(results9130)
png(paste0('/work/matheis/results/9130piceadiff.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = picea1971.1990), data=results9130, colour = NA) +
  scale_fill_viridis_c(limits = c(-0.25, 0.5), na.value = "transparent", direction = -1) +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Difference between predicted future and present occurrence probabilities in Habitat 9130 for Picea abies", fill="Difference in Occurrence Probability")
dev.off()

#91D0
#fagus
results  <- terra::extract(fagus.sylvatica_ref, 
                                   habitat91D0, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results91D0 <- cbind(habitat91D0, results)
summary(results91D0)
png(paste0('/work/matheis/results/91D0fagusref.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = fagus1971.1990), data=results91D0, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted present occurrence probabilities in Habitat 91D0 for Fagus sylvatica", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(pro50_fagus,
                                   habitat91D0, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results91D0 <- cbind(habitat91D0, results)
summary(results91D0)
png(paste0('/work/matheis/results/91D0fagusproj.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = Fagus.sylvatica_block_5_rf_2079_2098_quantile50_), data=results91D0, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +  
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted future occurrence probabilities in Habitat 91D0 for Fagus sylvatica", fill="Occurrence Probability") 
dev.off()

results  <- terra::extract(difffagus, 
                                   habitat91D0, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE) 
results91D0 <- cbind(habitat91D0, results)
summary(results91D0)
png(paste0('/work/matheis/results/91D0fagusdiff.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = fagus1971.1990), data=results91D0, colour = NA) +
  scale_fill_viridis_c(limits = c(-0.25, 0.5), na.value = "transparent", direction = -1) +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Difference between predicted future and present occurrence probabilities in Habitat 91D0 for Fagus sylvatica", fill="Difference in Occurrence Probability")
dev.off()

#picea
results  <- terra::extract(picea.abies_ref, 
                                   habitat91D0, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results91D0 <- cbind(habitat91D0, results)
summary(results91D0)
png(paste0('/work/matheis/results/91D0picearef.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = picea1971.1990), data=results91D0, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted present occurrence probabilities in Habitat 91D0 for Picea abies", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(pro50_picea,
                                   habitat91D0, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results91D0 <- cbind(habitat91D0, results)
summary(results91D0)
png(paste0('/work/matheis/results/91D0piceaproj.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = Picea.abies_block_3_rf_2079_2098_quantile50_), data=results91D0, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted future occurrence probabilities in Habitat 91D0 for Picea abies", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(diffpicea, 
                                   habitat91D0, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE) 
results91D0 <- cbind(habitat91D0, results)
summary(results91D0)
png(paste0('/work/matheis/results/91D0piceadiff.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = picea1971.1990), data=results91D0, colour = NA) +
  scale_fill_viridis_c(limits = c(-0.25, 0.5), na.value = "transparent", direction = -1) +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Difference between predicted future and present occurrence probabilities in Habitat 91D0 for Picea abies", fill="Difference in Occurrence Probability")
dev.off()

#9410
#fagus
results  <- terra::extract(fagus.sylvatica_ref, 
                                   habitat9410, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9410 <- cbind(habitat9410, results)
summary(results9410)
png(paste0('/work/matheis/results/9410fagusref.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = fagus1971.1990), data=results9410, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted present occurrence probabilities in Habitat 9410 for Fagus sylvatica", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(pro50_fagus,
                                   habitat9410, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9410 <- cbind(habitat9410, results)
summary(results9410)
png(paste0('/work/matheis/results/9410fagusproj.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = Fagus.sylvatica_block_5_rf_2079_2098_quantile50_), data=results9410, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +  
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted future occurrence probabilities in Habitat 9410 for Fagus sylvatica", fill="Occurrence Probability") 
dev.off()

results  <- terra::extract(difffagus, 
                                   habitat9410, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE) 
results9410 <- cbind(habitat9410, results)
summary(results9410)
png(paste0('/work/matheis/results/9410fagusdiff.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = fagus1971.1990), data=results9410, colour = NA) +
  scale_fill_viridis_c(limits = c(-0.25, 0.5), na.value = "transparent", direction = -1) +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Difference between predicted future and present occurrence probabilities in Habitat 9410 for Fagus sylvatica", fill="Difference in Occurrence Probability")
dev.off()

#picea
results  <- terra::extract(picea.abies_ref, 
                                   habitat9410, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9410 <- cbind(habitat9410, results)
summary(results9410)
png(paste0('/work/matheis/results/9410picearef.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = picea1971.1990), data=results9410, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted present occurrence probabilities in Habitat 9410 for Picea abies", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(pro50_picea,
                                   habitat9410, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9410 <- cbind(habitat9410, results)
summary(results9410)
png(paste0('/work/matheis/results/9410piceaproj.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = Picea.abies_block_3_rf_2079_2098_quantile50_), data=results9410, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted future occurrence probabilities in Habitat 9410 for Picea abies", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(diffpicea, 
                                   habitat9410, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE) 
results9410 <- cbind(habitat9410, results)
summary(results9410)
png(paste0('/work/matheis/results/9410piceadiff.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = picea1971.1990), data=results9410, colour = NA) +
  scale_fill_viridis_c(limits = c(-0.25, 0.5), na.value = "transparent", direction = -1) +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Difference between predicted future and present occurrence probabilities in Habitat 9410 for Picea abies", fill="Difference in Occurrence Probability")
dev.off()

#9420
#fagus
results  <- terra::extract(fagus.sylvatica_ref, 
                                   habitat9420, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9420 <- cbind(habitat9420, results)
summary(results9420)
png(paste0('/work/matheis/results/9420fagusref.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = fagus1971.1990), data=results9420, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted present occurrence probabilities in Habitat 9420 for Fagus sylvatica", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(pro50_fagus,
                                   habitat9420, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9420 <- cbind(habitat9420, results)
summary(results9420)
png(paste0('/work/matheis/results/9420fagusproj.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = Fagus.sylvatica_block_5_rf_2079_2098_quantile50_), data=results9420, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +  
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted future occurrence probabilities in Habitat 9420 for Fagus sylvatica", fill="Occurrence Probability") 
dev.off()

results  <- terra::extract(difffagus, 
                                   habitat9420, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE) 
results9420 <- cbind(habitat9420, results)
summary(results9420)
png(paste0('/work/matheis/results/9420fagusdiff.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = fagus1971.1990), data=results9420, colour = NA) +
  scale_fill_viridis_c(limits = c(-0.25, 0.5), na.value = "transparent", direction = -1) +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Difference between predicted future and present occurrence probabilities in Habitat 9420 for Fagus sylvatica", fill="Difference in Occurrence Probability")
dev.off()

#picea
results  <- terra::extract(picea.abies_ref, 
                                   habitat9420, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9420 <- cbind(habitat9420, results)
summary(results9420)
png(paste0('/work/matheis/results/9420picearef.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = picea1971.1990), data=results9420, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted present occurrence probabilities in Habitat 9420 for Picea abies", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(pro50_picea,
                                   habitat9420, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9420 <- cbind(habitat9420, results)
summary(results9420)
png(paste0('/work/matheis/results/9420piceaproj.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = Picea.abies_block_3_rf_2079_2098_quantile50_), data=results9420, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted future occurrence probabilities in Habitat 9420 for Picea abies", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(diffpicea, 
                                   habitat9420, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE) 
results9420 <- cbind(habitat9420, results)
summary(results9420)
png(paste0('/work/matheis/results/9420piceadiff.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = picea1971.1990), data=results9420, colour = NA) +
  scale_fill_viridis_c(limits = c(-0.25, 0.5), na.value = "transparent", direction = -1) +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Difference between predicted future and present occurrence probabilities in Habitat 9420 for Picea abies", fill="Difference in Occurrence Probability")
dev.off()

#9140
#fagus
results  <- terra::extract(fagus.sylvatica_ref, 
                                   habitat9140, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9140 <- cbind(habitat9140, results)
summary(results9140)
png(paste0('/work/matheis/results/9140fagusref.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = fagus1971.1990), data=results9140, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted present occurrence probabilities in Habitat 9140 for Fagus sylvatica", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(pro50_fagus,
                                   habitat9140, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9140 <- cbind(habitat9140, results)
summary(results9140)
png(paste0('/work/matheis/results/9140fagusproj.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = Fagus.sylvatica_block_5_rf_2079_2098_quantile50_), data=results9140, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +  
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted future occurrence probabilities in Habitat 9140 for Fagus sylvatica", fill="Occurrence Probability") 
dev.off()

results  <- terra::extract(difffagus, 
                                   habitat9140, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE) 
results9140 <- cbind(habitat9140, results)
summary(results9140)
png(paste0('/work/matheis/results/9140fagusdiff.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = fagus1971.1990), data=results9140, colour = NA) +
  scale_fill_viridis_c(limits = c(-0.25, 0.5), na.value = "transparent", direction = -1) +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Difference between predicted future and present occurrence probabilities in Habitat 9140 for Fagus sylvatica", fill="Difference in Occurrence Probability")
dev.off()

#picea
results  <- terra::extract(picea.abies_ref, 
                                   habitat9140, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9140 <- cbind(habitat9140, results)
summary(results9140)
png(paste0('/work/matheis/results/9140picearef.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = picea1971.1990), data=results9140, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted present occurrence probabilities in Habitat 9140 for Picea abies", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(pro50_picea,
                                   habitat9140, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9140 <- cbind(habitat9140, results)
summary(results9140)
png(paste0('/work/matheis/results/9140piceaproj.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = Picea.abies_block_3_rf_2079_2098_quantile50_), data=results9140, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted future occurrence probabilities in Habitat 9140 for Picea abies", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(diffpicea, 
                                   habitat9140, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE) 
results9140 <- cbind(habitat9140, results)
summary(results9140)
png(paste0('/work/matheis/results/9140piceadiff.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = picea1971.1990), data=results9140, colour = NA) +
  scale_fill_viridis_c(limits = c(-0.25, 0.5), na.value = "transparent", direction = -1) +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Difference between predicted future and present occurrence probabilities in Habitat 9140 for Picea abies", fill="Difference in Occurrence Probability")
dev.off()

#9150
#fagus
results  <- terra::extract(fagus.sylvatica_ref, 
                                   habitat9150, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9150 <- cbind(habitat9150, results)
summary(results9150)
png(paste0('/work/matheis/results/9150fagusref.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = fagus1971.1990), data=results9150, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted present occurrence probabilities in Habitat 9150 for Fagus sylvatica", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(pro50_fagus,
                                   habitat9150, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9150 <- cbind(habitat9150, results)
summary(results9150)
png(paste0('/work/matheis/results/9150fagusproj.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = Fagus.sylvatica_block_5_rf_2079_2098_quantile50_), data=results9150, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +  
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted future occurrence probabilities in Habitat 9150 for Fagus sylvatica", fill="Occurrence Probability") 
dev.off()

results  <- terra::extract(difffagus, 
                                   habitat9150, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE) 
results9150 <- cbind(habitat9150, results)
summary(results9150)
png(paste0('/work/matheis/results/9150fagusdiff.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = fagus1971.1990), data=results9150, colour = NA) +
  scale_fill_viridis_c(limits = c(-0.25, 0.5), na.value = "transparent", direction = -1) +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Difference between predicted future and present occurrence probabilities in Habitat 9150 for Fagus sylvatica", fill="Difference in Occurrence Probability")
dev.off()

#picea
results  <- terra::extract(picea.abies_ref, 
                                   habitat9150, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9150 <- cbind(habitat9150, results)
summary(results9150)
png(paste0('/work/matheis/results/9150picearef.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = picea1971.1990), data=results9150, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted present occurrence probabilities in Habitat 9150 for Picea abies", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(pro50_picea,
                                   habitat9150, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9150 <- cbind(habitat9150, results)
summary(results9150)
png(paste0('/work/matheis/results/9150piceaproj.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = Picea.abies_block_3_rf_2079_2098_quantile50_), data=results9150, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted future occurrence probabilities in Habitat 9150 for Picea abies", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(diffpicea, 
                                   habitat9150, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE) 
results9150 <- cbind(habitat9150, results)
summary(results9150)
png(paste0('/work/matheis/results/9150piceadiff.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = picea1971.1990), data=results9150, colour = NA) +
  scale_fill_viridis_c(limits = c(-0.25, 0.5), na.value = "transparent", direction = -1) +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Difference between predicted future and present occurrence probabilities in Habitat 9150 for Picea abies", fill="Difference in Occurrence Probability")
dev.off()

#9170
#fagus
results  <- terra::extract(fagus.sylvatica_ref, 
                                   habitat9170, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9170 <- cbind(habitat9170, results)
summary(results9170)
png(paste0('/work/matheis/results/9170fagusref.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = fagus1971.1990), data=results9170, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted present occurrence probabilities in Habitat 9170 for Fagus sylvatica", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(pro50_fagus,
                                   habitat9170, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9170 <- cbind(habitat9170, results)
summary(results9170)
png(paste0('/work/matheis/results/9170fagusproj.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = Fagus.sylvatica_block_5_rf_2079_2098_quantile50_), data=results9170, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +  
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted future occurrence probabilities in Habitat 9170 for Fagus sylvatica", fill="Occurrence Probability") 
dev.off()

results  <- terra::extract(difffagus, 
                                   habitat9170, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE) 
results9170 <- cbind(habitat9170, results)
summary(results9170)
png(paste0('/work/matheis/results/9170fagusdiff.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(mapping=aes(fill = fagus1971.1990), data=results9170, colour = NA) +
  scale_fill_viridis_c(limits = c(-0.25, 0.5), na.value = "transparent", direction = -1) +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Difference between predicted future and present occurrence probabilities in Habitat 9170 for Fagus sylvatica", fill="Difference in Occurrence Probability")
dev.off()

#picea
results  <- terra::extract(picea.abies_ref, 
                                   habitat9170, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9170 <- cbind(habitat9170, results)
summary(results9170)
png(paste0('/work/matheis/results/9170picearef.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = picea1971.1990), data=results9170, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted present occurrence probabilities in Habitat 9170 for Picea abies", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(pro50_picea,
                                   habitat9170, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
results9170 <- cbind(habitat9170, results)
summary(results9170)
png(paste0('/work/matheis/results/9170piceaproj.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = Picea.abies_block_3_rf_2079_2098_quantile50_), data=results9170, colour = NA) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Predicted future occurrence probabilities in Habitat 9170 for Picea abies", fill="Occurrence Probability")
dev.off()

results  <- terra::extract(diffpicea, 
                                   habitat9170, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE) 
results9170 <- cbind(habitat9170, results)
summary(results9170)
png(paste0('/work/matheis/results/9170piceadiff.png'), width = 2000, height = 2000, units = "px", pointsize = 30)
ggplot(data=roi) +
  geom_spatvector(fill = "white", colour = NA) +
  geom_spatvector(aes(fill = picea1971.1990), data=results9170, colour = NA) +
  scale_fill_viridis_c(limits = c(-0.25, 0.5), na.value = "transparent", direction = -1) +
  coord_sf(crs = crs) +
  theme(text=element_text(size=25), legend.key.height = unit(200, 'pt')) +
  labs(title="Difference between predicted future and present occurrence probabilities in Habitat 9170 for Picea abies", fill="Difference in Occurrence Probability")
dev.off()


#########raster
#raster
png(paste0('/work/matheis/results/fagus_sylvatica_projection_scaled.png'), width = 1500, height = 1500, units = "px", pointsize = 30)
ggplot() +
  geom_spatraster(data = pro50_fagus) +
  coord_sf(crs = crs) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  labs(title="Fagus sylvatica Projection", color="occurrence Probability")
dev.off()

png(paste0('/work/matheis/results/picea_abies_projection_scaled.png'), width = 1500, height = 1500, units = "px", pointsize = 30)
ggplot() +
  geom_spatraster(data = pro50_picea) +
  coord_sf(crs = crs) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  labs(title="Picea abies Projection", color="occurrence Probability")
dev.off()

png(paste0('/work/matheis/results/picea_abies_reference_scaled.png'), width = 1500, height = 1500, units = "px", pointsize = 30)
ggplot() +
  geom_spatraster(data = picea.abies_ref) +
  coord_sf(crs = crs) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  labs(title="Picea abies model predictions for the reference period", color="occurrence Probability")
dev.off()

png(paste0('/work/matheis/results/fagus_sylvatica_reference_scaled.png'), width = 1000, height = 1000, units = "px", pointsize = 30)
ggplot() +
  geom_spatraster(data = fagus.sylvatica_ref) +
  coord_sf(crs = crs) +
  scale_fill_viridis_c(limits = c(0, 0.62), na.value = "transparent") +
  labs(title="Fagus sylvatica model predictions for the reference period", color="occurrence Probability")
dev.off()

png(paste0('/work/matheis/results/fagus_difference_scaled.png'), width = 1000, height = 1000, units = "px", pointsize = 30)
ggplot() +
  geom_spatraster(data = difffagus) +
  coord_sf(crs = crs) +
  scale_fill_viridis_c(limits = c(-0.25, 0.5), na.value = "transparent") +
  labs(title="Difference between future and reference predicitons for Fagus sylvatica", color="occurrence Probability")
dev.off()

png(paste0('/work/matheis/results/picea_difference_scaled.png'), width = 1000, height = 1000, units = "px", pointsize = 30)
ggplot() +
  geom_spatraster(data = diffpicea) +
  coord_sf(crs = crs) +
  scale_fill_viridis_c(limits = c(-0.25, 0.5), na.value = "transparent") +
  labs(title="Difference between future and reference predictions for Picea abies", color="occurrence Probability")
dev.off()

##vector
#png(paste0('/work/matheis/results/picea_abies_predction_n2k_scaled.png'), width = 1000, height = 1000, units = "px", pointsize = 30)
#ggplot(resultshp) +
#  geom_spatvector(aes(fill = Picea.abi0), color = NA) +
#  scale_fill_viridis_c(limits = c(-0.5386064, 0.7276257873), na.value = "transparent") +
#  coord_sf(crs = crs) +
#  labs(title="Predicted occurrence Probability by Natura-2000 area 2079-2098 for Picea abies", color="occurrence Probability")
#dev.off()

#png(paste0('/work/matheis/results/fagus_sylvatica_predction_n2k_scaled.png'), width = 1000, height = 1000, units = "px", pointsize = 30)
#ggplot(resultshp) +
#  geom_spatvector(aes(fill = Fagus.syl0), color = NA) +
#  scale_fill_viridis_c(limits = c(-0.5386064, 0.7276257873), na.value = "transparent") +
#  coord_sf(crs = crs) +
#  labs(title="Predicted occurrence Probability by Natura-2000 area 2079-2098 for Fagus sylvatica", color="occurrence Probability")
#dev.off()

#png(paste0('/work/matheis/results/picea_abies_reference_n2k_scaled.png'), width = 1000, height = 1000, units = "px", pointsize = 30)
#ggplot(resultshp) +
#  geom_spatvector(aes(fill = picea19710), color = NA) +
#  scale_fill_viridis_c(limits = c(-0.5386064, 0.7276257873), na.value = "transparent") +
#  coord_sf(crs = crs) +
#  labs(title="Predicted occurrence Probability by Natura-2000 area 1971-1990 for Picea abies", color="occurrence Probability")
#dev.off()

#png(paste0('/work/matheis/results/fagus_sylvatica_reference_n2k_scaled.png'), width = 1000, height = 1000, units = "px", pointsize = 30)
#ggplot(resultshp) +
#  geom_spatvector(aes(fill = fagus19710), color = NA) +
#  scale_fill_viridis_c(limits = c(-0.5386064, 0.7276257873), na.value = "transparent") +
#  coord_sf(crs = crs) +
#  labs(title="Predicted occurrence Probability by Natura-2000 area 1971-1990 for Fagus sylvatica", color="occurrence Probability")
#dev.off()

#png(paste0('/work/matheis/results/picea_abies_difference_n2k_scaled.png'), width = 1000, height = 1000, units = "px", pointsize = 30)
#ggplot(resultshp) +
#  geom_spatvector(aes(fill = picea19711)) +
#  scale_fill_viridis_c(limits = c(-0.5386064, 0.7276257873), na.value = "transparent") +
#  coord_sf(crs = crs) +
#  labs(title="Difference between future and reference occurrence probabilities by Natura-2000 area for Picea abies", color="occurrence Probability")
#dev.off()

#png(paste0('/work/matheis/results/fagus_sylvatica_difference_n2k_scaled.png'), width = 1000, height = 1000, units = "px", pointsize = 30)
#ggplot(resultshp) +
#  geom_spatvector(aes(fill = fagus19711)) +
#  scale_fill_viridis_c(limits = c(-0.5386064, 0.7276257873), na.value = "transparent") +
#  coord_sf(crs = crs) +
#  labs(title="Difference between future and reference occurrence probabilities by Natura-2000 area for Fagus sylvatica", color="occurrence Probability")
#dev.off()


#histograms
#load data
full_df <- read.table('/work/matheis/modellingDataFrames/modellingDataFrame1971_1990_block_random_600000.csv', header=T, sep=',', colClasses=c("x"="numeric","y"="numeric","block"="numeric"))
colnames(full_df) <- sub("_1971_1990", "", colnames(full_df))
colnames(full_df) <- sub("_rpp", "", colnames(full_df))
colnames(full_df) <- sub("Block", "block", colnames(full_df))

#picea
png(paste0('/work/matheis/results/histpicea00225.png'), width = 1000, height = 1000, units = "px", pointsize = 18)
hist(full_df$Picea.abies, main="Histogram of Picea abies reference data", xlab="Occurrence Probability", xlim=c(-1,1), breaks=seq(-1,1,0.02), xaxp=c(-1,1,20), ylim=c(0,4000000))
dev.off()

picea_df <- full_df %>% filter(Picea.abies >= "0") %>% filter(Picea.abies != "0.0225")
picea_df = picea_df %>% drop_na()
png(paste0('/work/matheis/results/histpicearef.png'), width = 1000, height = 1000, units = "px", pointsize = 18)
hist(picea_df$Picea.abies, main="Histogram of Picea abies reference data", xlab="Occurrence Probability", xlim=c(0,1), breaks=seq(0,1,0.05), xaxp=c(0,1,20), ylim=c(0,4000000))
dev.off()

summary(ref_prediction_picea)
png(paste0('/work/matheis/results/histpiceaprojref.png'), width = 1000, height = 1000, units = "px", pointsize = 18)
hist(ref_prediction_picea$ref_prediction_picea, main="Histogram of Picea abies prediction to 1971-1990", xlab="Occurrence Probability", xlim=c(0,1), breaks=seq(0,1,0.05), xaxp=c(0,1,20), ylim=c(0,3500000))
dev.off()

#png(paste0('/work/matheis/results/histpiceaproj.png'), width = 1000, height = 1000, units = "px", pointsize = 30)
#hist(pro50_picea, main="Histogram of Picea abies projection", xlab="Picea abies", xlim=c(0,1), breaks=seq(0,1,0.05), xaxp=c(0,1,20))
#dev.off()

#fagus
print("fagus")
png(paste0('/work/matheis/results/histfagus00225.png'), width = 1000, height = 1000, units = "px", pointsize = 18)
hist(full_df$Fagus.sylvatica, main="Histogram of Fagus sylvatica reference data", xlab="Occurrence Probability", xlim=c(-1,1), breaks=seq(-1,1,0.02), xaxp=c(-1,1,20), ylim=c(0,4000000))
dev.off()

fagus_df <- full_df %>% filter(Fagus.sylvatica >= "0") %>% filter(Fagus.sylvatica != "0.0225")
fagus_df = fagus_df %>% drop_na()
png(paste0('/work/matheis/results/histfagusref.png'), width = 1000, height = 1000, units = "px", pointsize = 18)
hist(fagus_df$Fagus.sylvatica, main="Histogram of Fagus sylvatica reference data", xlab="Occurrence Probability", xlim=c(0,1), breaks=seq(0,1,0.05), xaxp=c(0,1,20), ylim=c(0,4000000))
dev.off()

summary(ref_prediction_fagus)
png(paste0('/work/matheis/results/histfagusprojref.png'), width = 1000, height = 1000, units = "px", pointsize = 18)
hist(ref_prediction_fagus$ref_prediction_fagus, main="Histogram of Fagus sylvatica prediction to 1971-1990", xlab="Occurrence Probability", xlim=c(0,1), breaks=seq(0,1,0.05), xaxp=c(0,1,20), ylim=c(0,3500000))
dev.off()

#png(paste0('/work/matheis/results/histfagusproj.png'), width = 1000, height = 1000, units = "px", pointsize = 18)
#hist(pro50_fagus, main="Histogram of Fagus sylvatica prediciton to 2079-2098", xlab="Fagus sylvatica", xlim=c(0,1), breaks=seq(0,1,0.05), xaxp=c(0,1,20))
#dev.off()


#####blocks
europe = raster('/data/satellite/forestProjection/sdm/Modelling/Europe_Modelling_all.tif')
europedf=as.data.frame(rasterToPoints(europe))
block = readRDS('/data/satellite/forestProjection/sdm/Modelling/blocks_random_600000.rds')
poly = block$blocks
poly = st_as_sf(poly)

png(paste0('/work/matheis/results/Blocks3.png'), width = 1000, height = 1000, units = "px", pointsize=60)
ggplot() +
  geom_raster(data=europedf, aes(x = x, y = y), fill="grey") + 
  geom_sf(data = poly, col = "#277d8e", fill = NA, lwd=1) +
  geom_sf_text(data = poly, aes(label = folds), fill = NA, col = "#277d8e",  size=18) +
  coord_sf(crs = crs) +
  theme_light() +
  labs(title="Blocks for model fitting and evaluation") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),text=element_text(size=12))
dev.off()

##response curves
#picea
png(paste0('/work/matheis/model/plots/partialresponsepicea.png'), width = 1500, height = 700, units = "px", pointsize = 30)
par(mfrow=c(1,4)) 
partial_response(modelpicea, predictors = full_df[,pred]) 
dev.off()

#picea
png(paste0('/work/matheis/model/plots/partialresponsefagus.png'), width = 1500, height = 700, units = "px", pointsize = 30)
par(mfrow=c(1,4)) 
partial_response(modelfagus, predictors = full_df[,pred]) 
dev.off()

#picea
png(paste0('/work/matheis/model/plots/inflatedresponsepicea.png'), width = 1500, height = 700, units = "px", pointsize = 30)
par(mfrow=c(1,4)) 
inflated_response(modelpicea, predictors = full_df[,pred]) 
dev.off()

#picea
png(paste0('/work/matheis/model/plots/inflatedresponsefagus.png'), width = 1500, height = 700, units = "px", pointsize = 30)
par(mfrow=c(1,4)) 
inflated_response(modelfagus, predictors = full_df[,pred]) 
dev.off()
