 #loding all needed libraries
library(terra)
library(sf)
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

##natura2000
#n2k <- vect("/work/matheis/n2k/Natura2000_end2021_rev1_epsg3035.shp")
#crs(n2k) <- crs

#habitatlist <- read.table('/work/matheis/n2k/n2k_forest_sites_germany_end2021.csv', header=T, sep=',')
#colnames(habitatlist) <- sub("sitecode", "SITECODE", colnames(habitatlist))
#roi <- merge(n2k, habitatlist, by='SITECODE')
#roi <- sf::st_as_sf(roi)
#st_write(roi, "/gpfs1/work/matheis/n2k/n2kselected.shp", append=FALSE)
roi <- terra::vect("/gpfs1/work/matheis/n2k/n2kselected.shp")
crs(roi) <- crs

#read the data
modelFilepicea = paste0('/work/matheis/model/train/model_Picea.abies_block_3_rf.RDS')
modelpicea <- readRDS(modelFilepicea)
modelFilefagus = paste0('/work/matheis/model/train/model_Fagus.sylvatica_block_5_rf.RDS')
modelfagus <- readRDS(modelFilefagus)

full_df <- read.table('/work/matheis/modellingDataFrames/modellingDataFrame1971_1990.csv', header=T, sep=',', colClasses=c("x"="numeric","y"="numeric","block"="numeric"))
colnames(full_df) <- sub("_1971_1990", "", colnames(full_df))
colnames(full_df) <- sub("_rpp", "", colnames(full_df))

#prediction reference data
ref_cols <- paste("x","y", paste(pred, collapse=", "), sep =", ")
ref_cols = unlist(strsplit(ref_cols, ", "))
modelling_df = (full_df)[ref_cols]

#ref_prediction_picea <- predict(modelpicea, newdata=modelling_df, type='response')
plotting_df = as.data.frame(cbind(modelling_df[,c(1,2)],ref_prediction_picea))
plotting_raster <- rasterFromXYZ(xyz=plotting_df, crs=crs)
writeRaster(plotting_raster, filename='/gpfs1/work/matheis/model/prediction/projection/tif/picea1971-1990.tif', overwrite=TRUE)
write.csv(plotting_df, file='/gpfs1/work/matheis/model/prediction/projection/csv/picea1971-1990.csv', row.names=FALSE)

ref_prediction_fagus <- predict(modelfagus, newdata=modelling_df, type='response')
plotting_df = as.data.frame(cbind(modelling_df[,c(1,2)],ref_prediction_fagus))
plotting_raster <- rasterFromXYZ(xyz=plotting_df, crs=crs)
writeRaster(plotting_raster, filename='/gpfs1/work/matheis/model/prediction/projection/tif/fagus1971-1990.tif', overwrite=TRUE)
write.csv(plotting_df, file='/gpfs1/work/matheis/model/prediction/projection/csv/fagus1971-1990.csv', row.names=FALSE)

#reference data 
pica.abies_ref <- rast('/gpfs1/work/matheis/model/prediction/projection/tif/picea1971-1990.tif')
ref_prediction_picea <- read.table('/gpfs1/work/matheis/model/prediction/projection/csv/picea1971-1990.csv', header=T, sep=',')
fagus.sylvativca_ref <- rast('/gpfs1/work/matheis/model/prediction/projection/tif/fagus1971-1990.tif')
ref_prediction_fagus <- read.table('/gpfs1/work/matheis/model/prediction/projection/csv/fagus1971-1990.csv', header=T, sep=',')

#projections
#pro95_fagus <- rast("/gpfs1/work/matheis/model/prediction/projection/tif/Fagus.sylvatica_block_5_rf_2079_2098_quantile95_.tif")
#pro5_fagus <- rast("/gpfs1/work/matheis/model/prediction/projection/tif/Fagus.sylvatica_block_5_rf_2079_2098_quantile5_.tif")
pro50_fagus <- rast("/gpfs1/work/matheis/model/prediction/projection/tif/Fagus.sylvatica_block_5_rf_2079_2098_quantile50_.tif")
#pro95_picea <- rast("/gpfs1/work/matheis/model/prediction/projection/tif/Picea.abies_block_3_rf_2079_2098_quantile95_.tif")
#pro5_picea <- rast("/gpfs1/work/matheis/model/prediction/projection/tif/Picea.abies_block_3_rf_2079_2098_quantile5_.tif")
pro50_picea <- rast("/gpfs1/work/matheis/model/prediction/projection/tif/Picea.abies_block_3_rf_2079_2098_quantile50_.tif")

difffagus <- fagus.sylvativca_ref - pro50_fagus
diffpicea <- pica.abies_ref - pro50_picea
decreasefagus <- difffagus / fagus.sylvativca_ref
decreasepicea <- diffpicea / pica.abies_ref

resultsdf <- data.frame()
filename <- paste0('/work/matheis/results/allhabitats.shp')
roiall <- aggregate(roi, by=NULL, dissolve=TRUE, fun="mean")
results  <- terra::extract(pica.abies_ref,
                                   roiall, 
                                   fun=mean,
                                   na.rm=TRUE,
                                   weights=TRUE,
                                   bind=TRUE,
                                  ID=FALSE)
roiall <- cbind(roiall, results)
results  <- terra::extract(fagus.sylvativca_ref,
                                   roiall, 
                                   fun=mean,
                                   na.rm=TRUE,
                                   weights=TRUE,
                                   bind=TRUE,
                                  ID=FALSE)
roiall <- cbind(roiall, results)
results  <- terra::extract(pro50_fagus,
                                   roiall, 
                                   fun=mean,
                                   na.rm=TRUE,
                                   weights=TRUE,
                                   bind=TRUE,
                                  ID=FALSE)
roiall <- cbind(roiall, results)
results  <- terra::extract(pro50_picea,
                                   roiall, 
                                   fun=mean,
                                   na.rm=TRUE,
                                   weights=TRUE,
                                   bind=TRUE,
                                  ID=FALSE)
roiall <- cbind(roiall, results)
results  <- terra::extract(difffagus,
                                   roiall, 
                                   fun=mean,
                                   na.rm=TRUE,
                                   weights=TRUE,
                                   bind=TRUE,
                                  ID=FALSE)
roiall <- cbind(roiall, results)
results  <- terra::extract(diffpicea,
                                   roiall, 
                                   fun=mean,
                                   na.rm=TRUE,
                                   weights=TRUE,
                                   bind=TRUE,
                                  ID=FALSE)
roiall <- cbind(roiall, results)
area <- expanse(roiall, unit="km", transform=TRUE)
roiall <- cbind(resultsdf, roiall)
summary(roiall)
resultsdf <- rbind(resultsdf, binddf)
writeVector(roiall, filename, overwrite=TRUE)
 stop()


#dataframe for each habitat
for (i in habitatcodes){
filename <- paste0('/work/matheis/results/subset',i,'.shp')
roihabitat <- dplyr::filter(roi, grepl(i, habttcd))
roihabitat <- aggregate(roihabitat, by=NULL, dissolve=TRUE, fun="mean")
results  <- terra::extract(pica.abies_ref,
                                   roihabitat, 
                                   fun=mean,
                                   na.rm=TRUE,
                                   weights=TRUE,
                                   bind=TRUE,
                                  ID=FALSE)
roihabitat <- cbind(roihabitat, results)
results  <- terra::extract(fagus.sylvativca_ref,
                                   roihabitat, 
                                   fun=mean,
                                   na.rm=TRUE,
                                   weights=TRUE,
                                   bind=TRUE,
                                  ID=FALSE)
roihabitat <- cbind(roihabitat, results)
results  <- terra::extract(pro50_fagus,
                                   roihabitat, 
                                   fun=mean,
                                   na.rm=TRUE,
                                   weights=TRUE,
                                   bind=TRUE,
                                  ID=FALSE)
roihabitat <- cbind(roihabitat, results)
results  <- terra::extract(pro50_picea,
                                   roihabitat, 
                                   fun=mean,
                                   na.rm=TRUE,
                                   weights=TRUE,
                                   bind=TRUE,
                                  ID=FALSE)
roihabitat <- cbind(roihabitat, results)
results  <- terra::extract(difffagus,
                                   roihabitat, 
                                   fun=mean,
                                   na.rm=TRUE,
                                   weights=TRUE,
                                   bind=TRUE,
                                  ID=FALSE)
roihabitat <- cbind(roihabitat, results)
results  <- terra::extract(diffpicea,
                                   roihabitat, 
                                   fun=mean,
                                   na.rm=TRUE,
                                   weights=TRUE,
                                   bind=TRUE,
                                  ID=FALSE)
roihabitat <- cbind(roihabitat, results)
habitat = c(i)
habitat <- as.data.frame(habitat, col.names = c("habitat"))
area <- expanse(roihabitat, unit="km", transform=TRUE)
area <- as.list(area)
area <- as.data.frame(area, col.names = c("area"))
roihabitat <- cbind(habitat, roihabitat)
roihabitat <- cbind(roihabitat, area)
binddf <- as.data.frame(roihabitat)
resultsdf <- rbind(resultsdf, binddf)
writeVector(roihabitat, filename, overwrite=TRUE)
png(paste0('/work/matheis/results/habitat',i,'.png'), width = 1000, height = 1000, units = "px", pointsize = 20)
print(ggplot(roihabitat) +
  geom_spatvector() +
  scale_fill_viridis(na.value = "transparent") +
  coord_sf(crs = crs) +
  labs(title=paste0('Habitat ',i)))
dev.off()
}

#extract reference data
results  <- terra::extract(pica.abies_ref, 
                                   roi, 
                                   fun=mean, 
                                   na.rm=TRUE,
                                   weights=TRUE,
                                   ID=FALSE,
                                   bind=TRUE)
resultshp <- cbind(roi, results)
results  <- terra::extract(fagus.sylvativca_ref,
                                   roi, 
                                   fun=mean,
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
resultshp <- cbind(resultshp, results)
resultshp

#extract projection
#results  <- terra::extract(pro5_picea, 
#                                   roi, 
#                                   fun=mean, 
#                                   na.rm=TRUE,
#                                   weights=TRUE,
#                                   ID=FALSE, bind=TRUE)
#resultshp <- cbind(resultshp, results)

results  <- terra::extract(pro50_picea,
                                   roi, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
resultshp <- cbind(resultshp, results)

#results  <- terra::extract(pro95_picea, 
#                                   roi, 
#                                   fun=mean, 
#                                   na.rm=TRUE,
#                                   weights=TRUE, 
#                                  bind=TRUE,
#ID=FALSE)
#resultshp <- cbind(resultshp, results)

#results  <- terra::extract(pro5_fagus,
#                                   roi, 
#                                   fun=mean,  
#                                   na.rm=TRUE,
#                                   weights=TRUE,
#                                   bind=TRUE,
#ID=FALSE)
#resultshp <- cbind(resultshp, results)

results  <- terra::extract(pro50_fagus, 
                                   roi, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
resultshp <- cbind(resultshp, results)

results  <- terra::extract(diffpicea,
                                   resultshp, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
resultshp <- cbind(resultshp, results)

results  <- terra::extract(difffagus, 
                                   resultshp, 
                                   fun=mean, 
                                   weights=TRUE,
                                   na.rm=TRUE,
                                   bind=TRUE,
                                   ID=FALSE)
resultshp <- cbind(resultshp, results)

write.csv(resultsdf, file='/gpfs1/work/matheis/results/resultsbyhabitat.csv', row.names=FALSE)

writeVector(resultshp, "/work/matheis/results/reference.shp", overwrite=TRUE)

