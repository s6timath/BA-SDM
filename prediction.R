#loding all needed libraries
library(terra)
library(sf)
library(raster)
library(ncdf4)
library(parallel)
library(stringr)
library(pryr)
library(tidyverse)
library(data.table)
library(randomForest)
library(lattice)
library(RColorBrewer)
library(PresenceAbsence)
library(mecofun)
library(dismo)

#for  traceability
sessionInfo()

for (package_name in sort(loadedNamespaces())) {
  print(paste(package_name, packageVersion(package_name)))
}

#definitions
species <- c('Fagus.sylvatica')
pred <- c('precipRange',
  'P10_MeanTempWarmestQuarter',
  'P15_PrecipSeasonality',
  'P4_TempSeasonality')
path= c("/work/matheis/model/prediction/projection/")
crs = c("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80
+units=m +no_defs")
algo = c('rf')
block = c('5')
period = c('2079_2098')
rcp = c('rcp45')
#quantile = c('quantile5','quantile50','quantile95')

#read the data
modelFile = paste0('/work/matheis/model/train/model_',species,'_block_',block,'_',algo,'.RDS')
model <- readRDS(modelFile)
modelling_df <- read.table('/work/matheis/modellingDataFrames/modellingDataFrame2079_2098.csv', header=T, sep=',')
colnames(modelling_df) <- sub("_2079_2098", "", colnames(modelling_df))
colnames(modelling_df) <- sub("_rpp", "", colnames(modelling_df))

#prediction 50th quantile
filenameorigin=paste0(species,'_block_',block,'_',algo,'_',period,'_quantile50_')
predict_df_50 = modelling_df %>% select('x', 'y', contains('rcp45') & contains('quantile50'))
print(colnames(predict_df_50))
colnames(predict_df_50) <- sub("_rcp45", "", colnames(predict_df_50))
colnames(predict_df_50) <- sub("_quantile50", "", colnames(predict_df_50))
print(colnames(predict_df_50))
filename <- paste0(path,filenameorigin,'.csv')
prediction <- predict(model, newdata=predict_df_50, type='response')
#saveRDS(prediction, file=predicitonfilename)

#plotting
png(paste0('/work/matheis/results/histfagusproj.png'), width = 1000, height = 1000, units = "px", pointsize = 18)
hist(prediction, main="Histogram of Fagus sylvatica prediction to 2079-2098", xlab="Occurrence Probability", xlim=c(0,1), breaks=seq(0,1,0.05), xaxp=c(0,1,20), ylim=c(0,3500000))
dev.off()
plotting_df = as.data.frame(cbind(predict_df_50[,c(1,2)],prediction))
head(plotting_df)
plotting_raster <- rasterFromXYZ(xyz=plotting_df, crs=crs)
plottingfilename=paste0('/gpfs1/work/matheis/model/prediction/projection/tif/',filenameorigin,'.tif')
print(plottingfilename)
writeRaster(plotting_raster, filename=plottingfilename, overwrite=TRUE)

#prediction 5th quantile
filenameorigin=paste0(species,'_block_',block,'_',algo,'_',period,'_quantile5_')
predict_df_5 = modelling_df %>% select('x', 'y', contains('rcp45') & contains('quantile5'))
print(colnames(predict_df_5))
colnames(predict_df_5) <- sub("_rcp45", "", colnames(predict_df_5))
colnames(predict_df_5) <- sub("_quantile5", "", colnames(predict_df_5))
print(colnames(predict_df_5))
filename <- paste0(path,filenameorigin,'.csv')
prediction <- predict(model, newdata=predict_df_5, type='response')
#saveRDS(prediction, file=predicitonfilename)

#plotting
plotting_df = as.data.frame(cbind(predict_df_5[,c(1,2)],prediction))
plotting_raster <- rasterFromXYZ(xyz=plotting_df, crs=crs)
plottingfilename=paste0('/gpfs1/work/matheis/model/prediction/projection/tif/',filenameorigin,'.tif')
writeRaster(plotting_raster, filename=plottingfilename, overwrite=TRUE)

#prediction 95th quantile
filenameorigin=paste0(species,'_block_',block,'_',algo,'_',period,'_quantile95_')
predict_df_95 = modelling_df %>% select('x', 'y', contains('rcp45') & contains('quantile95'))
print(colnames(predict_df_95))
colnames(predict_df_95) <- sub("_rcp45", "", colnames(predict_df_95))
colnames(predict_df_95) <- sub("_quantile95", "", colnames(predict_df_95))
print(colnames(predict_df_95))
filename <- paste0(path,filenameorigin,'.csv')
prediction <- predict(model, newdata=predict_df_95, type='response')
#saveRDS(prediction, file=predicitonfilename)

#plotting
plotting_df = as.data.frame(cbind(predict_df_95[,c(1,2)],prediction))
plotting_raster <- rasterFromXYZ(xyz=plotting_df, crs=crs)
plottingfilename=paste0('/gpfs1/work/matheis/model/prediction/projection/tif/',filenameorigin,'.tif')
writeRaster(plotting_raster, filename=plottingfilename, overwrite=TRUE)
