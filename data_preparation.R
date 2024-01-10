# load all needed libraries first
library(data.table)
library(raster)
library(tidyverse)

# this assures all libraries are included in the following commands
sessionInfo()

for (package_name in sort(loadedNamespaces())) {
  print(paste(package_name, packageVersion(package_name)))
}

#definitions
modelling_df <- read.table("/gpfs1/work/matheis/modellingDataFrames/modellingDataFrame1971_1990_block_random_600000.csv", header=T, sep=',')
roi <- raster("/gpfs1/data/satellite/forestProjection/sdm/Modelling/Europe_Modelling_all.tif")
crs = c("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80
+units=m +no_defs")

modelling_df <- read.table('/work/matheis/modellingDataFrames/modellingDataFrame1971_1990_block_random_600000.csv', header=T, sep=',')
colnames(modelling_df) <- sub("_1971_1990", "", colnames(modelling_df))
colnames(modelling_df) <- sub("_rpp", "", colnames(modelling_df))
colnames(modelling_df) <- sub("Block", "block", colnames(modelling_df))
summary(modelling_df)

distance=c(20)
print(c)
modelling_df$x = round(modelling_df$x) 
modelling_df$y = round(modelling_df$y) 
xrange = seq(min(modelling_df$x),max(modelling_df$x),20*(1000))
yrange = seq(max(modelling_df$y),min(modelling_df$y),20*(-1000))

thinned_df <- modelling_df %>% 
filter(x %in% xrange) %>%
filter(y %in% yrange)
sapply(thinned_df, typeof)

write.csv(thinned_df, paste0('/work/matheis/thinned_df_dist20.csv'))