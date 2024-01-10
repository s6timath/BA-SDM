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
print(pred)
testfold = c('1','2','3','4','5')
block = testfold
path= c("/work/matheis/")   
print(species)
crs = c("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80
+units=m +no_defs")

#read the data
modelling_df <- read.table('/work/matheis/thinned_df_dist20.csv', header=T, sep=',', colClasses=c("x"="numeric","y"="numeric","block"="numeric"))
ref_cols <- paste("x","y", paste(species , collapse=", "), paste(pred, collapse=", "),"block", sep =", ")
ref_cols = unlist(strsplit(ref_cols, ", "))
modelling_df = (modelling_df)[ref_cols]
modelling_df <- modelling_df %>% filter(get(species) >= "0") %>% filter(get(species) != "0.0225")
modelling_df = modelling_df %>% drop_na()

full_df <- read.table('/work/matheis/modellingDataFrames/modellingDataFrame1971_1990_block_random_600000.csv', header=T, sep=',', colClasses=c("x"="numeric","y"="numeric","block"="numeric"))
colnames(full_df) <- sub("_1971_1990", "", colnames(full_df))
colnames(full_df) <- sub("_rpp", "", colnames(full_df))
colnames(full_df) <- sub("Block", "block", colnames(full_df))
ref_cols <- paste("x","y", paste(species, collapse=", "), paste(pred, collapse=", "),"block", sep =", ")
ref_cols = unlist(strsplit(ref_cols, ", "))
full_df = (full_df)[ref_cols]
full_df <- full_df %>% filter(get(species) >= "0") %>% filter(get(species) != "0.0225")
full_df = full_df %>% drop_na()

#######################
####RANDOM FOREST######
#######################

for (i in testfold){
print(i)
method = c("rf")
print(method)
filename=paste0(path,'model/train/model_',species,'_block_',i,'_',method,'.RDS')
test = filter(full_df, block == i)
train = filter(modelling_df, block != i)
summary(train)

#model fitting RF
set.seed(1)
(model_rf <- randomForest(x=train[,pred], y=train[,species], ntree=1000, nodesize=10, importance=T))
print((importance(model_rf,type=1)))
saveRDS(model_rf, file=filename)

#model prediction to test data
testprediction <- predict(model_rf, newdata=test, type='response')
print(summary(testprediction))

#modelevaluation
print("train")
traineval_rf = mecofun::evalSDM(observation=train[,species], predictions=model_rf$predicted)
print(traineval_rf)
print("test")
testeval_rf = mecofun::evalSDM(observation=test[,species], predictions=testprediction)
print(testeval_rf)
evalvaluerf = (testeval_rf$TSS + traineval_rf$D2)
print(evalvaluerf)
}

#######################
######GLM##############
#######################

for (i in testfold){
print(i)
method = c("glm")
print(method)
filename=paste0(path,'model/train/model_',species,'_block_',i,'_',method,'.RDS')
test = filter(full_df, block == i)
print("test rows")
print(nrow(test))
train = filter(modelling_df, block != i)
print("train rows")
print(nrow(train))

#model fitting GLM
print(model_glm <- glm(get(species) ~ get(pred[1]) + I(get(pred[1])^2) + get(pred[2]) + I(get(pred[2])^2) + get(pred[3]) + I(get(pred[3])^2) + get(pred[4]) + I(get(pred[4])^2), family=binomial(link = "cloglog"), data=train))
saveRDS(model_glm, file=filename)

#model prediction to test data
testprediction <- predict(model_glm, newdata=test, type='response')
print("testprediction rows")
print(nrow(testprediction))

#modelevaluation
print("train")
traineval_glm = mecofun::evalSDM(observation=train[,species], predictions=model_glm$fitted.values)
print(traineval_glm)
print("test")
testeval_glm = mecofun::evalSDM(observation=test[,species], predictions=testprediction)
print(testeval_glm)
evalvalueglm = (testeval_glm$TSS + traineval_glm$D2)
print(evalvalueglm)
}