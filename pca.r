
library(raster)
library(tidyverse)
library(corrplot)
library(viridis)

#path = "/data/satellite/forestProjection/sdm/climateCalculations/combined/1971_1990"

#outputPath = "/work/matheis/ClimateData/ClimateCalculations/PCA"

#rasterstack of predictors
predfilelist <- list.files("/data/satellite/forestProjection/sdm/climateCalculations/combined/1971_1990", full.names=TRUE)
ref_stack <- stack(predfilelist)

#convert to dataframe
pred_df <- as.data.frame(ref_stack, xy=TRUE, na.rm=FALSE)
write.csv(pred_df, "/work/matheis/pred_df_from_datastack.csv")

#load data
pred_df <- read.table("/work/matheis/pred_df_from_datastack.csv", header=T, sep=',')
pred_df = pred_df %>% drop_na()
pca_df = pred_df[,4:29]
summary(pca_df)

#pca
pca = prcomp(pca_df, scale=TRUE)
print(pca)
summary(pca)
str(pca)
loadings <- pca$rotation
pcax <- pca$X
write.csv(loadings, "/work/matheis/results/pcaloadings.csv")
write.csv(pcax, "/work/matheis/results/pcax.csv")

#correlation matrix
cor_df = pred_df[c("precipRange","P10_MeanTempWarmestQuarter","P15_PrecipSeasonality","P4_TempSeasonality")]

cov2=cov(cor_df, y = NULL, use = "everything",
        method = c("spearman"))
print(cov2)

cor2=cov2cor(cov2)
print(cor2)

png(paste0('/work/matheis/results/corrplot.png'), width = 1000, height = 1200, units = "px", pointsize = 15)

corrplot(cor2, type="lower", tl.col="black", tl.srt=35, addCoef.col = "black", title = "Correlation plot of selected predictors")
dev.off()