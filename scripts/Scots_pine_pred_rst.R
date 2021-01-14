
# date: 15/12/20
# author: VB
# description: script to rasterise Scots pine csv data and test mapping uncertainty

wd <- "~/FFMPs" # sandbox
dirData <- paste0(wd,"/data-raw/")
dirOut <- paste0(wd,"/data-processed/")

### libraries ------------------------------------------------------------------

library(tidyverse)
library(sf)
library(raster)
library(ggplot2)

### csv data (height, survival, performance) -----------------------------------

# note. explanations of the variables in .txt file in Productionpredictions folder

# mean (across GCMs) results
mean45in50 <- read.csv(paste0(dirData,"Productionpredictions/MEAN45in50_SO1.5g_predictions.csv"))
head(mean45in50)
mean85in50 <- read.csv(paste0(dirData,"Productionpredictions/MEAN85in50_SO1.5g_predictions.csv"))
head(mean85in50)

# apply thresholds (remove data where production below 1 and survival below 0.5)
mean45in50$PrProdidxSOh60[which(mean45in50$PrProdidxSOh60<1)]<-NA
mean45in50$PrProdidxSOh60[which(mean45in50$PrSurvSOh60<0.5)]<-NA
mean85in50$PrProdidxSOh60[which(mean85in50$PrProdidxSOh60<1)]<-NA
mean85in50$PrProdidxSOh60[which(mean85in50$PrSurvSOh60<0.5)]<-NA

### convert to raster ----------------------------------------------------------

# RCP4.5
# convert to spatial points
coordinates(mean45in50) <- ~ CenterLong + CenterLat
# set crs - assume lat long
proj4string(mean45in50) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
crs(mean45in50)
extent(mean45in50)
# empty raster of correct extent and resolution
rst <- raster(crs = crs(mean45in50), resolution = c(0.1,0.1), ext = extent(mean45in50))
res(rst)
# rasterise while still lat long
prodIdxSOh60_45 <- rasterize(mean45in50, rst, mean45in50$PrProdidxSOh60, fun=max)
crs(prodIdxSOh60_45)
plot(prodIdxSOh60_45)
# now transform to utm, reprojection involves interpolation - default is bilinear
shpSZ <- st_read(paste0(dirData,"Seed_zones_SP_Sweden/Shaper/Frözoner_tall_Sverige.shp"))
utm <- crs(shpSZ)
prodIdxSOh60_45 <- projectRaster(prodIdxSOh60_45, crs = utm, res = 1000)
plot(prodIdxSOh60_45)

# RCP8.5
# convert to spatial points
coordinates(mean85in50) <- ~ CenterLong + CenterLat
# set crs - assume lat long
proj4string(mean85in50) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
crs(mean85in50)
extent(mean85in50)
# rasterise while still lat long
prodIdxSOh60_85 <- rasterize(mean85in50, rst, mean85in50$PrProdidxSOh60, fun=max)
crs(prodIdxSOh60_85)
plot(prodIdxSOh60_85)
# transform to utm
prodIdxSOh60_85 <- projectRaster(prodIdxSOh60_85, crs = utm, res = 1000)
plot(prodIdxSOh60_85)

### test uncertainty map -------------------------------------------------------

# Chakraborty et al (2016) maps of uncertainty 
# (calculated as the difference in predicted performance between two climate scenarios, 
# as a percentage of the mean of the two predictions).

diff <- prodIdxSOh60_85 - prodIdxSOh60_45
plot(diff)
avg <- mean(prodIdxSOh60_45,prodIdxSOh60_85)
plot(avg)

uncertainty <- diff/avg * 100
plot(uncertainty)

fig1 <- stack(prodIdxSOh60_45,prodIdxSOh60_85,avg,uncertainty)
names(fig1) <- c("RCP4.5","RCP8.5","Mean","Uncertainty")
plot(fig1)

### rasterise in loop ----------------------------------------------------------

# for comparison to kml data from planters guide (current climate)
# In order to check that the data you have got is a good approximation of real seed orchards 
# we suggest that you make a few such maps (using PrProdidxSOhXX). 

dfRef <- read.csv(paste0(dirData,"Productionpredictions/Refclimate_SO1.5g_predictions.csv"))
head(dfRef)

# apply thresholds prodidx > 1 and survival >0.005
dfRef %>% filter(PrProdidxSOh60>1)
dfRef %>% filter(PrSurvSOh60>0.5)

# only show any color for grids where the production index exceed 1 (or 100 if you count in %) 
# and where the survival of the simulated seed orchard material (PrSurvSOhXX) exceeds 0.5 (50%).
dfRef$PrProdidxSOh60[which(dfRef$PrProdidxSOh60<1)]<-NA
dfRef$PrProdidxSOh60[which(dfRef$PrSurvSOh60<0.5)]<-NA
dfRef$PrProdidxSOh62[which(dfRef$PrProdidxSOh62<1)]<-NA
dfRef$PrProdidxSOh62[which(dfRef$PrSurvSOh62<0.5)]<-NA
dfRef$PrProdidxSOh64[which(dfRef$PrProdidxSOh64<1)]<-NA
dfRef$PrProdidxSOh64[which(dfRef$PrSurvSOh64<0.5)]<-NA
dfRef$PrProdidxSOh66[which(dfRef$PrProdidxSOh66<1)]<-NA
dfRef$PrProdidxSOh66[which(dfRef$PrSurvSOh66<0.5)]<-NA

# convert to spatial
coordinates(dfRef) <- ~ CenterLong + CenterLat
# set crs - assume lat long
proj4string(dfRef) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
crs(dfRef)
extent(dfRef)

# empty raster of correct extent and resolution
rst <- raster(crs = crs(dfRef), resolution = c(0.1,0.1), ext = extent(dfRef))
res(rst)
# for utm projection
shpSZ <- st_read(paste0(dirData,"Seed_zones_SP_Sweden/Shaper/Frözoner_tall_Sverige.shp"))
utm <- crs(shpSZ)

names(dfRef)

# list production prediction files per scenario
files <-  list.files(paste0(dirData, "Productionpredictions/"),pattern = "*.csv",full.names = T)
# just RCP8.5 to test + current to compare
files <- grep("MEAN", files, value=TRUE)

for(f in files){
  
  #f <- files[1]
  
  scenario <- substring(f,75,84)
  
  dfP <- read.csv(f)
  
  # apply thresholds
  dfP$PrProdidxSOh60[which(dfP$PrProdidxSOh60<1)]<-NA
  dfP$PrProdidxSOh60[which(dfP$PrSurvSOh60<0.5)]<-NA
  dfP$PrProdidxSOh62[which(dfP$PrProdidxSOh62<1)]<-NA
  dfP$PrProdidxSOh62[which(dfP$PrSurvSOh62<0.5)]<-NA
  dfP$PrProdidxSOh64[which(dfP$PrProdidxSOh64<1)]<-NA
  dfP$PrProdidxSOh64[which(dfP$PrSurvSOh64<0.5)]<-NA
  dfP$PrProdidxSOh66[which(dfP$PrProdidxSOh66<1)]<-NA
  dfP$PrProdidxSOh66[which(dfP$PrSurvSOh66<0.5)]<-NA
  
  coordinates(dfP)<- ~ CenterLong + CenterLat
  # set crs - assume lat long
  proj4string(dfP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
  
  for (var in names(dfP)[17:20]){ # rasterise performance for 4 seed orchards
  
  #var <- names(dfP_sf)[17]
  
  # rasterise while still lat long
  rstP <- rasterize(dfP, rst, dfP[[var]], fun=max) # unsure of use of max function here
  
  # now transform to utm, reprojection involves interpolation - default is bilinear
  rstP <- projectRaster(rstP, crs = utm, res = 1000)
  
  # write to tif
  writeRaster(rstP, paste0(dirOut,"ProdIdx_rst/",var,"_",scenario,"_thresholds.tif"), overwrite=T)
  
}
  
}



# read in as stacks to compare
rstlist <- list.files(path = paste0(dirOut,"ProdIdx_rst/"), full.names=T)

rcp45list <- grep("MEAN45in50", rstlist, value=TRUE)
rcp85list <- grep("MEAN85in50", rstlist, value=TRUE)
rcp45list2 <- grep("MEAN45in50_thresholds", rstlist, value=TRUE)
rcp85list2 <- grep("MEAN85in50_thresholds", rstlist, value=TRUE)

rcp45 <- stack(rcp45list)
rcp85 <- stack(rcp85list)
rcp45t <- stack(rcp45list2)
rcp85t <- stack(rcp85list2)

diff <- rcp85 - rcp45
spplot(diff)
avg <- stack(mean(rcp45[[1]],rcp85[[1]]),
             mean(rcp45[[2]],rcp85[[2]]),
             mean(rcp45[[3]],rcp85[[3]]),
             mean(rcp45[[4]],rcp85[[4]]))
spplot(avg)

uncertainty <- diff/avg * 100
spplot(uncertainty)


diff2 <- rcp85t - rcp45t
spplot(diff2)
avg2 <- stack(mean(rcp45t[[1]],rcp85t[[1]]),
             mean(rcp45t[[2]],rcp85t[[2]]),
             mean(rcp45t[[3]],rcp85t[[3]]),
             mean(rcp45t[[4]],rcp85t[[4]]))
spplot(avg2)

uncertainty2 <- diff2/avg2 * 100
spplot(uncertainty2)

writeRaster(uncertainty, paste0(dirOut,"ProdIdx_rst/uncertainty_RCPs.tif"), overwrite=T)
writeRaster(uncertainty2, paste0(dirOut,"ProdIdx_rst/uncertainty_RCPs_thresholds.tif"), overwrite=T)


rstlist <- list.files(path = paste0(dirOut,"ProdIdx_rst/"), full.names=T)
plotlist <- grep("MEAN|Refclimate.tif", rstlist, value=TRUE)
plotlist
prodIdx <- stack(plotlist)

names(prodIdx)
ordered_names <- c("PrProdidxSOh60_Refclimate","PrProdidxSOh60_MEAN45in50","PrProdidxSOh60_MEAN85in50",
                   "PrProdidxSOh62_Refclimate","PrProdidxSOh62_MEAN45in50","PrProdidxSOh62_MEAN85in50",
                   "PrProdidxSOh64_Refclimate","PrProdidxSOh64_MEAN45in50","PrProdidxSOh64_MEAN85in50",
                   "PrProdidxSOh66_Refclimate","PrProdidxSOh66_MEAN45in50","PrProdidxSOh66_MEAN85in50")
prodIdx <- prodIdx[[ordered_names]]
spplot(prodIdx, layout=c(3,4))

names(uncertainty) <- c("SOh60_uncertainty",
                        "SOh62_uncertainty",
                        "SOh64_uncertainty",
                        "SOh66_uncertainty")
spplot(uncertainty, layout=c(1,4))

#png(paste0(wd,"/figures/test.png"))
#par(mfrow=c(1,2))
library(gridExtra)
grid.arrange(spplot(prodIdx, layout=c(3,4)),
             spplot(uncertainty, layout=c(1,4)), ncol=2)
#dev.off()

rstlist <- list.files(path = paste0(dirOut,"ProdIdx_rst/"), full.names=T)
plotlist2 <- grep("in50_thresholds|Refclimate_thresholds", rstlist, value=TRUE)
plotlist2
prodIdx2 <- stack(plotlist2)

names(prodIdx2)
ordered_names2 <- c("PrProdidxSOh60_Refclimate_thresholds","PrProdidxSOh60_MEAN45in50_thresholds","PrProdidxSOh60_MEAN85in50_thresholds",
                   "PrProdidxSOh62_Refclimate_thresholds","PrProdidxSOh62_MEAN45in50_thresholds","PrProdidxSOh62_MEAN85in50_thresholds",
                   "PrProdidxSOh64_Refclimate_thresholds","PrProdidxSOh64_MEAN45in50_thresholds","PrProdidxSOh64_MEAN85in50_thresholds",
                   "PrProdidxSOh66_Refclimate_thresholds","PrProdidxSOh66_MEAN45in50_thresholds","PrProdidxSOh66_MEAN85in50_thresholds")
prodIdx2 <- prodIdx2[[ordered_names2]]
spplot(prodIdx2, 
       layout=c(3,4),
       widths=40)

names(uncertainty2) <- c("SOh60_uncertainty",
                        "SOh62_uncertainty",
                        "SOh64_uncertainty",
                        "SOh66_uncertainty")
spplot(uncertainty2, layout=c(1,4))



#png(paste0(wd,"/figures/test.png"))
#par(mfrow=c(1,2))
library(gridExtra)
grid.arrange(spplot(prodIdx2, layout=c(3,4)),
             spplot(uncertainty2, layout=c(1,4)), ncol=2)

library(rasterVis) # use level plot instead
library(viridis)
cols <- colorRampPalette(viridis(20))
plotNames <- c("Reference","RCP4.5","RCP8.5",
               "","","",
               "","","",
               "","","")
# create a `levelplot` plot
png(paste0(wd,"/figures/prodIdx_scenarios.png"),units="cm", width = 10, height = 20, res=500)
levelplot(prodIdx2,
          #main="Production Index uncertainty",
          layout=c(3,4),
          names.attr=plotNames,
          col.regions=cols,
          scales=list(draw=FALSE))
dev.off()
cols2 <- colorRampPalette(viridis(10, option = "C"))
png(paste0(wd,"/figures/prodIdx_spatial_uncertainty.png"),units="cm", width = 10, height = 20, res=500)
levelplot(uncertainty2,
          layout=c(1,4),
          names.attr=c("Uncertainty","","",""),
          col.regions=cols2,
          scales=list(draw=FALSE))
dev.off()

