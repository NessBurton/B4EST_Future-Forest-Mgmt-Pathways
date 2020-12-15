
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
#prodIdxSOh60 <- projectRaster(prodIdxSOh60, crs = utm, res = 1000)
#plot(rstHeightLocal)
#res(rstHeightLocal)

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

### test uncertainty map -------------------------------------------------------

# Chakraborty et al (2016) maps of uncertainty 
# (calculated as the difference in predicted performance between two climate scenarios, 
# as a percentage of the mean of the two predictions).

diff <- prodIdxSOh60_85 - prodIdxSOh60_45
plot(diff)
mean <- mean(prodIdxSOh60_45,prodIdxSOh60_85)
plot(mean)

uncertainty <- diff/mean * 100
plot(uncertainty)

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

for (var in names(dfRef)[15:18]){ # rasterise performance for 4 seed orchards
  
  #var <- names(dfRef)[17]
  
  # rasterise while still lat long
  rstP <- rasterize(dfRef, rst, dfRef[[var]], fun=max) # unsure of use of max function here
  
  # now transform to utm, reprojection involves interpolation - default is bilinear
  rstP <- projectRaster(rstP, crs = utm, res = 1000)
  
  # write to tif
  writeRaster(rstP, paste0(dirOut,"ProdIdx_rst/",var,"_Refclimate_thresholds.tif"))
  
}

# read in as stacks to compare

rstlist <- list.files(path = paste0(dirOut,"ProdIdx_rst/"), pattern='.tif$', full.names=T)
rstlist
prodIdx <- stack(rstlist)
spplot(prodIdx)
res(prodIdx)
