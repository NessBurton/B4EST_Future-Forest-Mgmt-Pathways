
wd <- "~/FFMPs" # sandbox
dirData <- paste0(wd,"/data-raw/")
dataDrive <- "D:"
dirOut <- paste0(dataDrive,"/FFMP-data-processed/")
dirFigs <- paste0(wd,"/figures/")

### libraries ------------------------------------------------------------------

library(tidyverse)
library(sf)
library(raster)
library(ggplot2)
library(viridis)
library(rnaturalearth)

### load data ------------------------------------------------------------------

sfSeedZones <- st_read(paste0(dirData,"Seed_zones_SP_Sweden/Shaper/Frözoner_tall_Sverige.shp"))
utm <- crs(sfSeedZones)

# refclimate predictions
dfRef <-  read.csv(paste0(dirData, "Productionpredictions/Refclimate_SO1.5g_predictions.csv"))
head(dfRef)

### create ref climate rasters -------------------------------------------------

dfRef <- dfRef[,c(2:3,9:20)]
dfRef[,7:14] <- round(dfRef[,7:14]*100, digits = 3) # convert survival & prod indices to %

# apply thresholds
dfRef$PrProdidxSOh60[which(dfRef$PrSurvSOh60<50)]<-NA
dfRef$PrProdidxSOh62[which(dfRef$PrSurvSOh62<50)]<-NA
dfRef$PrProdidxSOh64[which(dfRef$PrSurvSOh64<50)]<-NA
dfRef$PrProdidxSOh66[which(dfRef$PrSurvSOh66<50)]<-NA
dfRef$PrProdidxSOh60[which(dfRef$PrProdidxSOh60<100)]<-NA
dfRef$PrProdidxSOh62[which(dfRef$PrProdidxSOh62<100)]<-NA
dfRef$PrProdidxSOh64[which(dfRef$PrProdidxSOh64<100)]<-NA
dfRef$PrProdidxSOh66[which(dfRef$PrProdidxSOh66<100)]<-NA

# convert to spatial points

spRef <- dfRef
#rm(dfRef)
coordinates(spRef) <- ~ CenterLong + CenterLat

# define lat long crs
proj4string(spRef) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 

# transform points to utm
spRef <- spTransform(spRef, CRSobj = utm)

# create an empty raster object to the extent of the points desired resolution
# res should be 1km - 1000m if UTM, using 1100m to deal with irregular grid (gaps if using 1000m)
rstUTM <- raster(crs = crs(spRef), resolution = c(1100,1100), ext = extent(spRef))

for (var in names(spRef)){ 
  
  #var <- names(spRef)[10]
  
  # create unique filepath for temp directory
  #dir.create(file.path("D:",var), showWarnings = FALSE)
  
  # set temp directory
  #rasterOptions(tmpdir=file.path("D:",var))
  #rasterOptions()
  
  print(paste0("Rasterising for var = ", var))
  
  # rasterise 
  #tmp <- rasterTmpFile()
  #rasterize(spRef, rstUTM, spRef[[var]], fun=max, na.rm=TRUE, filename=tmp)
  rst <- rasterize(spRef, rstUTM, spRef[[var]], fun=max, na.rm=TRUE) 
  
  print(paste0("Rasterised for var: ", var))
  
  writeRaster(rst, paste0(dirOut,"pred_rst/",var,"_Refclimate_thresholds.tif"),overwrite=TRUE)
  
  print(paste0("Written raster for: ", var))
  
  #unlink(file.path("D:",var), recursive = TRUE)
  
}

### elevation raster -----------------------------------------------------------

dfRef <-  read.csv(paste0(dirData, "Productionpredictions/Refclimate_SO1.5g_predictions.csv"))
head(dfRef)
spRef <- dfRef
#rm(dfRef)
coordinates(spRef) <- ~ CenterLong + CenterLat

# define lat long crs
proj4string(spRef) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 

# transform points to utm
spRef <- spTransform(spRef, CRSobj = utm)

# create an empty raster object to the extent of the points desired resolution
# res should be 1km - 1000m if UTM, using 1100m to deal with irregular grid (gaps if using 1000m)
rstUTM <- raster(crs = crs(spRef), resolution = c(1100,1100), ext = extent(spRef))

rst <- rasterize(spRef, rstUTM, spRef$GridAlt, fun=max, na.rm=TRUE) 

plot(rst)

writeRaster(rst, paste0(dirOut,"pred_rst/elevation.tif"),overwrite=TRUE)
