
# date: 26/01/21
# author: VB
# description:

wd <- "~/FFMPs" # sandbox
dirData <- paste0(wd,"/data-raw/")
dirOut <- paste0(wd,"/data-processed/")
dirFigs <- paste0(wd,"/figures/")

### libraries ------------------------------------------------------------------

library(tidyverse)
library(sf)
library(raster)
library(ggplot2)
library(viridis)
library(rnaturalearth)

### plan -----------------------------------------------------------------------

# loop through scenarios
# within each scenario, rasterise height, survival & performance for each seed orchard
# write stack per seed orchard
# per seed zone (discuss simplification with Mats?)
# extract mean, sd, min, max
# % of area where all models agree (on prediction above specified threshold)


### seed zones -----------------------------------------------------------------

sfSeedZones <- st_read(paste0(dirData,"Seed_zones_SP_Sweden/Shaper/Frözoner_tall_Sverige.shp"))
utm <- crs(sfSeedZones)

# dissolve/merge zones by ZON2 to simplify
head(sfSeedZones)

# add area to have a variable to be able to summarise
sfSeedZones$area <- st_area(sfSeedZones) 
sfSeedZones <-
  sfSeedZones %>%
  group_by(ZON2) %>% 
  summarise(area = sum(area))

unique(sfSeedZones$ZON2)
zoneOrder <- c("1a","1b","1c","2","3","6","7","10000","12000","13000","15000","16000","18100","18200","18300","18400","19100","19200","19300","19400","20100","20200")
sfSeedZones$ZON2 <- factor(sfSeedZones$ZON2, ordered = TRUE, levels = zoneOrder)

# plot
ggplot(sfSeedZones)+
  geom_sf(aes(fill=ZON2),col=NA)+theme_minimal()

# sp version to use for raster::extract later
spSeedZones <- as_Spatial(sfSeedZones)


### rasterise from csv ---------------------------------------------------------

# list production prediction files
files <-  list.files(paste0(dirData, "Productionpredictions/"),pattern = "*.csv",full.names = T)

# scenario list
scenario_list <- c()

for (f in files){
  
  f <- files[1]
  
  scenario <- strsplit(f, "[_]")[[1]][1]
  scenario <- strsplit(scenario, "[/]")[[1]][8]
  
  scenario_list[[length(scenario_list) + 1]] <- scenario
  
  print(paste0("Processing for scenario = ", scenario))
  
  dfP <- read.csv(f)
  
  #print("Remove prediction data where survival below 50%")
  
  #dfP$PrProdidxSOh60[which(dfP$PrSurvSOh60<0.5)] <- NA
  #dfP$PrProdidxSOh62[which(dfP$PrSurvSOh62<0.5)]<-NA
  #dfP$PrProdidxSOh64[which(dfP$PrSurvSOh64<0.5)]<-NA
  #dfP$PrProdidxSOh66[which(dfP$PrSurvSOh66<0.5)]<-NA
  
  print("Convert to spatial points")
  
  # convert to spatial
  spP <- dfP
  coordinates(spP) <- ~ CenterLong + CenterLat
  # define LAEA crs
  proj4string(spP) <- CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs") 
  # define lat long crs
  #proj4string(spP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
  
  print(paste0("Converted to spatial points & original crs defined"))
  
  #print(paste0("Transform to UTM"))
  spP <- spTransform(spP,CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs"))
  pxP <- SpatialPixelsDataFrame(spP, tolerance = 0.5, spP@data)
  crs(pxP)
  rst1 <- raster(pxP[,'PrSurvSOh60'])
  plot(rst1)
  res(rst1);crs(rst1)
  plot(rst);plot(spSeedZones,add=TRUE)
  
  rst2 <- projectRaster(rst1, crs = utm, res = 0.01)
  plot(rst2);plot(spSeedZones,add=TRUE)
  
  dfSeedZones <- extract(rst2, spSeedZones, fun=mean, df=TRUE, na.rm=TRUE)
  
  print(paste0("Loop through seed orchard performance and rasterise"))
  
  # empty raster of correct extent and resolution
  rst <- raster(crs = crs(spP), resolution = c(0.01,0.01), ext = extent(spP))
  res(rst)
  
  for (var in names(dfP)[17:20]){ # rasterise performance for 4 seed orchards
    
    var <- names(dfP)[17]
    print(paste0("Rasterising for var = ", var))
    
    # rasterise while still LAEA
    rstP <- rasterize(spP, rst, dfP[[var]], fun=max) # unsure of use of max function here
    plot(rstP)
    
    print(paste0("Rasterised (lat/long) for var: ", var))
    
    # now transform to utm, reprojection involves interpolation - default is bilinear
    rstP <- projectRaster(rstP, crs = utm, res = 0.01)
    plot(rstP);plot(spSeedZones,add=TRUE)
    #rstStack <- addLayer(rstStack, rstP)
    
    writeRaster(rstP, paste0(dirOut,"ProdIdx_rst/",var,"_",scenario,".tif"),overwrite=TRUE)
    print(paste0("Written raster for: ", var))
    
  }
  
}
