
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

### check memory ---------------------------------------------------------------

memory.size()
memory.limit()
memory.limit(size = 56000)

### rasterise from csv ---------------------------------------------------------

# list production prediction files
files <-  list.files(paste0(dirData, "Productionpredictions/"),pattern = "*.csv",full.names = T)

# scenario list
scenario_list <- c()

for (f in files){
  
  #f <- files[5]
  
  scenario <- strsplit(f, "[_]")[[1]][1]
  scenario <- strsplit(scenario, "[/]")[[1]][8]
  
  scenario_list[[length(scenario_list) + 1]] <- scenario
  
  print(paste0("Processing for scenario = ", scenario))
  
  dfP <- read.csv(f)
  
  #print("Remove data where survival below 50%")
  
  #dfP$PrProdidxSOh60[which(dfP$PrSurvSOh60<0.5)] <- NA
  #dfP$PrProdidxSOh62[which(dfP$PrSurvSOh62<0.5)]<-NA
  #dfP$PrProdidxSOh64[which(dfP$PrSurvSOh64<0.5)]<-NA
  #dfP$PrProdidxSOh66[which(dfP$PrSurvSOh66<0.5)]<-NA
  
  print("Convert to spatial points")
  
  # convert to spatial
  spP <- dfP
  coordinates(spP) <- ~ CenterLong + CenterLat
  crs(spP)
  
  # define lat long crs
  proj4string(spP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
  crs(spP)
  
  print(paste0("Transform to UTM"))
  # transform points to utm
  spP <- spTransform(spP, CRSobj = utm)
  
  # create an empty raster object to the extent of the points desired resolution
  # res should be 1km - 1000m if UTM, using 1100m to deal with irregular grid (gaps if using 1000m)
  rstUTM <- raster(crs = crs(spP), resolution = c(1100,1100), ext = extent(spP))
  
  for (var in names(dfP)[11:22]){ # rasterise performance for 4 seed orchards
    
    #var <- names(dfP)[11]
    print(paste0("Rasterising for var = ", var))
    
    # rasterise 
    rstP <- rasterize(spP, rstUTM, spP[[var]])
    p1 <- plot(rstP)
    print(p1)

    print(paste0("Rasterised for var: ", var))
    
    writeRaster(rstP, paste0(dirOut,"pred_rst/",var,"_",scenario,".tif"),overwrite=TRUE)
    print(paste0("Written raster for: ", var))
    
  }
  
}

# read in rasters as stacks and extract values to seed zones
