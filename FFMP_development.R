
# date: 09/12/20
# author: VB
# description: script to test the development of Future Forest Mangement Pathways (FFMPs) using
# data provided by Skogforsk.

wd <- "~/R/FFMPs" # laptop
wd <- "~/FFMPs" # sandbox
dirData <- paste0(wd,"/data-raw/")
dirOut <- paste0(wd,"/data-processed/")

### libraries -----------------------------------------------------------------------------------------

library(tidyverse)
library(sf)
library(raster)
library(ggplot2)

### explore seed zones --------------------------------------------------------------------------------

# seed zones for Sweden sent by Mats Berlin, Skogforsk
# Alt 0246 for umlaut over o (if Num Lock available) - otherwise copy paste
shpSZ <- st_read(paste0(dirData,"Seed_zones_SP_Sweden/Shapefiler/Frözoner_Norge.shp"))

head(shpSZ)
crs(shpSZ)

ggplot(shpSZ)+
  geom_sf(aes(fill=Zon))
# this is norway...

rstSZ <- raster(paste0(dirData,"Seed_zones_SP_Sweden/Raster/Frozoner_utm33.tif"))
crs(rstSZ)
plot(rstSZ)
# yup definitely both of norway


### filter to 6 northern seed zones and simplify? -----------------------------------------------------

unique(shpSZ$Zon)
# based on the map i think we want to focus on
zones <- c("1a","1b","1c","2","3","7")

shpSZ %>% 
  filter(Zon %in% zones == T) %>% 
  ggplot()+geom_sf(aes(fill=Zon))


### explore csv data (height, survival, performance) --------------------------------------------------

# data from Henrik Hallingback, Skogforsk
dfPerformance <- read.csv(paste0(dirData,"Productionpredictions/Refclimate_SO1.5g_predictions.csv"))
head(dfPerformance)
# note. explanations of the variables in .txt file

### convert to raster ---------------------------------------------------------------------------------

# if locations are a regular grid
rstHeightLocal <- rasterFromXYZ(dfPerformance[, c('CenterLong', 'CenterLat', 'PrHeightLocal')])
# Error in rasterFromXYZ(dfPerformance[, c("CenterLong", "CenterLat", "PrHeightLocal")]) : x cell sizes are not regular

# if not, use rasterise
# means we need to convert the points to regularly gridded data, which will require averaging or some sort of function
# create a spatialpoints dataframe
spPerformance <- dfPerformance
coordinates(spPerformance) <- ~ CenterLong + CenterLat
# set crs - assume lat long


proj4string(spPerformance) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
crs(spPerformance)
extent(spPerformance)
plot(spPerformance)

# explore coords
sfPerformance <- st_as_sf(spPerformance)

# create an empty raster object to the extent of the points and resolution
# note, resolution should be 1km
# because spPerformance has lat/long coords it is complicated... need to think more
rst <- raster(crs = crs(spPerformance), resolution = c(0.1,0.1), ext = extent(spPerformance))
res(rst)
plot(rst)

# rasterise
rstHeightLocal <- rasterize(spPerformance, rst, spPerformance$GridAlt)
# needs thought on the functio used in rasterise (currently default = 'last')
# could use mean/max/modal etc.
# adds more uncertainty!
plot(rstHeightLocal)
res(rstHeightLocal)


### test getting summary stats per seed zone ----------------------------------------------------------



### test visualisation --------------------------------------------------------------------------------
# potential to use this package https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/traffic-light-plots.html

