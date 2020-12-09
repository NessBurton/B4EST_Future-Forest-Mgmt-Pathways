
# date: 09/12/20
# author: VB
# description: script to test the development of Future Forest Mangement Pathways (FFMPs) using
# data provided by Skogforsk.

wd <- "~/R/FFMPs"
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
shpSZ <- st_read(paste0(dirData,"Seed_zones_SP_Sweden/Kartdata/Shapefiler/Frözoner_Norge.shp"))

head(shpSZ)
crs(shpSZ)

ggplot(shpSZ)+
  geom_sf(aes(fill=Zon))
# this is norway...

rstSZ <- raster(paste0(dirData,"Seed_zones_SP_Sweden/Kartdata/Raster/Frozoner_utm33.tif"))
crs(rstSZ)
plot(rstSZ)
# yup definitely norway


### filter to 6 northern seed zones and simplify? -----------------------------------------------------

unique(shpSZ$Zon)
# based on the map i think we want to focus on
zones <- c("1a","1b","1c","2","3","7")

shpSZ %>% 
  filter(Zon %in% zones == T) %>% 
  ggplot()+geom_sf(aes(fill=Zon))


### explore csv data (height, survival, performance) --------------------------------------------------

# e.g.
dfHeight <- read.csv()


### convert to raster ---------------------------------------------------------------------------------

# if locations are a regular grid
rstHeight <- rasterFromXYZ(dfHeight[, c('lon', 'lat', 'height')])

# if not, set extent and use rasterize
extent(shpSZ)
x <- raster(xmn=-76025, xmx=622975, ymn=6448975, ymx=7601975, res=1, crs="+proj=utm +zone=33 +datum=WGS84")
rstHeight <- rasterize(dfHeight[, c('lon', 'lat')], x, temp[, 'height'], fun=mean) # set different function - max? modal?

plot(rstHeight)


### test getting summary stats per seed zone ----------------------------------------------------------



### test visualisation --------------------------------------------------------------------------------
# potential to use this package https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/traffic-light-plots.html

