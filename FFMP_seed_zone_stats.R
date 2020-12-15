
wd <- "~/R/FFMPs" # laptop
wd <- "~/FFMPs" # sandbox
dirData <- paste0(wd,"/data-raw/")
dirOut <- paste0(wd,"/data-processed/")

### libraries ------------------------------------------------------------------

library(tidyverse)
library(sf)
library(raster)
library(ggplot2)

### reference data -------------------------------------------------------------

# refclimate
df_ref <- read.csv(paste0(dirData,"Productionpredictions/Refclimate_SO1.5g_predictions.csv"))
# create a spatialpoints dataframe
sp_ref <- df_ref
coordinates(sp_ref) <- ~ CenterLong + CenterLat
# set crs - assume lat long
proj4string(sp_ref) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

### seed zones -----------------------------------------------------------------

# seed zones for Sweden sent by Mats
# Alt 0246 for umlaut over o (if Num Lock available) - otherwise copy paste
shpSZ <- st_read(paste0(dirData,"Seed_zones_SP_Sweden/Shaper/Frözoner_tall_Sverige.shp"))

head(shpSZ)
crs(shpSZ)

ggplot(shpSZ)+
  geom_sf(aes(fill=ZON2))

# filter to 6 northern seed zones and simplify 
unique(shpSZ$ZON2)
# zones to focus on
zones <- c("1a","1b","1c","2","3","7")

# work with just 6 northernmost zones
shpSZ <- shpSZ %>% 
  filter(ZON2 %in% zones == T)

# dissolve/merge zones by ZON2 to simplify
shpSZ_sf <- st_as_sf(shpSZ)
shpSZ_sf$area <- st_area(shpSZ_sf) # add area to have a variable to be able to summarise
shpSZ_sf <-
  shpSZ_sf %>%
  group_by(ZON2) %>% 
  summarise(area = sum(area))
plot(shpSZ_sf[1])
shpSZ_sp <- as_Spatial(shpSZ_sf)

rstElev <- rasterize(sp_ref, rst, sp_ref$GridAlt, fun=max)
crs(rstElev)
plot(rstElev)
# now transform to utm, reprojection involves interpolation - default is bilinear
utm <- crs(shpSZ)
rstElev <- projectRaster(rstElev, crs = utm, res = 1000)
plot(rstElev)
dfElev <- extract(rstElev, shpSZ_sp, fun=max, df=TRUE, na.rm=TRUE)
dfElev$Zone <- shpSZ_sp$ZON2

ggplot(shpSZ_sf)+
  geom_sf(aes(fill=ZON2))

dfElev$elev <- rep(c("Upland","Lowland"),3)
dfElev$location <- c("67°N","67°N","66°N","66°N","64°N","64°N")          
dfElev$desc <- paste0(dfElev$location,"-",dfElev$elev)
dfElev$desc <- factor(dfElev$desc, levels=c("67°N-Upland","67°N-Lowland","66°N-Upland","66°N-Lowland","64°N-Upland", "64°N-Lowland"))

colnames(shpSZ_sf)[1] <- "Zone"
shpSZ_sf <- left_join(shpSZ_sf,dfElev,by="Zone")
ggplot(shpSZ_sf)+
  geom_sf(aes(fill=desc))
