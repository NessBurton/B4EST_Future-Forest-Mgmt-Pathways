
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

# seed zones for Sweden sent by Mats
# Alt 0246 for umlaut over o (if Num Lock available) - otherwise copy paste
shpSZ <- st_read(paste0(dirData,"Seed_zones_SP_Sweden/Shaper/Frözoner_tall_Sverige.shp"))

head(shpSZ)
crs(shpSZ)

ggplot(shpSZ)+
  geom_sf(aes(fill=ZON2))

### filter to 6 northern seed zones and simplify? -----------------------------------------------------

unique(shpSZ$ZON2)

# zones to focus on
zones <- c("1a","1b","1c","2","3","7")

shpSZ %>% 
  filter(ZON2 %in% zones == T) %>% 
  ggplot()+geom_sf(aes(fill=ZON2))

crs(shpSZ) # utm

# work with smaller area to speed things up
shpSZ <- shpSZ %>% 
  filter(ZON2 %in% zones == T)


### explore csv data (height, survival, performance) --------------------------------------------------

# data from Henrik
dfPerformance <- read.csv(paste0(dirData,"Productionpredictions/Refclimate_SO1.5g_predictions.csv"))
head(dfPerformance)
# note. explanations of the variables in .txt file in Productionpredictions folder

### convert to raster ---------------------------------------------------------------------------------

# if locations are a regular grid
#rstHeightLocal <- rasterFromXYZ(dfPerformance[, c('CenterLong', 'CenterLat', 'PrHeightLocal')])
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
#plot(spPerformance)

# transform to utm
utm <- crs(shpSZ)
spPerformance <- spTransform(spPerformance, CRSobj = utm)
plot(spPerformance)
crs(spPerformance)

# create an empty raster object to the extent of the points and resolution
# note, resolution should be 1km - so assuming 1000m res ok using utm projection
rst <- raster(crs = crs(spPerformance), resolution = c(1000,1000), ext = extent(shpSZ))
res(rst)

# rasterise
head(spPerformance)
rstHeightLocal <- rasterize(spPerformance, rst, spPerformance$PrHeightLocal, fun=max)
# needs thought on the function used in rasterise (currently default = 'last')
# could use mean/max/modal etc.
# adds more uncertainty!
plot(rstHeightLocal)
summary(rstHeightLocal)
# some gaps in the data - because of irregular grid?
res(rstHeightLocal)
# requires interpolation to remove gaps
# can we get around this a different way?
# ask Maurizio for advice


### test getting summary stats per seed zone ----------------------------------------------------------

# check projected ok.
plot(rstHeightLocal)
plot(shpSZ, add=TRUE)
# seed zones don't cover entire prediction area but that's ok...

# dissolve/merge zones by ZON2 to simplify
shpSZ_sf <- st_as_sf(shpSZ)
shpSZ_sf$area <- st_area(shpSZ_sf) # add area to have a variable to be able to summarise
shpSZ_sf <-
  shpSZ_sf %>%
  group_by(ZON2) %>% 
  summarise(area = sum(area))
plot(shpSZ_sf)

# convert back to sp to be able to use extract in next step
shpSZ_sp <- as_Spatial(shpSZ_sf)
plot(shpSZ_sp)

# data frame of mean predicted height per seed zone
dfSeedZones <- extract(rstHeightLocal, shpSZ_sp, fun=mean, df=TRUE, na.rm=TRUE)
# needs some discussion as to which summary stat is most appropriate.
# perhaps useful to have min, max, and mean to compare?
# then set thresholds based on these values


### loop to do this for several scenarios -------------------------------------------------------------

# just focus on RCP8.5 (all GCMs) compared to present
# and just one variable PrProdidxSOh60

# list production prediction files
files <-  list.files(paste0(dirData, "Productionpredictions/"),pattern = "*.csv",full.names = T)
# just RCP8.5 to test + current to compare
rcp85_files <- grep("85in50|Refclimate", files, value=TRUE)
#multiple_csv <- sapply(rcp85_files, read.csv)

# for reprojection in loop
utm <- crs(shpSZ)
# empty data frame to store results
df_results <- data.frame(shpSZ_sp$ZON2)
# scenario list
scenario_list <- c()

for (i in rcp85_files){
  
  #i <- rcp85_files[7]
  
  scenario <- substring(i,75,89)
  scenario_list[[length(scenario_list) + 1]] <- scenario
  
  dfP <- read.csv(i)
  # convert to spatial
  coordinates(dfP) <- ~ CenterLong + CenterLat
  # define lat long crs
  proj4string(dfP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
  #crs(dfP)
  #extent(dfP)
  # transform to utm
  dfP <- spTransform(dfP, CRSobj = utm)
  
  rstP <- rasterize(dfP, rst, dfP$PrProdidxSOh60, fun=max)
  
  values <- extract(rstP, shpSZ_sp, fun=mean, na.rm=TRUE)
  
  df_results <- cbind(df_results, values)
  
}

colnames(df_results)[2:8] <- scenario_list

df_results_lng <- df_results %>% 
  pivot_longer(cols = 2:7, names_to="scenario",values_to="performance")

df_results_lng <- df_results_lng %>% 
  mutate(diff = performance - Refclimate_SO1.)

### test visualisation --------------------------------------------------------------------------------
# potential to use this package https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/traffic-light-plots.html

install.packages("robvis")
library(robvis)

rob_traffic_light(data = data_rob2, tool = "ROB2")
