
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

# Method 1 (not using as it leaves data gaps)
# transform points to utm
#utm <- crs(shpSZ)
#spPerformance <- spTransform(spPerformance, CRSobj = utm)
#plot(spPerformance)
#crs(spPerformance)
# create an empty raster object to the extent of the points and resolution
# note, resolution should be 1km - so assuming 1000m res ok using utm projection
#rst <- raster(crs = crs(spPerformance), resolution = c(1000,1000), ext = extent(spPerformance))
#res(rst)
# rasterise
#head(spPerformance)
#rstHeightLocal <- rasterize(spPerformance, rst, spPerformance$PrHeightLocal, fun=max)
# needs thought on the function used in rasterise (currently default = 'last')
# could use mean/max/modal etc.
# adds more uncertainty!
#plot(rstHeightLocal)
#summary(rstHeightLocal)
# some gaps in the data - because of irregular grid?
#res(rstHeightLocal)
# requires interpolation to remove gaps
# can we get around this a different way?
# i think i've solved using method 2 below

# Method 2
# empty raster of correct extent and resolution
rst <- raster(crs = crs(spPerformance), resolution = c(0.1,0.1), ext = extent(spPerformance))
res(rst)
# rasterise while still lat long
rstHeightLocal <- rasterize(spPerformance, rst, spPerformance$PrHeightLocal, fun=max)
crs(rstHeightLocal)
plot(rstHeightLocal)
# now transform to utm, reprojection involves interpolation - default is bilinear
utm <- crs(shpSZ)
rstHeightLocal <- projectRaster(rstHeightLocal, crs = utm, res = 1000)
plot(rstHeightLocal)
res(rstHeightLocal)

### test getting summary stats per seed zone ----------------------------------------------------------

# check projected ok.
plot(rstHeightLocal)
plot(shpSZ, add=TRUE)
# seed zones don't cover entire prediction area but that's ok...

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
plot(shpSZ_sf)

# convert back to sp to be able to use extract in next step
shpSZ_sp <- as_Spatial(shpSZ_sf)
plot(shpSZ_sp)

plot(rstHeightLocal);plot(shpSZ_sp,add=TRUE)

# data frame of mean predicted height per seed zone
dfSeedZones <- extract(rstHeightLocal, shpSZ_sp, fun=mean, df=TRUE, na.rm=TRUE)
# needs some discussion as to which summary stat is most appropriate.
# perhaps useful to have min, max, and mean to compare?
# then set thresholds based on these values
sdSeedZones <- extract(rstHeightLocal,shpSZ_sp,fun=sd,df=TRUE,na.rm=TRUE)


### loop to do this for all GCMs -------------------------------------------------------------

# just focus on RCP8.5 (all GCMs) compared to present


# list production prediction files
files <-  list.files(paste0(dirData, "Productionpredictions/"),pattern = "*.csv",full.names = T)
# just RCP8.5 to test + current to compare
rcp85_files <- grep("85in50|Refclimate", files, value=TRUE)
#multiple_csv <- sapply(rcp85_files, read.csv)

# for reprojection in loop
utm <- crs(shpSZ)

# empty data frame to store results
#df_results <- data.frame(rep(shpSZ_sp$ZON2,4))
#colnames(df_results)[1] <- "Zone"
#df_results$stat <- c(rep("Pmin",6),rep("Pmean",6),rep("Pmax",6),rep("Psd",6))

# scenario list
scenario_list <- c()


for (f in rcp85_files){
  
  #f <- rcp85_files[1]
  
  scenario <- substring(f,75,89)
  scenario_list[[length(scenario_list) + 1]] <- scenario
  
  print(paste0("Processing for scenario = ", scenario))
  
  dfP <- read.csv(f)
  
  # convert to spatial
  coordinates(dfP) <- ~ CenterLong + CenterLat
  # define lat long crs
  proj4string(dfP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
  
  print(paste0("Converted to spatial points"))
  
  print(paste0("Loop through seed orchard performance and rasterise"))
  
  # empty raster stack for each scenario
  rstStack <- stack()
  
  for (var in names(dfP)[17:20]){ # rasterise performance for 4 seed orchards
    
    #var <- names(dfP)[17]
    
    # rasterise while still lat long
    rstP <- rasterize(dfP, rst, dfP[[var]], fun=max) # unsure of use of max function here
    
    print(paste0("Rasterised (lat/long) for var: ", var))
    
    # now transform to utm, reprojection involves interpolation - default is bilinear
    rstP <- projectRaster(rstP, crs = utm, res = 1000)
    rstStack <- addLayer(rstStack, rstP)
    
    #writeRaster(rstP, paste0(dirOut,var,"_",scenario,".tif"),overwrite=TRUE)
    
    #print(paste0("Raster transformed to UTM and written to .tif for var: ",var))
    
    #print(paste0("Extracting values for var: ",var))
    
    #Pmin <- extract(rstP, shpSZ_sp, fun=min, na.rm=TRUE)
    #Pmean <- extract(rstP, shpSZ_sp, fun=mean, na.rm=TRUE)
    #Pmax <- extract(rstP, shpSZ_sp, fun=max, na.rm=TRUE)
    #Psd <- extract(rstP, shpSZ_sp, fun=sd, na.rm=TRUE)
    
    #print(paste0("Min, Mean, Max extracted for var: ",var))
    
    #Pvalues <- rbind(Pmin,Pmean,Pmax,Psd)
    #df_results <- cbind(df_results, Pvalues)
    
    #print(paste0("Var ",var," added to data frame"))
    
  }
  
  #colnames(df_results)[3:9] <- scenario_list
  #write.csv(df_results, paste0(dirOut,"df_SO_performance_RCP85.csv"))
  names(rstStack) <- names(dfP)[17:20]
  writeRaster(rstStack, paste0(dirOut, "SO_performance_",scenario,".tif"), overwrite=TRUE)
  
}

# check
GCM_bc <- stack(paste0(dirOut,"SO_performance_bc85in50_SO1.5g.tif"))
names(GCM_bc)
spplot(GCM_bc)

# to do:
# a second loop or apply to extract values from each SO stack

#write.csv(df_results, paste0(dirOut,"df_performance_PrProdidxSOh66.csv"))
#write.csv(df_results, paste0(dirOut,"df_performance_PrProdidxSOh60.csv"))

SOh66 <- read.csv(paste0(dirOut,"df_performance_PrProdidxSOh66.csv"))
SOh66$seedOrchard <- "SOh66"
SOh60 <- read.csv(paste0(dirOut,"df_performance_PrProdidxSOh60.csv"))
SOh60$seedOrchard <- "SOh60"

df_results <- rbind(SOh66,SOh60)
df_results$X <- NULL

df_results_lng <- df_results %>% 
  pivot_longer(cols = 3:8, names_to="scenario",values_to="performance")

df_results_lng <- df_results_lng %>% 
  mutate(percChange = (performance - Refclimate_SO1.)/Refclimate_SO1. * 100)

summary(df_results_lng$percChange)

df_results_lng$change <- NA
df_results_lng$change[which(df_results_lng$percChange<0)] <- "Small decline"
df_results_lng$change[which(df_results_lng$percChange>0 
                            & df_results_lng$percChange<=5)] <- "Small increase"
df_results_lng$change[which(df_results_lng$percChange>5 
                            & df_results_lng$percChange<=20)] <- "Increase"
df_results_lng$change[which(df_results_lng$percChange>20)] <- "Large increase"

### test visualisation --------------------------------------------------------------------------------

# potential to use this package https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/traffic-light-plots.html
#install.packages("robvis")
#library(robvis)
#rob_traffic_light(data = data_rob2, tool = "ROB2")

df <- df_results_lng
df$Zone <- factor(df$Zone, levels = zones)
df$change <- factor(df$change, levels = c("Small decline","Small increase","Increase","Large increase"))

# colour scale
library(RColorBrewer)
display.brewer.all()
changeCols <- brewer.pal(4,"RdYlGn")
names(changeCols) <- levels(df$change)
colScale <- scale_colour_manual(name = "change",values = changeCols)

df$dummyX <- "x"

df <- left_join(df,dfElev,by="Zone")

df %>% 
  filter(stat=="Pmin") %>% 
  ggplot()+
  geom_point(aes(seedOrchard,dummyX,size=percChange,col=change))+ coord_flip()+
  #facet_wrap(~Zone, nrow = 7)+
  facet_grid(desc~scenario)+
  colScale +
  ylab("Seed orchard choice")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

df %>% 
  ggplot()+
  geom_boxplot(aes(seedOrchard,percChange))+ coord_flip()+
  facet_grid(Zone~scenario)+
  geom_vline(xintercept = 0)

# aim for facet_wrap(Zone~scenario)
# with x aes replaced by seed orchard

### get elevation per zone to define -------------------------------------------

rstElev <- rasterize(spPerformance, rst, spPerformance$GridAlt, fun=max)
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
dfElev$location <- c("North","North","Central","Central","South","South")          
dfElev$desc <- paste0(dfElev$location,"-",dfElev$elev)

colnames(shpSZ_sf)[1] <- "Zone"
shpSZ_sf <- left_join(shpSZ_sf,dfElev,by="Zone")
ggplot(shpSZ_sf)+
  geom_sf(aes(fill=desc))
