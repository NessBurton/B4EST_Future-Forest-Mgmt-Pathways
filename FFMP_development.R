
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
  
  scenario <- substring(f,75,82)
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
check <- stack(paste0(dirOut,"SO_performance_bc85in50_SO1.5g.tif"))
names(check)
spplot(check)

### test visualisation --------------------------------------------------------------------------------

# potential to use this package https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/traffic-light-plots.html
#install.packages("robvis")
#library(robvis)
#rob_traffic_light(data = data_rob2, tool = "ROB2")


### different approach - ignore rasters for now and just spatial join ---------------------------------

# work with just 6 northernmost zones
# zones to focus on
zones <- c("1a","1b","1c","2","3","7")
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


# refclimate
df_ref <- read.csv(paste0(dirData,"Productionpredictions/Refclimate_SO1.5g_predictions.csv"))
# create a spatialpoints dataframe
sp_ref <- df_ref
coordinates(sp_ref) <- ~ CenterLong + CenterLat
# set crs - assume lat long
proj4string(sp_ref) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# transform to utm
sp_ref <- spTransform(sp_ref, CRSobj = utm )
# convert to sf
sf_ref <- st_as_sf(sp_ref)
rm(sp_ref)
# spatial join
sf_ref <- st_join(sf_ref,shpSZ_sf)
head(sf_ref)
sf_ref$ZON2 <- factor(sf_ref$ZON2)
summary(dataSF$ZON2)

sf_ref <- sf_ref[,15:19] %>% 
  filter(!is.na(ZON2)) %>% # filter to just zones
  st_drop_geometry() %>%
  pivot_longer(1:4, names_to="seed_orchard",values_to="prod_idx")
colnames(sf_ref)[3] <- "ref_prod_idx"
rm(df_ref)

# list production prediction files per scenario
files <-  list.files(paste0(dirData, "Productionpredictions/"),pattern = "*.csv",full.names = T)
# just RCP8.5 to test + current to compare
files <- grep("85in50", files, value=TRUE)
files

utm <- crs(shpSZ)

df_results_lng <- data_frame()
df_results_summary <- data_frame()
scenario_list <- c()

for (f in files){
  
  #f <- rcp85_files[2]
  
  scenario <- substring(f,75,82)
  scenario_list[[length(scenario_list) + 1]] <- scenario
  
  print("Read in data, convert to spatial, transform to utm")
  dfP <- read.csv(f)
  coordinates(dfP)<- ~ CenterLong + CenterLat
  # set crs - assume lat long
  proj4string(dfP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
  # transform to utm
  dfP <- spTransform(dfP, CRSobj = utm )
  dfP_sf <- st_as_sf(dfP)
  rm(dfP)
  
  print("Spatial join to seed zones")
  # spatial join
  dfP_sf <- st_join(dfP_sf,shpSZ_sf)
  #head(dfP_sf)
  dfP_sf$ZON2 <- factor(dfP_sf$ZON2)
  #summary(dfP_sf$ZON2)
  
  print("Transform to long format")
  df_long <- dfP_sf[,17:21] %>% 
    filter(!is.na(ZON2)) %>% # filter to just zones
    st_drop_geometry() %>%
    pivot_longer(1:4, names_to="seed_orchard",values_to="prod_idx")
  
  print("Add reference production")
  # add ref production
  df_long <- cbind(df_long,sf_ref$ref_prod_idx)
  colnames(df_long)[4] <- "ref_prod"
  
  print("Calculate percentage change")
  df_long <- df_long %>% 
    mutate(perc_change = (prod_idx - ref_prod)/ref_prod * 100)
  df_long$scenario <- scenario
  
  print("Add to results table")
  df_results_lng <- rbind(df_results_lng, df_long)
  
  print("Calculate summaries")
  df_summary <- df_long %>% 
    group_by(ZON2, seed_orchard) %>% # group by zone
    summarise_if(is.numeric,c("mean","sd","IQR","min","max")) #%>% 
    #summarise(c("PrProdidxSOh60","PrProdidxSOh62","PrProdidxSOh64","PrProdidxSOh66"),.funs=c("mean","sd","IQR","min","max"))
  df_summary$scenario <- scenario
  
  print("Add to results table")
  df_results_summary <- rbind(df_results_summary, df_summary)
  
  print(paste0("Processed scenario: ",scenario))
  
}

df_results_lng$change <- NA
df_results_lng$change[which(df_results_lng$perc_change<0)]<-"decline"
df_results_lng$change[which(df_results_lng$perc_change>0)] <- "increase"

df_results_lng$seed.orchard <- substring(df_results_lng$seed_orchard,10,14)

library(RColorBrewer)
#brewer.pal(n = 8, name = "Dark2")
pal <- c("#D95F02","#1B9E77")
df_results_lng %>% 
  filter(scenario != "MEAN85in") %>% 
  ggplot()+
  geom_boxplot(aes(seed.orchard,perc_change,col=change))+coord_flip()+
  #scale_color_brewer(palette = "Dark2")+
  scale_color_manual(values=pal)+  
  facet_grid(ZON2~scenario)+
  xlab("Seed orchard choice")+
  ylab("Change in production index from reference climate (%)")

df_results_lng %>% 
  filter(scenario != "MEAN85in") %>% 
  ggplot()+
  geom_boxplot(aes(seed_orchard,perc_change,col=change))+coord_flip()+
  facet_grid(rows="ZON2")

df_results_summary$change <- NA
df_results_summary$change[which(df_results_summary$perc_change_mean<0)]<-"decline"
df_results_summary$change[which(df_results_summary$perc_change_mean>0)] <- "increase"

df_results_summary %>% 
  filter(scenario != "MEAN85in") %>% 
  ggplot()+
  geom_point(aes(seed_orchard,perc_change_mean,size=perc_change_mean,col=change))+coord_flip()+
  facet_grid(ZON2~scenario)


