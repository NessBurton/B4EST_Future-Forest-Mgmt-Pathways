
# date: 26/01/21
# author: VB
# description:

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
  dfP <- (dfP)[,c(2:3,11:22)]
  dfP[,7:14] <- round(dfP[,7:14]*100, digits = 3) # convert survival & prod indices to %
  
  # apply thresholds
  dfP$PrProdidxSOh60[which(dfP$PrSurvSOh60<50)]<-NA
  dfP$PrProdidxSOh62[which(dfP$PrSurvSOh62<50)]<-NA
  dfP$PrProdidxSOh64[which(dfP$PrSurvSOh64<50)]<-NA
  dfP$PrProdidxSOh66[which(dfP$PrSurvSOh66<50)]<-NA
  dfP$PrProdidxSOh60[which(dfP$PrProdidxSOh60<100)]<-NA
  dfP$PrProdidxSOh62[which(dfP$PrProdidxSOh62<100)]<-NA
  dfP$PrProdidxSOh64[which(dfP$PrProdidxSOh64<100)]<-NA
  dfP$PrProdidxSOh66[which(dfP$PrProdidxSOh66<100)]<-NA
  
  print("Convert to spatial points")
  
  # convert to spatial
  spP <- dfP
  rm(dfP)
  coordinates(spP) <- ~ CenterLong + CenterLat
  
  # define lat long crs
  proj4string(spP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 

  print(paste0("Transform to UTM"))
  # transform points to utm
  spP <- spTransform(spP, CRSobj = utm)
  
  # create an empty raster object to the extent of the points desired resolution
  # res should be 1km - 1000m if UTM, using 1100m to deal with irregular grid (gaps if using 1000m)
  rstUTM <- raster(crs = crs(spP), resolution = c(1100,1100), ext = extent(spP))
  
  for (var in names(spP)){ 
    
    #var <- names(spP)[10]
    
    # create unique filepath for temp directory
    #dir.create(file.path("D:",var), showWarnings = FALSE)
    
    # set temp directory
    #rasterOptions(tmpdir=file.path("D:",var))
    #rasterOptions()

    print(paste0("Rasterising for var = ", var))
    
    # rasterise 
    #tmp <- rasterTmpFile()
    #rasterize(spP, rstUTM, spP[[var]], fun=max, na.rm=TRUE, filename=tmp)
    rst <- rasterize(spP, rstUTM, spP[[var]], fun=max, na.rm=TRUE) 
    
    print(paste0("Rasterised for var: ", var))
    
    writeRaster(rst, paste0(dirOut,"pred_rst/",var,"_",scenario,"_thresholds.tif"),overwrite=TRUE)
    
    print(paste0("Written raster for: ", var))
    
    #unlink(file.path("D:",var), recursive = TRUE)
    
  }
  
 }

# link for setting up temp directory
# https://stackoverflow.com/questions/18955305/setting-an-overwriteable-temporary-file-for-rasters-in-r


# note issue with ref climate - different column set-up to the rest of the files?
files
refClimate <- read.csv(files[13])
head(refClimate)

### read in rasters as stacks and extract values to seed zones -----------------

dirInputRasters <- paste0(dirOut,"pred_rst")

# list tifs
tifs <- list.files(paste0(dirInputRasters), full.names = TRUE)
# just select per seed orchard & var
heightSO <- grep("PrHeightSOh60", tifs, value=TRUE)
heightSO <- grep("45in50", heightSO, value=TRUE)
heightSO <- grep("thresholds", heightSO, value=TRUE)
heightSO <- heightSO[-3] # remove mean

# read all scenarios in as stack
heightSOstack <- do.call(stack, lapply(heightSO, raster))
spplot(heightSOstack)

# data frame of stats per seed zone
dfSeedZones <- extract(heightSOstack, spSeedZones, fun=mean, df=TRUE, na.rm=TRUE)
dfSeedZones$ZON2 <- zoneOrder
head(dfSeedZones)
dfSeedZones <- dfSeedZones %>% 
  pivot_longer(cols = 2:6, names_to="fileName",values_to="mean")
dfSeedZones$GCM <- substring(dfSeedZones$fileName,15,22)
sfSeedZones <- left_join(sfSeedZones,dfSeedZones,by="ZON2")

ggplot(sfSeedZones)+
  geom_sf(aes(fill=mean),col=NA)+
  facet_wrap(~GCM)+
  theme_minimal()
# categorise mean instead of it being continuous?

dfSeedZones_sd <- extract(heightSOstack, spSeedZones, fun=sd, df=TRUE, na.rm=TRUE)
dfSeedZones_sd$ZON2 <- zoneOrder
dfSeedZones_sd <- dfSeedZones_sd %>% 
  pivot_longer(cols = 2:6, names_to="fileName",values_to="standard_deviation")
dfSeedZones_sd$GCM <- substring(dfSeedZones_sd$fileName,15,22)
sfSeedZones <- left_join(sfSeedZones,dfSeedZones_sd,by="ZON2")

ggplot(sfSeedZones)+
  geom_sf(aes(fill=standard_deviation),col=NA)+
  facet_wrap(~GCM.x)+
  theme_minimal()


### gcm spatial uncertainty ----------------------------------------------------

# threshold reclass
# lets say height above 1000mm
# reclass matrix
min(heightSOstack)
rules1 <- c(0, 1000, 0,  1000, 2500, 1)
rcl1 <- matrix(rules1, ncol=3, byrow=TRUE)
rclassStack <- reclassify(heightSOstack,rcl1)
spplot(rclassStack)

# sum
#nlayers(rclassStack)
sumStack <- stackApply(rclassStack, indices=1, fun=sum)
plot(sumStack)

# contour
contour1 <- rasterToContour(sumStack)
contour1 <- st_as_sf(contour1)
contour1$level <- as.numeric(contour1$level)
contour1$agreement <- NA
contour1$agreement[which(contour1$level<=1)]<-"1 scenario"
contour1$agreement[which(contour1$level<=2&contour1$level>1)]<-"2 scenarios"
contour1$agreement[which(contour1$level<=3&contour1$level>2)]<-"3 scenarios"
contour1$agreement[which(contour1$level<=2&contour1$level>1)]<-"2 scenarios"
contour1$agreement[which(contour1$level<=4&contour1$level>3)]<-"4 scenarios"
contour1$agreement[which(contour1$level<=5&contour1$level>4)]<-"All scenarios"

contour1$agreement <- as.factor(contour1$agreement)

# convert from MULTILINESTRING to polygon
contour1 <- st_cast(contour1, to="POLYGON")

plot.title <- paste0("Height above 1000m predicted by 5 GCMs")
p1 <- ggplot()+
  #geom_sf(data = sweden)+
  geom_sf(data=contour1,aes(fill=agreement),col=NA)+
  scale_fill_viridis(discrete = T, option = "C")+
  ggtitle(plot.title)+
  theme_minimal()
png(paste0(dirFigs,"RCP_GCM_scenario_agreement_SO",SO,".png"), units="cm", width = 20, height = 20, res=1000)
print(p1)
dev.off()

