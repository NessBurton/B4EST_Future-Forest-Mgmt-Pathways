
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
sfSeedZones$ZON2 <- as.factor(sfSeedZones$ZON2)

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
#heightSO <- grep("85in50", heightSO, value=TRUE)
heightSO <- grep("thresholds", heightSO, value=TRUE)
#heightSO <- heightSO[-3] # remove mean

# read all scenarios in as stack
heightSOstack <- do.call(stack, lapply(heightSO, raster))
spplot(heightSOstack)

# loop to calculate stats per seed zone

funcs <- c("mean","sd","min","max")

dfSeedZones <- data.frame()

for (f in funcs){
  
  #f <- funcs[1] # for testing
  
  dfValues <- extract(heightSOstack, spSeedZones, fun=f, df=TRUE, na.rm=TRUE)
  dfValues$ZON2 <- zoneOrder
  dfValues <- dfValues %>% pivot_longer(cols = 2:13, names_to="fileName",values_to=f)
  
  #dfValues$GCM <- substring(dfValues$fileName,15,22)
  for (i in 1:nrow(dfValues)){
    dfValues$GCM[i] <- strsplit(dfValues$fileName[i], "[_]")[[1]][2]
  }
  
  if(f=="mean"){
    dfSeedZones <- rbind(dfSeedZones,dfValues[,c(2,5,4)])
  }else{
      dfSeedZones <- left_join(dfSeedZones,dfValues,by=c("ZON2","GCM"))
    }
  
}

head(dfSeedZones)
dfSeedZones <- dfSeedZones[,c(1,2,3,6,9,12)]

sfSeedZones <- left_join(sfSeedZones,dfSeedZones,by="ZON2")

ggplot(sfSeedZones)+
  geom_sf(aes(fill=mean),col=NA)+
  facet_wrap(~GCM)+
  theme_minimal()
# categorise mean instead of it being continuous?

ggplot(sfSeedZones)+
  geom_sf(aes(fill=sd),col=NA)+
  facet_wrap(~GCM)+
  theme_minimal()

dfSeedZones$ZON2 <- factor(dfSeedZones$ZON2, ordered = T, levels=zoneOrder)
dfSeedZones$GCM <- factor(dfSeedZones$GCM)

# calculate number of cells in each zone
dfCount <- extract(heightSOstack, spSeedZones, fun=function(x,...)length(na.omit(x)), df=TRUE, na.rm=TRUE)
dfCount <- dfCount[,1:2]
dfCount$ZON2 <- zoneOrder
colnames(dfCount)[2] <- "count"
dfCount <- dfCount[,-1]
dfSeedZones <- left_join(dfSeedZones,dfCount)

dfSeedZones <- dfSeedZones %>% 
  mutate(SE = sd/sqrt(count),
         upr = mean + 1.96 * SE,
         lwr = mean - 1.96 * SE)

write.csv(dfSeedZones, paste0(dirOut, "HeightSO60_raster_seedZone_stats.csv"),row.names = F)

limits <- aes(ymin=lwr,ymax=upr)

scenarios <- unique(dfSeedZones$GCM)
scenario_filter <- grep("85in50", scenarios, value=TRUE)

df85 <- dfSeedZones %>% 
  filter(GCM %in% scenario_filter)

ggplot(df85, aes(x=GCM,y=mean, color=GCM))+
  geom_point()+
  #geom_errorbar(limits)+
  geom_errorbar(aes(ymin=min,ymax=max))+ # just using min/max as SE upr/lwr values tiny
  scale_y_continuous(limits=c(0,3000))+
  facet_wrap(~ZON2, nrow = 2, ncol = 11)+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())+
  ylab("Height (mm) - mean predicted")+
  theme_bw()

# example code for error bars 
#summarise(famGain = mean(na.omit(value)),
#seGain = sd(na.omit(value)/sqrt(n())),
#upr = famGain + 1.96 * seGain,
#lwr = famGain - 1.96 * seGain
#limits = aes(ymin = lwr, ymax = upr)

# reorganise to compare GCMs against ensemble mean results

#dfEnsembleMean <- dfSeedZones[,1:3] %>% filter(GCM %in% c("MEAN45in50","MEAN85in50")) %>% 
  #pivot_wider(names_from="GCM",values_from="mean")

#dfSeedZones <- dfSeedZones %>% filter(GCM %in% c("MEAN45in50","MEAN85in50")==FALSE) %>% 
  #left_join(., dfEnsembleMean,by="ZON2")

#gcms <- unique(dfSeedZones$GCM)
#rcp45 <-  grep("45", gcms, value=TRUE)
#rcp85 <- grep("85", gcms, value=TRUE)

#dfSeedZones$MEAN45in50[which(dfSeedZones$GCM %in% rcp85)] <- NA
#dfSeedZones$MEAN85in50[which(dfSeedZones$GCM %in% rcp45)] <- NA

# calculate new standard devation (from ensemble mean not seed zone mean)
# and calculate coefficient of variation for each GCM
# think this will actually be easier in df form
#height85 <- grep("85in50", heightSO, value=TRUE)
#height85 <- height85[-3] # remove mean
#height85stack <- do.call(stack, lapply(height85, raster))
#spplot(height85stack)

#dfSeedZones %>% mutate()

#ggplot(df85, aes(GCM,CoV))+
  #geom_point()+
  #facet_wrap(~ZON2)


### gcm spatial uncertainty ----------------------------------------------------

# sweden outline
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
sweden <- worldmap[worldmap$name == 'Sweden',]

# threshold reclass
# lets say height above 1000mm
# reclass matrix
min(heightSOstack)
rules1 <- c(0, 2000, 0,  2000, 3000, 1)
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

plot.title <- paste0("GCM agreement height > 2000m | RCP8.5 | 2050")
p1 <- ggplot()+
  geom_sf(data = sweden)+
  geom_sf(data=contour1,aes(fill=agreement),col=NA)+
  scale_fill_viridis(discrete = T, option = "C")+
  ggtitle(plot.title)+
  theme_minimal()
png(paste0(dirFigs,"GCM_agreement_SO60_h2000.png"), units="cm", width = 20, height = 20, res=1000)
print(p1)
dev.off()


# performance
# just select per seed orchard & var
prodIdxSO <- grep("PrProdidxSOh60", tifs, value=TRUE)
prodIdxSO <- grep("85in50", prodIdxSO, value=TRUE)
prodIdxSO <- grep("thresholds", prodIdxSO, value=TRUE)
prodIdxSO <- prodIdxSO[-3] # remove mean

# read all scenarios in as stack
prodIdxSOstack <- do.call(stack, lapply(prodIdxSO, raster))
spplot(prodIdxSOstack)

# threshold reclass
# lets say prodIdx above 120%
# reclass matrix
rules2 <- c(0, 120, 0,  120, 150, 1)
rcl2 <- matrix(rules2, ncol=3, byrow=TRUE)
rclassStack <- reclassify(prodIdxSOstack,rcl2)
spplot(rclassStack)

# sum
#nlayers(rclassStack)
sumStack <- stackApply(rclassStack, indices=1, fun=sum)
plot(sumStack)

# contour
contour2 <- rasterToContour(sumStack)
contour2 <- st_as_sf(contour2)
contour2$level <- as.numeric(contour2$level)
contour2$agreement <- NA
contour2$agreement[which(contour2$level<=1)]<-"1 scenario"
contour2$agreement[which(contour2$level<=2&contour2$level>1)]<-"2 scenarios"
contour2$agreement[which(contour2$level<=3&contour2$level>2)]<-"3 scenarios"
contour2$agreement[which(contour2$level<=2&contour2$level>1)]<-"2 scenarios"
contour2$agreement[which(contour2$level<=4&contour2$level>3)]<-"4 scenarios"
contour2$agreement[which(contour2$level<=5&contour2$level>4)]<-"All scenarios"

contour2$agreement <- as.factor(contour2$agreement)

# convert from MULTILINESTRING to polygon
contour2 <- st_cast(contour2, to="POLYGON")

plot.title <- paste0("GCM agreement prodIdx > 120% | RCP8.5 | 2050")
p2 <- ggplot()+
  geom_sf(data = sweden)+
  geom_sf(data=contour2,aes(fill=agreement),col=NA)+
  scale_fill_viridis(discrete = T, option = "C")+
  ggtitle(plot.title)+
  theme_minimal()
png(paste0(dirFigs,"GCM_agreement_SO60_p120.png"), units="cm", width = 20, height = 20, res=1000)
print(p2)
dev.off()
