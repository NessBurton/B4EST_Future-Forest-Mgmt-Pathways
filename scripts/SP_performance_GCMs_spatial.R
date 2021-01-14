
# date: 14/01/20
# author: VB
# description: plot seed orchard performance, demonstrating spatial comparison amongst GCMs

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

### steps ----------------------------------------------------------------------

# 1. rasterise performance of a single seed orchard for each GCM/RCP combo
# 2. choose appropriate threshold of performance (and remove if survival below 50%) - reclassify rasters - 1 if above threshold, 0 if below
# 3. sum all rasters
# 4. contour plot

### work -----------------------------------------------------------------------

### rasterise - CAN SKIP TO LINES 96/97 as the rasters exist now

# list production prediction files
files <-  list.files(paste0(dirData, "Productionpredictions/"),pattern = "*.csv",full.names = T)
files <- files[-c(5:6,13)]

# for reprojection in loop
shpSZ <- st_read(paste0(dirData,"Seed_zones_SP_Sweden/Shaper/Frözoner_tall_Sverige.shp"))
utm <- crs(shpSZ)

# scenario list
scenario_list <- c()

for (f in files){
  
  #f <- files[1]
  
  scenario <- substring(f,75,82)
  scenario_list[[length(scenario_list) + 1]] <- scenario
  
  print(paste0("Processing for scenario = ", scenario))
  
  dfP <- read.csv(f)
  
  print("Remove prediction data where survival below 50%")
  
  dfP$PrProdidxSOh60[which(dfP$PrSurvSOh60<0.5)] <- NA
  dfP$PrProdidxSOh62[which(dfP$PrSurvSOh62<0.5)]<-NA
  dfP$PrProdidxSOh64[which(dfP$PrSurvSOh64<0.5)]<-NA
  dfP$PrProdidxSOh66[which(dfP$PrSurvSOh66<0.5)]<-NA
  
  print("Convert to spatial points")
  
  # convert to spatial
  coordinates(dfP) <- ~ CenterLong + CenterLat
  # define lat long crs
  proj4string(dfP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
  
  print(paste0("Converted to spatial points & WGS84 crs defined"))
  
  print(paste0("Loop through seed orchard performance and rasterise"))
  
  # empty raster of correct extent and resolution
  rst <- raster(crs = crs(dfP), resolution = c(0.1,0.1), ext = extent(dfP))
  res(rst)
  
  for (var in names(dfP)[17:20]){ # rasterise performance for 4 seed orchards
    
    #var <- names(dfP)[17]
    print(paste0("Rasterising for var = ", var))
    
    # rasterise while still lat long
    rstP <- rasterize(dfP, rst, dfP[[var]], fun=max) # unsure of use of max function here
    
    print(paste0("Rasterised (lat/long) for var: ", var))
    
    # now transform to utm, reprojection involves interpolation - default is bilinear
    rstP <- projectRaster(rstP, crs = utm, res = 1000)
    #rstStack <- addLayer(rstStack, rstP)
    
    writeRaster(rstP, paste0(dirOut,"ProdIdx_rst/",var,"_",scenario,".tif"),overwrite=TRUE)
    print(paste0("Written raster for: ", var))
    
    }
  
}

# list production prediction tifs per scenario
rsts <-  list.files(paste0(dirOut, "ProdIdx_rst/"),pattern = "*.tif",full.names = T)

# loop to produce plot for each seed orchard
lstSO <- c("h60","h62","h64","h66")

# sweden outline
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
sweden <- worldmap[worldmap$name == 'Sweden',]

for (SO in lstSO){
  
  #SO <- lstSO[2]
  
  # filter to seed orchard
  rstsSO <- grep(SO, rsts, value=TRUE)
  rstsSO <- rstsSO[-c(5:8,15:16)]
  #rstsSO
  
  # raster stack
  prodIdxStack <- stack(rstsSO)
  print(spplot(prodIdxStack))
  print("Seed orchard results per RCP/GCM read in as stack")
  
  # threshold reclass
  # lets say production has to be above 1.2 (20% above local provenance)
    # reclass matrix
  rules1 <- c(-1, 1.2, 0,  1.2, 3, 1)
  rcl1 <- matrix(rules1, ncol=3, byrow=TRUE)
  rclassStack <- reclassify(prodIdxStack,rcl1)
  print("Raster stack reclassified")
  print(spplot(rclassStack))
  
  # sum
  #nlayers(rclassStack)
  sumStack <- stackApply(rclassStack, indices=1, fun=sum)
  print("Raster stack summed")
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
  contour1$agreement[which(contour1$level<=5&contour1$level>4)]<-"5 scenarios"
  contour1$agreement[which(contour1$level<=6&contour1$level>5)]<-"6 scenarios"
  contour1$agreement[which(contour1$level<=7&contour1$level>6)]<-"7 scenarios"
  contour1$agreement[which(contour1$level<=8&contour1$level>7)]<-"8 scenarios"
  contour1$agreement[which(contour1$level<=9&contour1$level>8)]<-"9 scenarios"
  contour1$agreement[which(contour1$level>=10)]<-"All scenarios"
  
  contour1$agreement <- as.factor(contour1$agreement)
  
  # convert from MULTILINESTRING to polygon
  contour1 <- st_cast(contour1, to="POLYGON")
  print("Contours calculated")
  
  plot.title <- paste0("SO",SO,"-1.5g production index 20% above local provenance (2050)")
  p1 <- ggplot()+
    geom_sf(data = sweden)+
    geom_sf(data=contour1,aes(fill=agreement),col=NA)+
    scale_fill_viridis(discrete = T, option = "C")+
    ggtitle(plot.title)+
    theme_minimal()
  png(paste0(dirFigs,"RCP_GCM_scenario_agreement_SO",SO,".png"), units="cm", width = 20, height = 20, res=1000)
  print(p1)
  dev.off()
  
  print(paste0("Plot saved for seed orchard: SO",SO))

}



