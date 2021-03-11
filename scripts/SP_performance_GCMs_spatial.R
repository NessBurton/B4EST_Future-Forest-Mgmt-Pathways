
# date: 14/01/20
# author: VB
# description: plot seed orchard performance, demonstrating spatial comparison amongst GCMs

wd <- "~/FFMPs" # sandbox
dirData <- paste0(wd,"/data-raw/")
#dirOut <- paste0(wd,"/data-processed/")
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
library(vroom)
library(scales)
library(ggpattern)


### rasterise ------------------------------------------------------------------

# list production prediction files
files <-  list.files(paste0(dirData, "Productionpredictions/"),pattern = "*.csv",full.names = T)
#files <- files[-c(5:6,13)]

# for reprojection in loop
shpSZ <- st_read(paste0(dirData,"Seed_zones_SP_Sweden/Shaper/Frözoner_tall_Sverige.shp"))
utm <- crs(shpSZ)

# scenario list
scenario_list <- c()

for (f in files){
  
  #f <- files[1]
  
  file <- stringr::str_split(f, fixed("_"))[[1]][1]
  scenario <- stringr::str_split(file, fixed("/"), n=8)[[1]][8]
  scenario_list[[length(scenario_list) + 1]] <- scenario
  
  print(paste0("Processing for scenario = ", scenario))
  
  dfP <- vroom(f)
  
  print("Apply thresholds to each seed orchard")
  
  # per seed orchard
  
  # create reclass field 
  dfP$PrProdidxSOh60RC <- NA
  dfP$PrProdidxSOh60RC[which(dfP$PrProdidxSOh60>=1.2)] <- 1
  dfP$PrProdidxSOh60RC[which(dfP$PrProdidxSOh60<1.2)] <- 0
  dfP$PrProdidxSOh60RC[which(dfP$PrProdidxSOh60RC != 1 & dfP$CenterLat < 55 | dfP$CenterLat > 65)] <- -1
  dfP$PrProdidxSOh60RC[which(dfP$PrProdidxSOh60RC != 1 & dfP$PrSurvSOh60 < 0.5)] <- -1
  dfP$PrProdidxSOh60RC[which(dfP$PrProdidxSOh60RC != 1 & dfP$GDD5Future < 500 | dfP$GDD5Future > 1400)] <- -1
  
  
  dfP$PrProdidxSOh62RC <- NA
  dfP$PrProdidxSOh62RC[which(dfP$PrProdidxSOh62>=1.2)] <- 1
  dfP$PrProdidxSOh62RC[which(dfP$PrProdidxSOh62<1.2)] <- 0
  dfP$PrProdidxSOh62RC[which(dfP$PrProdidxSOh62RC != 1 & dfP$CenterLat < 57 | dfP$CenterLat > 67)] <- -1
  dfP$PrProdidxSOh62RC[which(dfP$PrProdidxSOh62RC != 1 & dfP$PrSurvSOh62 < 0.5)] <- -1
  dfP$PrProdidxSOh62RC[which(dfP$PrProdidxSOh62RC != 1 & dfP$GDD5Future < 500 | dfP$GDD5Future > 1400)] <- -1
  
  
  dfP$PrProdidxSOh64RC <- NA
  dfP$PrProdidxSOh64RC[which(dfP$PrProdidxSOh64>=1.2)] <- 1
  dfP$PrProdidxSOh64RC[which(dfP$PrProdidxSOh64<1.2)] <- 0
  dfP$PrProdidxSOh64RC[which(dfP$PrProdidxSOh64RC != 1 & dfP$CenterLat < 59 | dfP$CenterLat > 69)] <- -1
  dfP$PrProdidxSOh64RC[which(dfP$PrProdidxSOh64RC != 1 & dfP$PrSurvSOh64 < 0.5)] <- -1
  dfP$PrProdidxSOh64RC[which(dfP$PrProdidxSOh64RC != 1 & dfP$GDD5Future < 500 | dfP$GDD5Future > 1400)] <- -1
 
  
  dfP$PrProdidxSOh66RC <- NA
  dfP$PrProdidxSOh66RC[which(dfP$PrProdidxSOh66>=1.2)] <- 1
  dfP$PrProdidxSOh66RC[which(dfP$PrProdidxSOh66<1.2)] <- 0
  dfP$PrProdidxSOh66RC[which(dfP$PrProdidxSOh66RC != 1 & dfP$CenterLat < 61 | dfP$CenterLat > 71)] <- -1
  dfP$PrProdidxSOh66RC[which(dfP$PrProdidxSOh66RC != 1 & dfP$PrSurvSOh66 < 0.5)] <- -1
  dfP$PrProdidxSOh66RC[which(dfP$PrProdidxSOh66RC != 1 & dfP$GDD5Future < 500 | dfP$GDD5Future > 1400)] <- -1
 
  
  # SOh60
  #dfP$PrProdidxSOh60T <- dfP$PrProdidxSOh60
  # lat
  #dfP$PrProdidxSOh60T[which(dfP$CenterLat < 55 | dfP$CenterLat > 65)] <- 9999
  # surv
  #dfP$PrProdidxSOh60T[which(dfP$PrSurvSOh60 < 0.5)] <- 8888
  # GDD5
  #dfP$PrProdidxSOh60T[which(dfP$GDD5Future < 500 | dfP$GDD5Future > 1400)] <- 7777
  
  # SOh62
  #dfP$PrProdidxSOh62T <- dfP$PrProdidxSOh62
  # lat
  #dfP$PrProdidxSOh62T[which(dfP$CenterLat < 55 | dfP$CenterLat > 65)] <- 9999
  # surv
  #dfP$PrProdidxSOh62T[which(dfP$PrSurvSOh62 < 0.5)] <- 8888
  # GDD5
  #dfP$PrProdidxSOh62T[which(dfP$GDD5Future < 500 | dfP$GDD5Future > 1400)] <- 7777
  
  # SOh64
  #dfP$PrProdidxSOh64T <- dfP$PrProdidxSOh64
  # lat
  #dfP$PrProdidxSOh64T[which(dfP$CenterLat < 55 | dfP$CenterLat > 65)] <- 9999
  # surv
  #dfP$PrProdidxSOh64T[which(dfP$PrSurvSOh64 < 0.5)] <- 8888
  # GDD5
  #dfP$PrProdidxSOh64T[which(dfP$GDD5Future < 500 | dfP$GDD5Future > 1400)] <- 7777
  
  # SOh66
  #dfP$PrProdidxSOh66T <- dfP$PrProdidxSOh66
  # lat
  #dfP$PrProdidxSOh66T[which(dfP$CenterLat < 55 | dfP$CenterLat > 65)] <- 9999
  # surv
  #dfP$PrProdidxSOh66T[which(dfP$PrSurvSOh66 < 0.5)] <- 8888
  # GDD5
  #dfP$PrProdidxSOh66T[which(dfP$GDD5Future < 500 | dfP$GDD5Future > 1400)] <- 7777
  
  print("Convert to spatial points")
  
  # convert to spatial
  coordinates(dfP) <- ~ CenterLong + CenterLat
  
  # define lat long crs
  proj4string(dfP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
  
  # transform points to utm
  dfP <- spTransform(dfP, CRSobj = utm)
  
  rstUTM <- raster(crs = crs(dfP), resolution = c(1100,1100), ext = extent(dfP))
  
  print(paste0("Loop through seed orchard performance and rasterise"))
  
  # rasterise reclass
  for (var in names(dfP)[33:36]){ # rasterise performance for 4 seed orchards
    
    #var <- names(dfP)[17]
    print(paste0("Rasterising for var = ", var))
    rstP <- rasterize(dfP, rstUTM, dfP[[var]], fun=max) # unsure of use of max function here
    
    writeRaster(rstP, paste0(dirOut,"ProdIdx_rst/",var,"_",scenario,"_reclass.tif"),overwrite=TRUE)
    print(paste0("Written raster for: ", var))
  
  }
  
  # rasterise no thresholds
  #for (var in names(dfP)[17:20]){ # rasterise performance for 4 seed orchards
    
    #var <- names(dfP)[17]
    #print(paste0("Rasterising for var = ", var))
    
    # rasterise 
    #rstP <- rasterize(dfP, rstUTM, dfP[[var]], fun=max) # unsure of use of max function here
    
    #writeRaster(rstP, paste0(dirOut,"ProdIdx_rst/",var,"_",scenario,".tif"),overwrite=TRUE)
    #print(paste0("Written raster for: ", var))
    
  #}
  
  # rasterise with thresholds
  #for (var in names(dfP)[33:36]){ # rasterise performance for 4 seed orchards
    
    #var <- names(dfP)[17]
    #print(paste0("Rasterising for var = ", var))
    
    # rasterise 
    #rstP <- rasterize(dfP, rstUTM, dfP[[var]], fun=max) # unsure of use of max function here
    
      #writeRaster(rstP, paste0(dirOut,"ProdIdx_rst/",var,"_",scenario,"_thresholds.tif"),overwrite=TRUE)
    #print(paste0("Written raster for: ", var))
    
  #}
  
}

# list production prediction tifs per scenario
rsts <-  list.files(paste0(dirOut, "ProdIdx_rst/"),pattern = "*.tif",full.names = T)
rstsT <- grep("thresholds",rsts, value = TRUE)
rsts <- rsts[!grepl("thresholds",rsts)]

# remove mean and ref files
rsts <- rsts[!grepl("MEAN|Ref",rsts)]
rstsT <- rstsT[!grepl("MEAN|Ref",rstsT)]

# nested loop to produce plot for each seed orchard in each RCP
lstRCP <- c("45in50","45in70","85in50","85in70")
lstSO <- c("h60","h62","h64","h66")

# sweden outline
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
sweden <- worldmap[worldmap$name == 'Sweden',]

sweden <- st_transform(sweden, crs=st_crs(prodIdxStack))

for (i in lstRCP){
  
  i <- lstRCP[1]
  # filter to rcp
  rstsRCP <- grep(i, rsts, value=TRUE)
  rstsRCPT <- grep(i, rstsT, value = T) # with thresholds reclassified
  
  for (SO in lstSO){
    
    SO <- lstSO[1]
    
    # filter to seed orchard
    rstsSO <- grep(SO, rstsRCP, value=TRUE)
    rstsSOT <- grep(SO, rstsRCPT, value=TRUE) # with thresholds reclassified
    
    # raster stack
    prodIdxStack <- stack(rstsSO) # no thresholds
    print(spplot(prodIdxStack))
    prodIdxStackT <- stack(rstsSOT) # thresholds
    print(spplot(prodIdxStackT))
    
    print("Seed orchard results per RCP/GCM read in as stack")
    
    # reclass
    # lets say production has to be above 120 (20% above local provenance)
    # reclass matrix
    rules1 <- c(0, 1.2, 0,  1.2, 3, 1)
    rcl1 <- matrix(rules1, ncol=3, byrow=TRUE)
    rclassStack <- reclassify(prodIdxStack,rcl1)
    print(spplot(rclassStack))
    
    rulesT <- c(0,6000,0,
                6000,10000,1)
    rclT <- matrix(rulesT, ncol=3, byrow=TRUE)
    rclassStackT <- reclassify(prodIdxStackT,rclT)
    print(spplot(rclassStackT))
    
    print("Raster stacks reclassified")
    
    # sum
    sumStack <- stackApply(rclassStack, indices=1, fun=sum)
    sumStackT <- stackApply(rclassStackT, indices=1, fun=sum)
    
    print("Raster stacks summed")
    
    spplot(sumStack)
    spplot(sumStackT)
    
    rules2 <- c(4,5,5,
                3,4,4,
                2,3,3,
                1,2,2,
                0,1,1,
                -1,0,NA)
    rcl2 <- matrix(rules2, ncol=3, byrow=TRUE)
    
    rclassStack2 <- reclassify(sumStack,rcl2)
    rclassStackT2 <- reclassify(sumStackT,rcl2)
    
    spplot(rclassStack2)
    spplot(rclassStackT2)
    
    # Convert raster to dataframe
    df <- as.data.frame(rclassStack2, xy=T)
    names(df) <- c("x", "y", "GCMagree")
    dfT <- as.data.frame(rclassStackT2, xy=T)
    names(dfT) <- c("x","y","GCMagree")
    
    #my.at <- c(5,4,3,2,1)#,-1,-2,-3,-4,-5)
    #cols0 <- c("#003C30","#01665E","#35978F","#80CDC1","#C7EAE5")#,"#FEEDDE", "#FDBE85", "#FD8D3C", "#E6550D", "#A63603")
    #colsT <- c("#A63603","#E6550D","#FD8D3C","#FDBE85","#FEEDDE")
    
    #Derive desired break/legend colours from gradient of selected brewer palette
    #cols1 <- colorRampPalette(cols0, space="rgb")(length(my.at))
    #colsT <- colorRampPalette(colsT, space="rgb")(length(my.at))
    
    my.labs <- c("5 GCMs agree",
                 "4 GCMs agree",
                 "3 GCMs agree", 
                 "2 GCMs agree",
                 "1 GCM agrees")#,
                 #"1 GCM beyond", 
                 #"2 GCMs beyond",
                 #"3 GCMs beyond",
                 #"4 GCMs beyond",
                 #"5 GCMs beyond")
    display.brewer.pal(n = 8, name = 'Greys')
    brewer.pal(n = 8, name = "Greys")
    
    library(ggnewscale)
    #p1 <- 
    ggplot(data = df) + 
      geom_tile(data = df %>% filter(!is.na(GCMagree)), mapping = aes(x = x, y = y, fill = GCMagree), size = 1) +
      scale_fill_gradient2("Certainty", limits = c(0, 5), 
                           low = "#762A83", mid = "white", high = "#1B7837")+
      new_scale("fill") +
      geom_tile(data = dfT %>% filter(!is.na(GCMagree)), mapping = aes(x=x,y=y,fill=GCMagree), size = 1, alpha=0.5) +
      scale_fill_gradient2("Uncertainty", limits = c(0, 5), 
                           low = "#F7F7F7", mid = "#969696", high = "#252525")+
      geom_sf(data = sweden, fill = NA, colour = "grey", size = 0.5) + 
      theme_bw()+
      labs(fill="Uncertainty")+
      theme(axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())
    
    
    #png(paste0(dirFigs,"GCM_agreement_SO",SO,"_RCP",i,".png"), units="cm", width = 8, height = 10, res=1000)
    print(p1)
    #dev.off()
    
    print(paste0("Plot saved for seed orchard: SO",SO))
    
  }
  
}

# OPTION2
# list production prediction tifs per scenario
rsts <-  list.files(paste0(dirOut, "ProdIdx_rst/"),pattern = "*.tif",full.names = T)
rstsRC <- grep("reclass",rsts, value = TRUE)
# remove mean and ref files
rstsRC <- rstsRC[!grepl("MEAN|Ref",rstsRC)]

for (i in lstRCP){
  
  #i <- lstRCP[1]
  # filter to rcp
  rstsRCP <- grep(i, rstsRC, value=TRUE)
  
  
  for (SO in lstSO){
    
    #SO <- lstSO[1]
    
    # filter to seed orchard
    rstsSO <- grep(SO, rstsRCP, value=TRUE)
    
    # raster stack
    rclassStack <- stack(rstsSO) 
    print(spplot(rclassStack))
    
    print("Seed orchard results per RCP/GCM read in as stack")
    
    # sum
    #sumStack <- stackApply(rclassStack, indices=1, fun=sum)
    sumStack <- sum(rclassStack[[1]],rclassStack[[2]],rclassStack[[3]],rclassStack[[4]],rclassStack[[5]])
    print("Raster stacks summed")
    
    spplot(sumStack)

    # Convert raster to dataframe
    df <- as.data.frame(sumStack, xy=T)
    names(df) <- c("x", "y", "GCMagree")
    
    #df$uncertainty <- NA
    #df$uncertainty[which(df$GCMagree >= 3)] <- "High certainty prodIdx > 120%"
    #df$uncertainty[which(df$GCMagree > -3 & df$GCMagree < 3)] <- "Uncertain"
    #df$uncertainty[which(df$GCMagree < -3)] <- "High certainty beyond model thresholds"
    
    my.at <- c(5,4,3,2,1,0,-1,-2,-3,-4,-5)
    #my.at <- c(5,3,-3,-5)
    
    #cols0 <- c("#003C30","#01665E","#35978F","#80CDC1","#C7EAE5","#FEEDDE", "#FDBE85", "#FD8D3C", "#E6550D", "#A63603")
    cols0 <- c("#003C30","#01665E","#35978F","#80CDC1","#FFDB58", "#F0F0F0", "#D9D9D9", "#BDBDBD", "#969696")
    #cols0 <- c("#003C30","#35978F","#FFDB58", "#D9D9D9", "#969696")
    
    #Derive desired break/legend colours from gradient of selected brewer palette
    cols1 <- colorRampPalette(cols0, space="rgb")(length(my.at))

    my.labs <- c("Very likely",
                 "",
                 "", 
                 "",
                 "",
                 "Possible",
                 "",
                 "",
                 "",
                 "",
                 "Very unlikely")
    
    #library(ggnewscale)
    p2 <- 
    ggplot(data = df) + 
      geom_tile(data = df %>% filter(!is.na(GCMagree)), mapping = aes(x = x, y = y, fill = GCMagree), size = 1) +
      coord_equal()+
      scale_x_continuous(expand=c(0,0))+
      scale_y_continuous(expand=c(0,0))+
      #scale_fill_discrete()+#labels = my.labs)+
      scale_fill_gradientn(colours=cols1,
                           values=rescale(my.at),
                           limits=range(df$GCMagree),
                           breaks=my.at,#)+#,
                           labels = my.labs)+
      labs(fill="Likelihood")+
      #geom_sf(data = sweden, fill = NA, colour = "black", size = 0.75) + 
      theme_bw()+
      theme(axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())
    
    
    png(paste0(dirFigs,"GCM_agreement_SO",SO,"_RCP",i,".png"), units="cm", width = 12, height = 14, res=1000)
    print(p2)
    dev.off()
    
    print(paste0("Plot saved for seed orchard: SO",SO))
    
  }
  
}

# arrange into single plot per RCP

library(gridExtra)
library(cowplot)

lstPlots <- list.files(dirFigs, full.names = TRUE)
lstPlots <- grep("GCM_agreement_SO", lstPlots, value = TRUE)
lstPlots <- lstPlots[-17]
# filter to rcp
lstPlots45 <- grep("RCP45",lstPlots, value=TRUE)
test <- lstPlots45[1:2]

rl <-  lapply(test, png::readPNG)
gl <- lapply(rl, grid::rasterGrob)
gridExtra::grid.arrange(grobs=gl)

### contour method -------------------------------------------------------------

# raster stack
prodIdxStack <- stack(rstsSO)
print(spplot(prodIdxStack))
print("Seed orchard results per RCP/GCM read in as stack")

# threshold reclass
# lets say production has to be above 120 (20% above local provenance)
# reclass matrix
rules1 <- c(80, 120, 0,  120, 300, 1)
rcl1 <- matrix(rules1, ncol=3, byrow=TRUE)
rclassStack <- reclassify(prodIdxStack,rcl1)
print("Raster stack reclassified")
print(spplot(rclassStack))

# sum
#nlayers(rclassStack)
sumStack <- stackApply(rclassStack, indices=1, fun=sum)
print("Raster stack summed")
spplot(sumStack)

# contour
contour1 <- rasterToContour(sumStack)
contour1 <- st_as_sf(contour1)
contour1$level <- as.numeric(contour1$level)
contour1$agreement <- NA
contour1$agreement[which(contour1$level<=1)]<-"1 GCM"
contour1$agreement[which(contour1$level<=2&contour1$level>1)]<-"2 GCMs"
contour1$agreement[which(contour1$level<=3&contour1$level>2)]<-"3 GCMs"
contour1$agreement[which(contour1$level<=4&contour1$level>3)]<-"4 GCMs"
contour1$agreement[which(contour1$level<=5&contour1$level>4)]<-"All GCMs"

contour1$agreement <- as.factor(contour1$agreement)

# convert from MULTILINESTRING to polygon
contour1 <- st_cast(contour1, to="POLYGON")
print("Contours calculated")

#plot.title <- paste0("SO",SO,"-1.5g production index 20% above local provenance (2050) | RCP",i)
p1 <- ggplot()+
  geom_sf(data = sweden)+
  geom_sf(data=contour1,aes(fill=agreement),col=NA)+
  scale_fill_viridis(discrete = T, option = "C")+
  #ggtitle(plot.title)+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
png(paste0(dirFigs,"GCM_agreement_SO",SO,"_RCP",i,".png"), units="cm", width = 8, height = 10, res=1000)
print(p1)
dev.off()


