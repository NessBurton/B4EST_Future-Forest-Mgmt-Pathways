
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
library(ggnewscale)

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
  
  ### 60 degrees lat -----------------------------------------------------------
  # create reclass field 
  dfP$PrProdidxSOh60RC <- NA
  dfP$PrProdidxSOh60RC[which(dfP$PrProdidxSOh60>=1.2)] <- 1 # above 120
  dfP$PrProdidxSOh60RC[which(dfP$PrProdidxSOh60<1.2)] <- -1 # below 120
  
  # create threshold field
  dfP$PrProdidxSOh60T <- NA
  dfP$PrProdidxSOh60T[which(dfP$CenterLat < 55 | dfP$CenterLat > 65)] <- 1
  dfP$PrProdidxSOh60T[which(dfP$PrSurvSOh60 < 0.5)] <- 1
  dfP$PrProdidxSOh60T[which(dfP$GDD5Future < 527 | dfP$GDD5Future > 1349)] <- 1
  
  # create reclass field 
  dfP$PrProdidxSOhs60RC <- NA
  dfP$PrProdidxSOhs60RC[which(dfP$PrProdidxSOhs60>=1.2)] <- 1 # above 120
  dfP$PrProdidxSOhs60RC[which(dfP$PrProdidxSOhs60<1.2)] <- -1 # below 120
  
  # create threshold field
  dfP$PrProdidxSOhs60T <- NA
  dfP$PrProdidxSOhs60T[which(dfP$CenterLat < 55 | dfP$CenterLat > 65)] <- 1
  dfP$PrProdidxSOhs60T[which(dfP$PrSurvSOhs60 < 0.5)] <- 1
  dfP$PrProdidxSOhs60T[which(dfP$GDD5Future < 527 | dfP$GDD5Future > 1349)] <- 1
  
  ### 62 degrees lat -----------------------------------------------------------
  # create reclass field 
  dfP$PrProdidxSOh62RC <- NA
  dfP$PrProdidxSOh62RC[which(dfP$PrProdidxSOh62>=1.2)] <- 1 # above 120
  dfP$PrProdidxSOh62RC[which(dfP$PrProdidxSOh62<1.2)] <- -1 # below 120
  
  # create threshold field
  dfP$PrProdidxSOh62T <- NA
  dfP$PrProdidxSOh62T[which(dfP$CenterLat < 57 | dfP$CenterLat > 67)] <- 1
  dfP$PrProdidxSOh62T[which(dfP$PrSurvSOh62 < 0.5)] <- 1
  dfP$PrProdidxSOh62T[which(dfP$GDD5Future < 527 | dfP$GDD5Future > 1349)] <- 1
  
  # create reclass field 
  dfP$PrProdidxSOhs62RC <- NA
  dfP$PrProdidxSOhs62RC[which(dfP$PrProdidxSOhs62>=1.2)] <- 1 # above 120
  dfP$PrProdidxSOhs62RC[which(dfP$PrProdidxSOhs62<1.2)] <- -1 # below 120
  
  # create threshold field
  dfP$PrProdidxSOhs62T <- NA
  dfP$PrProdidxSOhs62T[which(dfP$CenterLat < 57 | dfP$CenterLat > 67)] <- 1
  dfP$PrProdidxSOhs62T[which(dfP$PrSurvSOhs62 < 0.5)] <- 1
  dfP$PrProdidxSOhs62T[which(dfP$GDD5Future < 527 | dfP$GDD5Future > 1349)] <- 1
  
  ### 64 degrees lat -----------------------------------------------------------
  # create reclass field 
  dfP$PrProdidxSOh64RC <- NA
  dfP$PrProdidxSOh64RC[which(dfP$PrProdidxSOh64>=1.2)] <- 1 # above 120
  dfP$PrProdidxSOh64RC[which(dfP$PrProdidxSOh64<1.2)] <- -1 # below 120
  
  # create threshold field
  dfP$PrProdidxSOh64T <- NA
  dfP$PrProdidxSOh64T[which(dfP$CenterLat < 59 | dfP$CenterLat > 69)] <- 1
  dfP$PrProdidxSOh64T[which(dfP$PrSurvSOh64 < 0.5)] <- 1
  dfP$PrProdidxSOh64T[which(dfP$GDD5Future < 527 | dfP$GDD5Future > 1349)] <- 1
  
  # create reclass field 
  dfP$PrProdidxSOhs64RC <- NA
  dfP$PrProdidxSOhs64RC[which(dfP$PrProdidxSOhs64>=1.2)] <- 1 # above 120
  dfP$PrProdidxSOhs64RC[which(dfP$PrProdidxSOhs64<1.2)] <- -1 # below 120
  
  # create threshold field
  dfP$PrProdidxSOhs64T <- NA
  dfP$PrProdidxSOhs64T[which(dfP$CenterLat < 59 | dfP$CenterLat > 69)] <- 1
  dfP$PrProdidxSOhs64T[which(dfP$PrSurvSOhs64 < 0.5)] <- 1
  dfP$PrProdidxSOhs64T[which(dfP$GDD5Future < 527 | dfP$GDD5Future > 1349)] <- 1
  
  ### 66 degrees lat -----------------------------------------------------------
  # create reclass field 
  dfP$PrProdidxSOh66RC <- NA
  dfP$PrProdidxSOh66RC[which(dfP$PrProdidxSOh66>=1.2)] <- 1 # above 120
  dfP$PrProdidxSOh66RC[which(dfP$PrProdidxSOh66<1.2)] <- -1 # below 120
  
  # create threshold field
  dfP$PrProdidxSOh66T <- NA
  dfP$PrProdidxSOh66T[which(dfP$CenterLat < 61 | dfP$CenterLat > 71)] <- 1
  dfP$PrProdidxSOh66T[which(dfP$PrSurvSOh66 < 0.5)] <- 1
  dfP$PrProdidxSOh66T[which(dfP$GDD5Future < 527 | dfP$GDD5Future > 1349)] <- 1
  
  # create reclass field 
  dfP$PrProdidxSOhs66RC <- NA
  dfP$PrProdidxSOhs66RC[which(dfP$PrProdidxSOhs66>=1.2)] <- 1 # above 120
  dfP$PrProdidxSOhs66RC[which(dfP$PrProdidxSOhs66<1.2)] <- -1 # below 120
  
  # create threshold field
  dfP$PrProdidxSOhs66T <- NA
  dfP$PrProdidxSOhs66T[which(dfP$CenterLat < 61 | dfP$CenterLat > 71)] <- 1
  dfP$PrProdidxSOhs66T[which(dfP$PrSurvSOhs66 < 0.5)] <- 1
  dfP$PrProdidxSOhs66T[which(dfP$GDD5Future < 527 | dfP$GDD5Future > 1349)] <- 1
  
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
  for (var in names(dfP)[c(33,35,37,39,41,43,45,47)]){ # rasterise performance for 3 reclassed predictions
    
    #var <- names(dfP)[33]
    #print(paste0("Rasterising for var = ", var))
    
    rst <- rasterize(dfP, rstUTM, dfP[[var]], fun=max, na.rm=TRUE) 
    
    writeRaster(rst, paste0(dirOut,"D4.4_prodidx_rsts/",var,"_Sweden_",scenario,"_GCMagreement.tif"), overwrite=TRUE)
    
    print(paste0("Written raster for: ", var))
    
  }
  
  # rasterise thresholds
  for (var in names(dfP)[c(34,36,38,40,42,44,46,48)]){ # rasterise performance for 3 thresholds
    
    #var <- names(dfP)[33]
    #print(paste0("Rasterising for var = ", var))
    
    rst <- rasterize(dfP, rstUTM, dfP[[var]], fun=max, na.rm=TRUE) 
    
    writeRaster(rst, paste0(dirOut,"D4.4_prodidx_rsts/",var,"_Sweden_",scenario,"_modelThresholds.tif"), overwrite=TRUE)
    
    print(paste0("Written raster for: ", var))
    
  }
  
  
}

### Spatial agreement between GCMs, per RCP ------------------------------------

# list all tifs 
rsts <-  list.files(paste0(dirOut, "D4.4_prodidx_rsts/"),pattern = "*.tif", full.names = T)
# not mean
rsts <- grep("bc|mg|mi|no|he", rsts, value=TRUE)

# list reclassed rasters (agreement above or below mean)
rstsAg <- grep("GCMagreement", rsts, value=TRUE)
# list reclassed rasters (beyond model thresholds)
rstsTh <- grep("modelThresholds", rsts, value=TRUE)

lstRCP <- c("45in50","45in70","85in50","85in70")

lstSO <- c("PrProdidxSOh60","PrProdidxSOhs60","PrProdidxSOh62","PrProdidxSOhs62",
           "PrProdidxSOh64","PrProdidxSOhs64","PrProdidxSOh66","PrProdidxSOhs66")

#viridis(11); plasma(11)
#display.brewer.pal(8, "Greys"); brewer.pal(8, "Greys")

for (rcp in lstRCP){
  
  #rcp <- lstRCP[1]
  
  rstsRCP1 <- grep(rcp, rstsAg, value=TRUE)
  rstsRCP2 <- grep(rcp, rstsTh, value=TRUE)
  
  rcp.name <- ifelse(grepl("26in70", rcp), 'RCP2.6', 
                     ifelse(grepl("45in70", rcp), 'RCP4.5',
                            ifelse(grepl("60in70", rcp), 'RCP6.0',
                                   ifelse(grepl("85in70", rcp), 'RCP8.5', NA))))
  
  for (SO in lstSO){
    
    #SO <- lstSO[1]
    
    # filter to seed orchard
    rstsProv1 <- grep(SO, rstsRCP1, value=TRUE)
    # need to add ifelse for mean/min/max
    rstsProv2 <- grep(SO, rstsRCP2, value=TRUE)
    
    # raster stack
    rclassStack1 <- stack(rstsProv1) # agreement
    rclassStack2 <- stack(rstsProv2) # thresholds
    
    print("Provenance results per RCP/GCM read in as stack")
    
    # sum
    sumStack1 <- sum(rclassStack1[[1]],rclassStack1[[2]],rclassStack1[[3]],rclassStack1[[4]],rclassStack1[[5]])
    sumStack2 <- sum(rclassStack2[[1]],rclassStack2[[2]],rclassStack2[[3]],rclassStack2[[4]],rclassStack2[[5]])
    print("Raster stacks summed")
    
    spplot(sumStack1) # agreement
    spplot(sumStack2) # thresholds
    
    # Convert raster to dataframe
    df1 <- as.data.frame(sumStack1, xy=T)
    names(df1) <- c("x", "y", "GCMagree")
    df2 <- as.data.frame(sumStack2, xy=T)
    names(df2) <- c("x", "y", "Threshold")
    df2$binary <- NA
    df2$binary[which(df2$Threshold>=3)]<-"on"
    df2$binary[which(df2$Threshold<3 | is.na(df2$Threshold))] <- "off"
    
    (p2 <- ggplot(data = df1) + 
        geom_tile(data = df1 %>% filter(!is.na(GCMagree)), mapping = aes(x = x, y = y, fill = GCMagree), size = 1) +
        #scale_fill_gradientn(colours = cols1)+
        scale_fill_gradient2("Above 120%", limits = c(-5, 5), n.breaks = 3,
                             labels = c("Very unlikely","Possible","Very likely"),
                             low = "#FDE725FF", mid = "#21908CFF", high = "#440154FF")+
        #labs(fill="GCM agreement")+
        new_scale("fill") +
        geom_tile(data = df2 %>% filter(!is.na(Threshold)), mapping = aes(x=x,y=y,fill=binary), size = 1, alpha=0.7) +
        scale_fill_discrete("Beyond model thresholds", type = c("#969696"), labels = c(""))+
        #scale_fill_gradient2("Beyond model thresholds", limits = c(0, 5), n.breaks = 3,
                             #labels = c("Possible", "Likely", "Very likely"),
                             #low = "#FFFFFF", mid = "#D9D9D9" , high = "#969696")+
        theme_bw()+
        ggtitle(paste0(SO," | ",rcp.name))+
        theme(plot.title = element_text(face="bold",size=22),
              axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              #legend.title = element_text(size = 18, face = "bold", vjust = 3),
              #legend.text = element_text(size = 16)))
              legend.position = "none"))
    
    ggsave(p2, file=paste0(dirFigs,"GCM_agreement_",SO,"_RCP",rcp,".png"), width=8, height=10, dpi=300)
    
    print(paste0("Plot saved for seed orchard: ",SO))
    
  }
  
  print(paste0("Plots complete for RCP: ", rcp))
  
}

# get legend
# in loop, i've commented out the bits that plot the legend, but i ran once with the legend included & then extracted & saved
library(ggpubr)

# Extract the legend. Returns a gtable
legend <- get_legend(p2)

# Convert to a ggplot and save
legend <- as_ggplot(legend)
plot(legend)
ggsave(legend, file=paste0(dirFigs,"GCM_agreement_legend2.png"),width=4, height=6, dpi=300)


### arrange in single figure per seed orchard ----------------------------------

library(grid)
library(png)
library(gridExtra)

#for (SO in lstSO){
  
  #SO <- lstSO[1]
  #lstPlots <- list.files(paste0(dirFigs), full.names = T)
  #lstPlots <- grep("GCM_agreement", lstPlots, value=TRUE)
  #lstPlots <- grep(SO, lstPlots, value=TRUE)
  #lstPlots <- append(lstPlots, "C:/Users/vanessa.burton.sb/Documents/FFMPs/figures/GCM_agreement_legend2.png")
  
  #lstPlots <- lstPlots[c()]
  #lstPlots <- lstPlots[c()] # specific order for plotting
  
  #rl <- lapply(lstPlots, png::readPNG)
  #gl <- lapply(rl, grid::rasterGrob)
  #(c1 <- gridExtra::grid.arrange(grobs=gl, 
                                 #ncol=3,
                                 #layout_matrix = cbind(c(1), c(2), c(3))))
  
  #ggsave(c1, file=paste0(dirFigs,SO,"_Combined.png"),width=14, height=8, dpi=300)
  
#}

lstPlots <- list.files(paste0(dirFigs), full.names = T)
lstPlots <- grep("GCM_agreement", lstPlots, value=TRUE)
lstPlots <- grep("PrProdidx", lstPlots, value=TRUE)
lstPlots1 <- grep("SOh6", lstPlots, value=TRUE)
lstPlots2 <- grep("SOhs6", lstPlots, value=TRUE)

lstPlots1 <- append(lstPlots1, "C:/Users/vanessa.burton.sb/Documents/FFMPs/figures/GCM_agreement_legend2.png")
lstPlots2 <- append(lstPlots2, "C:/Users/vanessa.burton.sb/Documents/FFMPs/figures/GCM_agreement_legend2.png")

rl <- lapply(lstPlots1, png::readPNG)
gl <- lapply(rl, grid::rasterGrob)
(c1 <- gridExtra::grid.arrange(grobs=gl, 
                               ncol=3,
                               layout_matrix = cbind(c(1,3,5,7), c(2,4,6,8), c(9,9,9,9))))

ggsave(c1, file=paste0(dirFigs,"SO_height_gain_Combined_2070.png"),width=16, height=16, dpi=300)

rl2 <- lapply(lstPlots2, png::readPNG)
gl2 <- lapply(rl2, grid::rasterGrob)
(c2 <- gridExtra::grid.arrange(grobs=gl2, 
                               ncol=3,
                               layout_matrix = cbind(c(1,3,5,7), c(2,4,6,8), c(9,9,9,9))))

ggsave(c2, file=paste0(dirFigs,"SO_height&survival_gain_Combined_2070.png"),width=16, height=16, dpi=300)


### OLD ------------------------------------------------------------------------

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


