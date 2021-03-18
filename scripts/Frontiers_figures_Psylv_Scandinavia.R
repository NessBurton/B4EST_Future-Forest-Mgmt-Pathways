
# date: 16/03/21
# author: VB
# description: figures for Frontiers manuscript - analysis of Scandinavian height predictions

### libs -----------------------------------------------------------------------

library(dplyr)
library(sf)
library(sp)
library(gridExtra)
library(RColorBrewer)
library(ggplot2)
library(rnaturalearth)
library(tidyverse)
library(vroom)
library(stringr)
library(raster)
library(viridis)
library(scales)
library(ggnewscale)

### dirs -----------------------------------------------------------------------

wd <- "~/FFMPs" # sandbox
dirData <- paste0(wd,"/data-raw/")
dataDrive <- "D:"
dirOut <- paste0(dataDrive,"/FFMP-data-processed/Frontiers_manuscript/")
dirFigs <- paste0(wd,"/Frontiers_figures/")



### read in data ---------------------------------------------------------------

# list production prediction files
files <-  list.files(paste0(dirData, "Nordicpredictions/"),pattern = "*.csv",full.names = T)
files2 <- files[-25] # remove reference for now

# read in and combine
dfPredictions <- vroom(files2, id="path")

names(dfPredictions)

dfPredictions <- dfPredictions[,c("path","GridID","CenterLat","CenterLong","Country",
                                  "GDD5Future","PrHeightLocal",
                                  "PrHeightMinLat","PrHeightMeanLat","PrHeightMaxLat")]

dfPredictions$GCM <- ifelse(grepl("bc", dfPredictions$path), 'bc - BCC-CSM1-1',
                            ifelse(grepl("he", dfPredictions$path), 'he - HadGEM2-ES',
                                   ifelse(grepl("mg", dfPredictions$path), 'mg - MRI-CGCM3',
                                          ifelse(grepl("mi", dfPredictions$path), 'mi - MIROC-ESM-CHEM',
                                                 ifelse(grepl("no", dfPredictions$path), 'no - NorESM1-M',
                                                        ifelse(grepl("MEAN", dfPredictions$path), "Ensemble", NA))))))
dfPredictions$RCP <- ifelse(grepl("26", dfPredictions$path), '2.6', 
                            ifelse(grepl("45", dfPredictions$path), '4.5', 
                                   ifelse(grepl("60", dfPredictions$path), '6.0', 
                                          ifelse(grepl("85", dfPredictions$path), '8.5', 'Reference'))))



### climate baseline period means ----------------------------------------------

dfReference <- vroom(files[25])

# minLat = 57.6
# meanLat = 64.87
# maxLat = 69.77
# GDD5 thresholds = less than 527, greater than 1349

dfReference$PrHeightMinLat[which(dfReference$CenterLat < 52.6 | dfReference$CenterLat > 62.6)] <- NA
dfReference$PrHeightMinLat[which(dfReference$GDD5Current < 527 | dfReference$GDD5Current > 1349)] <- NA
minLatMean <- mean(dfReference$PrHeightMinLat, na.rm = TRUE) # 393.3 cm

dfReference$PrHeightMeanLat[which(dfReference$CenterLat < 59.87 | dfReference$CenterLat > 69.87)] <- NA
dfReference$PrHeightMeanLat[which(dfReference$GDD5Current < 527 | dfReference$GDD5Current > 1349)] <- NA
meanLatMean <- mean(dfReference$PrHeightMeanLat, na.rm = TRUE) # 308.8 cm

dfReference$PrHeightMaxLat[which(dfReference$CenterLat < 64.77 | dfReference$CenterLat > 74.77)] <- NA
dfReference$PrHeightMaxLat[which(dfReference$GDD5Current < 527 | dfReference$GDD5Current > 1349)] <- NA
maxLatMean <- mean(dfReference$PrHeightMaxLat, na.rm = TRUE) # 251.2 cm



### new var, remove data beyond thresholds -------------------------------------

dfPredictions$PrHeightMinLatT <- dfPredictions$PrHeightMinLat
dfPredictions$PrHeightMinLatT[which(dfPredictions$CenterLat < 52.6 | dfPredictions$CenterLat > 62.6)] <- NA
dfPredictions$PrHeightMinLatT[which(dfPredictions$GDD5Future < 527 | dfPredictions$GDD5Future > 1349)] <- NA

dfPredictions$PrHeightMeanLatT <- dfPredictions$PrHeightMeanLat
dfPredictions$PrHeightMeanLatT[which(dfPredictions$CenterLat < 59.87 | dfPredictions$CenterLat > 69.87)] <- NA
dfPredictions$PrHeightMeanLatT[which(dfPredictions$GDD5Future < 527 | dfPredictions$GDD5Future > 1349)] <- NA

dfPredictions$PrHeightMaxLatT <- dfPredictions$PrHeightMaxLat
dfPredictions$PrHeightMaxLatT[which(dfPredictions$CenterLat < 64.77 | dfPredictions$CenterLat > 74.77)] <- NA
dfPredictions$PrHeightMaxLatT[which(dfPredictions$GDD5Future < 527 | dfPredictions$GDD5Future > 1349)] <- NA



### raw height boxplots --------------------------------------------------------

dfPredictions$GCM <- factor(dfPredictions$GCM)
dfPredictions$RCP <- factor(dfPredictions$RCP)

(p1 <- dfPredictions %>% 
  filter(!is.na(PrHeightMeanLatT) & GCM != "Ensemble") %>% 
  ggplot(aes(x=RCP, y=PrHeightMeanLatT, fill=RCP))+
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2")+
  stat_summary(fun=mean, geom="point", color="black", size=2, pch=4) +   
  scale_y_continuous(limits = c(0, 600)) +
  facet_wrap(~GCM, ncol = 5)+
  xlab("RCP scenario") +
  ylab("Height distribution (cm)") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15)), 
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 15)),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)))
  
ggsave(p1, file=paste0(dirFigs,"GCM_RCP_PrHeightMeanLat_Nordic_boxplots_2070.png"), width=21, height=5, dpi=300)



### Coefficient of Variation (CoV) ---------------------------------------------

# take 100 random samples of 1000 data points
dfRandom <- bind_rows(replicate(100, dfPredictions %>% sample_n(1000), simplify=F), .id="Obs")
dfRandom$RCP <- as.factor(dfRandom$RCP)
dfRandom$Obs <- as.factor(dfRandom$Obs)

dfCoV <- dfRandom %>% 
  group_by(RCP,GCM,Obs) %>% 
  mutate(DiffMin = PrHeightMinLat - minLatMean,
         DiffMean = PrHeightMeanLat - meanLatMean,
         DiffMax = PrHeightMaxLat - maxLatMean,
         sqMin = DiffMin ^ 2,
         sqMean = DiffMean ^ 2,
         sqMax = DiffMax ^ 2) %>% 
  # manually calc sd from reference mean
  summarise(SDmin = sqrt(sum(sqMin)/1000),
            SDmean = sqrt(sum(sqMean)/1000),
            SDmax = sqrt(sum(sqMax)/1000)) %>%  
  mutate(CoV_min = SDmin/minLatMean*100,
         CoV_mean = SDmean/meanLatMean*100,
         CoV_max = SDmax/maxLatMean*100)

(CVmean <- dfCoV %>% filter(GCM != "Ensemble") %>% 
    ggplot()+
    geom_boxplot(aes(RCP, CoV_mean,fill=RCP))+
    scale_fill_brewer(palette = "Dark2")+
    ylim(0,30)+ylab("CoV (%)")+
    facet_wrap(~GCM, ncol = 5)+
    theme_bw()+
    theme(axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15)), 
          axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 15)),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14)))

ggsave(CVmean, file=paste0(dirFigs, "PrHeightMean_Nordic_CoV_vs_reference_mean.png"), width=21, height=5, dpi=300)

dfCoV2 <- dfRandom %>% 
  group_by(RCP,GCM,Obs) %>% 
  mutate(DiffMin = PrHeightMinLatT - minLatMean,
         DiffMean = PrHeightMeanLatT - meanLatMean,
         DiffMax = PrHeightMaxLatT - maxLatMean,
         sqMin = DiffMin ^ 2,
         sqMean = DiffMean ^ 2,
         sqMax = DiffMax ^ 2) %>% 
  # manually calc sd from reference mean
  summarise(SDmin = sqrt(sum(sqMin, na.rm = T)/1000),
            SDmean = sqrt(sum(sqMean, na.rm = T)/1000),
            SDmax = sqrt(sum(sqMax, na.rm = T)/1000)) %>%  
  mutate(CoV_min = SDmin/minLatMean*100,
         CoV_mean = SDmean/meanLatMean*100,
         CoV_max = SDmax/maxLatMean*100)

(CVmean2 <- dfCoV %>% filter(GCM != "Ensemble") %>% 
    ggplot()+
    geom_boxplot(aes(RCP, CoV_mean,fill=RCP))+
    scale_fill_brewer(palette = "Dark2")+
    ylim(0,30)+ylab("CoV (%)")+
    facet_wrap(~GCM, ncol = 5)+
    theme_bw()+
    theme(axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15)), 
          axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 15)),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14)))

ggsave(CVmean2, file=paste0(dirFigs, "PrHeightMeanThresholds_Nordic_CoV_vs_reference_mean.png"), width=21, height=5, dpi=300)



### Create reclassified rasters (above & below ref mean, plus beyond thresholds) --------

pathList <- unique(dfPredictions$path)

# for utm crs
shpSZ <- st_read(paste0(dirData,"Seed_zones_SP_Sweden/Shaper/Frözoner_tall_Sverige.shp"))
utm <- crs(shpSZ)

for (i in pathList){
  
  #i <- pathList[1]
  dfFilter <- dfPredictions %>% filter(path == i)
  
  file <- stringr::str_split(i, fixed("_"))[[1]][1]
  scenario <- stringr::str_split(file, fixed("/"), n=8)[[1]][8]
  
  print(paste0("Processing for scenario: ",scenario))
  
  print("Apply thresholds to each prediction")
  
  # per min/mean/max prediction
  # create reclass field & threshold field
  dfFilter$PrHeightMinLatRC <- NA
  dfFilter$PrHeightMinLatRC[which(dfFilter$PrHeightMinLat >= minLatMean)] <- 1 # above mean = 1
  dfFilter$PrHeightMinLatRC[which(dfFilter$PrHeightMinLat < minLatMean)] <- -1 # below mean = -1
  
  dfFilter$MinThresh <- NA
  dfFilter$MinThresh[which(dfFilter$CenterLat < 52.6 | dfFilter$CenterLat > 62.6)] <- 1 # beyond threshold = 1
  dfFilter$MinThresh[which(dfFilter$GDD5Future < 527 | dfFilter$GDD5Future > 1349)] <- 1
  
  dfFilter$PrHeightMeanLatRC <- NA
  dfFilter$PrHeightMeanLatRC[which(dfFilter$PrHeightMeanLat >= meanLatMean)] <- 1
  dfFilter$PrHeightMeanLatRC[which(dfFilter$PrHeightMeanLat < meanLatMean)] <- -1
  
  dfFilter$MeanThresh <- NA
  dfFilter$MeanThresh[which(dfFilter$CenterLat < 59.87 | dfFilter$CenterLat > 69.87)] <- 1
  dfFilter$MeanThresh[which(dfFilter$GDD5Future < 527 | dfFilter$GDD5Future > 1349)] <- 1
  
  dfFilter$PrHeightMaxLatRC <- NA
  dfFilter$PrHeightMaxLatRC[which(dfFilter$PrHeightMaxLat >= maxLatMean)] <- 1
  dfFilter$PrHeightMaxLatRC[which(dfFilter$PrHeightMaxLat < maxLatMean)] <- -1
  
  dfFilter$MaxThresh <- NA
  dfFilter$MaxThresh[which(dfFilter$CenterLat < 64.77 | dfFilter$CenterLat > 74.77)] <- 1
  dfFilter$MaxThresh[which(dfFilter$GDD5Future < 527 | dfFilter$GDD5Future > 1349)] <- 1
  
  # convert to spatial
  spP <- dfFilter
  rm(dfFilter)
  coordinates(spP) <- ~ CenterLong + CenterLat
  
  # define lat long crs
  proj4string(spP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
  
  # transform points to utm
  spP <- spTransform(spP, CRSobj = utm)
  
  rstUTM <- raster(crs = crs(spP), resolution = c(1100,1100), ext = extent(spP))
  
  # rasterise reclass
  for (var in names(spP)[c(14,16,18)]){ # rasterise performance for 3 reclassed predictions
    
    #var <- names(spP)[14]
    print(paste0("Rasterising for var = ", var))
    
    rst <- rasterize(spP, rstUTM, spP[[var]], fun=max, na.rm=TRUE) 
    
    writeRaster(rst, paste0(dirOut,"pred_rsts/",var,"_Nordic_",scenario,"_GCMagreement.tif"), overwrite=TRUE)
    
    print(paste0("Written raster for: ", var))
    
  }
  
  # rasterise thresholds
  for (var in names(spP)[c(15,17,19)]){ # rasterise performance for 3 thresholds
    
    #var <- names(spP)[9]
    print(paste0("Rasterising for var = ", var))
    
    rst <- rasterize(spP, rstUTM, spP[[var]], fun=max, na.rm=TRUE) 
    
    writeRaster(rst, paste0(dirOut,"pred_rsts/",var,"_Nordic_",scenario,"_modelThresholds.tif"), overwrite=TRUE)
    
    print(paste0("Written raster for: ", var))
    
  }
  
  
}



### Spatial agreement between GCMs, per RCP ------------------------------------

# list all tifs 
rsts <-  list.files(paste0(dirOut, "pred_rsts/"),pattern = "*.tif", full.names = T)
# not mean
rsts <- grep("bc|mg|mi|no|he", rsts, value=TRUE)

# list reclassed rasters (agreement above or below mean)
rstsAg <- grep("GCMagreement", rsts, value=TRUE)
# list reclassed rasters (beyond model thresholds)
rstsTh <- grep("modelThresholds", rsts, value=TRUE)

lstRCP <- c("26in70","60in70","45in70","85in70")

lstProv <- c("PrHeightMinLat","PrHeightMeanLat","PrHeightMaxLat")

#viridis(11)
display.brewer.pal(5, "Greys"); brewer.pal(5, "Greys")

for (rcp in lstRCP){
  
  #rcp <- lstRCP[1]
  
  rstsRCP1 <- grep(rcp, rstsAg, value=TRUE)
  rstsRCP2 <- grep(rcp, rstsTh, value=TRUE)
  
  rcp.name <- ifelse(grepl("26in70", rcp), 'RCP2.6', 
                     ifelse(grepl("45in70", rcp), 'RCP4.5',
                            ifelse(grepl("60in70", rcp), 'RCP6.0',
                                   ifelse(grepl("85in70", rcp), 'RCP8.5', NA))))
  
  for (prov in lstProv){
    
    #prov <- lstProv[1]
    
    # filter to provenance
    rstsProv1 <- grep(prov, rstsRCP1, value=TRUE)
    # need to add ifelse for mean/min/max
    ifelse(grepl("Min", prov), rstsProv2 <- grep("Min", rstsRCP2, value=TRUE),
           ifelse(grepl("Mean", prov), rstsProv2 <- grep("Mean", rstsRCP2, value=TRUE),
                  ifelse(grepl("Max", prov), rstsProv2 <- grep("Max", rstsRCP2, value=TRUE))))
      
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
    
    (p2 <- ggplot(data = df1) + 
        geom_tile(data = df1 %>% filter(!is.na(GCMagree)), mapping = aes(x = x, y = y, fill = GCMagree), size = 1) +
        #scale_fill_gradientn(colours = cols1)+
        scale_fill_gradient2("Above reference mean", limits = c(-5, 5), n.breaks = 3,
                             labels = c("Very unlikely","Possible","Very likely"),
                             low = "#FDE725FF" , mid = "#21908CFF", high = "#440154FF")+
        #labs(fill="GCM agreement")+
        new_scale("fill") +
        geom_tile(data = df2 %>% filter(!is.na(Threshold)), mapping = aes(x=x,y=y,fill=Threshold), size = 1, alpha=0.5) +
        scale_fill_gradient2("Beyond model thresholds", limits = c(0, 5), n.breaks = 3,
                             labels = c("Possible", "Likely", "Very likely"),
                             low = "#F7F7F7", mid = "#969696" , high = "#252525")+
        theme_bw()+
        ggtitle(rcp.name)+
        theme(plot.title = element_text(face="bold",size=16),
              axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              #legend.title = element_text(face = "bold", vjust = 3)))
              legend.position = "none"))
    
    ggsave(p2, file=paste0(dirFigs,"GCM_agreement_",prov,"_RCP",rcp,".png"), width=8, height=10, dpi=300)
    
    print(paste0("Plot saved for provenance: ",prov))
    
  }
  
  print(paste0("Plots complete for RCP: ", rcp))
  
}

# get legend
#library(ggpubr)

# Extract the legend. Returns a gtable
#legend <- get_legend(p2)

# Convert to a ggplot and save
#legend <- as_ggplot(legend)
#plot(legend)
#ggsave(legend, file=paste0(dirFigs,"GCM_agreement_legend.png"))


### arrange in single figure per provenance ------------------------------------

library(grid)
library(png)
library(gridExtra)

lstPlots <- list.files(paste0(dirFigs), full.names = T)
#lstPlots <- grep("PrHeightMeanLat", lstPlots, value=TRUE)
lstPlots <- grep("GCM_agreement", lstPlots, value=TRUE)
lstPlots <- lstPlots[c(1,6,7,8,9)]
lstPlots <- lstPlots[c(2,4,3,5,1)] # specific order for plotting

plots <- lapply(lstPlots,function(x){
  img <- as.raster(readPNG(x))
  rasterGrob(img, interpolate = FALSE)
})

# PDF
#ggsave(paste0(dirFigs,"PrHeightMeanLat_Combined.pdf"),width=8.5, height=11, 
       #marrangeGrob(grobs = plots, nrow=2, ncol=2,top=NULL))

# OR
# png
ggsave(paste0(dirFigs,"PrHeightMeanLat_Combined.png"),width=8.5, height=11, 
       marrangeGrob(grobs = plots, nrow=2, ncol=3,top=NULL))


