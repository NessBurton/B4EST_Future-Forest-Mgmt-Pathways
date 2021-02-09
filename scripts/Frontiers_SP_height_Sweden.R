
# date: 09/02/21
# authors: VB/FT
# description: assess uncertainty in height predictions from Nordic Scots pine model
# plot density distribution, identify outliers, calc z-scores, calc coefficient of variation

### libs -----------------------------------------------------------------------

library(dplyr)
library(sf)
library(sp)
library(plyr)
library(reshape2)
library(gridExtra)
library(resample)
library(RColorBrewer)
library(rstatix)
library(EnvStats)
library(ggplot2)
library(rnaturalearth)
library(tidyverse)

### dirs -----------------------------------------------------------------------

wd <- "~/FFMPs" # sandbox
dirData <- paste0(wd,"/data-raw/")
dataDrive <- "D:"
dirOut <- paste0(dataDrive,"/FFMP-data-processed/")
dirFigs <- paste0(wd,"/figures/")


### read in data ---------------------------------------------------------------

# list production prediction files
files <-  list.files(paste0(dirData, "Productionpredictions/"),pattern = "*.csv",full.names = T)
# remove reference climate and ensemble mean for now
files <- files[-c(5,6,13)]

# read in and combine
dfPredictions <- tibble()
for (i in files){
  #i <- files[1]
  scenario <- strsplit(i, "[_]")[[1]][1]
  scenario <- strsplit(scenario, "[/]")[[1]][8]
  f <- read.csv(i)
  f$scenario <- scenario
  f$Zscore_heightLocal <- spatialEco::outliers(f$PrHeightLocal) # calc Z-score
  #head(f)
  f <- f[,c("GridID","CenterLat","CenterLong","GridAlt","PrHeightLocal","PrHeightSOh60","PrHeightSOh62","PrHeightSOh64","PrHeightSOh66","Zscore_heightLocal","scenario")]
  dfPredictions <- rbind(dfPredictions,f)
  
}

head(dfPredictions)

dfPredictions$GCM <- ifelse(grepl("bc", dfPredictions$scenario), 'bc - BCC-CSM1-1',
                            ifelse(grepl("he", dfPredictions$scenario), 'he - HadGEM2-ES',
                                   ifelse(grepl("mg", dfPredictions$scenario), 'mg - MRI-CGCM3',
                                          ifelse(grepl("mi", dfPredictions$scenario), 'mi - MIROC-ESM-CHEM',
                                                 ifelse(grepl("no", dfPredictions$scenario), 'no - NorESM1-M', 'GCM_all')))))
dfPredictions$RCP <- ifelse(grepl("45in50", dfPredictions$scenario), '4.5',
                            ifelse(grepl("85in50", dfPredictions$scenario), '8.5', 'RCP_all'))


write.csv(dfPredictions, paste0(dirOut, "AllHeightPredictions_plus_Zscore.csv"), row.names = F)

### density distributions per GCM ----------------------------------------------

GCMs <- unique(dfPredictions$GCM)
GCM_boxplots <- list()

for(i in GCMs) {
  GCM_boxplots[[i]] <- ggplot(dfPredictions %>% filter(GCM == i), aes(x=RCP, y=PrHeightLocal, fill=RCP)) + 
    geom_boxplot() +
    stat_summary(fun=mean, geom="point", color="red", size=4) +   # plot the mean as a red dot
    scale_y_continuous(limits = c(0, 2000)) +
    xlab("RCP scenario") +
    ylab("Data distribution") +
    theme_bw() + 
    ggtitle(i) + 
    theme(plot.title = element_text(size = 20, face = "bold", hjust=0.5), 
          axis.title.x = element_text(size = 18, face = "bold"), 
          axis.title.y = element_text(size = 18, face = "bold"),
          axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16))
  # print the plots created to screen
  print(GCM_boxplots[[i]])
  # save the plots to home directory. file parameter is used to give plot file name - it can be a complete path of the file name. width and height give dimensions to the file in units = "cm". dpi is dots per inch for the quality of plot
  ggsave(GCM_boxplots[[i]], file=paste0(dirFigs,"PrHeightLocal_Boxplot_2050_", i,".png"), width = 10, height = 10, dpi=300)
}

# arrange all plots in one grid next to each other
GCMs_boxplots.all <- do.call("grid.arrange", c(GCM_boxplots[1:5], ncol= 5))
ggsave(GCMs_boxplots.all, file=paste0(dirFigs,"GCM_RCP_PrHeightLocal_boxplots_2050.png"), width=21, height=5, dpi=300)



### identify outliers across all RCPs and GCMs ---------------------------------

# unsure about this bit

#lsOutliers <- list()
#for(i in 4:8){
  #lsOutliers[[i]] <- boxplot(dfPredictions[[i]], plot=FALSE)$out 
#}

# identify the outliers within the dataset
#outliers.50 <- list()
#for(ii in 4:8){
  #outliers.50[[ii]] <- dfPredictions[dfPredictions[[ii]] %in% lsOutliers[[ii]],]
#}

#outliers.50[[4]]

### calculate z-scores and plot ------------------------------------------------

# load country outline
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
sweden <- worldmap[worldmap$name == 'Sweden',]

# use same cols as Felix
nb.cols <- 9
mycolors <- colorRampPalette(brewer.pal(9, "BrBG"))(nb.cols)

# calculate Zscores
#dfPredictions$Zscore_heightLocal <- spatialEco::outliers(dfPredictions$PrHeightLocal) 

RCP_GCMs <- unique(dfPredictions$scenario)

for (s in RCP_GCMs){
  
  #s <- RCP_GCMs[1]
  dfFilter <- dfPredictions %>% filter(scenario==s)
  #dfFilter$Zscore_heightLocal <- spatialEco::outliers(dfFilter$PrHeightLocal)
  
  # different method for identifying outliers
  #dfOut <- dfFilter %>% 
    #identify_outliers(PrHeightLocal) %>% 
    #filter(is.outlier==TRUE | is.extreme==TRUE)
  
  # make spatial
  spPredictions <- dfFilter
  #spPredictions <- dfOut
  coordinates(spPredictions) <- ~CenterLong+CenterLat
  crs(spPredictions) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  
  # convert to sf for plotting
  sfPredictions <- st_as_sf(spPredictions)
  p1 <- ggplot()+
    geom_sf(data = sfPredictions, aes(col=Zscore_heightLocal))+
    geom_sf(data = sweden, fill=NA)+
    scale_colour_gradientn(colors = mycolors)+
    ggtitle(s)+
    labs(col="Z-score")+
    theme_bw()
  print(p1)
  #ggsave(p1, file=paste0(dirFigs, s, "_PrHeightLocal_outliers.png"), width=8, height=8, dpi=300)
  ggsave(p1, file=paste0(dirFigs, s, "_PrHeightLocal_Zscore.png"), width=8, height=8, dpi=300)
  
}

### rasterize Z-score per RCP-GCM + plot agreement? ----------------------------

# Not sure how useful this is...

library(raster)

# for utm crs
sfSeedZones <- st_read(paste0(dirData,"Seed_zones_SP_Sweden/Shaper/Frözoner_tall_Sverige.shp"))
utm <- crs(sfSeedZones)

RCP_GCMs <- unique(dfPredictions$scenario)

for (s in RCP_GCMs){
  
  #s <- RCP_GCMs[1]
  dfFilter <- dfPredictions %>% filter(scenario==s)
  
  # convert to spatial
  spP <- dfFilter
  rm(dfFilter)
  coordinates(spP) <- ~ CenterLong + CenterLat
  
  # define lat long crs
  proj4string(spP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
  
  # transform points to utm
  spP <- spTransform(spP, CRSobj = utm)
  
  rstUTM <- raster(crs = crs(spP), resolution = c(1100,1100), ext = extent(spP))
  
  rst <- rasterize(spP, rstUTM, spP$Zscore_heightLocal, fun=max, na.rm=TRUE) 
  
  writeRaster(rst, paste0(dirOut,"Zscore_rst/Zscore_PrHeightLocal_",s,"_thresholds.tif"),overwrite=TRUE)
  
  }

# list Zscore tifs 
rsts <-  list.files(paste0(dirOut, "Zscore_rst/"),pattern = "*.tif", full.names = T)
rsts <- grep("45in50", rsts, value=TRUE)

# raster stack
ZscoreStack <- stack(rsts)
spplot(ZscoreStack)

# threshold reclass
# lets say Z score between +0.5 and -0.5
# reclass matrix
rules1 <- c(-2, -0.5, -1,  -0.5, 0.5, NA, 0.5, 1, 1)
rcl1 <- matrix(rules1, ncol=3, byrow=TRUE)
rclassStack <- reclassify(ZscoreStack,rcl1)
spplot(rclassStack)

sumStack <- stackApply(rclassStack, indices=1, fun=sum)
plot(sumStack)

# contour
contour1 <- rasterToContour(sumStack)
contour1 <- st_as_sf(contour1)
contour1$level <- as.numeric(contour1$level)
contour1$agreement <- NA
contour1$agreement[which(contour1$level<=-4)]<-"4 GCMs < mean"
contour1$agreement[which(contour1$level<=-3&contour1$level>-4)]<-"3 GCMs < mean"
contour1$agreement[which(contour1$level<=-2&contour1$level>-3)]<-"2 GCMs < mean"
contour1$agreement[which(contour1$level<=-1&contour1$level>-2)]<-"1 GCM < mean"
contour1$agreement[which(contour1$level<=0&contour1$level>-1)]<- NA
contour1$agreement[which(contour1$level<=1&contour1$level>0)]<-"1 GCM > mean"
contour1$agreement[which(contour1$level<=2&contour1$level>1)]<-"2 GCMs > mean"
contour1$agreement[which(contour1$level<=3&contour1$level>2)]<-"3 GCMs > mean"
contour1$agreement[which(contour1$level<=4&contour1$level>3)]<-"4 GCMs > mean"
contour1$agreement[which(contour1$level<=5&contour1$level>4)]<-"All GCMs > mean"

contour1$agreement <- factor(contour1$agreement, ordered=TRUE, levels=c("4 GCMs < mean","3 GCMs < mean","2 GCMs < mean", "1 GCM < mean",
                                                                           "1 GCM > mean","2 GCMs > mean","3 GCMs > mean","4 GCMs > mean","All GCMs > mean"))

contour1 <- st_cast(contour1, to="POLYGON")

library(viridis)
ggplot()+
  geom_sf(data = sweden)+
  geom_sf(data=contour1,aes(fill=agreement),col=NA)+
  scale_fill_viridis(discrete = T, option = "C")+
  #ggtitle(plot.title)+
  theme_minimal()

### height threshold agreement -------------------------------------------------

for (s in RCP_GCMs){
  
  #s <- RCP_GCMs[1]
  dfFilter <- dfPredictions %>% filter(scenario==s)
  
  # convert to spatial
  spP <- dfFilter
  rm(dfFilter)
  coordinates(spP) <- ~ CenterLong + CenterLat
  
  # define lat long crs
  proj4string(spP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
  
  # transform points to utm
  spP <- spTransform(spP, CRSobj = utm)
  
  rstUTM <- raster(crs = crs(spP), resolution = c(1100,1100), ext = extent(spP))
  
  rst <- rasterize(spP, rstUTM, spP$PrHeightLocal, fun=max, na.rm=TRUE) 
  
  writeRaster(rst, paste0(dirOut,"pred_rst/PrHeightLocal_",s,".tif"),overwrite=TRUE)
  
}

# list height tifs 
rsts <-  list.files(paste0(dirOut, "pred_rst/"),pattern = "*.tif", full.names = T)
rsts <- grep("PrHeightLocal|45in50", rsts, value=TRUE)

# raster stack
heightStack <- stack(rsts)
spplot(heightStack)

# threshold reclass
# lets say height above 1800 m
# reclass matrix
rules2 <- c(0, 1800, 0,  1800, 2500, 1)
rcl2 <- matrix(rules2, ncol=3, byrow=TRUE)
rclassStack <- reclassify(heightStack,rcl2)
spplot(rclassStack)

sumStack <- stackApply(rclassStack, indices=1, fun=sum)
plot(sumStack)

### calculate CoV --------------------------------------------------------------

# aim of CoV
# show variation per GCM in relation to the mean for each RCP/ensemble mean result

# unsure if calculating the mean per RCP/GCM is the same as using the ensemble model results...
# ...and calculate CoV in relation to that
# check and see..

# take 100 random samples of 1000 data points
dfRandom <- bind_rows(replicate(100, dfPredictions %>% sample_n(1000), simplify=F), .id="Obs")
dfRandom$RCP <- as.factor(dfRandom$RCP)
dfRandom$Obs <- as.factor(dfRandom$Obs)

# calculate the mean height for each RCP (across GCMs), for each random replicate
dfRCP <- dfRandom[,c("RCP","GCM","Obs","PrHeightLocal","ensembleHeight")] %>% 
  dplyr::group_by(RCP, Obs) %>% 
  dplyr::mutate(RCP.mean = mean(PrHeightLocal),
                RCP.diff2 = (PrHeightLocal-RCP.mean)^2) %>%
  ungroup() %>% 
  # then for each GCM (within RCP & replicate), calc standard deviation against the RCP mean
  dplyr::group_by(RCP,GCM,Obs) %>% 
  dplyr::summarise(RCP.mean = unique(RCP.mean),
                   GCM.sd= sqrt(sum(RCP.diff2)/1000)) %>% 
  mutate(CoV = GCM.sd/RCP.mean*100)

# boxplot to show results from random replicates
(CV1 <- ggplot(dfRCP)+
  geom_boxplot(aes(GCM, CoV,col=GCM))+
  ylim(0,50)+ylab("CoV (%)")+
  facet_wrap(~RCP)+
  theme_bw()+
  ggtitle("PrHeightLocal CoV (%) vs RCP mean height")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank()))
ggsave(CV1, file=paste0(dirFigs, "PrHeightLocal_CoV_vs_RCP_mean.png"), width=10, height=6, dpi=300)
# pretty much identical to results comparing against ensemble mean below
# so i think fine to use either?

# read in ensemble results
files <- list.files(paste0(dirData, "Productionpredictions/"),pattern = "*.csv",full.names = T)
ensembleFiles <- grep("MEAN", files, value=TRUE)

df4.5_ensemble <- read.csv(ensembleFiles[1])
df8.5_ensemble <- read.csv(ensembleFiles[2])

df4.5_ensemble <- df4.5_ensemble[,c("GridID","PrHeightLocal")]
colnames(df4.5_ensemble)[2] <- "HeightLocal_ENS45"
df8.5_ensemble <- df8.5_ensemble[,c("GridID","PrHeightLocal")]
colnames(df8.5_ensemble)[2] <- "HeightLocal_ENS85"

dfRandom <- left_join(dfRandom,df4.5_ensemble,by="GridID")
dfRandom <- left_join(dfRandom,df8.5_ensemble,by="GridID")

dfRandom$ensembleHeight <- NA
dfRandom$ensembleHeight[which(dfRandom$RCP=="4.5")]<-dfRandom$HeightLocal_ENS45[which(dfRandom$RCP=="4.5")]
dfRandom$ensembleHeight[which(dfRandom$RCP=="8.5")]<-dfRandom$HeightLocal_ENS85[which(dfRandom$RCP=="8.5")]

head(dfRandom)

ENSmean45 <- mean(df4.5_ensemble$HeightLocal_ENS45)
ENSmean85 <- mean(df8.5_ensemble$HeightLocal_ENS85)

# calculate CoV against ensemble model results mean
# manually calc sd against each ensemble mean, not mean of PrHeightLocal
dfENS <- dfRandom %>% 
  dplyr::group_by(RCP,GCM,Obs) %>% 
  dplyr::summarise(ENSmean = mean(ensembleHeight),
                   ENSsd1 = sqrt(sum((PrHeightLocal-ENSmean)^2)/1000),
                   ENSsd2 = if (RCP=="4.5"){sqrt(sum((PrHeightLocal-ENSmean45)^2)/1000)}
                   else{ sqrt(sum((PrHeightLocal-ENSmean85)^2)/1000)}) %>% # manually calc sd against ensemble mean, not mean of PrHeightLocal
  mutate(CoV1 = ENSsd1/ENSmean*100,
         CoV2 = if (RCP=="4.5"){ENSsd2/ENSmean45*100}else{ENSsd2/ENSmean85*100})

(CV2 <- ggplot(dfENS)+
  geom_boxplot(aes(GCM, CoV1,col=GCM))+
  ylim(0,50)+ylab("CoV (%)")+
  facet_wrap(~RCP)+
  theme_bw()+
  ggtitle("PrHeightLocal CoV (%) vs. ensemble mean height")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank()))
ggsave(CV2, file=paste0(dirFigs, "PrHeightLocal_CoV_vs_ensemble_mean.png"), width=10, height=6, dpi=300)

# or can it be this simple?
# calculate mean height for each GCM within each RCP, for each random replicate
dfGCM <- dfRandom %>% 
  dplyr::group_by(RCP, GCM, Obs) %>% 
  dplyr::summarise(GCM.mean = mean(PrHeightLocal),
                   GCM.sd = sd(PrHeightLocal)) %>% 
  mutate(CoV = GCM.sd/GCM.mean*100)

# boxplot to show results from random replicates
(CV3 <- ggplot(dfGCM)+
  geom_boxplot(aes(GCM, CoV,col=GCM))+
  ylim(0,50)+ylab("CoV (%)")+
  facet_wrap(~RCP)+
  theme_bw()+
  ggtitle("PrHeightLocal CoV (%) vs. GCM mean")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank()))
ggsave(CV3, file=paste0(dirFigs, "PrHeightLocal_CoV_vs_GCM_mean.png"), width=10, height=6, dpi=300)

