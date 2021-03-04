
# date: 09/02/21
# authors: VB
# description: assess uncertainty in height predictions from Nordic Scots pine model
# plot density distribution, identify outliers, calc z-scores, calc coefficient of variation

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

### dirs -----------------------------------------------------------------------

wd <- "~/FFMPs" # sandbox
dirData <- paste0(wd,"/data-raw/")
dataDrive <- "D:"
dirOut <- paste0(dataDrive,"/FFMP-data-processed/Frontiers_manuscript/")
dirFigs <- paste0(wd,"/figures/")


### read in data ---------------------------------------------------------------

# list production prediction files
files <-  list.files(paste0(dirData, "Nordicpredictions/"),pattern = "*.csv",full.names = T)
files2 <- files[-25] # remove reference for now

# read in and combine
dfPredictions <- vroom(files2, id="path")
#spec(dfPredictions)
names(dfPredictions)

dfPredictions <- dfPredictions[,c("path","GridID","CenterLat","CenterLong",
                                  "GDD5Future","PrHeightLocal","PrHeightMeanLat")]

dfPredictions <- dfPredictions %>% mutate(file = stringr::str_split(path, fixed("_"))[[1]][1],
                         scenario = stringr::str_split(file, fixed("/"), n=8)[[1]][8],
                         path = NULL,
                         file = NULL,
                         GDD5threshold = ifelse(GDD5Future < 500 | GDD5Future >1400, 1, NA),
                         LatThreshold = ifelse(CenterLat < 59.87 | CenterLat > 69.87, 1, NA),
                         Zscore_hLocal = spatialEco::outliers(PrHeightLocal), 
                         Zscore_hMeanLat = spatialEco::outliers(PrHeightMeanLat)) 

head(dfPredictions)
names(dfPredictions)

dfPredictions$GCM <- ifelse(grepl("bc", dfPredictions$scenario), 'bc - BCC-CSM1-1',
                            ifelse(grepl("he", dfPredictions$scenario), 'he - HadGEM2-ES',
                                   ifelse(grepl("mg", dfPredictions$scenario), 'mg - MRI-CGCM3',
                                          ifelse(grepl("mi", dfPredictions$scenario), 'mi - MIROC-ESM-CHEM',
                                                 ifelse(grepl("no", dfPredictions$scenario), 'no - NorESM1-M',
                                                        ifelse(grepl("MEAN", dfPredictions$scenario), "Ensemble", NA))))))
dfPredictions$RCP <- ifelse(grepl("26", dfPredictions$scenario), '2.6', 
                            ifelse(grepl("45", dfPredictions$scenario), '4.5', 
                                   ifelse(grepl("60", dfPredictions$scenario), '6.0', 
                                          ifelse(grepl("85", dfPredictions$scenario), '8.5', 'Reference'))))


dfPredictions$GCM <- factor(dfPredictions$GCM)
dfPredictions$RCP <- factor(dfPredictions$RCP)

summary(dfPredictions)

#write.csv(dfPredictions, paste0(dirOut, "HeightPredictions_Sweden_Frontiers.csv"), row.names = F)
write.csv(dfPredictions, paste0(dirOut, "HeightPredictions_Nordic_Frontiers.csv"), row.names = F)


dfPredictions <- read.csv(paste0(dirOut, "HeightPredictions_Sweden_Frontiers.csv"))
dfPredictions$GCM <- factor(dfPredictions$GCM)
dfPredictions$RCP <- factor(dfPredictions$RCP)

### density distributions per GCM ----------------------------------------------

# 1) plot 'raw2' preds first

GCMs <- unique(dfPredictions$GCM)
GCMs <- GCMs[-3] # remove ensemble
GCM_boxplots <- list()

for(i in GCMs) {
  GCM_boxplots[[i]] <- ggplot(dfPredictions %>% filter(GCM == i), aes(x=RCP, y=PrHeightMeanLat, fill=RCP)) + 
    geom_boxplot() +
    stat_summary(fun=mean, geom="point", color="red", size=4) +   # plot the mean as a red dot
    scale_y_continuous(limits = c(0, 1200)) +
    xlab("RCP scenario") +
    ylab("Height distribution (cm)") +
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
  ggsave(GCM_boxplots[[i]], file=paste0(dirFigs,"PrHeightMeanLat_Boxplot_2070_", i,".png"), width = 10, height = 10, dpi=300)
}

# arrange all plots in one grid next to each other
GCMs_boxplots.all <- do.call("grid.arrange", c(GCM_boxplots[1:5], ncol= 5))
ggsave(GCMs_boxplots.all, file=paste0(dirFigs,"GCM_RCP_PrHeightMeanLat_boxplots_2070.png"), width=21, height=5, dpi=300)


# again, but removing data beyond model thresholds
dfPredictions2 <- dfPredictions %>% filter(is.na(GDD5threshold) | is.na(LatThreshold))
GCM_boxplots <- list()

for(i in GCMs) {
  GCM_boxplots[[i]] <- ggplot(dfPredictions2 %>% filter(GCM == i), aes(x=RCP, y=PrHeightMeanLat, fill=RCP)) + 
    geom_boxplot() +
    stat_summary(fun=mean, geom="point", color="red", size=4) +   # plot the mean as a red dot
    scale_y_continuous(limits = c(0, 1200)) +
    xlab("RCP scenario") +
    ylab("Height distribution (cm)") +
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
  ggsave(GCM_boxplots[[i]], file=paste0(dirFigs,"PrHeightMeanLat_thresholds_Boxplot_2070_", i,".png"), width = 10, height = 10, dpi=300)
}

# arrange all plots in one grid next to each other
GCMs_boxplots.all <- do.call("grid.arrange", c(GCM_boxplots[1:5], ncol= 5))
ggsave(GCMs_boxplots.all, file=paste0(dirFigs,"GCM_RCP_PrHeightMeanLat_thresholds_boxplots_2070.png"), width=21, height=5, dpi=300)


# 2) plot distribution of difference between ref local height and future local height

dfReference <- read.csv(files[25])
summary(dfReference$PrHeightMeanLat)
# mean height for reference period is 282cm - use as threshold later

dfPredictions <- left_join(dfPredictions, dfReference[,c("GridID","PrHeightMeanLat")], by = "GridID")
head(dfPredictions)
colnames(dfPredictions)[7] <- "PrHeightMeanLat"
colnames(dfPredictions)[15] <- "RefHeightMeanLat"

# calculate difference
dfPredictions <- dfPredictions %>% mutate(HeightDiff = PrHeightMeanLat - RefHeightMeanLat)
dfPredictions$RCP <- factor(dfPredictions$RCP)

#g2 <- dfPredictions %>% 
#  ggplot(aes(RCP,HeightDiff, fill=RCP))+
#  geom_boxplot()+
#  facet_wrap(~GCM, ncol = 5)+
#  stat_summary(fun=mean, geom="point", color="red", size=4) +   # plot the mean as a red dot
#  #scale_y_continuous(limits = c(0, 600)) +
#  xlab("RCP scenario") +
#  ylab("Height change (cm) from reference") +
#  theme_bw() + 
#  #ggtitle(i) + 
#  theme(plot.title = element_text(size = 20, face = "bold", hjust=0.5), 
#        axis.title.x = element_text(size = 18, face = "bold"), 
#        axis.title.y = element_text(size = 18, face = "bold"),
#        axis.text.x = element_text(size = 16),
#        axis.text.y = element_text(size = 16),
#        strip.text.x = element_text(size = 16, face="bold"))
#ggsave(g2, file=paste0(dirFigs,"GCM_RCP_height_change_from_ref_boxplots_2070.png"), width=21, height=5, dpi=300)

GCM_boxplots <- list()
for(i in GCMs) {
  GCM_boxplots[[i]] <- ggplot(dfPredictions %>% filter(GCM == i), aes(x=RCP, y=HeightDiff, fill=RCP)) + 
    geom_boxplot() +
    stat_summary(fun=mean, geom="point", color="red", size=4) +   # plot the mean as a red dot
    scale_y_continuous(limits = c(0, 2000)) +
    xlab("RCP scenario") +
    ylab("Height distribution (cm)") +
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
  ggsave(GCM_boxplots[[i]], file=paste0(dirFigs,"PrHeightDiff_Boxplot_2070_", i,".png"), width = 10, height = 10, dpi=300)
}

# arrange all plots in one grid next to each other
GCMs_boxplots.all <- do.call("grid.arrange", c(GCM_boxplots[1:5], ncol= 5))
ggsave(GCMs_boxplots.all, file=paste0(dirFigs,"GCM_RCP_PrHeightDiff_boxplots_2070.png"), width=21, height=5, dpi=300)



### height threshold agreement -------------------------------------------------

for (s in RCP_GCMs){
  
  #s <- RCP_GCMs[1]
  dfFilter <- dfPredictions %>% filter(scenario==s)
  
  # use filters to highlight preds beyond thresholds
  dfFilter$PrHeightMeanLat[which(dfFilter$GDD5threshold==1)] <- 9999
  dfFilter$PrHeightMeanLat[which(dfFilter$LatThreshold==1)] <- -9999
  
  # convert to spatial
  spP <- dfFilter
  rm(dfFilter)
  coordinates(spP) <- ~ CenterLong + CenterLat
  
  # define lat long crs
  proj4string(spP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
  
  # transform points to utm
  spP <- spTransform(spP, CRSobj = utm)
  
  rstUTM <- raster(crs = crs(spP), resolution = c(1100,1100), ext = extent(spP))
  
  rst <- rasterize(spP, rstUTM, spP$PrHeightMeanLat, fun=max, na.rm=TRUE) 

  writeRaster(rst, paste0(dirOut,"pred_rsts/PrHeightMeanLat_",s,"_thresholds.tif"),overwrite=TRUE)
  
}

# list height tifs 
rsts <-  list.files(paste0(dirOut, "pred_rsts/"),pattern = "*.tif", full.names = T)
rsts <- grep("PrHeightMeanLat", rsts, value=TRUE)

lstRCP <- c("26in70","60in70","45in70","85in70")

for (rcp in lstRCP){
  
  #rcp <- lstRCP[1]
  rsts1 <- grep(rcp, rsts, value=TRUE)
  rsts1 <- rsts1[-3] # remove mean
  rcp.name <- ifelse(grepl("26in70", rcp), 'RCP2.6', 
                     ifelse(grepl("45in70", rcp), 'RCP4.5',
                            ifelse(grepl("60in70", rcp), 'RCP6.0',
                                   ifelse(grepl("85in70", rcp), 'RCP8.5', NA))))
  
  # raster stack
  heightStack <- stack(rsts1)
  spplot(heightStack)
  
  # threshold reclass
  # height above 282cm (mean height for reference period)
  # reclass matrix
  rules2 <- c(-10000, 0, 10,  
              0, 282, 0,
              282, 1500, 1,
              1500, 10000, 10)
  rcl2 <- matrix(rules2, ncol=3, byrow=TRUE)
  rclassStack <- reclassify(heightStack,rcl2)
  #spplot(rclassStack)
  
  sumStack <- stackApply(rclassStack, indices=1, fun=sum)
  #plot(sumStack)
  
  rules3 <- c(4,5,5,
              3,4,4,
              2,3,3,
              1,2,2,
              0,1,1,
              -1,0,NA,
              5,10,-1,
              10,20,-2,
              20,30,-3,
              30,40,-4,
              40,50,-5)
  rcl3 <- matrix(rules3, ncol=3, byrow=TRUE)
  rclassStack3 <- reclassify(sumStack,rcl3)
  spplot(rclassStack3)
  
  #display.brewer.all()
  #brewer.pal(5, "Oranges")
  #brewer.pal(10, "BrBG")
  my.at <- c(5,4,3,2,1,-1,-2,-3,-4,-5)
  #cols0 <- brewer.pal(n=length(my.at), name="PiYG")
  #cols0 <- brewer_pal("div","BrBG", direction=-1)(length(my.at))
  cols0 <- c("#003C30","#01665E","#35978F","#80CDC1","#C7EAE5","#FEEDDE", "#FDBE85", "#FD8D3C", "#E6550D", "#A63603")
  
  #Derive desired break/legend colours from gradient of selected brewer palette
  cols1 <- colorRampPalette(cols0, space="rgb")(length(my.at))
  
  # Convert raster to dataframe
  df <- as.data.frame(rclassStack3, xy=T)
  names(df) <- c("x", "y", "GCMagree")
  
  my.labs <- c("5 GCMs agree",
            "4 GCMs agree",
            "3 GCMs agree", 
            "2 GCMs agree",
            "1 GCM agrees",
            "1 GCM beyond", 
            "2 GCMs beyond",
            "3 GCMs beyond",
            "4 GCMs beyond",
            "5 GCMs beyond")
  
  plot.title <- paste0("Predicted height above 1961-1990 average in 2070 ",rcp.name)
  p1 <- df %>% filter(!is.na(GCMagree)) %>% 
    ggplot(aes(x=x, y=y)) + 
    geom_tile(aes(fill=GCMagree)) + 
    coord_equal()+
    theme_bw() + 
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
    #scale_fill_discrete(cols=cols1, labels = my.labs)+
    scale_fill_gradientn(colours=cols1,
                         values=rescale(my.at),
                         limits=range(df$GCMagree),
                         breaks=my.at,#)+#,
                         labels = my.labs)+
    labs(fill="Uncertainty")+
    ggtitle(plot.title)+
    theme(axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())#,
          #legend.position="none", panel.grid=element_blank())
  
  # make dummy plot discrete legend whose colour breaks go along `cols1`
  #df_leg <- data.frame(x=1:length(my.at), y=length(my.at):1, value=my.at)
  #gg_leg <- ggplot(data=df_leg, aes(x=x, y=y)) + geom_raster(aes(fill=factor(value))) +
    #scale_fill_manual(breaks=my.at, values=cols1,
                      #guide=guide_legend(title="",
                                         #label.position="bottom")) +
    #theme(legend.position="bottom")
  
  # extract discrete legend from dummy plot
  #tmp <- ggplot_gtable(ggplot_build(gg_leg))
  #leg <- which(sapply(tmp$grobs, function(x) x$name)=="guide-box")
  #legend <- tmp$grobs[[leg]]
  
  # combine continuous plot of your rasters with the discrete legend
  #grid.arrange(p1, legend, ncol=2)
  
  # contour
  #contour1 <- rasterToContour(sumStack,levels = c(1,2,3,4,5,10,20,30,40,50))
  #contour1 <- st_as_sf(contour1)
  #contour1$level <- as.numeric(contour1$level)
  #contour1$agreement <- NA
  #contour1$agreement[which(contour1$level<=1&contour1$level>0)]<-"1 GCM agrees"
  #contour1$agreement[which(contour1$level<=2&contour1$level>1)]<-"2 GCMs agree"
  #contour1$agreement[which(contour1$level<=3&contour1$level>2)]<-"3 GCMs agree"
  #contour1$agreement[which(contour1$level<=2&contour1$level>1)]<-"2 GCMs agree"
  #contour1$agreement[which(contour1$level<=4&contour1$level>3)]<-"4 GCMs agree"
  #contour1$agreement[which(contour1$level<=5&contour1$level>4)]<-"5 GCMs agree"
  #contour1$agreement[which(contour1$level<=10&contour1$level>5)]<-"1 GCM beyond"
  #contour1$agreement[which(contour1$level<=20&contour1$level>10)]<-"2 GCMs beyond" 
  #contour1$agreement[which(contour1$level<=30&contour1$level>20)]<-"3 GCMs beyond"
  #contour1$agreement[which(contour1$level<=40&contour1$level>30)]<-"4 GCMs beyond"
  #contour1$agreement[which(contour1$level<=50&contour1$level>40)]<-"5 GCMs beyond"
  
  # contour1$agreement <- factor(contour1$agreement, ordered = T,levels = c("5 GCMs agree", 
                                          #"4 GCMs agree", 
                                          #"3 GCMs agree", 
                                          #"2 GCMs agree",
                                          #"1 GCM agrees",
                                          #"1 GCM beyond",
                                          #"2 GCMs beyond",
                                          #"3 GCMs beyond",
                                          #"4 GCMs beyond",
                                          #"5 GCMs beyond"))
  #contour1 <- st_cast(contour1, to="POLYGON")
  
  #viridis_pal(option="C")(5)
  #display.brewer.all()
  #brewer.pal(5, "YlGn")
  #brewer.pal(5, "YlOrRd")
  #brewer.pal(10, "Spectral")
  #my.pal <- c("5 GCMs agree" = "#5E4FA2",
              #"4 GCMs agree" = "#3288BD",
              #"3 GCMs agree" = "#66C2A5",
              #"2 GCMs agree" = "#ABDDA4",
              #"1 GCM agrees" = "#E6F598",
              #"1 GCM beyond" = "#FEE08B",
              #"2 GCMs beyond" = "#FDAE61",
              #"3 GCMs beyond" = "#F46D43",
              #"4 GCMs beyond" = "#D53E4F",
              #"5 GCMs beyond" = "#9E0142")
  
  #plot.title <- paste0("Mean provenance height above reference mean in 2070 ",rcp.name)
  #g1 <- ggplot()+
    #geom_sf(data=contour1,aes(fill=agreement),col=NA)+
    #geom_sf(data = sweden, fill=NA)+
    #scale_fill_viridis(discrete = T, option = "C")+
    #scale_fill_manual(values=my.pal)+
    #ggtitle(plot.title)+
    #theme_bw()
  
  png(paste0(dirFigs,"GCM_agreement_meanProvHeight_over_refMean_",rcp.name,".png"), units="cm", width = 20, height = 20, res=1000)
  print(p1)
  dev.off()
  
}

### map uncertainty ------------------------------------------------------------

# Chakraborty et al (2016) maps of uncertainty 
# (calculated as the difference in predicted performance between two climate scenarios, 
# as a percentage of the mean of the two predictions).

# stacks of rcp4.5 and 8.5
rsts4.5 <- grep("45in50", rsts, value=TRUE)
rsts8.5 <- grep("85in50", rsts, value=TRUE)

stackHeight4.5 <- stack(rsts4.5)
spplot(stackHeight4.5)
stackHeight8.5 <- stack(rsts8.5)
spplot(stackHeight8.5)

stackDiff <- stackHeight8.5 - stackHeight4.5
spplot(stackDiff)
names(stackDiff)
#rstAvg <- mean(stackHeight4.5,stackHeight8.5)
#spplot(rstAvg)
stackAvg <- stack()
for (i in 1:5){
  rstAvg <- mean(stackHeight4.5[[i]],stackHeight8.5[[i]])
  stackAvg <- addLayer(stackAvg, rstAvg)
}
names(stackAvg) <- c("bc","he","mg","mi","no")
spplot(stackAvg)

rstUncert <- stackDiff/stackAvg * 100
spplot(rstUncert)
# why getting negative values?

# check using dataframe
df45 <- dfPredictions[,c("CenterLat","CenterLong","RCP","GCM","PrHeightLocal")] %>% 
  filter(RCP=="4.5")
df85 <- dfPredictions[,c("CenterLat","CenterLong","RCP","GCM","PrHeightLocal")] %>% 
  filter(RCP=="8.5")

dfUncert <- cbind(df45[,c(1,2,4,5)],df85$PrHeightLocal)
head(dfUncert)
colnames(dfUncert) <- c("Lat","Long","GCM","height45","height85")

# negative values occur when rcp85 pred is less than rcp45 pred 
# (when just calculating difference as rcp85 - rcp45)
# below I've added a conditon to always subtract the smaller value from the larger
# so that we just get one + measure of uncertainty %

dfUncert <- dfUncert %>% dplyr::mutate(
  heightDiff = ifelse(height85 > height45, height85 - height45, height45 - height85),
  heightMean = rowMeans(dplyr::select(.,height45:height85)),
  uncertainty = heightDiff/heightMean*100)

summary(dfUncert)
hist(dfUncert$uncertainty)

library(classInt)
breaks.qt <- classIntervals(dfUncert$uncertainty, n = 40, style = "pretty", intervalClosure = "right")

lstGCMs <- unique(dfUncert$GCM)

for (gcm in lstGCMs){
  
  #gcm <- lstGCMs[1]
  dfGCM <- dfUncert %>% filter(GCM==gcm)
  
  # convert to spatial
  spP <- dfGCM
  coordinates(spP) <- ~ Long + Lat
  
  # define lat long crs
  proj4string(spP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
  
  # transform points to utm
  spP <- spTransform(spP, CRSobj = utm)
  
  rstUTM <- raster(crs = crs(spP), resolution = c(1100,1100), ext = extent(spP))
  
  rstUncert <- rasterize(spP, rstUTM, spP$uncertainty, fun=max, na.rm=TRUE)
  
  sp1 <- spplot(rstUncert, at = breaks.qt$brks)
  
  png(paste0(dirFigs,"PrHeightLocal_",gcm,"_uncertainty.png"), units="cm", width = 20, height = 20, res=1000)
  print(sp1)
  dev.off()
  
  writeRaster(rstUncert, paste0(dirOut,"PrHeightLocal_",gcm,"_uncertainty.tif"),overwrite=TRUE)
  
  
}

rsts3 <-  list.files(dirOut,pattern = "*.tif", full.names = T)
stackUncertainty <- stack(rsts3)
names(stackUncertainty) <- lstGCMs

png(paste0(dirFigs,"PrHeightLocal_uncertainty_per_GCM.png"), units="cm", width = 20, height = 15, res=1000)
spplot(stackUncertainty)
dev.off()

png(paste0(dirFigs,"PrHeightLocal_uncertainty_dist_per_GCM.png"), units="cm", width = 15, height = 10, res=1000)
ggplot(dfUncert)+
  geom_histogram(aes(uncertainty), bins = 100)+
  facet_wrap(~GCM)+
  xlab("Uncertainty (%)")+
  theme_bw()
dev.off()

nrow(dfUncert %>% filter(uncertainty>100))

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

