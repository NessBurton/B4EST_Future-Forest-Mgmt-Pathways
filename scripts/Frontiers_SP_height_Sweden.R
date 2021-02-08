
library(raster)
library(rgdal)
library(rgeos)
library(rasterVis)
library(dplyr)
library(tidyverse)
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
  
  scenario <- strsplit(i, "[_]")[[1]][1]
  scenario <- strsplit(scenario, "[/]")[[1]][8]
  f <- read.csv(i)
  f$scenario <- scenario
  #head(f)
  f <- f[,c("CenterLat","CenterLong","GridAlt","PrHeightLocal","PrHeightSOh60","PrHeightSOh62","PrHeightSOh64","PrHeightSOh66","scenario")]
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

lsOutliers <- list()
for(i in 4:8){
  lsOutliers[[i]] <- boxplot(dfPredictions[[i]], plot=FALSE)$out 
}

# identify the outliers within the dataset
outliers.50 <- list()
for(ii in 4:8){
  outliers.50[[ii]] <- dfPredictions[dfPredictions[[ii]] %in% lsOutliers[[ii]],]
}

outliers.50[[4]]

# calculate z-scores and plot --------------------------------------------------

# load country outline
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
sweden <- worldmap[worldmap$name == 'Sweden',]

nb.cols <- 9
mycolors <- colorRampPalette(brewer.pal(9, "BrBG"))(nb.cols)

# calculate Zscores
#dfPredictions$Zscore_heightLocal <- spatialEco::outliers(dfPredictions$PrHeightLocal) 

scenarios <- unique(dfPredictions$scenario)

for (s in scenarios){
  
  #s <- scenarios[1]
  dfFilter <- dfPredictions %>% filter(scenario==s)
  dfFilter$Zscore_heightLocal <- spatialEco::outliers(dfFilter$PrHeightLocal)
  # identify outliers
  dfOut <- dfFilter %>% 
    identify_outliers(PrHeightLocal) %>% 
    filter(is.outlier==TRUE | is.extreme==TRUE)
  # make spatial
  spPredictions <- dfOut
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
  ggsave(p1, file=paste0(dirFigs, s, "_PrHeightLocal_outliers.png"), width=8, height=8, dpi=300)
  
}

### calculate CV ---------------------------------------------------------------

# will mean for entire df be same as ensemble mean?

test <- dfPredictions[,c(4,10,11)] %>% 
  filter(RCP =="4.5")

resample::bootstrap(test, statistic = mean(test$PrHeightLocal), R=10)

# against ensemble mean?

ensembleFiles <- list.files(paste0(dirData, "Productionpredictions/"),pattern = "*.csv",full.names = T)
ensembleFiles <- grep("MEAN", ensembleFiles, value=TRUE)

dfEnsemble <- tibble()
for (i in ensembleFiles){
  
  scenario <- strsplit(i, "[_]")[[1]][1]
  scenario <- strsplit(scenario, "[/]")[[1]][8]
  f <- read.csv(i)
  f$scenario <- scenario
  #head(f)
  f <- f[,c("CenterLat","CenterLong","GridAlt","PrHeightLocal","PrHeightSOh60","PrHeightSOh62","PrHeightSOh64","PrHeightSOh66","scenario")]
  dfEnsemble <- rbind(dfEnsemble,f)
  
}

rcps <- c("45in50","85in50")

head(dfPredictions)
head(dfEnsemble)

gcmList <- unique(dfPredictions$scenario)
rsamp <- 10000
dfCoV <- tibble()

library(sjstats) # for bootstrap

# loop to calc CV against ensemble mean per GCM/RCP combo
for (gcm in gcmList){
  
  #gcm <- gcmList[2]
  dfGCM <- dfPredictions %>% filter(scenario %in% gcm == TRUE)
  
  if (length(grep("45in50",gcm))>1){
    dfMean <- dfEnsemble %>% filter(scenario=="MEAN45in50")
  }else{
    dfMean <- dfEnsemble %>% filter(scenario=="MEAN85in50")
  }
  
  dfCV <- tibble(dfGCM$scenario,as.numeric(dfGCM$PrHeightLocal),as.numeric(dfMean$PrHeightLocal))
  colnames(dfCV) <- c("Scenario","height.GCM","height.Ensemble")
  
  boots <- bootstrap(dfCV, 10, rsamp)
  
  
  for (i in resamples){
    
    dfCV <- resamples[1] %>% 
      mutate(mean.GCM = mean(height.GCM),
             mean.Ensemble = mean(height.Ensemble),
             sd.GCM = sd(height.GCM),
             sd.Ensemble = sqrt(sum((height.GCM-mean.Ensemble)^2)/rsamp),
             CV.GCM = sd.GCM/mean.GCM*100,
             CV.Ensemble = sd.Ensemble/mean.Ensemble*100)
    
  }
  dfCV <- dfCV %>% 
    #sample_n(.,rsamp) %>% # randomly sample
    mutate(mean.GCM = mean(height.GCM),
           mean.Ensemble = mean(height.Ensemble),
           sd.GCM = sd(height.GCM),
           sd.Ensemble = sqrt(sum((height.GCM-mean.Ensemble)^2)/rsamp),
           CV.GCM = sd.GCM/mean.GCM*100,
           CV.Ensemble = sd.Ensemble/mean.Ensemble*100)
  
  dfCoV <- rbind(dfCoV,dfCV)
  
}

summary(dfCoV)
dfCoV$RCP <- ifelse(grepl("45in50", dfCoV$Scenario), 'RCP 4.5',
                            ifelse(grepl("85in50", dfCoV$Scenario), 'RCP 8.5', 'RCP_all'))
dfCoV$GCM <- ifelse(grepl("bc", dfCoV$Scenario), 'bc - BCC-CSM1-1',
                            ifelse(grepl("he", dfCoV$Scenario), 'he - HadGEM2-ES',
                                   ifelse(grepl("mg", dfCoV$Scenario), 'mg - MRI-CGCM3',
                                          ifelse(grepl("mi", dfCoV$Scenario), 'mi - MIROC-ESM-CHEM',
                                                 ifelse(grepl("no", dfCoV$Scenario), 'no - NorESM1-M', 'GCM_all')))))

ggplot(dfCoV)+
  geom_point(aes(GCM, CV.Ensemble,col=GCM))+
  ylim(0,50)+ylab("CoV vs. ensemble mean (%)")+
  facet_wrap(~RCP)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

ggplot(dfCoV)+
  geom_point(aes(GCM, CV.GCM,col=GCM))+
  ylim(0,50)+ylab("CoV vs. GCM mean (%)")+
  facet_wrap(~RCP)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())
