
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
library(wesanderson)

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


### apply thresholds -----------------------------------------------------------

dfPredictions$PrHeightMinLatRC <- dfPredictions$PrHeightMinLat
dfPredictions$PrHeightMinLatRC[which(dfPredictions$CenterLat < 52.6 | dfPredictions$CenterLat > 62.6)] <- NA
dfPredictions$PrHeightMinLatRC[which(dfPredictions$GDD5Future < 527 | dfPredictions$GDD5Future > 1349)] <- NA

dfPredictions$PrHeightMeanLatRC <- dfPredictions$PrHeightMeanLat
dfPredictions$PrHeightMeanLatRC[which(dfPredictions$CenterLat < 59.87 | dfPredictions$CenterLat > 69.87)] <- NA
dfPredictions$PrHeightMeanLatRC[which(dfPredictions$GDD5Future < 527 | dfPredictions$GDD5Future > 1349)] <- NA

dfPredictions$PrHeightMaxLatRC <- dfPredictions$PrHeightMaxLat
dfPredictions$PrHeightMaxLatRC[which(dfPredictions$CenterLat < 64.77 | dfPredictions$CenterLat > 74.77)] <- NA
dfPredictions$PrHeightMaxLatRC[which(dfPredictions$GDD5Future < 527 | dfPredictions$GDD5Future > 1349)] <- NA


### raw height boxplots --------------------------------------------------------

GCMs <- unique(dfPredictions$GCM)
GCMs <- GCMs[-3] # remove ensemble

dfPredictions$GCM <- factor(dfPredictions$GCM)
dfPredictions$RCP <- factor(dfPredictions$RCP)

#GCM_boxplots <- list()

#for(i in GCMs) {
  #GCM_boxplots[[i]] <- ggplot(dfPredictions %>% filter(GCM == i & !is.na(PrHeightMeanLatRC)), aes(x=RCP, y=PrHeightMeanLatRC, fill=RCP)) + 
    #geom_boxplot() +
    #scale_fill_brewer(palette = "Dark2")+
    #stat_summary(fun=mean, geom="point", color="black", size=3) +   
    #scale_y_continuous(limits = c(0, 600)) +
    #xlab("RCP scenario") +
    #ylab("Height distribution (cm)") +
    #theme_bw() + 
    #ggtitle(i) + 
    #theme(plot.title = element_text(size = 20, face = "bold", hjust=0.5), 
          #axis.title.x = element_text(size = 18, face = "bold"), 
          #axis.title.y = element_text(size = 18, face = "bold"),
          #axis.text.x = element_text(size = 16),
          #axis.text.y = element_text(size = 16))
  # save
  #ggsave(GCM_boxplots[[i]], file=paste0(dirFigs,"PrHeightMeanLat_Nordic_Boxplot_2070_", i,".png"), width = 10, height = 10, dpi=300)
#}

# arrange all plots in one grid next to each other
#GCMs_boxplots.all <- do.call("grid.arrange", c(GCM_boxplots[1:5], ncol= 5))
#ggsave(GCMs_boxplots.all, file=paste0(dirFigs,"GCM_RCP_PrHeightMeanLat_Nordic_boxplots_2070.png"), width=21, height=5, dpi=300)


# tidyverse version

(p1 <- dfPredictions %>% 
  filter(!is.na(PrHeightMeanLatRC) & GCM != "Ensemble") %>% 
  ggplot(aes(x=RCP, y=PrHeightMeanLatRC, fill=RCP))+
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
  #mutate(avgMinLat = minLatMean,
         #avgMeanLat = meanLatMean,
         #avgMaxLat = maxLatMean) %>% 
  # manually calc sd against reference mean
  summarise(SDmin = sqrt(sum((PrHeightMinLat-minLatMean)^2)/1000),
            SDmean = sqrt(sum((PrHeightMeanLat - meanLatMean)^2)/1000),
            SDmax = sqrt(sum((PrHeightMaxLat - maxLatMean)^2)/1000)) %>%  
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


### Spatial agreement between GCMs, per RCP ------------------------------------

# list height tifs 
rsts <-  list.files(paste0(dirOut, "pred_rsts/"),pattern = "*.tif", full.names = T)
rsts <- grep("threshold_reclass", rsts, value=TRUE)

lstRCP <- c("26in70","60in70","45in70","85in70")

lstProv <- c("PrHeightMinLat","PrHeightMeanLat","PrHeightMaxLat")

for (rcp in lstRCP){
  
  #rcp <- lstRCP[1]
  rstsRCP <- grep(rcp, rsts, value=TRUE)
  rstsRCP <- rstsRCP[-3] # remove mean
  rcp.name <- ifelse(grepl("26in70", rcp), 'RCP2.6', 
                     ifelse(grepl("45in70", rcp), 'RCP4.5',
                            ifelse(grepl("60in70", rcp), 'RCP6.0',
                                   ifelse(grepl("85in70", rcp), 'RCP8.5', NA))))
  
  for (prov in lstProv){
    
    rstsProv <- grep(prov, rstsRCP, value=TRUE)
    
    # raster stack
    rclassStack <- stack(rstsProv) 
    print(spplot(rclassStack))
    
    print("Provenance results per RCP/GCM read in as stack")
    
    # sum
    sumStack <- sum(rclassStack[[1]],rclassStack[[2]],rclassStack[[3]],rclassStack[[4]],rclassStack[[5]])
    print("Raster stacks summed")
    
    spplot(sumStack)
    
    # Convert raster to dataframe
    df <- as.data.frame(sumStack, xy=T)
    names(df) <- c("x", "y", "GCMagree")
    my.at <- c(5,4,3,2,1,0,-1,-2,-3,-4,-5)
    
    cols0 <- c("#003C30","#01665E","#35978F","#80CDC1","#FFDB58", "#F0F0F0", "#D9D9D9", "#BDBDBD", "#969696")
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
    
    
    p2 <- ggplot(data = df) + 
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
      theme_bw()+
      theme(axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())
    
    
    png(paste0(dirFigs,"GCM_agreement_",prov,"_RCP",rcp,".png"), units="cm", width = 12, height = 14, res=1000)
    print(p2)
    dev.off()
    
    print(paste0("Plot saved for provenance: ",prov))
    
  }
  
  print(paste0("Plots complete for RCP: ", rcp))
  
}

  
