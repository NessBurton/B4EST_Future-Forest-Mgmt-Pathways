
# date: 13/05/21
# author: VB
# description: figure for Frontiers manuscript - combined Nordic & Spain CoV boxplots

### libs -----------------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(vroom)
library(stringr)
library(viridis)

### dirs -----------------------------------------------------------------------

wd <- "~/FFMPs" # sandbox
dirData <- paste0(wd,"/data-raw/")
dataDrive <- "D:"
dirOut <- paste0(dataDrive,"/FFMP-data-processed/Frontiers_manuscript/")
dirFigs <- paste0(wd,"/Frontiers_figures/")


### Nordic data ----------------------------------------------------------------

# list production prediction files
files <-  list.files(paste0(dirData, "Nordicpredictions/"),pattern = "*.csv",full.names = T)
files2 <- files[-25] # remove reference for now
files2 <- files2[-c(9:12)] # remove ensemble

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

# new var, remove data beyond thresholds
dfPredictions$PrHeightMeanLatT <- dfPredictions$PrHeightMeanLat
dfPredictions$PrHeightMeanLatT[which(dfPredictions$CenterLat < 59.87 | dfPredictions$CenterLat > 69.87)] <- NA
dfPredictions$PrHeightMeanLatT[which(dfPredictions$GDD5Future < 527 | dfPredictions$GDD5Future > 1349)] <- NA

# take 100 random samples of 1000 data points
dfRandom <- bind_rows(replicate(100, dfPredictions %>% sample_n(1000), simplify=F), .id="Obs")

# reference mean
dfReference <- vroom(files[25])
meanLatMean <- mean(dfReference$PrHeightMeanLat, na.rm = TRUE) # 308.8 cm

# all data
dfNordicAll <- dfRandom %>% 
  group_by(RCP,GCM,Obs) %>% 
  mutate(RCP.mean = PrHeightMeanLat - meanLatMean,
         RCP.sq = RCP.mean ^ 2) %>% 
  # manually calc sd from reference mean
  summarise(GCM.sd = sqrt(sum(RCP.sq, na.rm = T)/1000)) %>%  
  mutate(CoV_mean = GCM.sd/meanLatMean*100)

head(dfNordicAll)
dfNordicAll$region <- "Nordic"
dfNordicAll$thresholds <- "All data"

# with data beyond model limits removed
dfNordicLim <- dfRandom %>% 
  group_by(RCP,GCM,Obs) %>% 
  mutate(RCP.mean = PrHeightMeanLatT - meanLatMean,
         RCP.sq = RCP.mean ^ 2) %>% 
  # manually calc sd from reference mean
  summarise(GCM.sd = sqrt(sum(RCP.sq, na.rm = T)/1000)) %>%  
  mutate(CoV_mean = GCM.sd/meanLatMean*100)

head(dfNordicLim)
dfNordicLim$region <- "Nordic"
dfNordicLim$thresholds <- "Model limits applied"


### Spanish data ---------------------------------------------------------------

# all data
dfSpainAll <- vroom(paste0(dirData, "dfRCP2_CoV_allData.csv"))
head(dfSpainAll)
dfSpainAll$region <- "Spain"
dfSpainAll$thresholds <- "All data"
dfSpainAll$RCP <- as.character(dfSpainAll$RCP)
unique(dfSpainAll$RCP)
dfSpainAll$RCP[which(dfSpainAll$RCP=="6")] <- "6.0"

# with data beyond model limits removed
dfSpainLim <- vroom(paste0(dirData, "dfRCP_CoV_Thresholds.csv"))
head(dfSpainLim)
dfSpainLim$region <- "Spain"
dfSpainLim$thresholds <- "Model limits applied"
dfSpainLim$RCP <- as.character(dfSpainLim$RCP)
unique(dfSpainLim$RCP)
dfSpainLim$RCP[which(dfSpainLim$RCP=="6")] <- "6.0"


### bind data ------------------------------------------------------------------

dfCoV <- rbind(dfNordicAll[,c("RCP","GCM","CoV_mean","region","thresholds")],
               dfNordicLim[,c("RCP","GCM","CoV_mean","region","thresholds")],
               dfSpainAll[,c("RCP","GCM","CoV_mean","region","thresholds")],
               dfSpainLim[,c("RCP","GCM","CoV_mean","region","thresholds")])

dfCoV$RCP <- as.factor(dfCoV$RCP)
dfCoV$region <- as.factor(dfCoV$region)
dfCoV$thresholds <- as.factor(dfCoV$thresholds)

summary(dfCoV)

### Coefficient of Variation (CoV) ---------------------------------------------

(CV <- dfCoV %>% 
    ggplot()+
    geom_boxplot(aes(RCP, CoV_mean,fill=RCP))+
    scale_fill_brewer(palette = "Dark2")+
    #ylim(0,40)+ 
    ylab("CoV (%)")+
    facet_grid(region~thresholds,scales = "free")+
    theme_bw()+
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_text(size = 18, face = "bold", margin = margin(r = 15)),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.ticks.x = element_blank(),
          legend.title = element_text(size = 16, face = "bold"),
          legend.text = element_text(size = 14),
          strip.text = element_text(face="bold", size = 14)))

library(gtable)
g <- ggplotGrob(CV)
strips <- g$layout[grep("strip-t", g$layout$name), ]
titles <- lapply(paste0("(", letters[seq_len(nrow(strips))], ")"), 
                 textGrob, x = 0, hjust = 0, vjust = 1)
g <- gtable_add_grob(g, grobs = titles, 
                     t = strips$t, b = strips$b - 2, 
                     l = strips$l, r = strips$r)
grid.newpage()
grid.draw(g)

ggsave(CV, file=paste0(dirFigs, "CoV_Nordic_&_Spain_comparison.png"), width=12, height=10, dpi=300)

