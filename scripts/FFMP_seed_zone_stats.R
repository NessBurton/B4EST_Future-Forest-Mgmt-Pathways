
# date: 14/12/20
# author: VB
# description: script to test the development of Future Forest Mangement Pathways (FFMPs) using
# data provided by Skogforsk.

#wd <- "~/R/FFMPs" # laptop
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
library(RColorBrewer)
library(vroom)

### reference data -------------------------------------------------------------

# refclimate
df_ref <- vroom(paste0(dirData,"Productionpredictions/Refclimate_SO1.5g_predictions.csv"))
# create a spatialpoints dataframe
sp_ref <- df_ref
coordinates(sp_ref) <- ~ CenterLong + CenterLat
# set crs - assume lat long
proj4string(sp_ref) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

### seed zones -----------------------------------------------------------------

# seed zones for Sweden sent by Mats
# Alt 0246 for umlaut over o (if Num Lock available) - otherwise copy paste
sfSeedZones <- st_read(paste0(dirData,"Seed_zones_SP_Sweden/Shaper/Frözoner_tall_Sverige.shp"))
utm <- crs(sfSeedZones)

# dissolve/merge zones by ZON2 to simplify
head(sfSeedZones)

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

# transform points to utm
sp_ref <- spTransform(sp_ref, CRSobj = utm)

# create an empty raster object to the extent of the points desired resolution
# res should be 1km - 1000m if UTM, using 1100m to deal with irregular grid (gaps if using 1000m)
rstUTM <- raster(crs = crs(sp_ref), resolution = c(1100,1100), ext = extent(sp_ref))

rstElev <- rasterize(sp_ref, rstUTM, sp_ref$GridAlt, fun=max)
crs(rstElev)
plot(rstElev);plot(spSeedZones,add=TRUE, fill=NA)

dfElev <- extract(rstElev, spSeedZones, fun=max, df=TRUE, na.rm=TRUE)
dfElev$Zone <- zoneOrder


### read in Scots pine predictions and join to zones ---------------------------

# need to convert reference data to correct format for use in loop
# will use unimproved provenance under reference climate to calculate future change

# convert to sf
sfReference <- st_as_sf(sp_ref)
#rm(sp_ref)
# spatial join
sfReference <- st_join(sfReference,sfSeedZones)
head(sfReference)
sfReference$ZON2 <- factor(sfReference$ZON2, ordered = TRUE, levels = zoneOrder)
summary(sfReference$ZON2)

head(sfReference)
crs(sfReference)

# convert survival + prodIdx to percentages
sfReference[,11:18] <- sfReference[,11:18] * 100

# needs adjusting
sfReference <- sfReference[,c(3:18)] %>% 
  st_drop_geometry() %>%
  pivot_longer(cols = 1:16, names_to="variable",values_to="value")
sfReference$prod_idx <- sfReference$prod_idx * 100 # convert from decimal to percentage
colnames(sfReference)[3] <- "ref_prod_idx"
rm(df_ref)

# list production prediction files per scenario
files <-  list.files(paste0(dirData, "Productionpredictions/"),pattern = "*.csv",full.names = T)
# just RCP8.5 to test + current to compare
#files <- grep("85in50", files, value=TRUE)
#files <- grep("45in50", files, value=TRUE)
files

#df_results_lng <- tibble()
df_results_summary <- tibble()
scenario_list <- c()

for (f in files){
  
  #f <- files[1]
  
  scenario <- strsplit(f, "[_]")[[1]][1]
  scenario <- strsplit(scenario, "[/]")[[1]][8]
  
  scenario_list[[length(scenario_list) + 1]] <- scenario
  
  print("Read in data and apply thresholds")
  dfP <- vroom(f)
  
  # apply thresholds (survival & latitudinal transfer)
  dfP$PrProdidxSOh60[which(dfP$PrSurvSOh60 <0.5)] <- NA
  dfP$PrProdidxSOh62[which(dfP$PrSurvSOh62 <0.5)] <- NA
  dfP$PrProdidxSOh64[which(dfP$PrSurvSOh64 <0.5)] <- NA
  dfP$PrProdidxSOh66[which(dfP$PrSurvSOh66 <0.5)] <- NA
  
  dfP$PrProdidxSOh60[which(dfP$CenterLat > 65 | dfP$CenterLat < 55)] <- NA
  dfP$PrProdidxSOh62[which(dfP$CenterLat > 67 | dfP$CenterLat < 57)] <- NA
  dfP$PrProdidxSOh64[which(dfP$CenterLat > 69 | dfP$CenterLat < 59)] <- NA
  dfP$PrProdidxSOh66[which(dfP$CenterLat > 71 | dfP$CenterLat < 61)] <- NA
  
  print("Convert to spatial, transform to utm")
  coordinates(dfP)<- ~ CenterLong + CenterLat
  # set crs - assume lat long
  proj4string(dfP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
  # transform to utm
  dfP <- spTransform(dfP, CRSobj = utm )
  dfP_sf <- st_as_sf(dfP)
  rm(dfP)
  
  print("Spatial join to seed zones")
  # spatial join
  dfP_sf <- st_join(dfP_sf,sfSeedZones)
  dfP_sf$ZON2 <- factor(dfP_sf$ZON2)
  
  print("Transform to long format")
  df_long <- dfP_sf[,c("PrProdidxSOh60","PrProdidxSOh62","PrProdidxSOh64","PrProdidxSOh66","ZON2")] %>% 
    filter(!is.na(ZON2)) %>% # filter to just zones
    st_drop_geometry() %>%
    pivot_longer(1:4, names_to="seed_orchard",values_to="prod_idx")
  
  df_long$prod_idx <- df_long$prod_idx * 100
  
  df_long$scenario <- scenario
  
  print("Calculate summaries")
  df_summary <- df_long %>% 
    group_by(ZON2, seed_orchard) %>% # group by zone
    summarise_if(is.numeric,c("mean","sd","IQR","min","max"), na.rm = TRUE) #%>% 
  #summarise(c("PrProdidxSOh60","PrProdidxSOh62","PrProdidxSOh64","PrProdidxSOh66"),.funs=c("mean","sd","IQR","min","max"))
  df_summary$scenario <- scenario
  
  print("Add to results table")
  df_results_summary <- rbind(df_results_summary, df_summary)
  
  print(paste0("Processed scenario: ",scenario))
  
  if (f == files[25]){
    vroom_write(df_results_summary, paste0(dirOut, "PrProdIdx_seed_zone_summaries_Sweden.csv"))
  }
  
}

head(df_results_summary)
summary(df_results_summary)

df_results_summary$ZON2 <- factor(df_results_summary$ZON2, ordered=T, levels = zoneOrder)

dfMaster <- df_results_summary

dfMaster$GCM <- ifelse(grepl("bc", dfMaster$scenario), 'bc - BCC-CSM1-1',
                        ifelse(grepl("he", dfMaster$scenario), 'he - HadGEM2-ES',
                               ifelse(grepl("mg", dfMaster$scenario), 'mg - MRI-Cscenario3',
                                      ifelse(grepl("mi", dfMaster$scenario), 'mi - MIROC-ESM-CHEM',
                                             ifelse(grepl("no", dfMaster$scenario), 'no - NorESM1-M',
                                                    ifelse(grepl("Ref", dfMaster$scenario), "Baseline", 'Mean all GCMs'))))))
dfMaster$RCP <- ifelse(grepl("45", dfMaster$scenario), '4.5',
                       ifelse(grepl("85", dfMaster$scenario), '8.5', 'Baseline'))
dfMaster$period <- ifelse(grepl("50", dfMaster$scenario), "2050",
                          ifelse(grepl("70", dfMaster$scenario), '2070', NA))
dfMaster$seedOrchard <- substr(dfMaster$seed_orchard, 10,14)

dfMaster$GCM <- factor(dfMaster$GCM)
dfMaster$RCP <- factor(dfMaster$RCP)
dfMaster$seedOrchard <- factor(dfMaster$seedOrchard)

# RCP4.5
df4.5 <- dfMaster %>%
  #filter(period == "2050") %>% 
  filter(RCP %in% c("Baseline","8.5")==FALSE) %>% 
  filter(GCM %in% c("Mean all GCMs","Baseline")==FALSE) %>% 
  group_by(ZON2,period,seedOrchard) %>% 
  summarise(n_GCMs = n(),
            n_m120 = sum(mean > 120),
            #n_m120 = sum(max > 120),
            p_m120 = n_m120 / n_GCMs*100)

head(df4.5)

df4.5 <- df4.5 %>% ungroup()
df4.5$trafficLight <- NA
df4.5$trafficLight[which(dfGCM$n_m120==5)] <- "All GCMs"
df4.5$trafficLight[which(dfGCM$n_m120==4)] <- "4 GCMs"
df4.5$trafficLight[which(dfGCM$n_m120==3)] <- "3 GCMs"
df4.5$trafficLight[which(dfGCM$n_m120==2)] <- "2 GCMs"
df4.5$trafficLight[which(dfGCM$n_m120==1)] <- "1 GCM"
df4.5$trafficLight[which(dfGCM$n_m120==0)] <- "No GCMs"
df4.5$trafficLight[which(is.na(dfGCM$n_m120))] <- "Beyond model threshold"

df4.5$trafficLight <- factor(df4.5$trafficLight, ordered = T,
                             levels = c("All GCMs","4 GCMs","3 GCMs","2 GCMs","1 GCM","No GCMs","Beyond model threshold"))

head(df4.5)
# this is getting there!
png(paste0(wd,"/figures/SO_mean_prodIdx_above_120_RCP4.5.png"), width = 600, height = 800)
ggplot(df4.5)+
  geom_tile(aes(seedOrchard,period, fill=trafficLight))+
  scale_fill_brewer(palette = "RdYlGn", direction = -1)+
  coord_flip()+
  facet_wrap(~ZON2, nrow = 11, ncol=2)+
  theme_bw()+
  ylab("RCP")+xlab("Seed orchard")+
  ggtitle("Likelihood of seed orchard performance > 120 under RCP4.5")+
  labs(fill="Likelihood")
dev.off()

# could join back & plot spatially
df4.5$ZON2 <- factor(df4.5$ZON2,ordered = T, levels=zoneOrder)

sfSeedZones4.5 <- left_join(sfSeedZones,df4.5,by="ZON2")

# load country outline
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
sweden <- worldmap[worldmap$name == 'Sweden',]

png(paste0(wd,"/figures/SO_mean_prodIdx_above_120_RCP4.5_spatial.png"), width = 1000, height = 800)
ggplot()+
  geom_sf(data = sweden, fill=NA)+
  geom_sf(data=sfSeedZones4.5, aes(fill=trafficLight), colour=0)+
  scale_fill_brewer(palette = "RdYlGn", direction = -1)+
  facet_grid(seedOrchard~period)+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  labs(fill = "Likelihood production index > 120%")
dev.off()

# RCP8.5
df8.5 <- dfMaster %>%
  #filter(period == "2050") %>% 
  filter(RCP %in% c("Baseline","4.5")==FALSE) %>% 
  filter(GCM %in% c("Mean all GCMs","Baseline")==FALSE) %>% 
  group_by(ZON2,period,seedOrchard) %>% 
  summarise(n_GCMs = n(),
            n_m120 = sum(mean > 120),
            #n_m120 = sum(max > 120),
            p_m120 = n_m120 / n_GCMs*100)

head(df8.5)

df8.5 <- df8.5 %>% ungroup()
df8.5$trafficLight <- NA
df8.5$trafficLight[which(dfGCM$n_m120==5)] <- "All GCMs"
df8.5$trafficLight[which(dfGCM$n_m120==4)] <- "4 GCMs"
df8.5$trafficLight[which(dfGCM$n_m120==3)] <- "3 GCMs"
df8.5$trafficLight[which(dfGCM$n_m120==2)] <- "2 GCMs"
df8.5$trafficLight[which(dfGCM$n_m120==1)] <- "1 GCM"
df8.5$trafficLight[which(dfGCM$n_m120==0)] <- "No GCMs"
df8.5$trafficLight[which(is.na(dfGCM$n_m120))] <- "Beyond model threshold"

df8.5$trafficLight <- factor(df8.5$trafficLight, ordered = T,
                             levels = c("All GCMs","4 GCMs","3 GCMs","2 GCMs","1 GCM","No GCMs","Beyond model threshold"))


head(df8.5)
png(paste0(wd,"/figures/SO_mean_prodIdx_above_120_RCP8.5.png"), width = 600, height = 800)
ggplot(df8.5)+
  geom_tile(aes(seedOrchard,period, fill=trafficLight))+
  scale_fill_brewer(palette = "RdYlGn", direction = -1)+
  coord_flip()+
  facet_wrap(~ZON2, nrow = 11, ncol=2)+
  theme_bw()+
  ylab("RCP")+xlab("Seed orchard")+
  ggtitle("Likelihood of seed orchard performance > 120 under RCP4.5")+
  labs(fill="Likelihood")
dev.off()

# could join back & plot spatially
df8.5$ZON2 <- factor(df8.5$ZON2,ordered = T, levels=zoneOrder)

sfSeedZones8.5 <- left_join(sfSeedZones,df8.5,by="ZON2")

# load country outline
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
sweden <- worldmap[worldmap$name == 'Sweden',]

png(paste0(wd,"/figures/SO_mean_prodIdx_above_120_RCP8.5_spatial.png"), width = 1000, height = 800)
ggplot()+
  geom_sf(data = sweden, fill=NA)+
  geom_sf(data=sfSeedZones8.5, aes(fill=trafficLight), colour=0)+
  scale_fill_brewer(palette = "RdYlGn", direction = -1)+
  facet_grid(seedOrchard~period)+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  labs(fill = "Likelihood production index > 120%")
dev.off()

### old figs -------------------------------------------------------------------

#brewer.pal(n = 8, name = "Dark2")
pal <- c("#D95F02","#1B9E77")
png(paste0(wd,"/figures/ProdIdx_change_frm_baseline_RCP85_perGCM.png"), units="cm", width = 20, height = 18, res=500)
#png(paste0(wd,"/figures/ProdIdx_change_frm_baseline_RCP45_perGCM.png"), units="cm", width = 20, height = 18, res=500)
df_results_lng %>% 
  filter(scenario != "MEAN85in") %>% 
  #filter(scenario != "MEAN45in") %>% 
  ggplot()+
  geom_boxplot(aes(seed.orchard,perc.change,col=change))+coord_flip()+
  #scale_color_brewer(palette = "Dark2")+
  scale_color_manual(values=pal)+  
  facet_grid(ZON2~scenario)+
  xlab("Seed orchard choice")+
  ylab("Change in production index (% units) from baseline")+
  labs(col = "Direction of change")+ 
  theme(legend.position="top")+
  ylim(c(-10,40))
dev.off()

png(paste0(wd,"/figures/ProdIdx_change_frm_baseline_RCP85.png"), units="cm", width = 20, height = 18, res=500)
#png(paste0(wd,"/figures/ProdIdx_change_frm_baseline_RCP45.png"), units="cm", width = 20, height = 18, res=500)
df_results_lng %>% 
  filter(scenario != "MEAN85in") %>% 
  #filter(scenario != "MEAN45in") %>% 
  ggplot()+
  geom_boxplot(aes(seed.orchard,perc.change,col=change))+coord_flip()+
  scale_color_manual(values = pal)+
  facet_grid(rows="ZON2")+
  xlab("Seed orchard choice")+
  ylab("Change in production index from baseline (%)")+
  labs(col = "Change in production from baseline")+ 
  theme(legend.position="top")+
  ylim(c(-10,100))
dev.off()

df_results_summary$change <- NA
df_results_summary$change[which(df_results_summary$perc_change_mean<0)]<-"decline"
df_results_summary$change[which(df_results_summary$perc_change_mean>0)] <- "increase"

df_results_summary %>% 
  filter(scenario != "MEAN85in") %>% 
  ggplot()+
  geom_point(aes(seed_orchard,perc_change_mean,size=perc_change_mean,col=change))+coord_flip()+
  facet_grid(ZON2~scenario)

# plot mean and standard error?
head(df_results_lng)

#limits <- aes(ymin = lwr, ymax = upr)

df_results_lng %>%
    group_by(scenario,ZON2, seed.orchard)%>%
    summarise(mnPrIdx = mean(na.omit(perc_change)),
              sePrIdx = sd(na.omit(perc_change)/sqrt(n())),
              upr = mnPrIdx + 1.96 * sePrIdx,
              lwr = mnPrIdx - 1.96 * sePrIdx) %>% 
  ggplot(aes(seed.orchard, mnPrIdx), colour = seed.orchard)+
  coord_flip()+
  #geom_errorbar(limits)+
  geom_point()+
  theme(panel.grid = element_blank())+
  facet_grid(ZON2~scenario, scales = "free")
