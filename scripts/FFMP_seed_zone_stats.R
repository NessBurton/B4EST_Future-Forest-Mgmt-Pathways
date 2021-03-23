
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
library(rnaturalearth)
library(viridis)

### reference data -------------------------------------------------------------

# refclimate
#df_ref <- vroom(paste0(dirData,"Productionpredictions/Refclimate_SO1.5g_predictions.csv"))
# create a spatialpoints dataframe
#sp_ref <- df_ref
#coordinates(sp_ref) <- ~ CenterLong + CenterLat
# set crs - assume lat long
#proj4string(sp_ref) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

### seed zones -----------------------------------------------------------------

# seed zones for Sweden sent by Mats
# Alt 0246 for umlaut over o (if Num Lock available) - otherwise copy paste
sfSeedZones <- st_read(paste0(dirData,"Seed_zones_SP_Sweden/Shaper/Frözoner_tall_Sverige.shp"))
utm <- crs(sfSeedZones)

# dissolve/merge zones by ZON2 to simplify
head(sfSeedZones)

# merge some southern seed zones
#i. 18100, 18200, 18300 and 18400 can be merged into 18
#ii. 19100, 19200, 19300 and 19400 can be merged into 19
#iii. 20100 and 20200 can be merged into 20

sfSeedZones$seed.zone <- sfSeedZones$ZON2
sfSeedZones$seed.zone[which(sfSeedZones$ZON2 == "18100" | 
                              sfSeedZones$ZON2 == "18200" | 
                              sfSeedZones$ZON2 == "18300" | 
                              sfSeedZones$ZON2 == "18400")] <- "18"
sfSeedZones$seed.zone[which(sfSeedZones$ZON2 == "19100" | 
                              sfSeedZones$ZON2 == "19200" | 
                              sfSeedZones$ZON2 == "19300" | 
                              sfSeedZones$ZON2 == "19400")] <- "19"
sfSeedZones$seed.zone[which(sfSeedZones$ZON2 == "20100" | 
                              sfSeedZones$ZON2 == "20200")] <- "20"
sfSeedZones$seed.zone[which(sfSeedZones$ZON2 == "10000")] <- "10"
sfSeedZones$seed.zone[which(sfSeedZones$ZON2 == "12000")] <- "12"
sfSeedZones$seed.zone[which(sfSeedZones$ZON2 == "13000")] <- "13"
sfSeedZones$seed.zone[which(sfSeedZones$ZON2 == "15000")] <- "15"
sfSeedZones$seed.zone[which(sfSeedZones$ZON2 == "16000")] <- "16"
sfSeedZones$seed.zone

# add area to have a variable to be able to summarise
sfSeedZones$area <- st_area(sfSeedZones) 
sfSeedZones <-
  sfSeedZones %>%
  group_by(seed.zone) %>% 
  summarise(area = sum(area))

unique(sfSeedZones$seed.zone)
zoneOrder <- c("1a","1b","1c","2","3","6","7","10","12","13","15","16","18","19","20")
sfSeedZones$seed.zone <- factor(sfSeedZones$seed.zone, ordered = TRUE, levels = zoneOrder)

# load country outline
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
sweden <- worldmap[worldmap$name == 'Sweden',]

library(RColorBrewer)
# Define the number of colors you want
nb.cols <- length(unique(sfSeedZones$seed.zone))
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)

#png(paste0(wd,"/figures/seed_zones_all.png"), width = 500, height = 600)
ggplot()+
  geom_sf(data = sweden, fill=NA)+
  geom_sf(data=sfSeedZones, aes(fill=seed.zone), colour=0)+
  #scale_fill_brewer(palette = "Paired")+
  scale_fill_manual(values = mycolors) +
  theme_bw()+
  labs(fill = "Seed Zone")
#dev.off()

# sp version to use for raster::extract later
spSeedZones <- as_Spatial(sfSeedZones)

# transform points to utm
#sp_ref <- spTransform(sp_ref, CRSobj = utm)

# create an empty raster object to the extent of the points desired resolution
# res should be 1km - 1000m if UTM, using 1100m to deal with irregular grid (gaps if using 1000m)
#rstUTM <- raster(crs = crs(sp_ref), resolution = c(1100,1100), ext = extent(sp_ref))

#rstElev <- rasterize(sp_ref, rstUTM, sp_ref$GridAlt, fun=max)
#crs(rstElev)
#plot(rstElev);plot(spSeedZones,add=TRUE, fill=NA)

#dfElev <- extract(rstElev, spSeedZones, fun=max, df=TRUE, na.rm=TRUE)
#dfElev$Zone <- zoneOrder


### read in Scots pine predictions and join to zones ---------------------------

# need to convert reference data to correct format for use in loop
# will use unimproved provenance under reference climate to calculate future change

# convert to sf
#sfReference <- st_as_sf(sp_ref)
#rm(sp_ref)
# spatial join
#sfReference <- st_join(sfReference,sfSeedZones)
#head(sfReference)
#sfReference$ZON2 <- factor(sfReference$ZON2, ordered = TRUE, levels = zoneOrder)
#summary(sfReference$ZON2)

#head(sfReference)
#crs(sfReference)

# convert survival + prodIdx to percentages
#sfReference[,11:18] <- sfReference[,11:18] * 100

# needs adjusting
#sfReference <- sfReference[,c(3:18)] %>% 
  #st_drop_geometry() %>%
  #pivot_longer(cols = 1:16, names_to="variable",values_to="value")
#sfReference$prod_idx <- sfReference$prod_idx * 100 # convert from decimal to percentage
#colnames(sfReference)[3] <- "ref_prod_idx"
#rm(df_ref)

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
  
  f <- files[1]
  
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
  dfP$PrProdidxSOhs60[which(dfP$PrSurvSOhs60 <0.5)] <- NA
  dfP$PrProdidxSOhs62[which(dfP$PrSurvSOhs62 <0.5)] <- NA
  dfP$PrProdidxSOhs64[which(dfP$PrSurvSOhs64 <0.5)] <- NA
  dfP$PrProdidxSOhs66[which(dfP$PrSurvSOhs66 <0.5)] <- NA
  
  dfP$PrProdidxSOh60[which(dfP$CenterLat > 65 | dfP$CenterLat < 55)] <- NA
  dfP$PrProdidxSOh62[which(dfP$CenterLat > 67 | dfP$CenterLat < 57)] <- NA
  dfP$PrProdidxSOh64[which(dfP$CenterLat > 69 | dfP$CenterLat < 59)] <- NA
  dfP$PrProdidxSOh66[which(dfP$CenterLat > 71 | dfP$CenterLat < 61)] <- NA
  dfP$PrProdidxSOhs60[which(dfP$CenterLat > 65 | dfP$CenterLat < 55)] <- NA
  dfP$PrProdidxSOhs62[which(dfP$CenterLat > 67 | dfP$CenterLat < 57)] <- NA
  dfP$PrProdidxSOhs64[which(dfP$CenterLat > 69 | dfP$CenterLat < 59)] <- NA
  dfP$PrProdidxSOhs66[which(dfP$CenterLat > 71 | dfP$CenterLat < 61)] <- NA
  
  # and GDD5
  dfP$PrProdidxSOh60[which(dfP$GDD5Future < 527| dfP$GDD5Future > 1349)] <- NA
  dfP$PrProdidxSOh62[which(dfP$GDD5Future < 527| dfP$GDD5Future > 1349)] <- NA
  dfP$PrProdidxSOh64[which(dfP$GDD5Future < 527| dfP$GDD5Future > 1349)] <- NA
  dfP$PrProdidxSOh66[which(dfP$GDD5Future < 527| dfP$GDD5Future > 1349)] <- NA
  dfP$PrProdidxSOhs60[which(dfP$GDD5Future < 527 | dfP$GDD5Future > 1349)] <- NA
  dfP$PrProdidxSOhs62[which(dfP$GDD5Future < 527 | dfP$GDD5Future > 1349)] <- NA
  dfP$PrProdidxSOhs64[which(dfP$GDD5Future < 527 | dfP$GDD5Future > 1349)] <- NA
  dfP$PrProdidxSOhs66[which(dfP$GDD5Future < 527 | dfP$GDD5Future > 1349)] <- NA
  
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
  dfP_sf$seed.zone <- factor(dfP_sf$seed.zone, ordered = T, levels = zoneOrder)
  
  print("Transform to long format")
  
  df_long <- dfP_sf[,c("PrProdidxSOh60","PrProdidxSOh62","PrProdidxSOh64","PrProdidxSOh66",
                       "PrProdidxSOhs60","PrProdidxSOhs62","PrProdidxSOhs64","PrProdidxSOhs66",
                       "seed.zone")] %>% 
    filter(!is.na(seed.zone)) %>% # filter to just zones
    st_drop_geometry() %>%
    pivot_longer(1:8, names_to="seed_orchard",values_to="prod_idx")
  
  df_long$prod_idx <- df_long$prod_idx * 100
  
  df_long$scenario <- scenario
  
  print("Calculate summaries")
  
  df_summary <- df_long %>% 
    filter(!is.na(prod_idx)) %>% 
    group_by(as.factor(seed.zone), as.factor(seed_orchard)) %>% # group by zone
    summarise_if(is.numeric,c("mean","sd","IQR","min","max")) #%>% 
  #summarise(c("PrProdidxSOh60","PrProdidxSOh62","PrProdidxSOh64","PrProdidxSOh66"),.funs=c("mean","sd","IQR","min","max"))
  df_summary$scenario <- scenario
  
  print("Add to results table")
  df_results_summary <- rbind(df_results_summary, df_summary)
  
  print(paste0("Processed scenario: ",scenario))
  
  if (f == files[25]){
    write.csv(df_results_summary, paste0(dirOut, "PrProdIdx_seed_zone_summaries_Sweden_GDD5thresh2.csv"))
  }
  
}

#df_results_summary <- vroom(paste0(dirOut, "PrProdIdx_seed_zone_summaries_Sweden_GDD5thresh2.csv"))
head(df_results_summary)
summary(df_results_summary)
colnames(df_results_summary) <- c("seed.zone","seed.orchard","mean","sd","IQR","min","max","scenario")

# all combinations seed orchards & seed zones
orchards <- c("PrProdidxSOh60",
              "PrProdidxSOhs60",
              "PrProdidxSOh62",
              "PrProdidxSOhs62",
              "PrProdidxSOh64",
              "PrProdidxSOhs64",
              "PrProdidxSOh66",
              "PrProdidxSOhs66")
dfSZSO <- expand.grid(zoneOrder,orchards,stringsAsFactors = T)
head(dfSZSO)
colnames(dfSZSO) <- c("seed.zone","seed.orchard")
dfSZSO <- data.frame(lapply(dfSZSO, as.character), stringsAsFactors=FALSE)

dfMaster <- left_join(dfSZSO, df_results_summary, by=c("seed.zone","seed.orchard"), all.x=TRUE)

dfMaster$GCM <- ifelse(grepl("bc", dfMaster$scenario), 'bc - BCC-CSM1-1',
                        ifelse(grepl("he", dfMaster$scenario), 'he - HadGEM2-ES',
                               ifelse(grepl("mg", dfMaster$scenario), 'mg - MRI-Cscenario3',
                                      ifelse(grepl("mi", dfMaster$scenario), 'mi - MIROC-ESM-CHEM',
                                             ifelse(grepl("no", dfMaster$scenario), 'no - NorESM1-M',
                                                    ifelse(grepl("Ref", dfMaster$scenario), "Baseline", 'Mean all GCMs'))))))
dfMaster$RCP <- ifelse(grepl("45", dfMaster$scenario), '4.5',
                       ifelse(grepl("85", dfMaster$scenario), '8.5', 'Baseline'))
dfMaster$period <- ifelse(grepl("50", dfMaster$scenario), "2041-2060",
                          ifelse(grepl("70", dfMaster$scenario), '2061-2080', '1971-2017'))
dfMaster$seed.orchard <- substr(dfMaster$seed.orchard, 10,15)

dfMaster$GCM <- factor(dfMaster$GCM)
dfMaster$RCP <- factor(dfMaster$RCP)
dfMaster$seed.zone <- factor(dfMaster$seed.zone, ordered=T, levels = zoneOrder)
dfMaster$seed.orchard <- factor(dfMaster$seed.orchard, ordered = T, levels = c("SOh60","SOhs60","SOh62","SOhs62","SOh64","SOhs64","SOh66","SOhs66"))


# RCP4.5
df4.5 <- dfMaster %>%
  #filter(period != "1961-1990") %>% 
  filter(RCP != "8.5") %>% 
  #filter(GCM %in% c("Mean all GCMs","Baseline")==FALSE) %>% 
  group_by(seed.zone,period,seed.orchard) %>% 
  summarise(n_GCMs = n(),
            above120 = sum(mean >= 120, na.rm = T),
            above110 = sum(mean >= 110, na.rm = T),
            above100 = sum(mean >= 100, na.rm = T),
            less100 = sum(mean < 100, na.rm = T))

# make sure all seed zones & seed orchards present
df4.5 <- left_join(dfSZSO,df4.5,by=c("seed.zone"), all.x=TRUE)
df4.5$seed.orchard.x <- NULL
colnames(df4.5)[3] <- "seed.orchard"
df4.5$seed.zone <- factor(df4.5$seed.zone, ordered=T, levels = zoneOrder)
df4.5$seed.orchard <- factor(df4.5$seed.orchard, ordered = T, levels = c("SOh60","SOhs60","SOh62","SOhs62","SOh64","SOhs64","SOh66","SOhs66"))

# plot agreement above 120% prodidx
df4.5$likelihood120 <- NA
df4.5$likelihood120[which(df4.5$above120>=5)] <- "Very likely"
df4.5$likelihood120[which(df4.5$above120==4)] <- "More likely than not"
df4.5$likelihood120[which(df4.5$above120==3)] <- "More likely than not"
df4.5$likelihood120[which(df4.5$above120==2)] <- "Possible"
df4.5$likelihood120[which(df4.5$above120==1)] <- "Possible"
df4.5$likelihood120[which(df4.5$above120==0)] <- "Unlikely"


df4.5$likelihood120 <- factor(df4.5$likelihood120, ordered = T,
                             levels = c("Very likely","More likely than not","Possible","More unlikely than not","Unlikely"))

png(paste0(wd,"/figures/SO_mean_prodIdx_above_120_RCP4.5.png"), width = 600, height = 800)
df4.5 %>% filter(!is.na(df4.5$period)) %>%
  ggplot()+
  geom_tile(aes(seed.orchard,period, fill=likelihood120))+
  #scale_fill_brewer(palette = "RdYlGn", direction = -1, labels = c("Very likely","More likely than not","Possible","More unlikely than not","Unlikely"), drop=FALSE)+
  scale_fill_viridis(discrete=T, direction = -1)+
  coord_flip()+
  facet_wrap(~seed.zone, nrow = 11, ncol=2)+
  theme_bw()+
  ylab("Time period")+xlab("Seed orchard")+
  labs(fill="Likelihood")
dev.off()


# plot agreement above 110% prodidx
df4.5$likelihood110 <- NA
df4.5$likelihood110[which(df4.5$above110>=5)] <- "Very likely"
df4.5$likelihood110[which(df4.5$above110==4)] <- "More likely than not"
df4.5$likelihood110[which(df4.5$above110==3)] <- "More likely than not"
df4.5$likelihood110[which(df4.5$above110==2)] <- "Possible"
df4.5$likelihood110[which(df4.5$above110==1)] <- "Possible"
df4.5$likelihood110[which(df4.5$above110==0)] <- "Unlikely"

df4.5$likelihood110 <- factor(df4.5$likelihood110, ordered = T,
                             levels = c("Very likely","More likely than not","Possible","More unlikely than not","Unlikely"))

png(paste0(wd,"/figures/SO_mean_prodIdx_above_110_RCP4.5.png"), width = 600, height = 800)
df4.5 %>%  filter(!is.na(df4.5$period)) %>% 
  ggplot()+
  geom_tile(aes(seed.orchard,period, fill=likelihood110))+
  #scale_fill_brewer(palette = "RdYlGn", direction = -1)+
  scale_fill_viridis(discrete=T, direction = -1)+
  coord_flip()+
  facet_wrap(~seed.zone, nrow = 11, ncol=2)+
  theme_bw()+
  ylab("Time period")+xlab("Seed orchard")+
  labs(fill="Likelihood")
dev.off()

# could join back & plot spatially
df4.5$seed.zone <- factor(df4.5$seed.zone,ordered = T, levels=zoneOrder)
sfSeedZones4.5 <- left_join(sfSeedZones,df4.5,by="seed.zone")

# load country outline
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
sweden <- worldmap[worldmap$name == 'Sweden',]

png(paste0(wd,"/figures/SO_mean_prodIdx_above_120_RCP4.5_spatial.png"), width = 800, height = 1000)
ggplot()+
  geom_sf(data = sweden, fill=NA)+
  geom_sf(data=sfSeedZones4.5 %>% filter(!is.na(period) & !is.na(seed.orchard)), aes(fill=likelihood120), colour=0)+
  #scale_fill_brewer(palette = "RdYlGn", direction = -1)+
  scale_fill_viridis(discrete=T, direction = -1)+
  facet_grid(seed.orchard~period)+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  labs(fill = "Likelihood production index > 120%")
dev.off()

png(paste0(wd,"/figures/SO_mean_prodIdx_above_110_RCP4.5_spatial.png"), width = 800, height = 1000)
ggplot()+
  geom_sf(data = sweden, fill=NA)+
  geom_sf(data=sfSeedZones4.5 %>% filter(!is.na(period) & !is.na(seed.orchard)), aes(fill=likelihood110), colour=0)+
  #scale_fill_brewer(palette = "RdYlGn", direction = -1)+
  scale_fill_viridis(discrete=T, direction = -1)+
  facet_grid(seed.orchard~period)+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  labs(fill = "Likelihood production index > 110%")
dev.off()


# RCP8.5
df8.5 <- dfMaster %>%
  #filter(period != "1961-1990") %>% 
  filter(RCP != "4.5") %>% 
  #filter(GCM %in% c("Mean all GCMs","Baseline")==FALSE) %>% 
  group_by(seed.zone,period,seed.orchard) %>% 
  summarise(n_GCMs = n(),
            above120 = sum(mean >= 120),
            above110 = sum(mean >= 110),
            above100 = sum(mean >= 100),
            less100 = sum(mean < 100))

# make sure all seed zones & seed orchards present
df8.5 <- left_join(dfSZSO,df8.5,by=c("seed.zone"), all.x=TRUE)
df8.5$seed.orchard.x <- NULL
colnames(df8.5)[3] <- "seed.orchard"
df8.5$seed.zone <- factor(df8.5$seed.zone, ordered=T, levels = zoneOrder)
df8.5$seed.orchard <- factor(df8.5$seed.orchard, ordered = T, levels = c("SOh60","SOhs60","SOh62","SOhs62","SOh64","SOhs64","SOh66","SOhs66"))

# plot agreement above 120% prodidx
df8.5$likelihood120 <- NA
df8.5$likelihood120[which(df8.5$above120>=5)] <- "Very likely"
df8.5$likelihood120[which(df8.5$above120==4)] <- "More likely than not"
df8.5$likelihood120[which(df8.5$above120==3)] <- "More likely than not"
df8.5$likelihood120[which(df8.5$above120==2)] <- "Possible"
df8.5$likelihood120[which(df8.5$above120==1)] <- "Possible"
df8.5$likelihood120[which(df8.5$above120==0)] <- "Unlikely"


df8.5$likelihood120 <- factor(df8.5$likelihood120, ordered = T,
                              levels = c("Very likely","More likely than not","Possible","More unlikely than not","Unlikely"))

png(paste0(wd,"/figures/SO_mean_prodIdx_above_120_RCP8.5.png"), width = 600, height = 800)
df8.5 %>% filter(!is.na(df8.5$period)) %>%
  ggplot()+
  geom_tile(aes(seed.orchard,period, fill=likelihood120))+
  #scale_fill_brewer(palette = "RdYlGn", direction = -1, labels = c("Very likely","More likely than not","Possible","More unlikely than not","Unlikely"), drop=FALSE)+
  scale_fill_viridis(discrete=T, direction = -1)+
  coord_flip()+
  facet_wrap(~seed.zone, nrow = 11, ncol=2)+
  theme_bw()+
  ylab("Time period")+xlab("Seed orchard")+
  labs(fill="Likelihood")
dev.off()


# plot agreement above 110% prodidx
df8.5$likelihood110 <- NA
df8.5$likelihood110[which(df8.5$above110>=5)] <- "Very likely"
df8.5$likelihood110[which(df8.5$above110==4)] <- "More likely than not"
df8.5$likelihood110[which(df8.5$above110==3)] <- "More likely than not"
df8.5$likelihood110[which(df8.5$above110==2)] <- "Possible"
df8.5$likelihood110[which(df8.5$above110==1)] <- "Possible"
df8.5$likelihood110[which(df8.5$above110==0)] <- "Unlikely"

df8.5$likelihood110 <- factor(df8.5$likelihood110, ordered = T,
                              levels = c("Very likely","More likely than not","Possible","More unlikely than not","Unlikely"))

png(paste0(wd,"/figures/SO_mean_prodIdx_above_110_RCP8.5.png"), width = 600, height = 800)
df8.5 %>%  filter(!is.na(df8.5$period)) %>% 
  ggplot()+
  geom_tile(aes(seed.orchard,period, fill=likelihood110))+
  #scale_fill_brewer(palette = "RdYlGn", direction = -1)+
  scale_fill_viridis(discrete=T, direction = -1)+
  coord_flip()+
  facet_wrap(~seed.zone, nrow = 11, ncol=2)+
  theme_bw()+
  ylab("Time period")+xlab("Seed orchard")+
  labs(fill="Likelihood")
dev.off()

# could join back & plot spatially
df8.5$seed.zone <- factor(df8.5$seed.zone,ordered = T, levels=zoneOrder)
sfSeedZones8.5 <- left_join(sfSeedZones,df8.5,by="seed.zone")

# load country outline
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
sweden <- worldmap[worldmap$name == 'Sweden',]

png(paste0(wd,"/figures/SO_mean_prodIdx_above_120_RCP8.5_spatial.png"), width = 800, height = 1000)
ggplot()+
  geom_sf(data = sweden, fill=NA)+
  geom_sf(data=sfSeedZones8.5 %>% filter(!is.na(period) & !is.na(seed.orchard)), aes(fill=likelihood120), colour=0)+
  #scale_fill_brewer(palette = "RdYlGn", direction = -1)+
  scale_fill_viridis(discrete=T, direction = -1)+
  facet_grid(seed.orchard~period)+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  labs(fill = "Likelihood production index > 120%")
dev.off()

png(paste0(wd,"/figures/SO_mean_prodIdx_above_110_RCP8.5_spatial.png"), width = 800, height = 1000)
ggplot()+
  geom_sf(data = sweden, fill=NA)+
  geom_sf(data=sfSeedZones8.5 %>% filter(!is.na(period) & !is.na(seed.orchard)), aes(fill=likelihood110), colour=0)+
  #scale_fill_brewer(palette = "RdYlGn", direction = -1)+
  scale_fill_viridis(discrete=T, direction = -1)+
  facet_grid(seed.orchard~period)+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  labs(fill = "Likelihood production index > 110%")
dev.off()

# combined version
df8.5$uncertainty <- NA
df8.5$uncertainty[which(df8.5$less100>=5)] <- "Below local (very likely)"
df8.5$uncertainty[which(df8.5$less100==4)] <- "Below local (more likely than not)"
df8.5$uncertainty[which(df8.5$less100==3)] <- "Below local (more likely than not)"
df8.5$uncertainty[which(df8.5$less100==2)] <- "Below local (possible)"
df8.5$uncertainty[which(df8.5$less100==1)] <- "Below local (possible)"
df8.5$uncertainty[which(df8.5$less100==0)] <- "Below local (possible)"

df8.5$uncertainty[which(df8.5$above100>=5 & df8.5$less100<3)] <- "Above local (very likely)"
df8.5$uncertainty[which(df8.5$above100==4 & df8.5$less100<3)] <- "Above local (more likely than not)"
df8.5$uncertainty[which(df8.5$above100==3 & df8.5$less100<3)] <- "Above local (more likely than not)"
df8.5$uncertainty[which(df8.5$above100==2 & df8.5$less100<3)] <- "Above local (possible)"
df8.5$uncertainty[which(df8.5$above100==1 & df8.5$less100<3)] <- "Above local (possible)"
df8.5$uncertainty[which(df8.5$above100==0 & df8.5$less100<3)] <- "Above local (possible)"

df8.5$uncertainty[which(df8.5$above110>=5)] <- "Above 110% (very likely)"
df8.5$uncertainty[which(df8.5$above110==4)] <- "Above 110% (more likely than not)"
df8.5$uncertainty[which(df8.5$above110==3)] <- "Above 110% (more likely than not)"
#df8.5$uncertainty[which(df8.5$above110==2)] <- "Above 110% (possible)"
#df8.5$uncertainty[which(df8.5$above110==1)] <- "Above 110% (possible)"
#df8.5$uncertainty[which(df8.5$above110==0)] <- "Above 110% (possible)"

df8.5$uncertainty[which(df8.5$above120>=5)] <- "Above 120% (very likely)"
df8.5$uncertainty[which(df8.5$above120==4)] <- "Above 120% (more likely than not)"
df8.5$uncertainty[which(df8.5$above120==3)] <- "Above 120% (more likely than not)"
#df8.5$uncertainty[which(df8.5$above120==2)] <- "Above 120% (possible)"
#df8.5$uncertainty[which(df8.5$above120==1)] <- "Above 120% (possible)"
#df8.5$uncertainty[which(df8.5$above120==0)] <- "Above 120% (possible)"

df8.5$uncertainty[which(is.na(df8.5$uncertainty))] <- "Beyond model thresholds"

df8.5$uncertainty <- factor(df8.5$uncertainty, ordered = T,
                              levels = c("Above 120% (very likely)",
                                         "Above 120% (more likely than not)",
                                         "Above 110% (very likely)",
                                         "Above 110% (more likely than not)",
                                         "Above local (very likely)",
                                         "Above local (possible)",
                                         "Below local (possible)",
                                         "Below local (more likely than not)",
                                         "Below local (very likely)",
                                         "Beyond model thresholds"))

df8.5 %>%  filter(!is.na(df8.5$period)) %>% 
  ggplot()+
  geom_tile(aes(seed.orchard,period, fill=uncertainty))+
  #scale_fill_brewer(palette = "RdYlGn", direction = -1)+
  scale_fill_viridis(discrete=T, direction = -1)+
  coord_flip()+
  facet_wrap(~seed.zone, nrow = 11, ncol=2)+
  theme_bw()+
  ylab("Time period")+xlab("Seed orchard")+
  labs(fill="Likelihood")






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
