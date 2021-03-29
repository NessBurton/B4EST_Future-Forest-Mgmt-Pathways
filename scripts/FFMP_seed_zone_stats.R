
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
# remove ensemble mean and reference
files <- files[-c(9:12,25)]

#df_results_lng <- tibble()
df_results_summary <- tibble()
scenario_list <- c()

for (f in files){
  
  #f <- files[1]
  
  scenario <- strsplit(f, "[_]")[[1]][1]
  scenario <- strsplit(scenario, "[/]")[[1]][8]
  GCM <- substr(scenario,1,6)
  
  scenario_list[[length(scenario_list) + 1]] <- scenario
  
  print("Read in data and apply thresholds")
  dfP <- vroom(f)
  
  # apply thresholds (survival, latitudinal transfer, and GDD5)
  
  # for survival, threshold for 2050 should use baseline period survival
  if (grepl("50", scenario)==TRUE){
    
    print(paste0("Reading in reference climate file for survival thresholds"))
    
    # read in reference file
    dfRef <- vroom(paste0(dirData, "Productionpredictions/Refclimate_SO1.5g_predictions.csv"))
    names(dfRef)
    dfP$refSurvivalSOh60 <- dfRef$PrSurvSOh60
    dfP$refSurvivalSOh62 <- dfRef$PrSurvSOh62
    dfP$refSurvivalSOh64 <- dfRef$PrSurvSOh64
    dfP$refSurvivalSOh66 <- dfRef$PrSurvSOh66
    dfP$refSurvivalSOhs60 <- dfRef$PrSurvSOhs60
    dfP$refSurvivalSOhs62 <- dfRef$PrSurvSOhs62
    dfP$refSurvivalSOhs64 <- dfRef$PrSurvSOhs64
    dfP$refSurvivalSOhs66 <- dfRef$PrSurvSOhs66
    
    dfP$PrProdidxSOh60[which(dfP$refSurvivalSOh60 <0.5)] <- NA
    dfP$PrProdidxSOh62[which(dfP$refSurvivalSOh62 <0.5)] <- NA
    dfP$PrProdidxSOh64[which(dfP$refSurvivalSOh64 <0.5)] <- NA
    dfP$PrProdidxSOh66[which(dfP$refSurvivalSOh66 <0.5)] <- NA
    dfP$PrProdidxSOhs60[which(dfP$refSurvivalSOhs60 <0.5)] <- NA
    dfP$PrProdidxSOhs62[which(dfP$refSurvivalSOhs62 <0.5)] <- NA
    dfP$PrProdidxSOhs64[which(dfP$refSurvivalSOhs64 <0.5)] <- NA
    dfP$PrProdidxSOhs66[which(dfP$refSurvivalSOhs66 <0.5)] <- NA
    
    # thresholds for 2070 should use 2050 survival
    }else{
      
      print(paste0("Reading in 2050 file for survival thresholds"))
      
      # read in 2050 file
      df2050 <- vroom(paste0(dirData, "Productionpredictions/",GCM,"50_SO1.5g_predictions.csv"))
      names(df2050)
      dfP$t50SurvivalSOh60 <- df2050$PrSurvSOh60
      dfP$t50SurvivalSOh62 <- df2050$PrSurvSOh62
      dfP$t50SurvivalSOh64 <- df2050$PrSurvSOh64
      dfP$t50SurvivalSOh66 <- df2050$PrSurvSOh66
      dfP$t50SurvivalSOhs60 <- df2050$PrSurvSOhs60
      dfP$t50SurvivalSOhs62 <- df2050$PrSurvSOhs62
      dfP$t50SurvivalSOhs64 <- df2050$PrSurvSOhs64
      dfP$t50SurvivalSOhs66 <- df2050$PrSurvSOhs66
      
      dfP$PrProdidxSOh60[which(dfP$t50SurvivalSOh60 <0.5)] <- NA
      dfP$PrProdidxSOh62[which(dfP$t50SurvivalSOh62 <0.5)] <- NA
      dfP$PrProdidxSOh64[which(dfP$t50SurvivalSOh64 <0.5)] <- NA
      dfP$PrProdidxSOh66[which(dfP$t50SurvivalSOh66 <0.5)] <- NA
      dfP$PrProdidxSOhs60[which(dfP$t50SurvivalSOhs60 <0.5)] <- NA
      dfP$PrProdidxSOhs62[which(dfP$t50SurvivalSOhs62 <0.5)] <- NA
      dfP$PrProdidxSOhs64[which(dfP$t50SurvivalSOhs64 <0.5)] <- NA
      dfP$PrProdidxSOhs66[which(dfP$t50SurvivalSOhs66 <0.5)] <- NA
      
    }
  
  
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
  
  if (f == files[20]){
    write.csv(df_results_summary, paste0(dirOut, "PrProdIdx_seed_zone_summaries_Sweden_GDD5thresh3.csv"))
  }
  
}


### read in summaries ----------------------------------------------------------

#df_results_summary <- vroom(paste0(dirOut, "PrProdIdx_seed_zone_summaries_Sweden_GDD5thresh3.csv"))
head(df_results_summary)
summary(df_results_summary)
#colnames(df_results_summary) <- c("row","seed.zone","seed.orchard","mean","sd","IQR","min","max","scenario")
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
dfMaster$seed.orchard <- ifelse(grepl("SOh60", dfMaster$seed.orchard), 'SO 1.5g 60°N', 
                                           ifelse(grepl("SOhs60", dfMaster$seed.orchard), 'SO 1.5gS 60°N',
                                                  ifelse(grepl("SOh62", dfMaster$seed.orchard), 'SO 1.5g 62°N',
                                                         ifelse(grepl("SOhs62", dfMaster$seed.orchard), 'SO 1.5gS 62°N',
                                                                ifelse(grepl("SOh64", dfMaster$seed.orchard), 'SO 1.5g 64°N',
                                                                       ifelse(grepl("SOhs64", dfMaster$seed.orchard), 'SO 1.5gS 64°N',
                                                                              ifelse(grepl("SOh66", dfMaster$seed.orchard), 'SO 1.5g 66°N',
                                                                                     ifelse(grepl("SOhs66", dfMaster$seed.orchard), 'SO 1.5gS 66°N', NA))))))))

dfMaster$GCM <- factor(dfMaster$GCM)
dfMaster$RCP <- factor(dfMaster$RCP)
dfMaster$seed.zone <- factor(dfMaster$seed.zone, ordered=T, levels = zoneOrder)
dfMaster$seed.orchard <- factor(dfMaster$seed.orchard, ordered = T, levels = c('SO 1.5g 60°N',
                                                                               'SO 1.5gS 60°N',
                                                                               'SO 1.5g 62°N',
                                                                               'SO 1.5gS 62°N',
                                                                               'SO 1.5g 64°N',
                                                                               'SO 1.5gS 64°N',
                                                                               'SO 1.5g 66°N',
                                                                               'SO 1.5gS 66°N'))
# reference

dfRef <- vroom(paste0(dirData, "Productionpredictions/Refclimate_SO1.5g_predictions.csv"))
names(dfRef)

coordinates(dfRef)<- ~ CenterLong + CenterLat
# set crs - assume lat long
proj4string(dfRef) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
# transform to utm
dfRef <- spTransform(dfRef, CRSobj = utm )
dfRef_sf <- st_as_sf(dfRef)
rm(dfRef)

# spatial join
dfRef_sf <- st_join(dfRef_sf,sfSeedZones)
dfRef_sf$seed.zone <- factor(dfRef_sf$seed.zone, ordered = T, levels = zoneOrder)

dfRef <- dfRef_sf[,c("PrProdidxSOh60","PrProdidxSOh62","PrProdidxSOh64","PrProdidxSOh66",
                             "PrProdidxSOhs60","PrProdidxSOhs62","PrProdidxSOhs64","PrProdidxSOhs66",
                             "seed.zone")] %>% 
  filter(!is.na(seed.zone)) %>% # filter to just zones
  pivot_longer(1:8, names_to="seed.orchard",values_to="prod_idx") %>% 
  #st_drop_geometry() %>% 
  group_by(seed.zone,seed.orchard) %>% 
  summarise(refMean = mean(prod_idx))

# RCP4.5
df4.5 <- dfMaster %>%
  #filter(period != "1971-2017") %>% 
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
df4.5$seed.orchard <- factor(df4.5$seed.orchard, ordered = T, levels = c('SO 1.5g 60°N',
                                                                         'SO 1.5gS 60°N',
                                                                         'SO 1.5g 62°N',
                                                                         'SO 1.5gS 62°N',
                                                                         'SO 1.5g 64°N',
                                                                         'SO 1.5gS 64°N',
                                                                         'SO 1.5g 66°N',
                                                                         'SO 1.5gS 66°N'))

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

png(paste0(wd,"/figures/SO_mean_prodIdx_above_120_RCP4.5.png"), width = 600, height = 850)
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

png(paste0(wd,"/figures/SO_mean_prodIdx_above_110_RCP4.5.png"), width = 600, height = 850)
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
df8.5$seed.orchard <- factor(df8.5$seed.orchard, ordered = T, levels = c('SO 1.5g 60°N',
                                                                         'SO 1.5gS 60°N',
                                                                         'SO 1.5g 62°N',
                                                                         'SO 1.5gS 62°N',
                                                                         'SO 1.5g 64°N',
                                                                         'SO 1.5gS 64°N',
                                                                         'SO 1.5g 66°N',
                                                                         'SO 1.5gS 66°N'))

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

png(paste0(wd,"/figures/SO_mean_prodIdx_above_120_RCP8.5.png"), width = 600, height = 850)
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

png(paste0(wd,"/figures/SO_mean_prodIdx_above_110_RCP8.5.png"), width = 600, height = 850)
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


### combined FFMP version ------------------------------------------------------

dfFFMP <- dfMaster %>%
  group_by(RCP,period,seed.zone,seed.orchard) %>% 
  summarise(n_GCMs = n(),
            above120 = sum(mean >= 120),
            above110 = sum(mean >= 110),
            above100 = sum(mean >= 100),
            less100 = sum(mean < 100))

dfFFMP$pathway <- NA
dfFFMP$pathway[which(dfFFMP$less100>=5)] <- "Expiry (below local)"
dfFFMP$pathway[which(dfFFMP$less100==4)] <- "Expiry (below local)"
dfFFMP$pathway[which(dfFFMP$less100==3)] <- "Expiry (below local)"
dfFFMP$pathway[which(dfFFMP$less100==2)] <- "Poor performance (below local)"
dfFFMP$pathway[which(dfFFMP$less100==1)] <- "Poor performance (below local)"
dfFFMP$pathway[which(dfFFMP$less100==0)] <- "Poor performance (below local)"

dfFFMP$pathway[which(dfFFMP$above100>=5 & dfFFMP$less100<3)] <- "Good performance (above local)"
dfFFMP$pathway[which(dfFFMP$above100==4 & dfFFMP$less100<3)] <- "Good performance (above local)"
dfFFMP$pathway[which(dfFFMP$above100==3 & dfFFMP$less100<3)] <- "Good performance (above local)"
#dfFFMP$pathway[which(dfFFMP$above100==2 & dfFFMP$less100<3)] <- "Moderate performance (above local)"
#dfFFMP$pathway[which(dfFFMP$above100==1 & dfFFMP$less100<3)] <- "Moderate performance (above local)"
#dfFFMP$pathway[which(dfFFMP$above100==0 & dfFFMP$less100<3)] <- "Moderate performance (above local)"

dfFFMP$pathway[which(dfFFMP$above110>=5)] <- "Very good performance (above 110)"
dfFFMP$pathway[which(dfFFMP$above110==4)] <- "Very good performance (above 110)"
dfFFMP$pathway[which(dfFFMP$above110==3)] <- "Very good performance (above 110)"
#dfFFMP$pathway[which(dfFFMP$above110==2)] <- "Above 110% (possible)"
#dfFFMP$pathway[which(dfFFMP$above110==1)] <- "Above 110% (possible)"
#dfFFMP$pathway[which(dfFFMP$above110==0)] <- "Above 110% (possible)"

dfFFMP$pathway[which(dfFFMP$above120>=5)] <- "Excellent performance (above 120)"
dfFFMP$pathway[which(dfFFMP$above120==4)] <- "Excellent performance (above 120)"
dfFFMP$pathway[which(dfFFMP$above120==3)] <- "Excellent performance (above 120)"
#dfFFMP$pathway[which(dfFFMP$above120==2)] <- "Above 120% (possible)"
#dfFFMP$pathway[which(dfFFMP$above120==1)] <- "Above 120% (possible)"
#dfFFMP$pathway[which(dfFFMP$above120==0)] <- "Above 120% (possible)"

dfFFMP$pathway[which(is.na(dfFFMP$pathway))] <- "Beyond model thresholds"

dfFFMP$pathway <- factor(dfFFMP$pathway, ordered = T,
                              levels = c("Excellent performance (above 120)",
                                         "Very good performance (above 110)",
                                         "Good performance (above local)",
                                         "Moderate performance (above local)",
                                         "Poor performance (below local)",
                                         "Expiry (below local)",
                                         "Beyond model thresholds"))

png(paste0(wd,"/figures/SO_FFMP_RCP4.5.png"), width = 600, height = 850)
dfFFMP %>% 
  filter(RCP == "4.5" | RCP == "Baseline") %>% 
  ggplot()+
  geom_tile(aes(seed.orchard,period, fill=pathway))+
  #scale_fill_viridis(discrete=T, direction = -1)+
  scale_fill_brewer(palette = "RdYlGn", direction=-1)+
  coord_flip()+
  facet_wrap(~seed.zone, ncol = 2)+
  theme_bw()+
  ylab("Time period")+xlab("Seed orchard")+
  labs(fill="Seed orchard production index")
dev.off()

png(paste0(wd,"/figures/SO_FFMP_RCP8.5.png"), width = 600, height = 850)
dfFFMP %>% 
  filter(RCP == "8.5" | RCP == "Baseline") %>% 
  ggplot()+
  geom_tile(aes(seed.orchard,period, fill=pathway))+
  #scale_fill_viridis(discrete=T, direction = -1)+
  scale_fill_brewer(palette = "RdYlGn", direction=-1)+
  coord_flip()+
  facet_wrap(~seed.zone, ncol = 2)+
  theme_bw()+
  ylab("Time period")+xlab("Seed orchard")+
  labs(fill="Seed orchard production index")
dev.off()

# could join back & plot spatially
sfFFMPs <- left_join(sfSeedZones,dfFFMP,by="seed.zone")

# load country outline
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
sweden <- worldmap[worldmap$name == 'Sweden',]

png(paste0(wd,"/figures/SO_FFMP_RCP4.5_spatial.png"), width = 800, height = 1000)
ggplot()+
  geom_sf(data = sweden, fill=NA)+
  geom_sf(data = sfFFMPs %>% filter(RCP == "4.5" | RCP == "Baseline"), aes(fill=pathway), colour=0)+
  scale_fill_brewer(palette = "RdYlGn", direction = -1)+
  #scale_fill_viridis(discrete=T, direction = -1)+
  facet_grid(seed.orchard~period)+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  labs(fill = "Seed orchard production index")
dev.off()

png(paste0(wd,"/figures/SO_FFMP_RCP8.5_spatial.png"), width = 800, height = 1000)
ggplot()+
  geom_sf(data = sweden, fill=NA)+
  geom_sf(data = sfFFMPs %>% filter(RCP == "8.5" | RCP == "Baseline"), aes(fill=pathway), colour=0)+
  scale_fill_brewer(palette = "RdYlGn", direction = -1)+
  #scale_fill_viridis(discrete=T, direction = -1)+
  facet_grid(seed.orchard~period)+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  labs(fill = "Seed orchard production index")
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
