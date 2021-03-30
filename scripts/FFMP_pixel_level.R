
# date: 30/03/21
# author: VB
# description: script to develop Future Forest Mangement Pathways (FFMPs) using data provided by Skogforsk.

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


### plan -----------------------------------------------------------------------

# per file
# apply thresholds (NA if beyond)
# then new vars, above120, above110, above100, below100
# if pred for each location meets any of these, then assign 1 in the new var
# group by RCP, the new vars across GCMs (should then get agreement)
# can then use this to assign pathway classification - rasterise and plot this
# repeat for reference period separately


### sweden outline -------------------------------------------------------------

# load country outline
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
sweden <- worldmap[worldmap$name == 'Sweden',]

# seed zones file for utm crs
sfSeedZones <- st_read(paste0(dirData,"Seed_zones_SP_Sweden/Shaper/Frözoner_tall_Sverige.shp"))
utm <- crs(sfSeedZones)



### read in each file, remove data > thresholds, & get pathway -----------------

# list production prediction files per scenario
files <-  list.files(paste0(dirData, "Productionpredictions/"),pattern = "*.csv",full.names = T)
files
# remove ensemble mean and reference
files <- files[-c(9:12,25)]

scenario_list <- c()

for (f in files){
  
  f <- files[1]
  
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
  
  # lat transfer
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
  
  # new var - pathway
  
  
  # spatial
  coordinates(dfRef)<- ~ CenterLong + CenterLat
  # set crs - assume lat long
  proj4string(dfRef) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
  # transform to utm
  dfRef <- spTransform(dfRef, CRSobj = utm )
  dfRef_sf <- st_as_sf(dfRef)
  
  }
    
    