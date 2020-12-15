
# date: 14/12/20
# author: VB
# description: script to test the development of Future Forest Mangement Pathways (FFMPs) using
# data provided by Skogforsk.

wd <- "~/R/FFMPs" # laptop
wd <- "~/FFMPs" # sandbox
dirData <- paste0(wd,"/data-raw/")
dirOut <- paste0(wd,"/data-processed/")

### libraries ------------------------------------------------------------------

library(tidyverse)
library(sf)
library(raster)
library(ggplot2)
library(RColorBrewer)

### reference data -------------------------------------------------------------

# refclimate
df_ref <- read.csv(paste0(dirData,"Productionpredictions/Refclimate_SO1.5g_predictions.csv"))
# create a spatialpoints dataframe
sp_ref <- df_ref
coordinates(sp_ref) <- ~ CenterLong + CenterLat
# set crs - assume lat long
proj4string(sp_ref) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# empty reference raster of correct extent and resolution
rst <- raster(crs = crs(sp_ref), resolution = c(0.1,0.1), ext = extent(sp_ref))
res(rst)

### seed zones -----------------------------------------------------------------

# seed zones for Sweden sent by Mats
# Alt 0246 for umlaut over o (if Num Lock available) - otherwise copy paste
shpSZ <- st_read(paste0(dirData,"Seed_zones_SP_Sweden/Shaper/Frözoner_tall_Sverige.shp"))

head(shpSZ)
crs(shpSZ)

ggplot(shpSZ)+
  geom_sf(aes(fill=ZON2))

# filter to 6 northern seed zones and simplify 
unique(shpSZ$ZON2)
# zones to focus on
zones <- c("1a","1b","1c","2","3","7")

# work with just 6 northernmost zones
shpSZ <- shpSZ %>% 
  filter(ZON2 %in% zones == T)

# dissolve/merge zones by ZON2 to simplify
shpSZ_sf <- st_as_sf(shpSZ)
shpSZ_sf$area <- st_area(shpSZ_sf) # add area to have a variable to be able to summarise
shpSZ_sf <-
  shpSZ_sf %>%
  group_by(ZON2) %>% 
  summarise(area = sum(area))
plot(shpSZ_sf[1])
shpSZ_sp <- as_Spatial(shpSZ_sf)

rstElev <- rasterize(sp_ref, rst, sp_ref$GridAlt, fun=max)
crs(rstElev)
plot(rstElev)
# now transform to utm, reprojection involves interpolation - default is bilinear
utm <- crs(shpSZ)
rstElev <- projectRaster(rstElev, crs = utm, res = 1000)
plot(rstElev)
dfElev <- extract(rstElev, shpSZ_sp, fun=max, df=TRUE, na.rm=TRUE)
dfElev$Zone <- shpSZ_sp$ZON2

ggplot(shpSZ_sf)+
  geom_sf(aes(fill=ZON2))

dfElev$elev <- rep(c("Upland","Lowland"),3)
dfElev$location <- c("67°N","67°N","66°N","66°N","64°N","64°N")          
dfElev$desc <- paste0(dfElev$location,"-",dfElev$elev)
dfElev$desc <- factor(dfElev$desc, levels=c("67°N-Upland","67°N-Lowland","66°N-Upland","66°N-Lowland","64°N-Upland", "64°N-Lowland"))

colnames(shpSZ_sf)[1] <- "Zone"
shpSZ_sf <- left_join(shpSZ_sf,dfElev,by="Zone")
ggplot(shpSZ_sf)+
  geom_sf(aes(fill=desc))

### read in Scots pine predictions and join to zones ---------------------------

# need to convert reference data to correct format for use in loop
# (will use to calculate percentage change from current predictions)
# transform to utm
sp_ref <- spTransform(sp_ref, CRSobj = utm )
# convert to sf
sf_ref <- st_as_sf(sp_ref)
rm(sp_ref)
# spatial join
sf_ref <- st_join(sf_ref,shpSZ_sf)
head(sf_ref)
sf_ref$Zone <- factor(sf_ref$Zone)
sf_ref$desc <- factor(sf_ref$desc)
summary(sf_ref$Zone)

sf_ref <- sf_ref[,c(15:18,25)] %>% 
  filter(!is.na(desc)) %>% # filter to just zones
  st_drop_geometry() %>%
  pivot_longer(1:4, names_to="seed_orchard",values_to="prod_idx")
colnames(sf_ref)[3] <- "ref_prod_idx"
rm(df_ref)

# list production prediction files per scenario
files <-  list.files(paste0(dirData, "Productionpredictions/"),pattern = "*.csv",full.names = T)
# just RCP8.5 to test + current to compare
#files <- grep("85in50", files, value=TRUE)
files <- grep("45in50", files, value=TRUE)
files

utm <- crs(shpSZ)

df_results_lng <- data_frame()
df_results_summary <- data_frame()
scenario_list <- c()

for (f in files){
  
  #f <- rcp85_files[1]
  
  scenario <- substring(f,75,82)
  scenario_list[[length(scenario_list) + 1]] <- scenario
  
  print("Read in data, convert to spatial, transform to utm")
  dfP <- read.csv(f)
  coordinates(dfP)<- ~ CenterLong + CenterLat
  # set crs - assume lat long
  proj4string(dfP) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
  # transform to utm
  dfP <- spTransform(dfP, CRSobj = utm )
  dfP_sf <- st_as_sf(dfP)
  rm(dfP)
  
  print("Spatial join to seed zones")
  # spatial join
  dfP_sf <- st_join(dfP_sf,shpSZ_sf)
  #head(dfP_sf)
  dfP_sf$desc <- factor(dfP_sf$desc)
  #summary(dfP_sf$ZON2)
  
  print("Transform to long format")
  df_long <- dfP_sf[,c(17:20,27)] %>% 
    filter(!is.na(desc)) %>% # filter to just zones
    st_drop_geometry() %>%
    pivot_longer(1:4, names_to="seed_orchard",values_to="prod_idx")
  
  print("Add reference production")
  # add ref production
  df_long <- cbind(df_long,sf_ref$ref_prod_idx)
  colnames(df_long)[4] <- "ref_prod"
  
  print("Calculate percentage change")
  df_long <- df_long %>% 
    mutate(perc_change = (prod_idx - ref_prod)/ref_prod * 100)
  df_long$scenario <- scenario
  
  print("Add to results table")
  df_results_lng <- rbind(df_results_lng, df_long)
  
  print("Calculate summaries")
  df_summary <- df_long %>% 
    group_by(desc, seed_orchard) %>% # group by zone
    summarise_if(is.numeric,c("mean","sd","IQR","min","max")) #%>% 
  #summarise(c("PrProdidxSOh60","PrProdidxSOh62","PrProdidxSOh64","PrProdidxSOh66"),.funs=c("mean","sd","IQR","min","max"))
  df_summary$scenario <- scenario
  
  print("Add to results table")
  df_results_summary <- rbind(df_results_summary, df_summary)
  
  print(paste0("Processed scenario: ",scenario))
  
}

head(df_results_lng)
df_results_lng$change <- NA
df_results_lng$change[which(df_results_lng$perc_change<0)]<-"decline"
df_results_lng$change[which(df_results_lng$perc_change>0)] <- "increase"

df_results_lng$seed.orchard <- substring(df_results_lng$seed_orchard,10,14)

head(df_results_lng)

#write.csv(df_results_lng,paste0(dirOut,"prProdIdx_rcp45_long.csv"), row.names = F)
#write.csv(df_results_summary,paste0(dirOut,"prProdIdx_rcp45_summary.csv"), row.names = F)
#write.csv(df_results_lng,paste0(dirOut,"prProdIdx_rcp85_long.csv"), row.names = F)
#write.csv(df_results_summary,paste0(dirOut,"prProdIdx_rcp85_summary.csv"), row.names = F)

#brewer.pal(n = 8, name = "Dark2")
pal <- c("#D95F02","#1B9E77")
#png(paste0(wd,"/figures/ProdIdx_change_frm_baseline_RCP85_perGCM.png"), width = 900, height = 800)
png(paste0(wd,"/figures/ProdIdx_change_frm_baseline_RCP45_perGCM.png"), width = 900, height = 800)
df_results_lng %>% 
  filter(scenario != "MEAN45in") %>% 
  ggplot()+
  geom_boxplot(aes(seed.orchard,perc_change,col=change))+coord_flip()+
  #scale_color_brewer(palette = "Dark2")+
  scale_color_manual(values=pal)+  
  facet_grid(desc~scenario)+
  xlab("Seed orchard choice")+
  ylab("Change in production index from baseline (%)")+
  labs(col = "Change in production from baseline")+ 
  theme(legend.position="top")+
  ylim(c(-10,100))
dev.off()

#png(paste0(wd,"/figures/ProdIdx_change_frm_baseline_RCP85.png"), width = 900, height = 800)
png(paste0(wd,"/figures/ProdIdx_change_frm_baseline_RCP45.png"), width = 900, height = 800)
df_results_lng %>% 
  filter(scenario != "MEAN45in") %>% 
  ggplot()+
  geom_boxplot(aes(seed.orchard,perc_change,col=change))+coord_flip()+
  scale_color_manual(values = pal)+
  facet_grid(rows="desc")+
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
    group_by(scenario,desc, seed.orchard)%>%
    summarise(mnPrIdx = mean(na.omit(perc_change)),
              sePrIdx = sd(na.omit(perc_change)/sqrt(n())),
              upr = mnPrIdx + 1.96 * sePrIdx,
              lwr = mnPrIdx - 1.96 * sePrIdx) %>% 
  ggplot(aes(seed.orchard, mnPrIdx), colour = seed.orchard)+
  coord_flip()+
  #geom_errorbar(limits)+
  geom_point()+
  theme(panel.grid = element_blank())+
  facet_grid(desc~scenario, scales = "free")
