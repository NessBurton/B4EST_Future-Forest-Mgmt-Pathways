
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

### reference data -------------------------------------------------------------

# refclimate
df_ref <- read.csv(paste0(dirData,"Productionpredictions/Refclimate_SO1.5g_predictions.csv"))
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
files <- grep("85in50", files, value=TRUE)
#files <- grep("45in50", files, value=TRUE)
files

df_results_lng <- tibble()
df_results_summary <- tibble()
scenario_list <- c()

for (f in files){
  
  f <- files[1]
  
  scenario <- strsplit(f, "[_]")[[1]][1]
  scenario <- strsplit(scenario, "[/]")[[1]][8]
  
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
  
  #head(dfP_sf)
  
  print("Transform to long format")
  df_long <- dfP_sf[,c("PrProdidxSOh60","PrProdidxSOh62","PrProdidxSOh64","PrProdidxSOh66","desc")] %>% 
    filter(!is.na(desc)) %>% # filter to just zones
    st_drop_geometry() %>%
    pivot_longer(1:4, names_to="seed_orchard",values_to="prod_idx")
  df_long$prod_idx <- df_long$prod_idx * 100
  
  print("Add reference production")
  # add ref production
  df_long <- cbind(df_long,sfReference$ref_prod_idx)
  colnames(df_long)[4] <- "ref_prod"
  
  print("Calculate change")
  df_long <- df_long %>% 
    mutate(perc.change = prod_idx - ref_prod,
           #perc_change = (prod_idx - ref_prod)/ref_prod * 100)
    )
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
df_results_lng$change[which(df_results_lng$perc.change<0)]<-"decline"
df_results_lng$change[which(df_results_lng$perc.change>0)] <- "increase"
unique(df_results_lng$change)
df_results_lng$seed.orchard <- substring(df_results_lng$seed_orchard,10,14)

head(df_results_lng)

#write.csv(df_results_lng,paste0(dirOut,"prProdIdx_rcp45_long.csv"), row.names = F)
#write.csv(df_results_summary,paste0(dirOut,"prProdIdx_rcp45_summary.csv"), row.names = F)
#write.csv(df_results_lng,paste0(dirOut,"prProdIdx_rcp85_long.csv"), row.names = F)
#write.csv(df_results_summary,paste0(dirOut,"prProdIdx_rcp85_summary.csv"), row.names = F)

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
  facet_grid(desc~scenario)+
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
