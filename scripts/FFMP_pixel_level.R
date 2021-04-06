
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
# ONLY FOR 2050


### sweden outline -------------------------------------------------------------

# load country outline
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
sweden <- worldmap[worldmap$name == 'Sweden',]

# seed zones file for utm crs
sfSeedZones <- st_read(paste0(dirData,"Seed_zones_SP_Sweden/Shaper/Frözoner_tall_Sverige.shp"))
utm <- crs(sfSeedZones)

sweden <- st_transform(sweden, utm)


### read in each file, remove data > thresholds, & new threshold vars ----------

# list production prediction files per scenario
files <-  list.files(paste0(dirData, "Productionpredictions/"),pattern = "*.csv",full.names = T)
files
# remove ensemble mean and reference
files <- files[-c(9:12,25)]
# just 2050

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
  dfP <- dfP[,c("GridID","CenterLat","CenterLong","PrProdidxSOh60","PrProdidxSOh62","PrProdidxSOh64","PrProdidxSOh66",
         "PrProdidxSOhs60","PrProdidxSOhs62","PrProdidxSOhs64","PrProdidxSOhs66")] %>% 
    mutate(SOh60_120 = ifelse(PrProdidxSOh60 >= 1.2, 1, NA),
           SOh60_110 = ifelse(PrProdidxSOh60 >= 1.1, 1, NA),
           SOh60_100 = ifelse(PrProdidxSOh60 >= 1.0, 1, NA),
           SOh60_less = ifelse(PrProdidxSOh60 < 1, 1, NA),
           SOh62_120 = ifelse(PrProdidxSOh62 >= 1.2, 1, NA),
           SOh62_110 = ifelse(PrProdidxSOh62 >= 1.1, 1, NA),
           SOh62_100 = ifelse(PrProdidxSOh62 >= 1.0, 1, NA),
           SOh62_less = ifelse(PrProdidxSOh62 < 1, 1, NA),
           SOh64_120 = ifelse(PrProdidxSOh64 >= 1.2, 1, NA),
           SOh64_110 = ifelse(PrProdidxSOh64 >= 1.1, 1, NA),
           SOh64_100 = ifelse(PrProdidxSOh64 >= 1.0, 1, NA),
           SOh64_less = ifelse(PrProdidxSOh64 < 1, 1, NA),
           SOh66_120 = ifelse(PrProdidxSOh66 >= 1.2, 1, NA),
           SOh66_110 = ifelse(PrProdidxSOh66 >= 1.1, 1, NA),
           SOh66_100 = ifelse(PrProdidxSOh66 >= 1.0, 1, NA),
           SOh66_less = ifelse(PrProdidxSOh66 < 1, 1, NA),
           SOhs60_120 = ifelse(PrProdidxSOhs60 >= 1.2, 1, NA),
           SOhs60_110 = ifelse(PrProdidxSOhs60 >= 1.1, 1, NA),
           SOhs60_100 = ifelse(PrProdidxSOhs60 >= 1.0, 1, NA),
           SOhs60_less = ifelse(PrProdidxSOhs60 < 1, 1, NA),
           SOhs62_120 = ifelse(PrProdidxSOhs62 >= 1.2, 1, NA),
           SOhs62_110 = ifelse(PrProdidxSOhs62 >= 1.1, 1, NA),
           SOhs62_100 = ifelse(PrProdidxSOhs62 >= 1.0, 1, NA),
           SOhs62_less = ifelse(PrProdidxSOhs62 < 1, 1, NA),
           SOhs64_120 = ifelse(PrProdidxSOhs64 >= 1.2, 1, NA),
           SOhs64_110 = ifelse(PrProdidxSOhs64 >= 1.1, 1, NA),
           SOhs64_100 = ifelse(PrProdidxSOhs64 >= 1.0, 1, NA),
           SOhs64_less = ifelse(PrProdidxSOhs64 < 1, 1, NA),
           SOhs66_120 = ifelse(PrProdidxSOhs66 >= 1.2, 1, NA),
           SOhs66_110 = ifelse(PrProdidxSOhs66 >= 1.1, 1, NA),
           SOhs66_100 = ifelse(PrProdidxSOhs66 >= 1.0, 1, NA),
           SOhs66_less = ifelse(PrProdidxSOhs66 < 1, 1, NA))
  
  dfP <- dfP[,c(1:3,12:43)]
  
  dfP$scenario <- scenario
  
  dfP <- dfP %>% pivot_longer(cols = 4:35,
                       names_to = "threshold",
                       values_to = paste0(scenario,"_ag"))
  
  vroom_write(dfP, path = paste0(dirOut,"SO_choice_per_pixel_",scenario,".csv"), append=FALSE)
  
  }


### list new files & merge -----------------------------------------------------  

files2 <-  list.files(dirOut,pattern = "*.csv",full.names = T)
files2 <- grep("SO_choice",files2, value=TRUE)

lstRCP <- c("45in50","85in50","45in70","85in70")

for (rcp in lstRCP){
  
  #rcp <- lstRCP[1]
  files3 <- grep(rcp,files2, value=TRUE)
  
  rcp.name <- ifelse(grepl("45",rcp),"RCP4.5","RCP8.5")
  period <- ifelse(grepl("50",rcp), "2050", "2070")
  
  for(f in files3){
    
    #f <- files2[1]
    scenario <- strsplit(f, "[_]")[[1]][5]
    scenario <- strsplit(scenario, "[.]")[[1]][1]
    assign(scenario, vroom(f))
    
  }
  
  #head(bc45in50)
  
  df <- cbind(bc45in50[,c(1,2,3,4,5,6)],he45in50$he45in50_ag, mg45in50$mg45in50_ag, mi45in50$mi45in50_ag, no45in50$no45in50_ag)
  colnames(df)[7:10] <- c("he45in50_ag","mg45in50_ag","mi45in50_ag","no45in50_ag")
  
  head(df)
  
  df <- df %>% mutate(tot = rowSums(.[6:10], na.rm = TRUE))
  
  df <- df %>% pivot_wider(id_cols = c("GridID","CenterLat","CenterLong"),
                           names_from = threshold,
                           values_from = tot)
  head(df)
  
  dfPathway <- df %>% mutate(SOh60_pathway = ifelse(SOh60_120 >=3, 4,
                                                    ifelse(SOh60_110 >=3, 3,
                                                           ifelse(SOh60_100 >= 3, 2,
                                                                  ifelse(SOh60_less >=3, 1, NA)))),
                             SOh62_pathway = ifelse(SOh62_120 >=3, 4,
                                                    ifelse(SOh62_110 >=3, 3,
                                                           ifelse(SOh62_100 >= 3, 2,
                                                                  ifelse(SOh62_less >=3, 1, NA)))),
                             SOh64_pathway = ifelse(SOh64_120 >=3, 4,
                                                    ifelse(SOh64_110 >=3, 3,
                                                           ifelse(SOh64_100 >= 3, 2,
                                                                  ifelse(SOh64_less >=3, 1, NA)))),
                             SOh66_pathway = ifelse(SOh66_120 >=3, 4,
                                                    ifelse(SOh66_110 >=3, 3,
                                                           ifelse(SOh66_100 >= 3, 2,
                                                                  ifelse(SOh66_less >=3, 1, NA)))),
                             SOhs60_pathway = ifelse(SOhs60_120 >=3, 4,
                                                     ifelse(SOhs60_110 >=3, 3,
                                                            ifelse(SOhs60_100 >= 3, 2,
                                                                   ifelse(SOhs60_less >=3, 1, NA)))),
                             SOhs62_pathway = ifelse(SOhs62_120 >=3, 4,
                                                     ifelse(SOhs62_110 >=3, 3,
                                                            ifelse(SOhs62_100 >= 3, 2,
                                                                   ifelse(SOhs62_less >=3, 1, NA)))),
                             SOhs64_pathway = ifelse(SOhs64_120 >=3, 4,
                                                     ifelse(SOhs64_110 >=3, 3,
                                                            ifelse(SOhs64_100 >= 3, 2,
                                                                   ifelse(SOhs64_less >=3, 1, NA)))),
                             SOhs66_pathway = ifelse(SOhs66_120 >=3, 4,
                                                     ifelse(SOhs66_110 >=3, 3,
                                                            ifelse(SOhs66_100 >= 3, 2,
                                                                   ifelse(SOhs66_less >=3, 1, NA)))))
  
  colnames(dfPathway)
  
  dfPathway <- dfPathway[,c("GridID","CenterLat","CenterLong",
                            "SOh60_pathway","SOh62_pathway","SOh64_pathway","SOh66_pathway","SOhs60_pathway","SOhs62_pathway","SOhs64_pathway","SOhs66_pathway")]
  
  coordinates(dfPathway) <- ~ CenterLong + CenterLat
  
  # define lat long crs
  proj4string(dfPathway) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
  
  # transform points to utm
  spPathway <- spTransform(dfPathway, CRSobj = utm)
  
  rstUTM <- raster(crs = crs(spPathway), resolution = c(1100,1100), ext = extent(spPathway))
  
  for (var in names(spPathway)[c(2:9)]){ 
    
    #var <- names(spPathway)[2]
    
    rst <- rasterize(spPathway, rstUTM, spPathway[[var]], fun=max, na.rm=TRUE) 
    
    writeRaster(rst, paste0(dirOut,"pathway_rst/",var,"_",rcp.name,"_",period,"_GCMagreement.tif"), overwrite=TRUE)
    
    print(paste0("Written raster for: ", var))
    
  }
  
  
}


### convert to df and assign pathway based on code -----------------------------

lstRsts <- list.files(paste0(dirOut,"pathway_rst"),full.names = T)

lstRCP2 <- c("RCP4.5","RCP8.5")

for (rcp in lstRCP2){
  
  #rcp <- lstRCP2[1]
  
  RCP_rsts <- grep(rcp,lstRsts,value=TRUE)
  
  for (i in RCP_rsts){
    
    #i <- RCP_rsts[1]
    
    seed.orchard <- stringr::str_split(i,"/") %>% map_chr(.,4)
    period <- stringr::str_split(seed.orchard,"_") %>% map_chr(.,4)
    seed.orchard <- stringr::str_split(seed.orchard,"_") %>% map_chr(.,1)
    
    rst <- raster(i)
    
    # Convert raster to dataframe
    dfPathway <- as.data.frame(rst, xy=T)
    colnames(dfPathway) <- c("x","y","code")
    dfPathway$pathway <- NA
    dfPathway$pathway[which(dfPathway$code == 1)] <- "Expiry (below local)"
    dfPathway$pathway[which(dfPathway$code == 2)] <- "Good performance (above local)"
    dfPathway$pathway[which(dfPathway$code == 3)] <- "Very good performance (above 110)"
    dfPathway$pathway[which(dfPathway$code == 4)] <- "Excellent performance (above 120)"
    
    dfPathway$pathway <- factor(dfPathway$pathway, ordered=T, levels=c("Excellent performance (above 120)","Very good performance (above 110)",
                                                                       "Good performance (above local)","Expiry (below local)"))
    
    (p1 <- ggplot(data = dfPathway) +
        geom_sf(data = sweden, fill=NA)+
        geom_tile(data = dfPathway, mapping = aes(x = x, y = y, fill = pathway), size = 1) +
        scale_fill_viridis(discrete=T, direction = -1, drop=FALSE, 
                           labels = c("Excellent performance (above 120)","Very good performance (above 110)",
                                      "Good performance (above local)","Expiry (below local)","Beyond model thresholds"))+
        #labs(fill="Performance")+
        theme_bw()+
        ggtitle(paste0(seed.orchard, " | ", period))+
        theme(plot.title = element_text(face="bold",size=22),
              axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              #legend.title = element_text(size = 16, face = "bold", vjust = 3),
              #legend.text = element_text(size = 14)))
              legend.position = "none"))
    
    ggsave(p1, file=paste0(dirFigs,"Pathway_per_pixel_",seed.orchard,"_",rcp,"_",period,".png"), width=8, height=10, dpi=300)
    
  }
  
}



# get legend
# in loop, i've commented out the bits that plot the legend, but i ran once with the legend included & then extracted & saved
library(ggpubr)

# Extract the legend. Returns a gtable
legend <- get_legend(p1)

# Convert to a ggplot and save
legend <- as_ggplot(legend)
plot(legend)
ggsave(legend, file=paste0(dirFigs,"Pixel_Pathway_legend.png"),width=4, height=6, dpi=300)


### arrange in single figure per RCP? ------------------------------------------

library(grid)
library(png)
library(gridExtra)

lstPlots <- list.files(paste0(dirFigs), full.names = T)
lstPlots <- grep("ixel", lstPlots, value=TRUE)

#
lstPlots_RCP45 <- grep("4.5", lstPlots, value=TRUE)

lst1 <- grep("Oh6", lstPlots_RCP45, value=TRUE)
lst1 <- append(lst1, "C:/Users/vanessa.burton.sb/Documents/FFMPs/figures/Pixel_Pathway_legend.png" )

r1 <- lapply(lst1, png::readPNG)
g1 <- lapply(r1, grid::rasterGrob)

ggsave(gridExtra::grid.arrange(grobs=g1, 
                               ncol=3,
                               layout_matrix = cbind(c(1,3,5,7),
                                                     c(2,4,6,8),
                                                     c(9,9,9,9))), 
       file=paste0(dirFigs,"Seed_orchard_H_pathways_RCP4.5.png"),
       width=24, 
       height=40, 
       dpi=300)

lst2 <- grep("Ohs6", lstPlots_RCP45, value=TRUE)
lst2 <- append(lst2, "C:/Users/vanessa.burton.sb/Documents/FFMPs/figures/Pixel_Pathway_legend.png" )

r2 <- lapply(lst2, png::readPNG)
g2 <- lapply(r2, grid::rasterGrob)

ggsave(gridExtra::grid.arrange(grobs=g2, 
                               ncol=3,
                               layout_matrix = cbind(c(1,3,5,7),
                                                     c(2,4,6,8),
                                                     c(9,9,9,9))), 
       file=paste0(dirFigs,"Seed_orchard_HS_pathways_RCP4.5.png"),
       width=24, 
       height=40, 
       dpi=300)
