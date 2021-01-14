
# date: 14/01/20
# author: VB
# description: plot seed orchard performance, demonstrating spatial comparison amongst GCMs

wd <- "~/FFMPs" # sandbox
dirData <- paste0(wd,"/data-raw/")
dirOut <- paste0(wd,"/data-processed/")

### libraries ------------------------------------------------------------------

library(tidyverse)
library(sf)
library(raster)
library(ggplot2)
library(viridis)

### steps ----------------------------------------------------------------------

# 1. rasterise performance of a single seed orchard for each GCM/RCP combo
# 2. choose appropriate threshold of performance (and remove if survival below 50%) - reclassify rasters - 1 if above threshold, 0 if below
# 3. sum all rasters
# 4. contour plot

# 1. rasterise -----------------------------------------------------------------

# list production prediction tifs per scenario
files <-  list.files(paste0(dirOut, "single_rasters/"),pattern = "*.tif",full.names = T)
# just RCP8.5 to test + current to compare
files <- grep("h60", files, value=TRUE)
files

SOh60 <- stack(files)
spplot(SOh60)

# 2. threshold reclass ---------------------------------------------------------

# lets say production has to be above 1.2 (20% above local provenance)

# reclass matrix
rules1 <- c(-1, 1.2, 0,  1.2, 1.5, 1)
rcl1 <- matrix(rules1, ncol=3, byrow=TRUE)
SOh60_rc <- reclassify(SOh60,rcl1)
spplot(SOh60_rc)

# 3. sum -----------------------------------------------------------------------

nlayers(SOh60_rc)
SOh60_sum <- stackApply(SOh60_rc, indices=c(1), fun=sum)
plot(SOh60_sum)

# 4. contour -------------------------------------------------------------------

contour1 <- rasterToContour(SOh60_sum)
contour1 <- st_as_sf(contour1)

contour1$agreement <- NA
contour1$agreement[which(contour1$level<=1)]<-"1 GCM"
contour1$agreement[which(contour1$level<=2&contour1$level>1)]<-"2 GCMs"
contour1$agreement[which(contour1$level<=3&contour1$level>2)]<-"3 GCMs"
contour1$agreement[which(contour1$level<=2&contour1$level>1)]<-"2 GCMs"
contour1$agreement[which(contour1$level<=4&contour1$level>3)]<-"4 GCMs"
contour1$agreement[which(contour1$level<=5&contour1$level>4)]<-"5 GCMs"
contour1$agreement[which(contour1$level<=6&contour1$level>5)]<-"6 GCMs"
contour1$agreement[which(contour1$level<=7&contour1$level>6)]<-"All GCMs"
contour1$agreement <- as.factor(contour1$agreement)

# convert from MULTILINESTRING to polygon
contour1 <- st_cast(contour1, to="POLYGON")

ggplot()+
  geom_sf(data=contour1,aes(fill=agreement),col=NA)+
  scale_fill_viridis(discrete = T, option = "C")+
  ggtitle("1.5g Seed Orchard 60 performance in 2050")+
  theme_minimal()
