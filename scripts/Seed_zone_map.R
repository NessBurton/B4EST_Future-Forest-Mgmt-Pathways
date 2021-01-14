
library(ggplot2)
library(sf)
library(rnaturalearthdata)

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
shpSZ6 <- shpSZ %>% 
  filter(ZON2 %in% zones == T)

# dissolve/merge zones by ZON2 to simplify
shpSZ_sf <- st_as_sf(shpSZ6)
shpSZ_sf$area <- st_area(shpSZ_sf) # add area to have a variable to be able to summarise
shpSZ_sf <-
  shpSZ_sf %>%
  group_by(ZON2) %>% 
  summarise(area = sum(area))
plot(shpSZ_sf[1])

# read in descriptions

dfSZ <- read.csv(paste0(dirOut,"seed_zone_descriptions.csv"))
colnames(shpSZ_sf)[1] <- "Zone"
shpSZ_sf <- merge(shpSZ_sf,dfSZ,by="Zone")
shpSZ_sf$desc <- factor(shpSZ_sf$desc, levels=c("67°N-Upland","67°N-Lowland","66°N-Upland","66°N-Lowland","64°N-Upland", "64°N-Lowland"))

# plot

worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
sweden <- worldmap[worldmap$name == 'Sweden',]

library(RColorBrewer)
display.brewer.pal(n=3,name="Dark2")
display.brewer.pal(n=3,name="Accent")
brewer.pal(n = 3, name = "Dark2")
brewer.pal(n = 3, name = "Accent")


SZcols <- c("67°N-Upland" = "#1B9E77",
            "67°N-Lowland" = "#7FC97F",
            "66°N-Upland" = "#D95F02",
            "66°N-Lowland" = "#FDC086",
            "64°N-Upland" = "#7570B3",
            "64°N-Lowland" = "#BEAED4")

png(paste0(wd,"/figures/seed_zones.png"), width = 500, height = 600)
ggplot()+
  geom_sf(data = sweden)+
  geom_sf(data=shpSZ_sf, aes(fill=desc), colour=0)+
  scale_fill_manual(values = SZcols)+
  theme_minimal()+
  labs(fill = "Seed Zone")
dev.off()

