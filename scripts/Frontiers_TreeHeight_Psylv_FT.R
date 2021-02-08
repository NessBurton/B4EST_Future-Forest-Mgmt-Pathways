
    ######################################################################################################################

    ###  Comparing uncertainties in the 5x GCMs with 4x RCPs used to calculate tree height of P. sylvestris in Spain ###

    ######################################################################################################################


    # load libraries
    library(raster)
    library(rgdal)
    library(rgeos)
    library(rasterVis)
    library(tidyverse)
    library(plyr)
    library(reshape2)
    library(gridExtra)
    library(resample)
    library(RColorBrewer)


    rasterOptions(tmpdir='D:/tempdir')


    # Load files 
    All.predictions.70 <- list.files(path = "D:/FR_Datasets/SpainPredictions_70", pattern='*in70.tif$', all.files=TRUE, full.names=TRUE)
    All.predictions.present <- raster::raster("SpainPredictions/ScotsPine_Prediction_Present.tif")


    # turn into rasters
    All.predictions.70.rast <- lapply(All.predictions.70, raster)
    # for later analysis and plotting, turn rasters into dataframes
    #All.predictions.70.stack <- stack(All.predictions.70.rast)
    #crs(All.predictions.70.stack) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"     # two different projections
    #crs(All.predictions.stack) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
    
    # change the names of the layers
    names(All.predictions.70.rast) <- c("bc_RCP2_6_in70", "bc_RCP4_5_in70", "bc_RCP6_0in70", "bc_RCP8_5_in70",
                                        "he_RCP2_6_in70", "he_RCP4_5_in70", "he_RCP6_0in70", "he_RCP8_5_in70", 
                                        "mg_RCP2_6_in70", "mg_RCP4_5_in70", "mg_RCP6_0in70", "mg_RCP8_5_in70",
                                        "mi_RCP2_6_in70", "mi_RCP4_5_in70", "mi_RCP6_0in70", "mi_RCP8_5_in70", 
                                        "no_RCP2_6_in70", "no_RCP4_5_in70", "no_RCP6_0in70", "no_RCP8_5_in70") 
        
    All.predictions.70.stack <- stack(All.predictions.70.rast, All.predictions.present)
    
    # calculate the coefficient of variance (CV) for all GCMs and RCPs

    # grouped by RCP from every GCM
    RCP_VC <- stackApply(All.predictions.70.stack, indices=c(1,2,3,4, 1,2,3,4, 1,2,3,4, 1,2,3,4, 1,2,3,4), fun=cv)

    # grouped by GCM from every GCM
    GCM_VC <- stackApply(All.predictions.70.stack, indices=c(1,1,1,1, 2,2,2,2, 3,3,3,3, 4,4,4,4, 5,5,5,5), fun=cv)

    # for each RCP in each GCM
    GCM_RCP_VC.all <- stackApply(All.predictions.70.stack, indices=c(1:nlayers(All.predictions.70.stack)), fun=cv)


    # randomly sample rasters for stats tests
    GCM_RCP.rs.rast.ls <- list()
    for(i in 1:nlayers(All.predictions.70.stack)) {
        GCM_RCP.rs.rast.ls[[i]] <- sampleRandom(All.predictions.70.stack[[i]], size=2000, na.rm=TRUE, ext=NULL, cells=TRUE, rowcol=FALSE, xy=TRUE, sp=FALSE, asRaster=TRUE)
    }

    All.predictions.70.cv <- raster::calc(All.predictions.70.stack, fun=cv)

    All.predictions.70.cv.ls <- list()
    for(i in All.predictions.70.rast) {
        All.predictions.70.cv.ls[[i]] <- cv(All.predictions.70.rast[[i]])
    }
    




    # load ASTER elevation data (30m resolution)
    ASTER.elevation.dem.list <- list.files(path = "Aster_Elevation", pattern='*_dem.tif$', all.files=TRUE, full.names=TRUE)

    # turn files into rasters
    ASTER.elevation.dem.rast <- lapply(ASTER.elevation.dem.list, raster)
    
    # for later analysis and plotting, turn rasters into dataframes
    ASTER.elevation.dem.rast$fun <- mean    # add the mean function to the list of rasters
    ASTER.elevation.dem.mosaic <- do.call(mosaic, ASTER.elevation.dem.rast) # use mosaic from the raster package to merge all tiles together

    # resample the data 
    ASTER.elevation.dem.res.70 <- resample(ASTER.elevation.dem.mosaic, All.predictions.70.stack, method="bilinear", filename="Aster_Elevation/Aster_Elevation_resampled.tif", overwrite=TRUE)
    crs(ASTER.elevation.dem.res.70) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"   


    All.predictions.70.elev.stack <- stack(All.predictions.70.stack, ASTER.elevation.dem.res.70)
    All.predictions.70.cv <- cv(All.predictions.70.elev.stack)


    # this also works with projectRaster
    # ASTER.elevation.dem.reproj <- projectRaster(from=ASTER.elevation.dem.mosaic, to=All.predictions.stack)

    # bring the rasters of tree height data from RCPs/GCMs together with elevation. Note that when converting to a data.frame, tree height
    # data and elevation need to be converted seperately into spatialpolygonsdataframes, and then joined (see below)
    

    # keep locations by turning into SpatialPolygonsDataFrame first
    ASTER.elevation.70.sppolyg <- as(ASTER.elevation.dem.res.70, 'SpatialPolygonsDataFrame')

    # turn the locations of each polygon into data
    ASTER.elevation.70.sppolyg@data$id <- rownames(ASTER.elevation.70.sppolyg@data)
    ASTER.elevation.70.sppolyg.data <- ASTER.elevation.70.sppolyg@data
    ASTER.elevation.70.sppolyg@data <- plyr::join(ASTER.elevation.70.sppolyg@data, ASTER.elevation.70.sppolyg.data, by = "id")


    # keep locations by turning into SpatialPolygonsDataFrame first
    All.predictions.70.sppolyg <- as(All.predictions.70.stack, 'SpatialPolygonsDataFrame')

    # turn the locations of each polygon into data
    All.predictions.70.sppolyg@data$id <- rownames(All.predictions.70.sppolyg@data)
    All.predictions.70.sppolyg.data <- All.predictions.70.sppolyg@data
    All.predictions.70.sppolyg@data <- plyr::join(All.predictions.70.sppolyg@data, All.predictions.70.sppolyg.data, by = "id")

    # convert the spatial dataframe to dataframe
    All.predictions.70.sppolyg.df <- fortify(All.predictions.70.sppolyg)
    All.predictions.70.sppolyg.df <- plyr::join(All.predictions.70.sppolyg.df, All.predictions.70.sppolyg@data, by = "id")
    # join the tree height data with elevation data
    All.predictions.70.sppolyg.df <- plyr::join(All.predictions.70.sppolyg.df, ASTER.elevation.50.sppolyg@data, by = "id") # add elevation data to RCP/GCM data


    # change the column names
    names(All.predictions.70.sppolyg.df) <- c("long", "lat", "order", "hole", "piece", "id", "group", "bc_RCP_26_in70", "bc_RCP_45_in70", "bc_RCP_60_in70", "bc_RCP_85_in70",
                                           "he_RCP_26_in70", "he_RCP_45_in70", "he_RCP_60_in70", "he_RCP_85_in70", "mg_RCP_26_in70", "mg_RCP_45_in70", "mg_RCP_60_in70", "mg_RCP_85_in70",
                                           "mi_RCP_26_in70", "mi_RCP_45_in70", "mi_RCP_60_in70", "mi_RCP_85_in70", "no_RCP_26_in70", "no_RCP_45_in70", "no_RCP_60_in70", "no_RCP_85_in70",
                                            "Elevation_m")

    # turn dataframe into long format
    All.predictions.70.sppolyg.melt <- melt(All.predictions.70.sppolyg.df, id.vars = c("long", "lat", "Elevation_m", "order", "hole", "piece", "id", "group"))

    # classify the data into GCM categories where the variable contains the initials of the GCM (e.g. 'bc', 'mg'). This can be done with a conditional ifelse statement
    # which identifies if a certain string (e.g. 'bc') is contained within a variable. If the string is not, the identify something else (e.g. 'he') and so on
    All.predictions.70.sppolyg.melt$GCM <- ifelse(grepl("bc", All.predictions.70.sppolyg.melt$variable), 'bc - BCC-CSM1-1', 
                                            ifelse(grepl("he", All.predictions.70.sppolyg.melt$variable), 'he - HadGEM2-ES', 
                                                ifelse(grepl("mg", All.predictions.70.sppolyg.melt$variable), 'mg - MRI-CGCM3', 
                                                    ifelse(grepl("mi", All.predictions.70.sppolyg.melt$variable), 'mi - MIROC-ESM-CHEM', 
                                                        ifelse(grepl("no", All.predictions.70.sppolyg.melt$variable), 'no - NorESM1-M', 'GCM_all')))))

    # do the same for RCP scenarios
    All.predictions.70.sppolyg.melt$RCP <- ifelse(grepl("RCP_26", All.predictions.70.sppolyg.melt$variable), '2.6', 
                                            ifelse(grepl("RCP_45", All.predictions.70.sppolyg.melt$variable), '4.5', 
                                                ifelse(grepl("RCP_60", All.predictions.70.sppolyg.melt$variable), '6.0', 
                                                    ifelse(grepl("RCP_85", All.predictions.70.sppolyg.melt$variable), '8.5', 'RCP_all'))))


    # Plot the data in density distributions per GCM, and save a al plots via a for loop
    # Getting unique GCM names to loop over. This is used to create a different plot for every GCM
    GCMs.70 <- unique(All.predictions.70.sppolyg.melt$GCM)
    # Create an empty list to save plots created. Lists in R are very versatile. They can pretty much store any type of data in them.
    GCMs_boxplots.70 <- list()
    # looping over unique GCMs
        for(i in GCMs.70) {
            GCMs_boxplots.70[[i]] <- ggplot(All.predictions.70.sppolyg.melt %>% filter(GCM == i), aes(x=RCP, y=value, fill=RCP)) + 
                                        geom_boxplot() +
                                        stat_summary(fun=mean, geom="point", color="red", size=4) +   # plot the mean as a red dot
                                        scale_y_continuous(limits = c(-1000, 1700)) +
                                        xlab("RCP scenario") +
                                        ylab("Data distribution") +
                                        theme_bw() + 
                                        ggtitle(i) + 
                                        theme(plot.title = element_text(size = 20, face = "bold", hjust=0.5), 
                                            axis.title.x = element_text(size = 18, face = "bold"), 
                                            axis.title.y = element_text(size = 18, face = "bold"),
                                            axis.text.x = element_text(size = 16),
                                            axis.text.y = element_text(size = 16))
        # print the plots created to screen
        print(GCMs_boxplots.70[[i]])
        # save the plots to home directory. file parameter is used to give plot file name - it can be a complete path of the file name. width and height give dimensions to the file in units = "cm". dpi is dots per inch for the quality of plot
        ggsave(GCMs_boxplots.70[[i]], file=paste0("Plots/Boxplot_2070_", i,".png"), width = 10, height = 10, dpi=300)
        }

        # load cowplot gridExtra packages

        # arrange all plots in one grid next to each other
        GCMs_boxplots.70.all <- do.call("grid.arrange", c(GCMs_boxplots.70[1:5], ncol= 5))
        ggsave(GCMs_boxplots.70.all, file="Plots/GCM_RCP_boxplots_2070.png", width=21, height=5, dpi=300)




    # identify outliers across all RCPs and GCMs
    OutVals_GCM_RCP.70 <- list()
    for(i in 8:length(All.predictions.70.sppolyg.df)){
        OutVals_GCM_RCP.70[[i]] <- boxplot(All.predictions.70.sppolyg.df[[i]], plot=FALSE)$out 
    }

    # identify the outliers within the dataset
    outliers.70 <- list()
    for(ii in 8:length(OutVals_GCM_RCP.70)){
        outliers.70[[ii]] <- All.predictions.70.sppolyg.df[All.predictions.70.sppolyg.df[[ii]] %in% OutVals_GCM_RCP.70[[ii]],]
    }




    #All.predictions.70.outliers.df <- plyr::join(All.predictions.70.sppolyg.df, outliers.70[[8]], by = "id")

    # for a better overview, not all GCMs (bc, he, mg, mi, no) are coded here. The name of the GCM can be replaced by others with "ctrl + d" (select GCM and replace name)"
    #All.predictions.70.outliers.df <- plyr::join(All.predictions.70.sppolyg.df, ASTER.elevation.sppolyg@data, by = "id")

    # copy the dataframe for reproducability
    All.predictions.70.sppolyg.df.sp <- All.predictions.70.sppolyg.df
    
    # turn longs and lats to spatial coordinates
    coordinates(All.predictions.70.sppolyg.df.sp) <- ~long+lat
    # add coordiante system
    crs(All.predictions.70.sppolyg.df.sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

    # create grid for plotting
    par(mfrow=c(2,2))
    
    # calculate z-scores for outliers for each GCM and RCP and add columns with outliers to dataframe (z-scores show how far the outliers are away from the mean)
    # afterwards, plot the outliers in space with spplot

    # NOTE: this can surely be done in a loop, but I've done it one by one for now as my loops didn't work for now

    # load country outlines
    country.outline <- readOGR("D:/FR_Datasets/Country_Outlines/CNTR_BN_01M_2020_4326_COASTL.shp")
    country.outline.Spain <- raster::crop(country.outline, extent(All.predictions.70.sppolyg.df.sp))




    nb.cols <- 9
    mycolors <- colorRampPalette(brewer.pal(9, "BrBG"))(nb.cols)

    # bc - BCC-CSM1-1
    All.predictions.70.sppolyg.df.sp$Zscore_bc_26_in70 <- spatialEco::outliers(All.predictions.70.sppolyg.df.sp$bc_RCP_26_in70) 
        bc.RCP.2.6.spplot.70 <- spplot(All.predictions.70.sppolyg.df.sp, "Zscore_bc_26_in70", 
            col.regions = mycolors, 
            colorkey = mycolors,
            scales = list(draw = TRUE),
            #colorkey = list(right = list(fun = draw.colorkey, 
            #    args = list(key = list(at = seq(-16, 16, 4), 
            #        col = mycolors, labels = list(at = c(-16, -12, -8, -4, 0, 4, 8, 12, 16), 
            #            labels = c("-16", "-12", "-8", "-4", "0", "4", "8", "12", "16")))))), 
            key.space = "right", 
            main = "RCP 2.6", 
            sp.layout = country.outline.Spain)
        bc.RCP.2.6.spplot.70

    All.predictions.70.sppolyg.df.sp$Zscore_bc_45_in70 <- spatialEco::outliers(All.predictions.70.sppolyg.df.sp$bc_RCP_45_in70) 
        bc.RCP.4.5.spplot.70 <- spplot(All.predictions.70.sppolyg.df.sp, "Zscore_bc_45_in70", 
            col.regions = mycolors, 
            #colorkey = list(right = list(fun = draw.colorkey, 
            #    args = list(key = list(at = seq(-16, 16, 4), 
           #         col = mycolors, labels = list(at = c(-16, -12, -8, -4, 0, 4, 8, 12, 16), 
            #            labels = c("-16", "-12", "-8", "-4", "0", "4", "8", "12", "16")))))), 
            key.space = "right", 
            main = "RCP 4.5", 
            sp.layout = country.outline.Spain)
    
    All.predictions.70.sppolyg.df.sp$Zscore_bc_60_in70 <- spatialEco::outliers(All.predictions.70.sppolyg.df.sp$bc_RCP_60_in70) 
        bc.RCP.6.0.spplot.70 <- spplot(All.predictions.70.sppolyg.df.sp, "Zscore_bc_60_in70", 
            col.regions = mycolors, 
            #at = seq(-16, 16, 4),
            #colorkey = list(right = list(fun = draw.colorkey, 
            #    args = list(key = list(at = seq(-16, 16, 4), 
            #        col = mycolors, labels = list(at = c(-16, -12, -8, -4, 0, 4, 8, 12, 16), 
            #            labels = c("-16", "-12", "-8", "-4", "0", "4", "8", "12", "16")))))),
            #right = list(fun = draw.colorkey),
                #labels = list(
            #    at = c(-16, -12, -8, -4, 0, 4, 8, 12, 16), 
                #labels = c("-16", "-12", "-8", "-4", "0", "4", "8", "12", "16")),
            #colorkey = list(right = list(fun = draw.colorkey, 
            #    args = list(key = list(at = seq(-16, 16, 4), 
            #        col = mycolors, labels = list(at = c(-16, -12, -8, -4, 0, 4, 8, 12, 16), 
            #            labels = c("-16", "-12", "-8", "-4", "0", "4", "8", "12", "16")))))), 
            key.space = "right", 
            main = "RCP 6.0", 
            sp.layout = country.outline.Spain)
        bc.RCP.6.0.spplot.70
    

    All.predictions.70.sppolyg.df.sp$Zscore_bc_85_in70 <- spatialEco::outliers(All.predictions.70.sppolyg.df.sp$bc_RCP_85_in70)
        bc.RCP.8.5.spplot.70 <- spplot(All.predictions.70.sppolyg.df.sp, "Zscore_bc_85_in70", 
            col.regions = mycolors, 
            #at = seq(-16, 16, 4),
            #colorkey = list(right = list(fun = draw.colorkey, 
            #    args = list(key = list(at = seq(-16, 16, 4), 
            #        col = mycolors, labels = list(at = c(-16, -12, -8, -4, 0, 4, 8, 12, 16), 
            #            labels = c("-16", "-12", "-8", "-4", "0", "4", "8", "12", "16")))))),
            #colorkey = list(right = list(fun = draw.colorkey, 
            #    args = list(key = list(at = seq(-16, 16, 4), 
            #        col = mycolors, labels = list(at = c(-16, -12, -8, -4, 0, 4, 8, 12, 16), 
            #            labels = c("-16", "-12", "-8", "-4", "0", "4", "8", "12", "16")))))), 
            key.space = "right", 
            main = "RCP 8.5", 
            sp.layout = country.outline.Spain)
        bc.RCP.8.5.spplot.70

    # put all spplots into list
    bc.RCP.all.spplot.70.list <- list(bc.RCP.2.6.spplot.70, bc.RCP.4.5.spplot.70, bc.RCP.6.0.spplot.70, bc.RCP.8.5.spplot.70)


    # arrange all plots in one grid
    bc.RCP.all.spplot.70 <- do.call(grid.arrange, bc.RCP.all.spplot.70.list)
    plot(bc.RCP.all.spplot.70)
    ggsave("2070_BCC-CSM1-1_Outliers.png")
    ggsave(bc.RCP.all.spplot.70, file="Plots/2070_BCC-CSM1-1_Outliers.png", width=21, height=5, dpi=300)




    # he - HadGEM2-ES
    All.predictions.70.sppolyg.df.sp$Zscore_he_26_in70 <- spatialEco::outliers(All.predictions.70.sppolyg.df.sp$he_RCP_26_in70) 
        he.RCP.2.6.spplot.70 <- spplot(All.predictions.70.sppolyg.df.sp, "Zscore_he_26_in70", 
            col.regions = mycolors, 
            colorkey = list(right = list(fun = draw.colorkey, 
                args = list(key = list(at = seq(-16, 16, 4), 
                    col = mycolors, labels = list(at = c(-16, -12, -8, -4, 0, 4, 8, 12, 16), 
                        labels = c("-16", "-12", "-8", "-4", "0", "4", "8", "12", "16")))))), 
            key.space = "right", 
            main = "RCP 2.6", 
            sp.layout = country.outline.Spain)
        he.RCP.2.6.spplot.70

    All.predictions.70.sppolyg.df.sp$Zscore_he_45_in70 <- spatialEco::outliers(All.predictions.70.sppolyg.df.sp$he_RCP_45_in70) 
        he.RCP.4.5.spplot.70 <- spplot(All.predictions.70.sppolyg.df.sp, "Zscore_he_45_in70", 
            col.regions = mycolors, 
            colorkey = list(right = list(fun = draw.colorkey, 
                args = list(key = list(at = seq(-16, 16, 4), 
                    col = mycolors, labels = list(at = c(-16, -12, -8, -4, 0, 4, 8, 12, 16), 
                        labels = c("-16", "-12", "-8", "-4", "0", "4", "8", "12", "16")))))), 
            key.space = "right", 
            main = "RCP 4.5", 
            sp.layout = country.outline.Spain)
    
    All.predictions.70.sppolyg.df.sp$Zscore_he_60_in70 <- spatialEco::outliers(All.predictions.70.sppolyg.df.sp$he_RCP_60_in70) 
        he.RCP.6.0.spplot.70 <- spplot(All.predictions.70.sppolyg.df.sp, "Zscore_he_60_in70", 
            col.regions = mycolors, 
            colorkey = list(right = list(fun = draw.colorkey, 
                args = list(key = list(at = seq(-16, 16, 4), 
                    col = mycolors, labels = list(at = c(-16, -12, -8, -4, 0, 4, 8, 12, 16), 
                        labels = c("-16", "-12", "-8", "-4", "0", "4", "8", "12", "16")))))), 
            key.space = "right", 
            main = "RCP 6.0", 
            sp.layout = country.outline.Spain)
    
    All.predictions.70.sppolyg.df.sp$Zscore_he_85_in70 <- spatialEco::outliers(All.predictions.70.sppolyg.df.sp$he_RCP_85_in70)
        he.RCP.8.5.spplot.70 <- spplot(All.predictions.70.sppolyg.df.sp, "Zscore_he_85_in70", 
            col.regions = mycolors, 
            colorkey = list(right = list(fun = draw.colorkey, 
                args = list(key = list(at = seq(-16, 16, 4), 
                    col = mycolors, labels = list(at = c(-16, -12, -8, -4, 0, 4, 8, 12, 16), 
                        labels = c("-16", "-12", "-8", "-4", "0", "4", "8", "12", "16")))))), 
            key.space = "right", 
            main = "RCP 8.5", 
            sp.layout = country.outline.Spain)

    # put all spplots into list
    he.RCP.all.spplot.70.list <- list(he.RCP.2.6.spplot.70, he.RCP.4.5.spplot.70, he.RCP.6.0.spplot.70, he.RCP.8.5.spplot.70)


    # arrange all plots in one grid
    he.RCP.all.spplot.70 <- do.call(grid.arrange, he.RCP.all.spplot.70.list)
    png(he.RCP.all.spplot.70, "HadGEM2-ES_2070_Outliers.png")
    
   

    # mg - MRI-CGCM3
    All.predictions.70.sppolyg.df.sp$Zscore_mg_26_in70 <- spatialEco::outliers(All.predictions.70.sppolyg.df.sp$mg_RCP_26_in70) 
        mg.RCP.2.6.spplot.70 <- spplot(All.predictions.70.sppolyg.df.sp, "Zscore_mg_26_in70", 
            col.regions = mycolors, 
            colorkey = list(right = list(fun = draw.colorkey, 
                args = list(key = list(at = seq(-16, 16, 4), 
                    col = mycolors, labels = list(at = c(-16, -12, -8, -4, 0, 4, 8, 12, 16), 
                        labels = c("-16", "-12", "-8", "-4", "0", "4", "8", "12", "16")))))), 
            key.space = "right", 
            main = "RCP 2.6", 
            sp.layout = country.outline.Spain)
        mg.RCP.2.6.spplot.70

    All.predictions.70.sppolyg.df.sp$Zscore_mg_45_in70 <- spatialEco::outliers(All.predictions.70.sppolyg.df.sp$mg_RCP_45_in70) 
        mg.RCP.4.5.spplot.70 <- spplot(All.predictions.70.sppolyg.df.sp, "Zscore_mg_45_in70", 
            col.regions = mycolors, 
            colorkey = list(right = list(fun = draw.colorkey, 
                args = list(key = list(at = seq(-16, 16, 4), 
                    col = mycolors, labels = list(at = c(-16, -12, -8, -4, 0, 4, 8, 12, 16), 
                        labels = c("-16", "-12", "-8", "-4", "0", "4", "8", "12", "16")))))), 
            key.space = "right", 
            main = "RCP 4.5", 
            sp.layout = country.outline.Spain)
    
    All.predictions.70.sppolyg.df.sp$Zscore_mg_60_in70 <- spatialEco::outliers(All.predictions.70.sppolyg.df.sp$mg_RCP_60_in70) 
        mg.RCP.6.0.spplot.70 <- spplot(All.predictions.70.sppolyg.df.sp, "Zscore_mg_60_in70", 
            col.regions = mycolors, 
            colorkey = list(right = list(fun = draw.colorkey, 
                args = list(key = list(at = seq(-16, 16, 4), 
                    col = mycolors, labels = list(at = c(-16, -12, -8, -4, 0, 4, 8, 12, 16), 
                        labels = c("-16", "-12", "-8", "-4", "0", "4", "8", "12", "16")))))), 
            key.space = "right", 
            main = "RCP 6.0", 
            sp.layout = country.outline.Spain)
    
    All.predictions.70.sppolyg.df.sp$Zscore_mg_85_in70 <- spatialEco::outliers(All.predictions.70.sppolyg.df.sp$mg_RCP_85_in70)
        mg.RCP.8.5.spplot.70 <- spplot(All.predictions.70.sppolyg.df.sp, "Zscore_mg_85_in70", 
            col.regions = mycolors, 
            colorkey = list(right = list(fun = draw.colorkey, 
                args = list(key = list(at = seq(-16, 16, 4), 
                    col = mycolors, labels = list(at = c(-16, -12, -8, -4, 0, 4, 8, 12, 16), 
                        labels = c("-16", "-12", "-8", "-4", "0", "4", "8", "12", "16")))))), 
            key.space = "right", 
            main = "RCP 8.5", 
            sp.layout = country.outline.Spain)

    # put all spplots into list
    mg.RCP.all.spplot.70.list <- list(mg.RCP.2.6.spplot.70, mg.RCP.4.5.spplot.70, mg.RCP.6.0.spplot.70, mg.RCP.8.5.spplot.70)

    #mg.RCP.all.spplot.70.list.agg <- aggregate(rbind(mg.RCP.2.6.spplot.70, country.outline.Spain))

    # arrange all plots in one grid
    mg.RCP.all.spplot.70 <- do.call(grid.arrange, mg.RCP.all.spplot.70.list)
    png(mg.RCP.all.spplot.70, "MRI-CGCM3_2070_Outliers.png")



    # mi - MIROC-ESM-CHEM
    All.predictions.70.sppolyg.df.sp$Zscore_mi_26_in70 <- spatialEco::outliers(All.predictions.70.sppolyg.df.sp$mi_RCP_26_in70) 
        mi.RCP.2.6.spplot.70 <- spplot(All.predictions.70.sppolyg.df.sp, "Zscore_mi_26_in70", 
            col.regions = mycolors, 
            colorkey = list(right = list(fun = draw.colorkey, 
                args = list(key = list(at = seq(-16, 16, 4), 
                    col = mycolors, labels = list(at = c(-16, -12, -8, -4, 0, 4, 8, 12, 16), 
                        labels = c("-16", "-12", "-8", "-4", "0", "4", "8", "12", "16")))))), 
            key.space = "right", 
            main = "RCP 2.6", 
            sp.layout = country.outline.Spain)
        mi.RCP.2.6.spplot.70

    All.predictions.70.sppolyg.df.sp$Zscore_mi_45_in70 <- spatialEco::outliers(All.predictions.70.sppolyg.df.sp$mi_RCP_45_in70) 
        mi.RCP.4.5.spplot.70 <- spplot(All.predictions.70.sppolyg.df.sp, "Zscore_mi_45_in70", 
            col.regions = mycolors, 
            colorkey = list(right = list(fun = draw.colorkey, 
                args = list(key = list(at = seq(-16, 16, 4), 
                    col = mycolors, labels = list(at = c(-16, -12, -8, -4, 0, 4, 8, 12, 16), 
                        labels = c("-16", "-12", "-8", "-4", "0", "4", "8", "12", "16")))))), 
            key.space = "right", 
            main = "RCP 4.5", 
            sp.layout = country.outline.Spain)
    
    All.predictions.70.sppolyg.df.sp$Zscore_mi_60_in70 <- spatialEco::outliers(All.predictions.70.sppolyg.df.sp$mi_RCP_60_in70) 
        mi.RCP.6.0.spplot.70 <- spplot(All.predictions.70.sppolyg.df.sp, "Zscore_mi_60_in70", 
            col.regions = mycolors, 
            colorkey = list(right = list(fun = draw.colorkey, 
                args = list(key = list(at = seq(-16, 16, 4), 
                    col = mycolors, labels = list(at = c(-16, -12, -8, -4, 0, 4, 8, 12, 16), 
                        labels = c("-16", "-12", "-8", "-4", "0", "4", "8", "12", "16")))))), 
            key.space = "right", 
            main = "RCP 6.0", 
            sp.layout = country.outline.Spain)
    
    All.predictions.70.sppolyg.df.sp$Zscore_mi_85_in70 <- spatialEco::outliers(All.predictions.70.sppolyg.df.sp$mi_RCP_85_in70)
        mi.RCP.8.5.spplot.70 <- spplot(All.predictions.70.sppolyg.df.sp, "Zscore_mi_85_in70", 
            col.regions = mycolors, 
            colorkey = list(right = list(fun = draw.colorkey, 
                args = list(key = list(at = seq(-16, 16, 4), 
                    col = mycolors, labels = list(at = c(-16, -12, -8, -4, 0, 4, 8, 12, 16), 
                        labels = c("-16", "-12", "-8", "-4", "0", "4", "8", "12", "16")))))), 
            key.space = "right", 
            main = "RCP 8.5", 
            sp.layout = country.outline.Spain)

    # put all spplots into list
    mi.RCP.all.spplot.70.list <- list(mi.RCP.2.6.spplot.70, mi.RCP.4.5.spplot.70, mi.RCP.6.0.spplot.70, mi.RCP.8.5.spplot.70)

    #mi.RCP.all.spplot.70.list.agg <- aggregate(rbind(mi.RCP.2.6.spplot.70, country.outline.Spain))

    # arrange all plots in one grid
    mi.RCP.all.spplot.70 <- do.call(grid.arrange, mi.RCP.all.spplot.70.list)
    png(mi.RCP.all.spplot.70, "MIROC-ESM-CHEM_2070_Outliers.png")



    # no - NorESM1-M
    All.predictions.70.sppolyg.df.sp$Zscore_no_26_in70 <- spatialEco::outliers(All.predictions.70.sppolyg.df.sp$no_RCP_26_in70) 
        no.RCP.2.6.spplot.70 <- spplot(All.predictions.70.sppolyg.df.sp, "Zscore_no_26_in70", 
            col.regions = mycolors, 
            colorkey = list(right = list(fun = draw.colorkey, 
                args = list(key = list(at = seq(-16, 16, 4), 
                    col = mycolors, labels = list(at = c(-16, -12, -8, -4, 0, 4, 8, 12, 16), 
                        labels = c("-16", "-12", "-8", "-4", "0", "4", "8", "12", "16")))))), 
            key.space = "right", 
            main = "RCP 2.6", 
            sp.layout = country.outline.Spain)
        no.RCP.2.6.spplot.70

    All.predictions.70.sppolyg.df.sp$Zscore_no_45_in70 <- spatialEco::outliers(All.predictions.70.sppolyg.df.sp$no_RCP_45_in70) 
        no.RCP.4.5.spplot.70 <- spplot(All.predictions.70.sppolyg.df.sp, "Zscore_no_45_in70", 
            col.regions = mycolors, 
            colorkey = list(right = list(fun = draw.colorkey, 
                args = list(key = list(at = seq(-16, 16, 4), 
                    col = mycolors, labels = list(at = c(-16, -12, -8, -4, 0, 4, 8, 12, 16), 
                        labels = c("-16", "-12", "-8", "-4", "0", "4", "8", "12", "16")))))), 
            key.space = "right", 
            main = "RCP 4.5", 
            sp.layout = country.outline.Spain)
    
    All.predictions.70.sppolyg.df.sp$Zscore_no_60_in70 <- spatialEco::outliers(All.predictions.70.sppolyg.df.sp$no_RCP_60_in70) 
        no.RCP.6.0.spplot.70 <- spplot(All.predictions.70.sppolyg.df.sp, "Zscore_no_60_in70", 
            col.regions = mycolors, 
            colorkey = list(right = list(fun = draw.colorkey, 
                args = list(key = list(at = seq(-16, 16, 4), 
                    col = mycolors, labels = list(at = c(-16, -12, -8, -4, 0, 4, 8, 12, 16), 
                        labels = c("-16", "-12", "-8", "-4", "0", "4", "8", "12", "16")))))), 
            key.space = "right", 
            main = "RCP 6.0", 
            sp.layout = country.outline.Spain)
    
    All.predictions.70.sppolyg.df.sp$Zscore_no_85_in70 <- spatialEco::outliers(All.predictions.70.sppolyg.df.sp$no_RCP_85_in70)
        no.RCP.8.5.spplot.70 <- spplot(All.predictions.70.sppolyg.df.sp, "Zscore_no_85_in70", 
            col.regions = mycolors, 
            colorkey = list(right = list(fun = draw.colorkey, 
                args = list(key = list(at = seq(-16, 16, 4), 
                    col = mycolors, labels = list(at = c(-16, -12, -8, -4, 0, 4, 8, 12, 16), 
                        labels = c("-16", "-12", "-8", "-4", "0", "4", "8", "12", "16")))))), 
            key.space = "right", 
            main = "RCP 8.5", 
            sp.layout = country.outline.Spain)

    # put all spplots into list
    no.RCP.all.spplot.70.list <- list(no.RCP.2.6.spplot.70, no.RCP.4.5.spplot.70, no.RCP.6.0.spplot.70, no.RCP.8.5.spplot.70)

    #no.RCP.all.spplot.70.list.agg <- aggregate(rbind(no.RCP.2.6.spplot.70, country.outline.Spain))

    # arrange all plots in one grid
    no.RCP.all.spplot.70 <- do.call(grid.arrange, no.RCP.all.spplot.70.list)
    png(no.RCP.all.spplot.70, "NorESM1-M_2070_Outliers.png")
















    # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # let's look at a data distribution and uncertainties across all RCPs and GCMs by using random sampling, bootstrapping
    # and statistical tests (wilcox test, anova, TukeyHSD)

    # NOTE: again this may be more efficient in a loop. For now I have created dataframes for each GCM and RCP. 
    # You can replace the name of the GCM (bc, he, mg, mi, no) by highlighting the name, using "ctrl + D" to mark 
    # all GCM names one by one and then replace the name
    

    library(EnvStats)

    # let's calculate the coeffcient of variance for the data
    All.predictions.70.sppolyg.df

    All.predictions.70.df.noCoords <- All.predictions.70.sppolyg.df[, c(8:27)]

    All.predictions.70.cv <- as.data.frame(lapply(All.predictions.70.df.noCoords, FUN=cv))
    write.csv(All.predictions.70.cv, "GCM_RCP_2070_CoefVar.csv")



    # the coefficient of variance is quite low for the entire datasets. This may be due to overfitting
    All.predictions.70.rs <- bind_rows(replicate(2, All.predictions.70.sppolyg.df%>% sample_n(1000), simplify=F), .id="Obs")

    # select random samples
    All.predictions.70.rs <- All.predictions.70.rs[, c(9:28)]

    # calculate the coefficient of variation for the randomly sampled data
    All.predictions.70.rs.cv <- as.data.frame(lapply(All.predictions.70.rs, FUN=cv))
    write.csv(All.predictions.70.rs.cv, "GCM_RCP_2070_CoefVar_randomS.csv")

    # load backage for bootstrapping
    #library(sjstats)

    # run bootstrap function
    All.predictions.70.bs <- resample::bootstrap(All.predictions.70.df.noCoords, statistic=mean, R=2000)
    