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


    # Load files 
    All.predictions.50 <- list.files(path = "SpainPredictions", pattern='*50.tif$', all.files=TRUE, full.names=TRUE)
    All.predictions.present <- raster::raster("SpainPredictions/ScotsPine_Prediction_Present.tif")


    rasterOptions(tmpdir='D:/tempdir')

    # turn into rasters
    All.predictions.50.rast <- lapply(All.predictions.50, raster)

    # for later analysis and plotting, turn rasters into dataframes
    All.predictions.50.stack <- stack(All.predictions.50.rast)
    #crs(All.predictions.50.stack) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"     # two different projections
    #crs(All.predictions.stack) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
    
    # change the names of the layers
    names(All.predictions.50.stack) <- c("bc_RCP2_6_in50", "bc_RCP4_5_in50", "bc_RCP6_0in50", "bc_RCP8_5_in50",
                                           "he_RCP2_6_in50", "he_RCP4_5_in50", "he_RCP6_0in50", "he_RCP8_5_in50", 
                                           "mg_RCP2_6_in50", "mg_RCP4_5_in50", "mg_RCP6_0in50", "mg_RCP8_5_in50",
                                           "mi_RCP2_6_in50", "mi_RCP4_5_in50", "mi_RCP6_0in50", "mi_RCP8_5_in50", 
                                           "no_RCP2_6_in50", "no_RCP4_5_in50", "no_RCP6_0in50", "no_RCP8_5_in50") 
        
    All.predictions.50.stack <- stack(All.predictions.50.stack, All.predictions.present)
    

    # load ASTER elevation data (30m resolution)
    ASTER.elevation.dem.list <- list.files(path = "Aster_Elevation", pattern='*_dem.tif$', all.files=TRUE, full.names=TRUE)

    # turn files into rasters
    ASTER.elevation.dem.rast <- lapply(ASTER.elevation.dem.list, raster)
    
    # for later analysis and plotting, turn rasters into dataframes
    ASTER.elevation.dem.rast$fun <- mean    # add the mean function to the list of rasters
    ASTER.elevation.dem.mosaic <- do.call(mosaic, ASTER.elevation.dem.rast) # use mosaic from the raster package to merge all tiles together

    # resample the data 
    ASTER.elevation.dem.res.50 <- resample(ASTER.elevation.dem.mosaic, All.predictions.50.stack, method="bilinear", filename="Aster_Elevation/Aster_Elevation_resampled.tif", overwrite=TRUE)
    crs(ASTER.elevation.dem.res.50) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"   

    # this also works with projectRaster
    # ASTER.elevation.dem.reproj <- projectRaster(from=ASTER.elevation.dem.mosaic, to=All.predictions.stack)

    # bring the rasters of tree height data from RCPs/GCMs together with elevation. Note that when converting to a data.frame, tree height
    # data and elevation need to be converted seperately into spatialpolygonsdataframes, and then joined (see below)
    

    # keep locations by turning into SpatialPolygonsDataFrame first
    ASTER.elevation.50.sppolyg <- as(ASTER.elevation.dem.res.50, 'SpatialPolygonsDataFrame')

    # turn the locations of each polygon into data
    ASTER.elevation.50.sppolyg@data$id <- rownames(ASTER.elevation.50.sppolyg@data)
    ASTER.elevation.50.sppolyg.data <- ASTER.elevation.50.sppolyg@data
    ASTER.elevation.50.sppolyg@data <- plyr::join(ASTER.elevation.50.sppolyg@data, ASTER.elevation.50.sppolyg.data, by = "id")


    # keep locations by turning into SpatialPolygonsDataFrame first
    All.predictions.50.sppolyg <- as(All.predictions.50.stack, 'SpatialPolygonsDataFrame')

    # turn the locations of each polygon into data
    All.predictions.50.sppolyg@data$id <- rownames(All.predictions.50.sppolyg@data)
    All.predictions.50.sppolyg.data <- All.predictions.50.sppolyg@data
    All.predictions.50.sppolyg@data <- plyr::join(All.predictions.50.sppolyg@data, All.predictions.50.sppolyg.data, by = "id")

    # convert the spatial dataframe to dataframe
    All.predictions.50.sppolyg.df <- fortify(All.predictions.50.sppolyg)
    All.predictions.50.sppolyg.df <- plyr::join(All.predictions.50.sppolyg.df, All.predictions.50.sppolyg@data, by = "id")
    # join the tree height data with elevation data
    All.predictions.50.sppolyg.df <- plyr::join(All.predictions.50.sppolyg.df, ASTER.elevation.50.sppolyg@data, by = "id") # add elevation data to RCP/GCM data


    # change the column names
    names(All.predictions.50.sppolyg.df) <- c("long", "lat", "order", "hole", "piece", "id", "group", "bc_RCP2_6_in50", "bc_RCP4_5_in50", "bc_RCP6_0in50", "bc_RCP8_5_in50",
                                           "he_RCP2_6_in50", "he_RCP4_5_in50", "he_RCP6_0in50", "he_RCP8_5_in50", "mg_RCP2_6_in50", "mg_RCP4_5_in50", "mg_RCP6_0in50", "mg_RCP8_5_in50",
                                           "mi_RCP2_6_in50", "mi_RCP4_5_in50", "mi_RCP6_0in50", "mi_RCP8_5_in50", "no_RCP2_6_in50", "no_RCP4_5_in50", "no_RCP6_0in50", "no_RCP8_5_in50",
                                            "Present", "Elevation_m")

    # turn dataframe into long format
    All.predictions.50.sppolyg.melt <- melt(All.predictions.50.sppolyg.df, id.vars = c("long", "lat", "Elevation_m", "order", "hole", "piece", "id", "group"))

    # classify the data into GCM categories where the variable contains the initials of the GCM (e.g. 'bc', 'mg'). This can be done with a conditional ifelse statement
    # which identifies if a certain string (e.g. 'bc') is contained within a variable. If the string is not, the identify something else (e.g. 'he') and so on
    All.predictions.50.sppolyg.melt$GCM <- ifelse(grepl("bc", All.predictions.50.sppolyg.melt$variable), 'bc - BCC-CSM1-1', 
                                            ifelse(grepl("he", All.predictions.50.sppolyg.melt$variable), 'he - HadGEM2-ES', 
                                                ifelse(grepl("mg", All.predictions.50.sppolyg.melt$variable), 'mg - MRI-CGCM3', 
                                                    ifelse(grepl("mi", All.predictions.50.sppolyg.melt$variable), 'mi - MIROC-ESM-CHEM', 
                                                        ifelse(grepl("no", All.predictions.50.sppolyg.melt$variable), 'no - NorESM1-M', 'GCM_all')))))

    # do the same for RCP scenarios
    All.predictions.50.sppolyg.melt$RCP <- ifelse(grepl("RCP2.6", All.predictions.50.sppolyg.melt$variable), '2.6', 
                                            ifelse(grepl("RCP4.5", All.predictions.50.sppolyg.melt$variable), '4.5', 
                                                ifelse(grepl("RCP6.0", All.predictions.50.sppolyg.melt$variable), '6.0', 
                                                    ifelse(grepl("RCP8.5", All.predictions.50.sppolyg.melt$variable), '8.5', 'RCP_all'))))


    # Plot the data in density distributions per GCM, and save a al plots via a for loop
    # Getting unique GCM names to loop over. This is used to create a different plot for every GCM
    GCMs.50 <- unique(All.predictions.50.sppolyg.melt$GCM)
    # Create an empty list to save plots created. Lists in R are very versatile. They can pretty much store any type of data in them.
    GCMs_boxplots.50 <- list()
    # looping over unique GCMs
        for(i in GCMs.50) {
            GCMs_boxplots.50[[i]] <- ggplot(All.predictions.50.sppolyg.melt %>% filter(GCM == i), aes(x=RCP, y=value, fill=RCP)) + 
                                    geom_boxplot() +
                                    stat_summary(fun=mean, geom="point", color="red", size=4) +   # plot the mean as a red dot
                                    scale_y_continuous(limits = c(-500, 1500)) +
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
        print(GCMs_boxplots.50[[i]])
        # save the plots to home directory. file parameter is used to give plot file name - it can be a complete path of the file name. width and height give dimensions to the file in units = "cm". dpi is dots per inch for the quality of plot
        ggsave(GCMs_boxplots.50[[i]], file=paste0("Plots/Boxplot_", i,".png"), width = 10, height = 10, dpi=300)
        }

        # load cowplot gridExtra packages

        # arrange all plots in one grid next to each other
        GCMs_boxplots.50.all <- do.call("grid.arrange", c(GCMs_boxplots.50[1:5], ncol= 5))
        ggsave(GCMs_boxplots.50.all, file="Plots/GCM_RCP_boxplots.png", width=21, height=5, dpi=300)




    # identify outliers across all RCPs and GCMs
    OutVals_GCM_RCP.50 <- list()
    for(i in 8:length(All.predictions.50.sppolyg.df)){
        OutVals_GCM_RCP.50[[i]] <- boxplot(All.predictions.50.sppolyg.df[[i]], plot=FALSE)$out 
    }

    # identify the outliers within the dataset
    outliers.50 <- list()
    for(ii in 8:length(OutVals_GCM_RCP.50)){
        outliers.50[[ii]] <- All.predictions.50.sppolyg.df[All.predictions.50.sppolyg.df[[ii]] %in% OutVals_GCM_RCP.50[[ii]],]
    }

    All.predictions.50.outliers.df <- plyr::join(All.predictions.50.sppolyg.df, outliers.50[[8]], by = "id")

    # for a better overview, not all GCMs (bc, he, mg, mi, no) are coded here. The name of the GCM can be replaced by others with "ctrl + d" (select GCM and replace name)"
    All.predictions.50.outliers.df <- plyr::join(All.predictions.50.sppolyg.df, ASTER.elevation.sppolyg@data, by = "id")

    # copy the dataframe for reproducability
    All.predictions.50.sppolyg.df.sp <- All.predictions.50.sppolyg.df
    
    # turn longs and lats to spatial coordinates
    coordinates(All.predictions.50.sppolyg.df.sp) <- ~long+lat


    # create grid for plotting
    par(mfrow=c(2,2))
    
    # calculate z-scores for outliers for each GCM and RCP and add columns with outliers to dataframe (z-scores show how far the outliers are away from the mean)
    # afterwards, plot the outliers in space with spplot

    # NOTE: this can surely be done in a loop, but I've done it one by one for now as my loops didn't work for now


    # bc - BCC-CSM1-1
    All.predictions.50.sppolyg.df.sp$Zscore_bc2_6_in50 <- spatialEco::outliers(All.predictions.50.sppolyg.df.sp$bc_RCP2_6_in50) 
        bc.RCP.2.6.spplot.50 <- spplot(All.predictions.50.sppolyg.df.sp, "Zscore_bc2_6_in50", col.regions=cm.colors(10), key.space="right", main="RCP 2.6", at=seq(-16, 14, 2))

    All.predictions.50.sppolyg.df.sp$Zscore_bc4_5_in50 <- spatialEco::outliers(All.predictions.50.sppolyg.df.sp$bc_RCP4_5_in50) 
        bc.RCP.4.5.spplot.50 <- spplot(All.predictions.50.sppolyg.df.sp, "Zscore_bc4_5_in50", col.regions=cm.colors(10), key.space="right", main="RCP 4.5")
    
    All.predictions.50.sppolyg.df.sp$Zscore_bc6_0_in50 <- spatialEco::outliers(All.predictions.50.sppolyg.df.sp$bc_RCP6_0in50) 
        bc.RCP.6.0.spplot.50 <- spplot(All.predictions.50.sppolyg.df.sp, "Zscore_bc6_0_in50", col.regions=cm.colors(10), key.space="right", main="RCP 6.0")
    
    All.predictions.50.sppolyg.df.sp$Zscore_bc8_5in50 <- spatialEco::outliers(All.predictions.50.sppolyg.df.sp$bc_RCP8_5_in50)
        bc.RCP.8.5.spplot.50 <- spplot(All.predictions.50.sppolyg.df.sp, "Zscore_bc8_5in50", col.regions=cm.colors(10), key.space="right", main="RCP 8.5")

    # put all spplots into list
    bc.RCP.all.spplot.50.list <- list(bc.RCP.2.6.spplot.50, bc.RCP.4.5.spplot.50, bc.RCP.6.0.spplot.50, bc.RCP.8.5.spplot.50)

    # arrange all plots in one grid
    bc.RCP.all.spplot.50 <- do.call(grid.arrange, bc.RCP.all.spplot.50.list)



    # he - HadGEM2-ES
    All.predictions.50.sppolyg.df.sp$Zscore_he2_6 <- spatialEco::outliers(All.predictions.50.sppolyg.df.sp$he_RCP2_6_in50) 
        he.RCP.2.6.spplot.50 <- spplot(All.predictions.50.sppolyg.df.sp, "Zscore_he2_6", col.regions=cm.colors(10), key.space="right", main="RCP 2.6", at=seq(-16, 14, 2))

    All.predictions.50.sppolyg.df.sp$Zscore_he4_5 <- spatialEco::outliers(All.predictions.50.sppolyg.df.sp$he_RCP4_5_in50) 
        he.RCP.4.5.spplot.50 <- spplot(All.predictions.50.sppolyg.df.sp, "Zscore_he4_5", col.regions=cm.colors(10), key.space="right", main="RCP 4.5")
    
    All.predictions.50.sppolyg.df.sp$Zscore_he6_0 <- spatialEco::outliers(All.predictions.50.sppolyg.df.sp$he_RCP6_0in50) 
        he.RCP.6.0.spplot.50 <- spplot(All.predictions.50.sppolyg.df.sp, "Zscore_he6_0", col.regions=cm.colors(10), key.space="right", main="RCP 6.0")
    
    All.predictions.50.sppolyg.df.sp$Zscore_he8_5 <- spatialEco::outliers(All.predictions.50.sppolyg.df.sp$he_RCP8_5_in50)
        he.RCP.8.5.spplot.50 <- spplot(All.predictions.50.sppolyg.df.sp, "Zscore_he8_5", col.regions=cm.colors(10), key.space="right", main="RCP 8.5")

    # put all spplots into list
    he.RCP.all.spplot.50.list <- list(he.RCP.2.6.spplot.50, he.RCP.4.5.spplot.50, he.RCP.6.0.spplot.50, he.RCP.8.5.spplot.50)

    # arrange all plots in one grid
    he.RCP.all.spplot.50 <- do.call(grid.arrange, he.RCP.all.spplot.50.list)
    
   

    # mg - MRI-CGCM3
    All.predictions.50.sppolyg.df.sp$Zscore_mg2_6 <- spatialEco::outliers(All.predictions.50.sppolyg.df.sp$mg_RCP2_6_in50) 
        mg.RCP.2.6.spplot.50 <- spplot(All.predictions.50.sppolyg.df.sp, "Zscore_mg2_6", col.regions=cm.colors(10), key.space="right", main="RCP 2.6", at=seq(-16, 14, 2))

    All.predictions.50.sppolyg.df.sp$Zscore_mg4_5 <- spatialEco::outliers(All.predictions.50.sppolyg.df.sp$mg_RCP4_5_in50) 
        mg.RCP.4.5.spplot.50 <- spplot(All.predictions.50.sppolyg.df.sp, "Zscore_mg4_5", col.regions=cm.colors(10), key.space="right", main="RCP 4.5")
    
    All.predictions.50.sppolyg.df.sp$Zscore_mg6_0 <- spatialEco::outliers(All.predictions.50.sppolyg.df.sp$mg_RCP6_0in50) 
        mg.RCP.6.0.spplot.50 <- spplot(All.predictions.50.sppolyg.df.sp, "Zscore_mg6_0", col.regions=cm.colors(10), key.space="right", main="RCP 6.0")
    
    All.predictions.50.sppolyg.df.sp$Zscore_mg8_5 <- spatialEco::outliers(All.predictions.50.sppolyg.df.sp$mg_RCP8_5_in50)
        mg.RCP.8.5.spplot.50 <- spplot(All.predictions.50.sppolyg.df.sp, "Zscore_mg8_5", col.regions=cm.colors(10), key.space="right", main="RCP 8.5")

    # put all spplots into list
    mg.RCP.all.spplot.50.list <- list(mg.RCP.2.6.spplot.50, mg.RCP.4.5.spplot.50, mg.RCP.6.0.spplot.50, mg.RCP.8.5.spplot.50)

    # arrange all plots in one grid
    mg.RCP.all.spplot.50 <- do.call(grid.arrange, mg.RCP.all.spplot.50.list)



    # mi - MIROC-ESM-CHEM
    All.predictions.50.sppolyg.df.sp$Zscore_mi2_6 <- spatialEco::outliers(All.predictions.50.sppolyg.df.sp$mi_RCP2_6_in50) 
        mi.RCP.2.6.spplot.50 <- spplot(All.predictions.50.sppolyg.df.sp, "Zscore_mi2_6", col.regions=cm.colors(10), key.space="right", main="RCP 2.6", at=seq(-16, 14, 2))

    All.predictions.50.sppolyg.df.sp$Zscore_mi4_5 <- spatialEco::outliers(All.predictions.50.sppolyg.df.sp$mi_RCP4_5_in50) 
        mi.RCP.4.5.spplot.50 <- spplot(All.predictions.50.sppolyg.df.sp, "Zscore_mi4_5", col.regions=cm.colors(10), key.space="right", main="RCP 4.5")
    
    All.predictions.50.sppolyg.df.sp$Zscore_mi6_0 <- spatialEco::outliers(All.predictions.50.sppolyg.df.sp$mi_RCP6_0in50) 
        mi.RCP.6.0.spplot.50 <- spplot(All.predictions.50.sppolyg.df.sp, "Zscore_mi6_0", col.regions=cm.colors(10), key.space="right", main="RCP 6.0")
    
    All.predictions.50.sppolyg.df.sp$Zscore_mi8_5 <- spatialEco::outliers(All.predictions.50.sppolyg.df.sp$mi_RCP8_5_in50)
        mi.RCP.8.5.spplot.50 <- spplot(All.predictions.50.sppolyg.df.sp, "Zscore_mi8_5", col.regions=cm.colors(10), key.space="right", main="RCP 8.5")

    # put all spplots into list
    mi.RCP.all.spplot.50.list <- list(mi.RCP.2.6.spplot.50, mi.RCP.4.5.spplot.50, mi.RCP.6.0.spplot.50, mi.RCP.8.5.spplot.50)

    # arrange all plots in one grid
    mi.RCP.all.spplot.50 <- do.call(grid.arrange, mi.RCP.all.spplot.50.list)



    # no - NorESM1-M
    All.predictions.50.sppolyg.df.sp$Zscore_no2_6 <- spatialEco::outliers(All.predictions.50.sppolyg.df.sp$no_RCP2_6_in50) 
        no.RCP.2.6.spplot.50 <- spplot(All.predictions.50.sppolyg.df.sp, "Zscore_no2_6", col.regions=cm.colors(10), key.space="right", main="RCP 2.6", at=seq(-16, 14, 2))

    All.predictions.50.sppolyg.df.sp$Zscore_no4_5 <- spatialEco::outliers(All.predictions.50.sppolyg.df.sp$no_RCP4_5_in50) 
        no.RCP.4.5.spplot.50 <- spplot(All.predictions.50.sppolyg.df.sp, "Zscore_no4_5", col.regions=cm.colors(10), key.space="right", main="RCP 4.5")
    
    All.predictions.50.sppolyg.df.sp$Zscore_no6_0 <- spatialEco::outliers(All.predictions.50.sppolyg.df.sp$no_RCP6_0in50) 
        no.RCP.6.0.spplot.50 <- spplot(All.predictions.50.sppolyg.df.sp, "Zscore_no6_0", col.regions=cm.colors(10), key.space="right", main="RCP 6.0")
    
    All.predictions.50.sppolyg.df.sp$Zscore_no8_5 <- spatialEco::outliers(All.predictions.50.sppolyg.df.sp$no_RCP8_5_in50)
        no.RCP.8.5.spplot.50 <- spplot(All.predictions.50.sppolyg.df.sp, "Zscore_no8_5", col.regions=cm.colors(10), key.space="right", main="RCP 8.5")

    # put all spplots into list
    no.RCP.all.spplot.50.list <- list(no.RCP.2.6.spplot.50, no.RCP.4.5.spplot.50, no.RCP.6.0.spplot.50, no.RCP.8.5.spplot.50)

    # arrange all plots in one grid
    no.RCP.all.spplot.50 <- do.call(grid.arrange, no.RCP.all.spplot.50.list)





    # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # let's look at a data distribution and uncertainties across all RCPs and GCMs by using random sampling, bootstrapping
    # and statistical tests (wilcox test, anova, TukeyHSD)

    # NOTE: again this may be more efficient in a loop. For now I have created dataframes for each GCM and RCP. 
    # You can replace the name of the GCM (bc, he, mg, mi, no) by highlighting the name, using "ctrl + D" to mark 
    # all GCM names one by one and then replace the name
    
    # load a library that lets me compare differences in the datasets
    library(multcompView)
    # compare the tree height datasets based on the four RCP scenarios, and identify differences via statistical testing (ANOVA, TukeyHSD)
    
    bc.GCM.melt.50 <- All.predictions.50.sppolyg.melt %>% filter(GCM == "bc")


    # plot a histogram with the mean and median 
    par(mfrow=c(2,2))

    bc.GCM.melt.50.RCP.2.6 <- bc.GCM.melt.50 %>% filter(RCP == "2.6") 
    hist(bc.GCM.melt.50.RCP.2.6$value, main="Distribution RCP 2.6", freq=FALSE, xlim=c(-500, 1500))
    abline(v = c(mean(bc.GCM.melt.50.RCP.2.6$value), median(bc.GCM.melt.50.RCP.2.6$value)), col=c("green", "blue"), lty=c(2,2), lwd=c(3, 3))

    bc.GCM.melt.50.RCP.4.5 <- bc.GCM.melt.50 %>% filter(RCP == "4.5")
    hist(bc.GCM.melt.50.RCP.4.5$value, main="Distribution RCP 4.5", freq=FALSE, xlim=c(-500, 1500))
    abline(v = c(mean(bc.GCM.melt.50.RCP.4.5$value), median(bc.GCM.melt.50.RCP.4.5$value)), col=c("green", "blue"), lty=c(2,2), lwd=c(3, 3))

    bc.GCM.melt.50.RCP.6.0 <- bc.GCM.melt.50 %>% filter(RCP == "6.0")
    hist(bc.GCM.melt.50.RCP.6.0$value, main="Distribution RCP 6.0", freq=FALSE, xlim=c(-500, 1500))
    abline(v = c(mean(bc.GCM.melt.50.RCP.6.0$value), median(bc.GCM.melt.50.RCP.6.0$value)), col=c("green", "blue"), lty=c(2,2), lwd=c(3, 3))

    bc.GCM.melt.50.RCP.8.5 <- bc.GCM.melt.50 %>% filter(RCP == "8.5")
    hist(bc.GCM.melt.50.RCP.8.5$value, main="Distribution RCP 8.5", freq=FALSE, xlim=c(-500, 1500))
    abline(v = c(mean(bc.GCM.melt.50.RCP.8.5$value), median(bc.GCM.melt.50.RCP.8.5$value)), col=c("green", "blue"), lty=c(2,2), lwd=c(3, 3))
    
    dev.off()
    

    # check data distribution with qqplots
    par(mfrow=c(2,2))
    
    qqnorm(bc.GCM.melt.50.RCP.2.6$value, main="QQ-Plot RCP 2.6")
    qqline(bc.GCM.melt.50.RCP.2.6$value, lwd=2)

    qqnorm(bc.GCM.melt.50.RCP.4.5$value, main="QQ-Plot RCP 4.5")
    qqline(bc.GCM.melt.50.RCP.4.5$value, lwd=2)

    qqnorm(bc.GCM.melt.50.RCP.6.0$value, main="QQ-Plot RCP 6.0")
    qqline(bc.GCM.melt.50.RCP.6.0$value, lwd=2)        

    qqnorm(bc.GCM.melt.50.RCP.8.5$value, main="QQ-Plot RCP 8.5")
    qqline(bc.GCM.melt.50.RCP.8.5$value, lwd=2)

    
    # Now lets randomly sample the data to avoid overfitting from data and associated increases in significance when running ANOVA, Tukey or other statisitcal tests. 
    # We use 2x1000 random samples for each RCP scenario. 
    
    set.seed(10)
    par(mfrow=c(2,2))

    bc.RCP.2.6.rs.combined.50 <- bind_rows(replicate(2, bc.GCM.melt.50.RCP.2.6%>% sample_n(1000), simplify=F), .id="Obs")
    hist(bc.RCP.2.6.rs.combined.50$value, main="Distribution RCP 2.6", freq=FALSE, xlim=c(-500, 1500))
    abline(v = c(mean(bc.RCP.2.6.rs.combined.50$value),median(bc.RCP.2.6.rs.combined.50$value)),  col=c("green", "blue"), lty=c(2,2), lwd=c(3, 3))

    bc.RCP.4.5.rs.combined.50 <- bind_rows(replicate(2, bc.GCM.melt.50.RCP.4.5%>% sample_n(1000), simplify=F), .id="Obs")
    hist(bc.RCP.4.5.rs.combined.50$value, main="Distribution RCP 2.6", freq=FALSE, xlim=c(-500, 1500))
    abline(v = c(mean(bc.RCP.4.5.rs.combined.50$value),median(bc.RCP.4.5.rs.combined.50$value)),  col=c("green", "blue"), lty=c(2,2), lwd=c(3, 3))

    bc.RCP.6.0.rs.combined.50 <- bind_rows(replicate(2, bc.GCM.melt.50.RCP.6.0%>% sample_n(1000), simplify=F), .id="Obs")
    hist(bc.RCP.6.0.rs.combined.50$value, main="Distribution RCP 2.6", freq=FALSE, xlim=c(-500, 1500))
    abline(v = c(mean(bc.RCP.6.0.rs.combined.50$value),median(bc.RCP.6.0.rs.combined.50$value)),  col=c("green", "blue"), lty=c(2,2), lwd=c(3, 3))

    bc.RCP.8.5.rs.combined.50 <- bind_rows(replicate(2, bc.GCM.melt.50.RCP.8.5%>% sample_n(1000), simplify=F), .id="Obs")
    hist(bc.RCP.8.5.rs.combined.50$value, main="Distribution RCP 2.6", freq=FALSE, xlim=c(-500, 1500))
    abline(v = c(mean(bc.RCP.8.5.rs.combined.50$value),median(bc.RCP.8.5.rs.combined.50$value)),  col=c("green", "blue"), lty=c(2,2), lwd=c(3, 3))

    bc.RCP.all.rs.combined.50 <- rbind(bc.RCP.2.6.rs.combined.50, bc.RCP.4.5.rs.combined.50, bc.RCP.6.0.rs.combined.50, bc.RCP.8.5.rs.combined.50)
    

    # perform the wilcox test which can statistically compare two unpaired skewed datasets
    wilcox.test(bc.RCP.2.6.rs.combined.50$value, bc.RCP.4.5.rs.combined.50$value)
    wilcox.test(bc.RCP.2.6.rs.combined.50$value, bc.RCP.6.0.rs.combined.50$value)
    wilcox.test(bc.RCP.2.6.rs.combined.50$value, bc.RCP.8.5.rs.combined.50$value)
    wilcox.test(bc.RCP.4.5.rs.combined.50$value, bc.RCP.6.0.rs.combined.50$value)
    wilcox.test(bc.RCP.4.5.rs.combined.50$value, bc.RCP.8.5.rs.combined.50$value)
    wilcox.test(bc.RCP.6.0.rs.combined.50$value, bc.RCP.8.5.rs.combined.50$value)


    # To double check, we can use bootsrapping (2x 1000) too, which gives us more confidence in assessing the data distibutions
    bc.RCP.2.6.rs.bootstr.50 <- lapply(1:2, function(i) sample(bc.GCM.melt.50.RCP.2.6$value, size=1000, replace = TRUE))
    bc.RCP.4.5.rs.bootstr.50 <- lapply(1:2, function(i) sample(bc.GCM.melt.50.RCP.4.5$value, size=1000, replace = TRUE))
    bc.RCP.6.0.rs.bootstr.50 <- lapply(1:2, function(i) sample(bc.GCM.melt.50.RCP.6.0$value, size=1000, replace = TRUE))
    bc.RCP.8.5.rs.bootstr.50 <- lapply(1:2, function(i) sample(bc.GCM.melt.50.RCP.8.5$value, size=1000, replace = TRUE))


    # perform the wilcox test which can statistically compare two unpaired skewed datasets
    wilcox.test(bc.RCP.2.6.rs.bootstr.50[[2]], bc.RCP.4.5.rs.bootstr.50[[2]]) 
    wilcox.test(bc.RCP.2.6.rs.bootstr.50[[2]], bc.RCP.6.0.rs.bootstr.50[[2]])
    wilcox.test(bc.RCP.2.6.rs.bootstr.50[[2]], bc.RCP.8.5.rs.bootstr.50[[2]])
    wilcox.test(bc.RCP.4.5.rs.bootstr.50[[2]], bc.RCP.6.0.rs.bootstr.50[[2]])
    wilcox.test(bc.RCP.4.5.rs.bootstr.50[[2]], bc.RCP.8.5.rs.bootstr.50[[2]])
    wilcox.test(bc.RCP.6.0.rs.bootstr.50[[2]], bc.RCP.8.5.rs.bootstr.50[[2]])



    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # combine all randomly sampled data.frames

    # turn RCPs into factors
    bc.RCP.8.5.rs.combined.50$RCP_fac <- as.factor(bc.RCP.8.5.rs.combined.50$RCP)
    he.RCP.8.5.rs.combined.50$RCP_fac <- as.factor(he.RCP.8.5.rs.combined.50$RCP)
    mg.RCP.8.5.rs.combined.50$RCP_fac <- as.factor(mg.RCP.8.5.rs.combined.50$RCP)
    mi.RCP.8.5.rs.combined.50$RCP_fac <- as.factor(mi.RCP.8.5.rs.combined.50$RCP)
    no.RCP.8.5.rs.combined.50$RCP_fac <- as.factor(no.RCP.8.5.rs.combined.50$RCP)

    # bind all GCMs
    bc.RCP.rs.combined.50 <- rbind(bc.RCP.2.6.rs.combined.50, bc.RCP.4.5.rs.combined.50, bc.RCP.6.0.rs.combined.50, bc.RCP.8.5.rs.combined.50)
    he.RCP.rs.combined.50 <- rbind(he.RCP.2.6.rs.combined.50, he.RCP.4.5.rs.combined.50, he.RCP.6.0.rs.combined.50, he.RCP.8.5.rs.combined.50)
    mg.RCP.rs.combined.50 <- rbind(mg.RCP.2.6.rs.combined.50, mg.RCP.4.5.rs.combined.50, mg.RCP.6.0.rs.combined.50, mg.RCP.8.5.rs.combined.50)
    mi.RCP.rs.combined.50 <- rbind(mi.RCP.2.6.rs.combined.50, mi.RCP.4.5.rs.combined.50, mi.RCP.6.0.rs.combined.50, mi.RCP.8.5.rs.combined.50)
    no.RCP.rs.combined.50 <- rbind(no.RCP.2.6.rs.combined.50, no.RCP.4.5.rs.combined.50, no.RCP.6.0.rs.combined.50, no.RCP.8.5.rs.combined.50)





    ######## ----------------------------------------------------------------------------------------------------------
    # Run statistical tests


    # ANOVA
    bc.GCM.RCP.aov.50 <- aov(value ~ RCP_fac, data = bc.RCP.rs.combined.50)
    summary(bc.GCM.RCP.aov.50)

    # TukeyHSD (compares differences between RCPs)
    bc.GCM.RCP.aov.50.Tukey <- TukeyHSD(bc.GCM.RCP.aov.50)
    summary(bc.GCM.RCP.aov.50.Tukey)

    # save Tukey results in csv file
    write.csv(bc.GCM.RCP.aov.50.Tukey$RCP, file="Data_Output/bc_ANOVA_TukeyHSD.csv")
    # plot Tukey results
    plot(bc.GCM.RCP.aov.50.Tukey , las=1 , col="brown")


    

    # do the same for the other GCMs
    he.GCM.RCP.aov.50 <- aov(value ~ RCP_fac, data = he.RCP.rs.combined.50)
    summary(he.GCM.RCP.aov.50)
    
    he.GCM.RCP.aov.50.Tukey <- TukeyHSD(he.GCM.RCP.aov.50)
    summary(he.GCM.RCP.aov.50)

    plot(he.GCM.RCP.aov.50.Tukey , las=1 , col="brown")

    write.csv(he.GCM.RCP.aov.50.Tukey$RCP, file="Data_Output/he_ANOVA_TukeyHSD.csv")


    # do the same for the other GCMs
    mg.GCM.RCP.aov.50 <- aov(value ~ RCP_fac, data = mg.RCP.rs.combined.50)
    summary(mg.GCM.RCP.aov.50)
    
    mg.GCM.RCP.aov.50.Tukey <- TukeyHSD(mg.GCM.RCP.aov.50)
    summary(mg.GCM.RCP.aov.50)

    plot(mg.GCM.RCP.aov.50.Tukey , las=1 , col="brown")

    write.csv(mg.GCM.RCP.aov.50.Tukey$RCP, file="Data_Output/mg_ANOVA_TukeyHSD.csv")


    # do the same for the other GCMs
    mi.GCM.RCP.aov.50 <- aov(value ~ RCP_fac, data = mi.RCP.rs.combined.50)
    summary(mi.GCM.RCP.aov.50)

    mi.GCM.RCP.Tukey <- TukeyHSD(mi.GCM.RCP.aov.50)
    summary(mi.GCM.RCP.Tukey)

    plot(mi.GCM.RCP.Tukey , las=1 , col="brown")

    write.csv(mi.GCM.RCP.Tukey$RCP, file="Data_Output/mi_ANOVA_TukeyHSD.csv")


    # do the same for the other GCMs
    no.GCM.RCP.aov.50 <- aov(value ~ RCP_fac, data = no.RCP.rs.combined.50)
    summary(no.GCM.RCP.aov.50)

    no.GCM.RCP.aov.50.Tukey <- TukeyHSD(no.GCM.RCP.aov.50)
    summary(no.GCM.RCP.aov.50)

    plot(no.GCM.RCP.aov.50.Tukey , las=1 , col="brown")

    write.csv(no.GCM.RCP.aov.50.Tukey$RCP, file="Data_Output/no_ANOVA_TukeyHSD.csv")


    # plot all differences in mean levels of RCPs per GCM next to each other. The closer the difference is to 0, the more similar two RCPs are in their data distribution
    par(mfrow=c(2,3))
    plot(bc.GCMs.RCP.aov.50.Tukey, las=1 , col="brown", xlim=c(-25, 60))
    plot(he.GCMs.RCP.aov.50.Tukey, las=1 , col="brown", xlim=c(-25, 60))
    plot(mg.GCMs.RCP.aov.50.Tukey, las=1 , col="brown", xlim=c(-25, 60))
    plot(mi.GCMs.RCP.aov.50.Tukey, las=1 , col="brown", xlim=c(-25, 60))
    plot(no.GCMs.RCP.aov.50.Tukey, las=1 , col="brown", xlim=c(-25, 60))
    
    bc.LABELS <- generate_label_df(bc.GCMs.RCP.aov.50.Tukey, "data$treatment")

    

    # unlink the tempdir on the external harddrive
    unlink('D:/tempdir', recursive=TRUE)



