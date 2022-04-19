#Author: Carl A. Norlen
#Date Created: November 11, 2019
#Date Edited: April 19, 2022
#Purpose: Work on spatial autocorrelation

#Packages to load
p <- c('dplyr','tidyr','ggplot2','ggpubr','segmented', 'patchwork','RColorBrewer','gt', 'gtsummary', 
       'webshot', 'kableExtra', 'broom', 'ggpmisc', 'relaimpo', 'mlr', 'caret', 'stats', 'purrr', 'gstat',
       'sp', 'rgdal', 'raster', 'sf', 'elsa', 'nlme')

#Install a package
#Load packages
lapply(p,require,character.only=TRUE)

#Set working directory
setwd('C:/Users/can02/mystuff/subsequent-drought')

#Increase the memory limit for R. Helps with spatially explicit analyses.
# memory.limit()
memory.limit(32000)
# memory.limit()
#Read in csv data for Regression Data Sets
dir_in <- "D:\\Large_Files\\Landsat"
all.ca <- read.csv(file.path(dir_in, "Regression_all_socal_300m_v23.csv"))
all.ca
#Calculate the difference between SPI48 2002 and SPI48 2015
all.ca$dSPI48 <- abs(all.ca$spi48_09_2015 - all.ca$spi48_09_2002)

#Adding a drought sequence column to the data set
all.ca <- all.ca %>% mutate(drought.sequence = case_when((spi48_09_2002 <= -1.5) & (spi48_09_2015 <= -1.5) & (dSPI48 <= 0.5) ~ 'Both Droughts', 
                     (spi48_09_2015 <= -1.5) & (spi48_09_2002 > spi48_09_2015) & (spi48_09_2002 > -1.5) & (dSPI48 > 0.5) ~ '2nd Drought Only',
                     (spi48_09_2002) <= -1.5 & (spi48_09_2002 < spi48_09_2015) & (spi48_09_2015 > -1.5) & (dSPI48 > 0.5) ~ '1st Drought Only')) 
# all.ca %>% dplyr::select(dNDMI_2004)
#Select columns of data
all.ca.1stDrought <- dplyr::select(all.ca, c(system.index, NDMI_1999, dNDMI_2004, dET_2004, dBiomass_2004, PET_4yr_2002, ppt_4yr_2002, tmax_4yr_2002, ET_4yr_2002, ET_1999, biomass_1999, ADS_2004, spi48_09_2002, elevation, latitude, longitude, USFS_zone, drought.sequence))

#Add the year of the 1999-2002 data
all.ca.1stDrought$drought <- '1999-2002'

#Rename the columns
colnames(all.ca.1stDrought) <- c('pixel.id','NDMI', 'dNDMI', 'dET', 'dBiomass', 'PET_4yr', 'ppt_4yr', 'tmax_4yr', 'ET_4yr', 'ET', 'biomass', 'ADS', 'spi48', 'elevation', 'latitude', 'longitude', 'USFS', 'sequence', 'drought')

#Select columns of the 2012-2015 data
all.ca.2ndDrought <- dplyr::select(all.ca, c(system.index, NDMI_2012, dNDMI_2017, dET_2017, dBiomass_2017, PET_4yr_2015, ppt_4yr_2015, tmax_4yr_2015, ET_4yr_2015, ET_2012, biomass_2012, ADS_2017, spi48_09_2015, elevation, latitude, longitude, USFS_zone, drought.sequence))

#Add the year of the 2012-2015 data
all.ca.2ndDrought$drought <- '2012-2015'

#Rename the columns
colnames(all.ca.2ndDrought) <- c('pixel.id', 'NDMI', 'dNDMI', 'dET', 'dBiomass', 'PET_4yr', 'ppt_4yr', 'tmax_4yr', 'ET_4yr', 'ET', 'biomass', 'ADS', 'spi48', 'elevation', 'latitude', 'longitude', 'USFS', 'sequence', 'drought')

#Combine all the data in one data frame
all.ca.combined <- rbind(all.ca.1stDrought, all.ca.2ndDrought)

#Translate the region code to text
all.ca.combined$region[all.ca.combined$USFS == 261] <- "Sierra Nevada"
all.ca.combined$region[all.ca.combined$USFS == 262] <- "Southern California"

#Convert the ADS data to categorical mortality or no mortality
all.ca.combined <- all.ca.combined %>% mutate(ADS.cat = case_when(
                                          (ADS) >= 5 ~ 1, #mortality
                                          (ADS) < 5 ~ 0)) #no mortality

#Make drought sequence into dummy categorical variables for statistical analysis
all.ca.sample <- all.ca.combined %>% mutate(sequence.f = case_when(
                                   sequence == 'Both Droughts' ~ 0, 
                                   sequence == '2nd Drought Only' ~ 1))

#Make years into dummy variables for statistical analysis
all.ca.sample <- all.ca.sample %>% mutate(drought.f = case_when(
                                    drought == '1999-2002' ~ 0, 
                                    drought == '2012-2015' ~ 1))

#Testing out a spatial data.frame to check for spatial autocorrelation with a semivariogram
#Setting variable for ESPG 5070, proj4 crs
c <- raster::crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

#directory for raster files
socal_dir <- "D:\\Large_Files\\socal"
sub_dir <- "D:\\Subsequent_Drought"

#dNDMI 2004
dndmi.2004 <- raster::raster(file.path(socal_dir, 'dNDMI_2004_bigger_region_300m_v4.tif'))
raster::crs(dndmi.2004) <- c
dndmi.2004.m <- is.na(dndmi.2004)
dndmi.2004.mask <- raster::mask(dndmi.2004, mask = dndmi.2004.m, maskvalue = 1)

#dNDMI 2017
dndmi.2017 <- raster::raster(file.path(socal_dir, 'dNDMI_2017_bigger_region_300m_v4.tif'))
raster::crs(dndmi.2017) <- c
dndmi.2017.m <- is.na(dndmi.2017)
dndmi.2017.mask <- raster::mask(dndmi.2017, mask = dndmi.2017.m, maskvalue = 1)

#dNDMI 2004, 30 meters
dndmi.2004.30m.1 <- raster::raster(file.path(sub_dir, 'dNDMI_2004_bigger_region_30m-1.tif'))
dndmi.2004.30m.2 <- raster::raster(file.path(sub_dir, 'dNDMI_2004_bigger_region_30m-2.tif'))
dndmi.2004.30m <- raster::merge(dndmi.2004.30m.1, dndmi.2004.30m.2)
raster::crs(dndmi.2004.30m) <- c
dndmi.2004.30m.m <- is.na(dndmi.2004.30m, )
dndmi.2004.30m.mask <- raster::mask(dndmi.2004.30m, mask = dndmi.2004.30m.m, maskvalue = 1)

v.2004.30m <- elsa::Variogram(dndmi.2004.30m.mask, width = 300, cutoff = 10000, s = 10000)
v.2004.30m <- gstat::variogram(dndmi.2004.30m.mask)
png(file = 'SFig_17_dNDMI_2004_30m_semivariogram.png', width=12, height=8, units="cm", res=900)
plot(v.2004.30m, ylab = 'Semivariance', xlab = 'Distance (m)', main = 'dNDMI 2004 30-meter Variogram')
dev.off()

#Save merged raster for later.
writeRaster(dndmi.2004.30m, filename=file.path(sub_dir, "dNDMI_2004_bigger_region_30m.tif"), format="GTiff", overwrite=TRUE)

#Save the raster mask for later
writeRaster(dndmi.2004.30m.m, filename=file.path(sub_dir, "dNDMI_2004_bigger_region_30m_mask.tif"), format="GTiff", overwrite=TRUE)

# ca.rast <- raster::rasterFromXYZ(all.ca.combined)
# terra::autocor(ca.rast, global = TRUE, method = 'moran')
# terra::autocor(all.ca.combined, global = TRUE, method = 'moran')
# elsa::Variogr
v.2004 <- elsa::Variogram(dndmi.2004.mask, width = 300, cutoff = 10000, s = 10000)
# v.2004.30m 
png(file = 'SFig_17_dNDMI_2004_semivariogram.png', width=12, height=8, units="cm", res=900)
plot(v.2004, ylab = 'Semivariance', xlab = 'Distance (m)', main = 'dNDMI 2004 Variogram')
dev.off()

# p1 <- ggplot() + geom_line(data = v.2004, mapping = aes(x = distance, y = gamma)) + ylab('Semivariance') + xlab('Distance (m)')
# # crs(v) <- c
#Calculate a variogram
co.2004 <- elsa::correlogram(dndmi.2004.mask, width = 300, cutoff = 10000, s = 10000)

png(file = 'SFig_18_dNDMI_2004_correlogram.png', width=12, height=8, units="cm", res=900)
plot(co.2004, ylab = "Moran's I", xlab = 'Distance (m)', main = 'dNDMI 2004 Correlogram')
dev.off()

#2017 autocorrelation
v.2017 <- elsa::Variogram(dndmi.2017.mask, width = 300, cutoff = 10000, s = 10000)

png(file = 'SFig_19_dNDMI_2017_semivariogram.png', width=12, height=8, units="cm", res=900)
plot(v.2017, ylab = 'Semivariance', xlab = 'Distance (m)', main = 'dNDMI 2017 Variogram')
dev.off()

# p1 <- ggplot() + geom_line(data = v.2004, mapping = aes(x = distance, y = gamma)) + ylab('Semivariance') + xlab('Distance (m)')
# # crs(v) <- c
#Calculate a variogram
co.2017 <- elsa::correlogram(dndmi.2017.mask, width = 300, cutoff = 10000, s = 10000)

png(file = 'SFig_20_dNDMI_2017_correlogram.png', width=12, height=8, units="cm", res=900)
plot(co.2017, ylab = "Moran's I", xlab = 'Distance (m)', main = 'dNDMI 2017 Correlogram')
dev.off()


#Do a spatial plot of the data
# sp::spplot(ca.rast, c('dNDMI'))
# crs(all.ca.combined)
# plot(ca.rast, col = 'dNDMI')
# p_test <- ggplot(data = as.data.frame(v), mapping = aes(x = dist, y = gamma)) + geom_point() + ylim(c(0, 0.003)) + theme_bw() +
          # xlab('Distance (km)') + ylab('Semivariance')

# p_test
# ggsave(filename = 'SFig_17_dNDMI_semivariogram.png', height=8, width= 12, units = 'cm', dpi=900)
# all.ca.combined


data.spatialCor.lm <- lm(dNDMI ~ drought.f * sequence.f + PET_4yr + tmax_4yr + biomass, all.ca.sample)

png(file = 'SFig_21_analysis_of_model.png', width=12, height=12, units="cm", res=900)
par(mfrow = c(2, 2))
plot(data.spatialCor.lm, which = 1:4)
dev.off()

#Make the data a spatial data frame
coordinates(all.ca.sample) <- ~ latitude + longitude
raster::crs(all.ca.sample) <- raster::crs("+proj=longlat")

data.spatialCor.gls <- gls(model = dNDMI ~ drought.f * sequence.f + PET_4yr + tmax_4yr + biomass, data = all.ca.sample, method = "REML")
data.spatialCor.gls
plot(data.spatialCor.gls)

#Do an nlme semi-variogram
plot(nlme:::Variogram(data.spatialCor.gls, form = ~latitude + longitude, resType = "response", maxDist = 10))

plot(gstat::variogram(residuals(data.spatialCor.gls, "normalized") ~
    1, data = all.ca.sample, cutoff = 6))