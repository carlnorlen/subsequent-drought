#Author: Carl A. Norlen
#Date Created: November 11, 2019
#Date Edited: April 25, 2022
#Purpose: Work on spatial autocorrelation

#Packages to load
p <- c('dplyr','tidyr','ggplot2','ggpubr','segmented', 'patchwork','RColorBrewer','gt', 'gtsummary', 
       'webshot', 'kableExtra', 'broom', 'ggpmisc', 'relaimpo', 'mlr', 'caret', 'stats', 'purrr', 'gstat',
       'sp', 'rgdal', 'raster', 'sf', 'elsa', 'nlme', 'ggspatial')

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



#Do a spatial plot of the data
# sp::spplot(ca.rast, c('dNDMI'))
# crs(all.ca.combined)
# plot(ca.rast, col = 'dNDMI')
# p_test <- ggplot(data = as.data.frame(v), mapping = aes(x = dist, y = gamma)) + geom_point() + ylim(c(0, 0.003)) + theme_bw() +
          # xlab('Distance (km)') + ylab('Semivariance')

# p_test
# ggsave(filename = 'SFig_17_dNDMI_semivariogram.png', height=8, width= 12, units = 'cm', dpi=900)
# all.ca.combined




#Make the data a spatial data frame
# summary(all.ca.sample)
all.ca.filter <- all.ca.sample %>% filter(!is.na(sequence.f)) #Filter out NA values

coordinates(all.ca.filter) <- ~ latitude + longitude
raster::crs(all.ca.filter) <- raster::crs("+proj=longlat")

#Check for spatial autocorrelation
all.ca.filter.lm <- lm(dNDMI ~ drought.f * sequence.f + PET_4yr + tmax_4yr + biomass, all.ca.filter)

png(file = 'SFig_21_analysis_of_model.png', width=12, height=12, units="cm", res=900)
par(mfrow = c(2, 2))
plot(all.ca.filter.lm, which = 1:4)
dev.off()

# all.ca.filter$Resid <- rstandard(data.spatialCor.lm)
# all.ca.filter %>% summary()
# coordinates(data.spatialCor) <- ~LAT + LONG  #effectively convert the data into a spatial data frame
# bubble(all.ca.filter, "Resid")
# ggplot() + coord_sf() + 
# p_map <- geom_point(data = as.data.frame(all.ca.filter), mapping = aes(y = latitude, x = longitude, color = Resid), alpha = 0.2, size = 0.1) +
# scale_color_viridis_c()
# ggsave(filename = 'SFig_spatial_model_residuals.png', height=15, width= 10, units = 'cm', dpi=900)

#Do the GLS model without incorporating the lats and longs
# data.spatialCor.gls <- gls(model = dNDMI ~ drought.f * sequence.f + PET_4yr + tmax_4yr + biomass, data = all.ca.filter, method = "REML")
# data.spatialCor.gls
# summary(data.spatialCor.gls)
# plot(data.spatialCor.gls)

#Do an nlme semi-variogram
# plot(nlme:::Variogram(data.spatialCor.gls, form = ~latitude + longitude, resType = "response", maxDist = 10))
# png(file = 'SFig_17_gstat_semivariogram_300m.png', width=12, height=8, units="cm", res=900)
rstandard(all.ca.filter.lm)

#Calculate teh variogram values for the standardized linear model residuals
var.lm <- gstat::variogram(rstandard(all.ca.filter.lm) ~ 1, data = all.ca.filter, cutoff = 6)
# dev.off()
var.lm %>% as.data.frame()


# summary(all.ca.filter)
#Do an lme model with random effects for location
#LME random effects model doesn't work.
all.ca.filter.lme <- lme(fixed = dNDMI ~  drought.f * sequence.f + PET_4yr + tmax_4yr + biomass, 
                         data = as.data.frame(all.ca.filter),
                         random = ~ 1 | pixel.id, #Trying out the random effects, this is within subjects, there are two periods for each pixel
                         method = "ML")
# summary(all.ca.filter.lme)

# png(file = 'SFig_gstat_lme_semivariogram_300m.png', width=12, height=8, units="cm", res=900)
var.lme <- gstat::variogram(residuals(all.ca.filter.lme, type = "normalized") ~ 1, data = all.ca.filter, cutoff = 6)
var.lme
#Do a semi-variogram plto
p.var.lm <- ggplot() + geom_point(data = var.lm, mapping = aes(x = dist, y = gamma)) + #Linear model data
  geom_line(data = var.lm, mapping = aes(x = dist, y = gamma)) + #Linear model data
  # geom_point(data = var.lme, mapping = aes(x = dist, y = gamma), color = 'gray') + #Linear model data
  # geom_line(data = var.lme, mapping = aes(x = dist, y = gamma), color = 'gray') + #Linear model data
  geom_hline(yintercept = 0, linetype = 'dashed', size = 1) +
  # geom_vline(xintercept = 0.3, linetype = 'solid', size = 0.5, color = 'red') +
  theme_bw() + ylim(0,1) + xlab('Distance (km)') + ylab('Semivariance')

p.var.lm

ggsave(filename = 'SFig_variogram_lm_residuals.png', height=8, width= 12, units = 'cm', dpi=900)
# plot(gstat::variogram(residuals(all.ca.filter.lme, type = "normalized") ~
#                         1, data = all.ca.filter, cutoff = 5), xlab = 'Distance (km)', ylab = 'Semivariance', main = 'Variogram')
# dev.off()



# all.ca.filter.gls.corGaus <- gls(model = dNDMI ~  drought.f * sequence.f + PET_4yr + tmax_4yr + biomass, 
#                          data = as.data.frame(all.ca.filter),
#                          correlation = corGaus(form = ~ latitude + longitude | drought.f, nugget = TRUE),
#                          method = "REML")
# summary(all.ca.filter.gls.corGaus)

#Apply the spatial model to the data
# all.ca.filter.corGaus <- lme(fixed = dNDMI ~  drought.f * sequence.f + PET_4yr + tmax_4yr + biomass, 
#                              data = as.data.frame(all.ca.filter),
#                              na.action = na.omit,
#                              random = ~ 1 | pixel.id, #Trying out the random effects, this is within subjects, there are two periods for each pixel
#                              correlation = corExp(form = ~ latitude + longitude | pixel.id / drought.f), #Add spatial correlation
#                              method = "REML")
# summary(all.ca.filter.corGaus)

# Initialize(object = corGaus, data = as.data.frame(all.ca.filter))
#Do a map of residual values
# all.ca.filter$Resid.lme <- residuals(all.ca.filter.lme)
# all.ca.filter %>% summary()


# coordinates(data.spatialCor) <- ~LAT + LONG  #effectively convert the data into a spatial data frame
# bubble(all.ca.filter, "Resid")
# p_map <- ggplot() + coord_sf() + 
#          geom_point(data = as.data.frame(all.ca.filter), mapping = aes(y = latitude, x = longitude, color = Resid.lme), alpha = 0.2, size = 0.1) +
#          scale_color_viridis_c()
# 
# p_map

ggsave(filename = 'SFig_lme_model_residuals.png', height=15, width= 10, units = 'cm', dpi=900)

#Do a Gstat variogram with lme random effects added
p.lme <- plot(gstat::variogram(residuals(all.ca.filter.lme, type = "normalized") ~
                        1, data = all.ca.filter, cutoff = 5), xlab = 'Distance (km)', ylab = 'Semivariance', main = 'Variogram')

ggsave(filename = 'SFig_lme_semiveriogram.png', height=15, width= 10, units = 'cm', dpi=900)

#Check different ways to correct for spatial autocorrelation
# cs1Exp <- corExp(1, form = ~ east + north)
# cs1Exp <- Initialize(cs1Exp, spdata)
# corMatrix(cs1Exp)[1:10, 1:4]

# data.spatialCor.glsExp <- gls(model = dNDMI ~ drought.f * sequence.f + PET_4yr + tmax_4yr + biomass, data = all.ca.filter,
#                               correlation = corExp(form = ~latitude + longitude | drought.f / sequence.f, nugget = TRUE),
#                               method = "REML")
# data.spatialCor.glsGaus <- gls(model = dNDMI ~ drought.f * sequence.f + PET_4yr + tmax_4yr + biomass, data = all.ca.filter,
#                                correlation = corGaus(form = ~latitude + longitude),
#                                method = "ML")
# data.spatialCor.glsLin <- gls(model = dNDMI ~ drought.f * sequence.f + PET_4yr + tmax_4yr + biomass, data = all.ca.filter,
#                               correlation = corLin(form = ~latitude + longitude, nugget = TRUE),
#                               method = "REML")
# data.spatialCor.glsRatio <- gls(model = dNDMI ~ drought.f * sequence.f + PET_4yr + tmax_4yr + biomass, data = all.ca.filter,
#                                 correlation = corRatio(form = ~latitude + longitude, nugget = TRUE),
#                                 method = "REML")
# data.spatialCor.glsSpher <- gls(model = dNDMI ~ drought.f * sequence.f + PET_4yr + tmax_4yr + biomass, data = all.ca.filter,
#                                 correlation = corSpher(form = ~latitude + longitude, nugget = TRUE),
#                                 method = "REML")

#Compare the AIC values for the different models
AIC(data.spatialCor.gls, data.spatialCor.glsExp, data.spatialCor.glsGaus,
    data.spatialCor.glsLin, data.spatialCor.glsRatio,
    data.spatialCor.glsSpher)

#Raster based spatial autocorrelation, Is it even working?
#dNDMI 2004
#Testing out a spatial data.frame to check for spatial autocorrelation with a semivariogram
#Setting variable for ESPG 5070, proj4 crs
#directory for raster files
socal_dir <- "D:\\Large_Files\\socal"
sub_dir <- "D:\\Subsequent_Drought"

c <- raster::crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

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