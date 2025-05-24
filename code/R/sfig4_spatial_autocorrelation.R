#Author: Carl A. Norlen
#Date Created: November 11, 2019
#Date Edited: September 9, 2022
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
dir_in <- "D:\\Subsequent_Drought"
dir_in <- "D:\\Large_Files\\Landsat"
all.ca <- read.csv(file.path(dir_in, "Regression_all_socal_300m_v23_v3.csv"))

#Calculate the difference between SPI48 2002 and SPI48 2015
all.ca$dSPI48 <- abs(all.ca$spi48_09_2015 - all.ca$spi48_09_2002)

#Adding a drought sequence column to the data set
all.ca <- all.ca %>% mutate(drought.sequence = case_when((spi48_09_2002 <= -1.5) & (spi48_09_2015 <= -1.5) & (dSPI48 <= 0.5) ~ 'Both Droughts', 
                                                         (spi48_09_2015 <= -1.5) & (spi48_09_2002 > spi48_09_2015) & (spi48_09_2002 > -1.5) & (dSPI48 > 0.5) ~ '2nd Drought Only',
                                                         (spi48_09_2002) <= -1.5 & (spi48_09_2002 < spi48_09_2015) & (spi48_09_2015 > -1.5) & (dSPI48 > 0.5) ~ '1st Drought Only')) 

#Check if PET_4yr_2009 is postive or negative
all.ca %>% filter(drought.sequence == 'Both Droughts') %>% dplyr::select(PET_4yr_2009, PET_4yr_2002, PET_4yr_2015) %>% 
  summarize(PET_2002.mean = mean(PET_4yr_2002), PET_2009.mean = mean(PET_4yr_2009), PET_2015.mean = mean(PET_4yr_2015))

#Select columns of data
all.ca.1stDrought <- dplyr::select(all.ca, c(system.index, dNDMI_2004, PET_4yr_2002, ppt_4yr_2002, tmax_4yr_2002, ET_4yr_2002, biomass_1999, ADS_2004, spi48_09_2002, elevation, latitude, longitude, USFS_zone, drought.sequence))

#Add the year of the 1999-2002 data
all.ca.1stDrought$drought <- '1999-2002'

#Rename the columns
colnames(all.ca.1stDrought) <- c('pixel.id', 'dNDMI', 'PET_4yr', 'ppt_4yr', 'tmax_4yr', 'ET_4yr', 'biomass', 'ADS', 'spi48', 'elevation', 'latitude', 'longitude', 'USFS', 'sequence', 'drought')

#Select columns of the 2012-2015 data
all.ca.2ndDrought <- dplyr::select(all.ca, c(system.index, dNDMI_2017, PET_4yr_2015, ppt_4yr_2015, tmax_4yr_2015, ET_4yr_2015, biomass_2012, ADS_2017, spi48_09_2015, elevation, latitude, longitude, USFS_zone, drought.sequence))

#Add the year of the 2012-2015 data
all.ca.2ndDrought$drought <- '2012-2015'

#Rename the columns
colnames(all.ca.2ndDrought) <- c('pixel.id', 'dNDMI', 'PET_4yr', 'ppt_4yr', 'tmax_4yr', 'ET_4yr', 'biomass', 'ADS', 'spi48', 'elevation', 'latitude', 'longitude', 'USFS', 'sequence', 'drought')

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


#Make the data a spatial data frame
all.ca.filter <- all.ca.sample %>% filter(!is.na(sequence.f)) #Filter out NA values

coordinates(all.ca.filter) <- ~ latitude + longitude
raster::crs(all.ca.filter) <- raster::crs("+proj=longlat")

#Check for spatial autocorrelation
all.ca.filter.lm <- lm(dNDMI ~ drought.f * sequence.f + PET_4yr + tmax_4yr + biomass, all.ca.filter)

#Calculate teh variogram values for the standardized linear model residuals
var.lm <- gstat::variogram(rstandard(all.ca.filter.lm) ~ 1, data = all.ca.filter, cutoff = 6)

#Set the random seed for the random sample
set.seed(1234)

all.ca.prop <- all.ca.sample %>% filter(!is.na(sequence.f)) %>% group_by(sequence.f) %>% slice_sample(prop = 0.05, replace = F) %>% ungroup()
all.ca.test <- all.ca.sample %>% filter(!is.na(sequence.f)) %>% group_by(sequence.f) %>% slice_sample(prop = 0.05, replace = F) %>% ungroup()

coordinates(all.ca.prop) <- ~ latitude + longitude
raster::crs(all.ca.prop) <- raster::crs("+proj=longlat")

#Check for spatial autocorrelation
all.ca.prop.lm <- lm(dNDMI ~ drought.f * sequence.f + PET_4yr + tmax_4yr + biomass, all.ca.prop)
summary(all.ca.prop.lm)
#Calculate teh variogram values for the standardized linear model residuals
var.prop.lm <- gstat::variogram(rstandard(all.ca.prop.lm) ~ 1, data = all.ca.prop, cutoff = 6)

#Define the colore
cols <- c("All" = "black", "5% Sample" ="gray")

p.var.prop.lm <- ggplot() + 
  #Full Data set
  geom_point(data = var.lm, mapping = aes(x = dist, y = gamma, color = 'All')) + #Linear model data
  geom_line(data = var.lm, mapping = aes(x = dist, y = gamma, color = 'All'), linetype = 'dashed') + #Linear model data
  #Sampled data set
  geom_point(data = var.prop.lm, mapping = aes(x = dist, y = gamma, color = '5% Sample')) + #Linear model data
  geom_line(data = var.prop.lm, mapping = aes(x = dist, y = gamma, color = '5% Sample'), linetype = 'solid') + #Linear model data
  geom_hline(yintercept = 0, linetype = 'solid', size = 0.5) +
  scale_colour_manual(name="Data",values=cols, aesthetics = 'color') +
  theme_bw() + theme(legend.background = element_rect(colour = NA, fill = NA), # This removes the white square behind the legend
                               legend.position = c(0.85, 0.2)) + #Presentation text sizes.) + 
  ylim(0,1) + xlab('Distance (km)') + ylab('Semivariance')

p.var.prop.lm
ggsave(filename = 'SFig16_variogram_lm_sampled_residuals.png', height=8, width= 12, units = 'cm', dpi=900)

#Add the data
#Filter the data into subsets for modeling
all.ca.test.both.1999 <- all.ca.test %>% filter(sequence == 'Both Droughts' & drought == '1999-2002' & !is.na(sequence))
all.ca.test.both.2012 <- all.ca.test %>% filter(sequence == 'Both Droughts' & drought == '2012-2015' & !is.na(sequence)) 
all.ca.test.second.1999 <- all.ca.test %>% filter(sequence == '2nd Drought Only' & drought == '1999-2002' & !is.na(sequence))
all.ca.test.second.2012 <- all.ca.test %>% filter(sequence == '2nd Drought Only' & drought == '2012-2015' & !is.na(sequence))

# #Linear Models for dNDMI ~ Pr-ET
#Models for Both Droughts
all.ca.test.both.1999.lm <- lm(data = all.ca.test.both.1999, dNDMI ~ PET_4yr) # 1999-2002 Model
all.ca.test.both.2012.lm <- lm(data = all.ca.test.both.2012, dNDMI ~ PET_4yr) # 2012-2015 Model

#Models for 2012-2015 Only
all.ca.test.second.1999.lm <- lm(data = all.ca.test.second.1999, dNDMI ~ PET_4yr) # 1999-2002 Model
all.ca.test.second.2012.lm <- lm(data = all.ca.test.second.2012, dNDMI ~ PET_4yr) # 2012-2015 Model

#Calculate the sgemented models
all.ca.test.both.1999.seg <- segmented(all.ca.test.both.1999.lm)
all.ca.test.both.2012.seg <- segmented(all.ca.test.both.2012.lm)
all.ca.test.second.1999.seg <- segmented(all.ca.test.second.1999.lm)
all.ca.test.second.2012.seg <- segmented(all.ca.test.second.2012.lm)

#Add predicted dNDMI values
all.ca.test.both.1999$dNDMI_predict = predict(all.ca.test.both.1999.seg)
all.ca.test.both.2012$dNDMI_predict = predict(all.ca.test.both.2012.seg )
all.ca.test.second.1999$dNDMI_predict = predict(all.ca.test.second.1999.seg)
all.ca.test.second.2012$dNDMI_predict = predict(all.ca.test.second.2012.seg)

#Add the segmented fits and Standard Errors
#Fits
all.ca.test.both.1999$dNDMI.fit = broken.line(all.ca.test.both.1999.seg)$fit
all.ca.test.both.2012$dNDMI.fit = broken.line(all.ca.test.both.2012.seg )$fit
all.ca.test.second.1999$dNDMI.fit = broken.line(all.ca.test.second.1999.seg)$fit
all.ca.test.second.2012$dNDMI.fit = broken.line(all.ca.test.second.2012.seg)$fit

#SE fit
all.ca.test.both.1999$dNDMI.se.fit = broken.line(all.ca.test.both.1999.seg)$se.fit
all.ca.test.both.2012$dNDMI.se.fit = broken.line(all.ca.test.both.2012.seg )$se.fit
all.ca.test.second.1999$dNDMI.se.fit = broken.line(all.ca.test.second.1999.seg)$se.fit
all.ca.test.second.2012$dNDMI.se.fit = broken.line(all.ca.test.second.2012.seg)$se.fit



#Recombine the data frames with the model fitted dNDMI as a column
all.ca.test.models <- rbind(all.ca.test.both.1999, all.ca.test.both.2012, all.ca.test.second.1999, all.ca.test.second.2012)

#R-Squared values for the four models
test.r2.a  <- format(summary(all.ca.test.both.1999.seg)$r.squared, digits = 2) #I could switch this back to segmented
test.r2.b <- format(summary(all.ca.test.both.2012.seg)$r.squared, digits = 2)
test.r2.c <- format(summary(all.ca.test.second.1999.seg)$r.squared, digits = 2)
test.r2.d <- format(summary(all.ca.test.second.2012.seg)$r.squared, digits = 2) #I could switch this back to segmented

#Create a data.frame of R.squared values
test.r2.text <- data.frame(
  label = c(as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = test.r2.a)))), 
            as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = test.r2.b)))),
            as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = test.r2.c)))),
            as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = test.r2.d))))
  ),
  sequence = c('Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only'),
  drought = c('1999-2002', '2012-2015', '1999-2002', '2012-2015'),
  x = c(2500, 2500, 2500, 2500),
  y = c(-0.25, -0.25, -0.25, -0.25)
)

test.letter.text <- data.frame(label = c("a)", "b)", "c)", "d)"),
                          sequence   = c('Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only'),
                          drought = c('1999-2002', '2012-2015', '1999-2002',  '2012-2015'),
                          y     = c(-0.3, -0.3, -0.3, -0.3),
                          x     = c(-2400, -2400, -2400, -2400)
)




#Testing out the model with the points selected at random.
p1 <- ggscatter(all.ca.test.models, x = "PET_4yr", y = "dNDMI", point = FALSE) +
  geom_bin2d(binwidth = c(100, 0.0075)) +
  geom_line(data = all.ca.test.models, mapping = aes(x=PET_4yr, y=dNDMI.fit), size=2, linetype = 'dotdash', color = 'black') +
  geom_ribbon(data = all.ca.test.models, mapping = aes(x = PET_4yr, y = dNDMI.fit, ymax = dNDMI.fit + 1.96*dNDMI.se.fit, ymin = dNDMI.fit - 1.96*dNDMI.se.fit), alpha = 0.4) +
  # geom_smooth(method = 'lm', color = 'black', size = 2) +
  # geom_smooth(method = 'lm', formula = y ~ x, color = 'black', size = 2, se = FALSE, na.rm = TRUE) +
  # stat_cor(aes(label = paste(..rr.label..)), size = 3.5, digits = 2, label.x.npc = 0.75, label.y.npc = 0.9) +
  theme_bw() +
  ylab(label = "Die-off During Period (dNDMI)") +  xlab(label = expression('Cummulative Moisture Deficit (Pr-ET; mm 4 yr'^-1*')')) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_text(data = test.r2.text, mapping = aes(x = x, y = y, label = label), size = 3.5, parse = TRUE) +
  geom_text(data = test.letter.text, mapping = aes(x = x, y = y, label = label), size = 5, fontface = "bold") +
  labs(fill = "Grid Cells") +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 10, hjust = 0.5), strip.text.x = element_text(size = 10, face = 'bold'), strip.text.y = element_text(size = 10, face = 'bold')) +
  scale_fill_gradient2(limits = c(0,42), breaks = c(10,20,30,40), midpoint = 21, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  ylim(0.1, -0.3) + xlim(-2500, 3500) +
  facet_grid(factor(sequence, levels = c('Both Droughts', '2nd Drought Only')) ~ drought,
             labeller = as_labeller(c('1999-2002'='Response During 1st Period', '2012-2015'='Response During 2nd Period',
                                      'Both Droughts' = 'Exposed to Both Droughts', '2nd Drought Only' = 'Exposed to 2nd Drought Only')))

#Add a shared legend in a customized position on the figure
p2 <- p1 + theme(
  legend.background = element_rect(colour = NA, fill = NA), # This removes the white square behind the legend
  legend.justification = c(1, 0),
  legend.position = c(0.68, 0.8),
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 10),
  legend.direction = "vertical") +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 3,
                               title.position = "top", 
                               title.hjust = 0.5, 
                               ticks.colour = "black"))

p2

ggsave(filename = 'SFig17_dNDMI_PET_4yr_regression_sampled_residuals.png', height=16, width=16, units = 'cm', dpi=900)
