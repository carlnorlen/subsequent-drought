#Author: Carl A. Norlen
#Date Created: March 29, 2022
#Date Edited: March 29, 2022
#Purpose: Create bar charts to compare different time periods and drought sequences

#Packages to load
p <- c('dplyr','tidyr','ggplot2','ggpubr','segmented', 'patchwork','RColorBrewer','gt', 'gtsummary', 
       'webshot', 'kableExtra', 'broom', 'ggpmisc', 'relaimpo', 'mlr', 'caret', 'stats', 'purrr')

#Load packages
lapply(p,require,character.only=TRUE)

#Set working directory
setwd('C:/Users/can02/mystuff/subsequent-drought')

#Read in csv data for Regression Data Sets
dir_in <- "D:\\Large_Files\\Landsat"
all.ca <- read.csv(file.path(dir_in, "Regression_all_socal_300m_v23.csv"))

#Calculate the difference between SPI48 2002 and SPI48 2015
all.ca$dSPI48 <- abs(all.ca$spi48_09_2015 - all.ca$spi48_09_2002)

#Adding a drought sequence column to the data set
all.ca <- all.ca %>% mutate(drought.sequence = case_when((spi48_09_2002 <= -1.5) & (spi48_09_2015 <= -1.5) & (dSPI48 <= 0.5) ~ 'Both Droughts', 
                                                         (spi48_09_2015 <= -1.5) & (spi48_09_2002 > spi48_09_2015) & (spi48_09_2002 > -1.5) & (dSPI48 > 0.5) ~ '2012-2015 Only',
                                                         (spi48_09_2002) <= -1.5 & (spi48_09_2002 < spi48_09_2015) & (spi48_09_2015 > -1.5) & (dSPI48 > 0.5) ~ '1999-2002 Only')) 

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
colnames(all.ca.2ndDrought) <- c('pixel.id', 'NDMI', 'dNDMI', 'dET', 'dBiomass', 'PET_4yr', 'ppt_4yr', 'tmax_4yr', 'ET_4yr', 'ET', 'biomass', 'ADS', 'spi48', 'elevation', 'latitude', 'longitude', 'USFS', 'sequence','drought')

#Combine all the data in one data frame
all.ca.combined <- rbind(all.ca.1stDrought, all.ca.2ndDrought)

#Translate the region code to text
all.ca.combined$region[all.ca.combined$USFS == 261] <- "Sierra Nevada"
all.ca.combined$region[all.ca.combined$USFS == 262] <- "Southern California"

#Convert the ADS data to categorical mortality or no mortality
all.ca.combined <- all.ca.combined %>% mutate(ADS.cat = case_when(
  (ADS) >= 5 ~ 1, #mortality
  (ADS) < 5 ~ 0)) #no mortality

#Bar chart with FIA Mortality (%) for the two drought sequences and time periods
#Letters to indicate significant differences
# p1_texta <- data.frame(label = c("a", "bc", "b", "c"),
#                        sequence   = c('Both Droughts', 'Both Droughts', '2012-2015 Only', '2012-2015 Only'),
#                        y     = c(39.5, 16, 8, 22.5),
#                        x     = c(1, 2, 1, 2)
# )
# 
# #Letters to indicate sample sizes
# p1_textb <- data.frame(label = c("n = 44", "n = 26", "n = 199", "n = 192"),
#                        sequence   = c('Both Droughts', 'Both Droughts', '2012-2015 Only', '2012-2015 Only'),
#                        y     = c(34.5, 11, 3, 17.5),
#                        x     = c(1, 2, 1, 2)
# )
all.ca.combined

#Create the dNDMI bar plot
p1 <- ggbarplot(filter(all.ca.combined, !is.na(all.ca.combined$sequence) & all.ca.combined$sequence != '1999-2002 Only'), x = "drought", y = "dNDMI", fill = "sequence",
                ylab = expression(atop('Die-off','(dNDMI)')), xlab = F, order = c("1999-2002", "2012-2015"), position = position_dodge(), #stat = "density",
                add = "mean_se", error.plot = "errorbar", alpha = 0.8) + 
  guides(color = "none") + theme_bw() + scale_y_reverse() + 
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("#E66100", "#5D3A9B"), name = 'Drought \nSequence',
                    labels = c("Both Droughts" =  "Both \nDroughts", "2012-2015 Only" = "2012-2015 \nOnly"),
                    aesthetics = "fill") +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = c(0.75, 0.35), legend.text = element_text(size = 6), legend.title = element_text(size = 8),
        legend.direction = "vertical", axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10), plot.margin = unit(c(0.5,0.5,0.5,0.5), "pt")) +
  # geom_text(data = p1_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
  # geom_text(data = p1_textb, mapping = aes(x = x, y = y, label = label), size = 3) +
  geom_text(data = data.frame(label = "Mean \n+/- SE", y = -0.052, x = 1.25, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2) + 
  facet_grid(~ factor(sequence, levels = c('Both Droughts', '2012-2015 Only')))

#Create the biomass bar plot
p2 <- ggbarplot(filter(all.ca.combined, !is.na(all.ca.combined$sequence) & all.ca.combined$sequence != '1999-2002 Only'), x = "drought", y = "biomass", fill = "sequence",
                ylab = expression(atop("Biomass",paste("(Mg ",ha^{-1},")"))), #expression('Biomass \n(Mg ha'^-1*')'), 
                xlab = F, order = c("1999-2002", "2012-2015"), position = position_dodge(), #stat = "density",
                add = "mean_se", error.plot = "errorbar", alpha = 0.8) + 
  guides(color = "none") + theme_bw() + 
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("#E66100", "#5D3A9B"), name = 'Drought \nSequence',
                    labels = c("Both Droughts" =  "Both \nDroughts", "2012-2015 Only" = "2012-2015 \nOnly"),
                    aesthetics = "fill") +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = 'none', legend.text = element_text(size = 6), legend.title = element_text(size = 8),
        legend.direction = "vertical", axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        strip.background = element_blank(), strip.text.x = element_blank(), plot.margin = unit(c(0.5,0.5,0.5,0.5), "pt")) +
  # geom_text(data = p1_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
  # geom_text(data = p1_textb, mapping = aes(x = x, y = y, label = label), size = 3) +
  # geom_text(data = data.frame(label = "Mean \n+/- SE", y = -0.048, x = 1.25, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2) + 
  facet_grid(~ factor(sequence, levels = c('Both Droughts', '2012-2015 Only')))

#Create the four-year Pr-ET bar plto
p3 <- ggbarplot(filter(all.ca.combined, !is.na(all.ca.combined$sequence) & all.ca.combined$sequence != '1999-2002 Only'), x = "drought", y = "PET_4yr", fill = "sequence",
                ylab = expression(atop('Pr-ET', paste('(mm 4',yr^{-1},')'))), xlab = "Time Period", order = c("1999-2002", "2012-2015"), position = position_dodge(), #stat = "density",
                add = "mean_se", error.plot = "errorbar", alpha = 0.8) + 
  guides(color = "none") + theme_bw() + 
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("#E66100", "#5D3A9B"), name = 'Drought \nSequence',
                    labels = c("Both Droughts" =  "Both \nDroughts", "2012-2015 Only" = "2012-2015 \nOnly"),
                    aesthetics = "fill") +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = 'none', legend.text = element_text(size = 6), legend.title = element_text(size = 8),
        legend.direction = "vertical", axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10),
        strip.background = element_blank(), strip.text.x = element_blank(), plot.margin = unit(c(0.5,0.5,0.5,0.5), "pt")) +
  # geom_text(data = p1_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
  # geom_text(data = p1_textb, mapping = aes(x = x, y = y, label = label), size = 3) +
  # geom_text(data = data.frame(label = "Mean \n+/- SE", y = -0.048, x = 1.25, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2) + 
  facet_grid(~ factor(sequence, levels = c('Both Droughts', '2012-2015 Only')))
p3

(p1 / p2 / p3) + plot_annotation(tag_levels = 'a', theme = theme(legend.margin=margin(t = 0, unit='cm'))) + plot_layout(heights = c(1,0.95,1))

#Save figure as a .png file
ggsave(filename = 'Fig3_geospatial_barplot.png', height=14, width=12, units = 'cm', dpi=900)
