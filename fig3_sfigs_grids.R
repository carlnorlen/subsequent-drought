#Author: Carl A. Norlen
#Date Created: November 11, 2019
#Date Edited: January 14, 2022
#Purpose: Create Figures 3 and 5 for publication

#Packages to load
p <- c('grid', 'gridExtra','dplyr', 'tidyr','ggplot2','ggpubr',
       'patchwork','RColorBrewer','gt', 'gtsummary', 'webshot', 
       'stargazer', 'kableExtra', 'broom', 'svglite','sjPlot','purrr', 'magick', 'magrittr', 
       'knitr', 'xtable', 'mgcv')
# Load packages
lapply(p,require,character.only=TRUE)

#Set working directory
setwd('C:/Users/can02/mystuff/subsequent-drought')

#Directory to load data from
dir_in <- "D:\\Large_Files\\Landsat"

#Load CSV data into the script.
all.ca <- read.csv(file.path(dir_in, "Regression_all_socal_300m_v23.csv"))

#Calculate the difference in SPI48 between 2002 and 2015
all.ca$dSPI48 <- abs(all.ca$spi48_09_2015 - all.ca$spi48_09_2002)

#Sort pixels by drought sequence
all.ca <- all.ca %>% mutate(drought.sequence = case_when((spi48_09_2002 <= -1.5) & (spi48_09_2015 <= -1.5) & (dSPI48 <= 0.5) ~ 'Both Droughts', 
                                                         (spi48_09_2015 <= -1.5) & (spi48_09_2002 > spi48_09_2015) & (spi48_09_2002 > -1.5) & (dSPI48 > 0.5) ~ '2012-2015 Only',
                                                         (spi48_09_2002) <= -1.5 & (spi48_09_2002 < spi48_09_2015) & (spi48_09_2015 > -1.5) & (dSPI48 > 0.5) ~ '1999-2002 Only'))

#Add Categorical ADS mortality data data (I had an error here)
all.ca <- all.ca %>% mutate(ADS_2004.cat = case_when(
  (ADS_2004) >= 5 ~ 1, #Mortality 
  (ADS_2004) < 5 ~ 0), #No Mortality
  ADS_2017.cat = case_when(
    (ADS_2017) >= 2 ~ 1, #Mortality 
    (ADS_2017) < 2 ~ 0)) #No Mortality

#Do binning by SPI48 and Pr-ET over four years.
all.ca.spi48 <- all.ca %>%
  dplyr::mutate(socal = as.integer(USFS_zone == 262), sierra = as.integer(USFS_zone == 261)) %>% #Make new columns that have 0,1 for Sierra and socal to calculate proportions later
  dplyr::mutate(spi48_09_2002.bin = cut(spi48_09_2002, breaks = seq(-3.5, 2, by = 0.1)),
         spi48_09_2015.bin = cut(spi48_09_2015, breaks = seq(-3.5, 2, by = 0.1)),
  ) %>%
  dplyr::group_by(spi48_09_2002.bin, spi48_09_2015.bin) %>%
  dplyr::mutate(dNDMI_1994.mean = mean(dNDMI_1994)) %>%
  dplyr::mutate(dNDMI_2004.mean = mean(dNDMI_2004)) %>%
  dplyr::mutate(dNDMI_2017.mean = mean(dNDMI_2017)) %>%
  dplyr::mutate(PET_4yr_1992.mean = mean(PET_4yr_1992)) %>%
  dplyr::mutate(PET_4yr_2002.mean = mean(PET_4yr_2002)) %>%
  dplyr::mutate(PET_4yr_2015.mean = mean(PET_4yr_2015)) %>%
  dplyr::mutate(biomass_1999.mean = mean(biomass_1999)) %>%
  dplyr::mutate(biomass_2012.mean = mean(biomass_2012)) %>%
  dplyr::mutate(tmax_4yr_2002.mean = mean(tmax_4yr_2002)) %>%
  dplyr::mutate(tmax_4yr_2015.mean = mean(tmax_4yr_2015)) %>%
  dplyr::mutate(dNDMI_2004.median = median(dNDMI_2004)) %>%
  dplyr::mutate(dNDMI_2017.median = median(dNDMI_2017)) %>%
  dplyr::mutate(ADS_2004.mean = mean(ADS_2004)) %>%
  dplyr::mutate(ADS_2017.mean = mean(ADS_2017)) %>%
  dplyr::mutate(count = n()) %>%
  dplyr::mutate(socal_count = sum(socal), sierra_count = sum(sierra)) %>%
  dplyr::mutate(socal_prop = (socal_count / count) * 100, sierra_prop = (sierra_count / count) * 100) %>% # Calculate Sierra and SoCal proportions.
  dplyr::mutate(dominant_region = as.integer(socal_prop > sierra_prop)) %>%
  dplyr::mutate(ADS_2004.count = sum(ADS_2004.cat), ADS_2017.count = sum(ADS_2017.cat)) %>%
  dplyr::mutate(ADS_2004.total = count, ADS_2017.total = count) %>%
  dplyr::mutate(ADS_2004.prop = (ADS_2004.count / ADS_2004.total) * 100, ADS_2017.prop = (ADS_2017.count / ADS_2017.total) * 100) %>%
  ungroup()

both.socal <- all.ca.spi48 %>% filter(drought.sequence == 'Both Droughts') %>% select(socal) %>% sum()
both.count <- all.ca.spi48 %>% filter(drought.sequence == 'Both Droughts') %>% select(socal) %>% count()
both.socal / both.count

second.socal <- all.ca.spi48 %>% filter(drought.sequence == '2012-2015 Only') %>% select(sierra) %>% sum()
second.count <- all.ca.spi48 %>% filter(drought.sequence == '2012-2015 Only') %>% select(sierra) %>% count()
second.socal / second.count

#Add labels for plots
all.ca.spi48$both <- '1999-2002'
all.ca.spi48$second <- '2012-2015'

#Plot dNDMI 2004 with SPI48 2002 by SPI48 2015 grid
p1 <- ggplot(subset(all.ca.spi48, count >= 20), mapping = aes(x = spi48_09_2002, y = spi48_09_2015, fill = dNDMI_2004.mean, group = dNDMI_2004.mean)) +
	  geom_bin2d(mapping = aes(group = dNDMI_2004.mean), binwidth = c(0.1, 0.1)) + theme_bw() + 
	  # theme(legend.position="bottom", legend.text = element_text(size=6)) + 
	  ylim(-3.5, 0) + xlim(-3, 2) +
	  ylab(NULL) +  xlab(NULL) + 
  geom_vline(xintercept = 0, size = 0.25) + 
  geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = -1.5, size = 1, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 1, color = 'black', linetype='dashed') +
    guides(fill = guide_colorbar(barwidth = 5, barheight = 1, title.position = "top", title.hjust = 0.5, ticks.colour = "black"), alpha = "none") +
    theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.x = element_text(size = 10), 
          axis.title.y = element_text(size = 10), plot.title = element_text(size = 10, hjust = 0.5), legend.background = element_rect(colour = NA, fill = NA), 
          legend.justification = c(1, 0), legend.position = c(0.89, 0.6), legend.text = element_text(size = 6), 
          legend.title = element_text(size = 8), legend.direction = "horizontal", strip.text = element_text(size = 10)) +  
	  scale_fill_gradient2(name = "Die-off (dNDMI)", limits = c(-0.16, 0.07), midpoint = 0, low = "#D41159", mid = "lightyellow1", high = "#1A85FF") +
	  # scale_alpha(range = c(0.1, 1), breaks = c(500,1000,1500,2000)) +
    facet_wrap(~ both) #+ scale_alpha(range = c(1, 1), limits = c(20, 2950), na.value = 0.8)

#Plot dNDMI 2004 with SPI48 2002 by SPI48 2015 grid
p2 <-ggplot(subset(all.ca.spi48, count >= 20), mapping = aes(x = spi48_09_2002, y = spi48_09_2015, fill = dNDMI_2017.mean, group = dNDMI_2017.mean)) +
	  geom_bin2d(mapping = aes(group = dNDMI_2017.mean), binwidth = c(0.1, 0.1)) + theme_bw() + 
	  # theme(legend.position="bottom", legend.text = element_text(size=6)) + 
	  ylim(-3.5, 0) + xlim(-3, 2) +
	  ylab(NULL) +  xlab(NULL) +  
  geom_vline(xintercept = 0, size = 0.25) + 
  geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
    guides(fill = "none", alpha = "none") +  
    theme(axis.text.x = element_text(size = 8), axis.text.y = element_blank(), axis.title.x = element_text(size = 10),  
          axis.title.y = element_text(size = 10), strip.text = element_text(size = 10)) +  
	  scale_fill_gradient2(name = "Die-off (dNDMI)", limits = c(-0.16, 0.07), midpoint = 0, low = "#D41159", mid = "lightyellow1", high = "#1A85FF") +
	  # scale_alpha(range = c(0.1, 1), breaks = c(500,1000,1500,2000)) +
  annotate("text", x = 1, y = -0.5, label = "Neither \nDrought", size = 3) + annotate("text", x = -2.4, y = -0.5, label = "1992-2002 \nOnly", size = 3) +
  annotate("text", x = -2.4, y = -3, label = "Both \nDroughts", size = 3) + annotate("text", x = 1, y = -2, label = "2012-2015 \nOnly", size = 3) + 
  facet_wrap( ~ second) #+ scale_alpha(range = c(1, 1), limits = c(20, 2950), na.value = 0.8)
	  
f1 <- ggarrange(p1, p2, ncol = 2, nrow = 1, widths = c(1, 0.95), common.legend = FALSE)
# f1

#Add shared Y label and x-label
annotate_figure(f1, left = textGrob(label = "SPI48 2012-2015", rot = 90, hjust = 0.5, vjust = 0.3), 
                    bottom = textGrob(label = 'SPI48 1999-2002', vjust = 0.1, hjust = 0.5)) 

#Save the figure as a png
ggsave(filename = 'Fig3_spi48_dNDMI_SPI48_grid_scatter.png', device = 'png', height=7, width=14, units = 'cm', dpi=900)

#Pr-ET (mm/4yr) SPI48 grid cells
p3 <- ggplot(subset(all.ca.spi48, count >= 20), mapping = aes(x = spi48_09_2002, y = spi48_09_2015, fill = PET_4yr_2002.mean, group = PET_4yr_2002.mean)) +
  geom_bin2d(mapping = aes(group = PET_4yr_2002.mean), binwidth = c(0.1, 0.1)) + theme_bw() + 
  ylim(-3.5, 0) + xlim(-3, 2) +
  ylab(NULL) +  xlab(NULL) + 
  geom_vline(xintercept = 0, size = 0.25) + 
  geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  guides(fill = guide_colorbar(barwidth = 5, barheight = 1, title.position = "top", title.hjust = 0.5, ticks.colour = "black")) +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA), 
        legend.justification = c(1, 0), legend.position = c(0.92, 0.6), legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8), legend.direction = "horizontal", strip.text = element_text(size = 10)) + 
  scale_fill_gradient2(name = expression("Pr-ET (mm 4yr"^-1*")"), limits = c(-1700, 3000), midpoint = 0, low = "#D41159", mid = "lightyellow1", high = "#1A85FF") +
  facet_wrap(~ both)#+ scale_alpha(range = c(1, 1), limits = c(20, 2950), na.value = 0.2)

p4 <-ggplot(subset(all.ca.spi48, count >= 20), mapping = aes(x = spi48_09_2002, y = spi48_09_2015, fill = PET_4yr_2015.mean, group = PET_4yr_2015.mean)) +
  geom_bin2d(mapping = aes(group = PET_4yr_2015.mean), binwidth = c(0.1, 0.1)) + theme_bw() +  
  ylim(-3.5, 0) + xlim(-3, 2) +
  ylab(NULL) +  xlab(NULL) +  
  geom_vline(xintercept = 0, size = 0.25) + 
  geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  # geom_abline(slope = 1, intercept = 0, linetype='dashed', size = 1, color = 'gray') +
  guides(fill = "none") +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_blank(), axis.title.x = element_text(size = 10),  
        axis.title.y = element_text(size = 10), strip.text = element_text(size = 10)) +
  scale_fill_gradient2(name = expression("Pr-ET (mm 4yr"^-1*")"), limits = c(-1700, 3000), midpoint = 0, low = "#D41159", mid = "lightyellow1", high = "#1A85FF") +
  annotate("text", x = 1, y = -0.5, label = "Neither \nDrought", size = 3) + annotate("text", x = -2.4, y = -0.5, label = "1992-2002 \nOnly", size = 3) +
  annotate("text", x = -2.4, y = -3, label = "Both \nDroughts", size = 3) + annotate("text", x = 1, y = -2, label = "2012-2015 \nOnly", size = 3) +
  facet_wrap( ~ second) #+ scale_alpha(range = c(1, 1), limits = c(20, 2950), na.value = 0.2)

f2 <- ggarrange(p3, p4, ncol = 2, nrow = 1, widths = c(1, 0.95), common.legend = FALSE)
f2

#Add shared x-axis and y-axis labels
annotate_figure(f2, left = textGrob(label = "SPI48 2012-2015", rot = 90, hjust = 0.5, vjust = 0.3), bottom = textGrob(label = 'SPI48 1999-2002', vjust = 0.1, hjust = 0.5))

#Save the figure as a png
ggsave(filename = 'SFig2_spi48_PrET_4yr_SPI48_grid_scatter.png', device = 'png', height=7, width=14, units = 'cm', dpi=900)

#Create a figure of the sample size
p5 <- ggplot(all.ca.spi48, mapping = aes(x = spi48_09_2002, y = spi48_09_2015, fill = count, group = count)) +
  geom_bin2d(mapping = aes(group = count, alpha = ..count..), binwidth = c(0.1, 0.1)) + theme_bw() + 
  theme(legend.position="bottom", legend.text = element_text(size=6)) + 
  ylim(-3.5, 0) + xlim(-3, 2) +
  ylab('SPI48 2012-2015') +  xlab('SPI48 1999-2002') + 
  geom_vline(xintercept = 0, size = 0.25) + 
  geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  guides(fill = guide_colorbar(barwidth = 5, barheight = 1, title.position = "top", title.hjust = 0.5, ticks.colour = "black"), alpha = FALSE) +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA), 
        legend.justification = c(1, 0), legend.position = c(0.99, 0.6), legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8), legend.direction = "horizontal", strip.text = element_text(size = 12)) +  
  scale_fill_gradient2(name = "Grid Cells", limits = c(0,2950), midpoint = 1475, low = "cornflowerblue", 
                       mid = "yellow", high = "red", na.value = 'transparent') +
  scale_alpha(range = c(1, 1), limits = c(20, 2950), na.value = 0.4)
p5

#Save the figure as a png
ggsave(filename = 'SFig5_spi48_count_SPI48_grid_scatter.png', device = 'png', height=6, width=7, units = 'cm', dpi=900)

#ADS proportion SPI48 grid cells
p6 <- ggplot(subset(all.ca.spi48, count >= 20), mapping = aes(x = spi48_09_2002, y = spi48_09_2015, fill = ADS_2004.prop, group = ADS_2004.prop)) +
  geom_bin2d(mapping = aes(group = ADS_2004.prop), binwidth = c(0.1, 0.1)) + theme_bw() + 
  ylim(-3.5, 0) + xlim(-3, 2) +
  ylab(NULL) +  xlab(NULL) + 
  geom_vline(xintercept = 0, size = 0.25) + 
  geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  guides(fill = guide_colorbar(barwidth = 5, barheight = 1, title.position = "top", title.hjust = 0.5, ticks.colour = "black")) +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA), 
        legend.justification = c(1, 0), legend.position = c(0.92, 0.6), legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8), legend.direction = "horizontal", strip.text = element_text(size = 10)) + 
  scale_fill_gradient(name = "Mortality (% Grid Cells)", limits = c(0, 100), high = "#D41159", low = "lightyellow1", na.value = 'transparent') +
  facet_wrap(~ both)#+ scale_alpha(range = c(1, 1), limits = c(20, 2950), na.value = 0.2)

p7 <-ggplot(subset(all.ca.spi48, count >= 20), mapping = aes(x = spi48_09_2002, y = spi48_09_2015, fill = ADS_2017.prop, group = ADS_2017.prop)) +
  geom_bin2d(mapping = aes(group = ADS_2017.prop), binwidth = c(0.1, 0.1)) + theme_bw() +  
  ylim(-3.5, 0) + xlim(-3, 2) +
  ylab(NULL) +  xlab(NULL) +  
  geom_vline(xintercept = 0, size = 0.25) + 
  geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  # geom_abline(slope = 1, intercept = 0, linetype='dashed', size = 1, color = 'gray') +
  guides(fill = "none") +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_blank(), axis.title.x = element_text(size = 10),  
        axis.title.y = element_text(size = 10), strip.text = element_text(size = 10)) +
  scale_fill_gradient(name = "Mortality (% Grid Cells)", limits = c(0, 100), high = "#D41159", low = "lightyellow1", na.value = 'transparent') +
  annotate("text", x = 1, y = -0.5, label = "Neither \nDrought", size = 3) + annotate("text", x = -2.4, y = -0.5, label = "1992-2002 \nOnly", size = 3) +
  annotate("text", x = -2.4, y = -3, label = "Both \nDroughts", size = 3) + annotate("text", x = 1, y = -2, label = "2012-2015 \nOnly", size = 3) +
  facet_wrap( ~ second) #+ scale_alpha(range = c(1, 1), limits = c(20, 2950), na.value = 0.2)

f3 <- ggarrange(p6, p7, ncol = 2, nrow = 1, widths = c(1, 0.95), common.legend = FALSE)
f3

annotate_figure(f3, left = textGrob(label = "SPI48 2012-2015", rot = 90, hjust = 0.5, vjust = 0.3), bottom = textGrob(label = 'SPI48 1999-2002', vjust = 0.1, hjust = 0.5))

ggsave(filename = 'SFig1_ADS_prop_SPI48_grid_scatter.png', device = 'png', height=7, width=14, units = 'cm', dpi=900)

#Biomass (Mg/ha) SPI48 grid cells
p8 <- ggplot(subset(all.ca.spi48, count >= 20), mapping = aes(x = spi48_09_2002, y = spi48_09_2015, fill = biomass_1999.mean, group = biomass_1999.mean)) +
  geom_bin2d(mapping = aes(group = biomass_1999.mean), binwidth = c(0.1, 0.1)) + theme_bw() + 
  ylim(-3.5, 0) + xlim(-3, 2) +
  ylab(NULL) +  xlab(NULL) + 
  geom_vline(xintercept = 0, size = 0.25) + 
  geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  guides(fill = guide_colorbar(barwidth = 5, barheight = 1, title.position = "top", title.hjust = 0.5, ticks.colour = "black")) +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA), 
        legend.justification = c(1, 0), legend.position = c(0.96, 0.6), legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8), legend.direction = "horizontal", strip.text = element_text(size = 10)) + 
  scale_fill_gradient(name = expression("Biomass (Mg ha"^-1*")"), limits = c(10, 280), breaks = c(100,200), low = 'brown', high = 'green', na.value = 'transparent') +
  facet_wrap(~ both)#+ scale_alpha(range = c(1, 1), limits = c(20, 2950), na.value = 0.2)

p9 <-ggplot(subset(all.ca.spi48, count >= 20), mapping = aes(x = spi48_09_2002, y = spi48_09_2015, fill = biomass_2012.mean, group = biomass_2012.mean)) +
  geom_bin2d(mapping = aes(group = biomass_2012.mean), binwidth = c(0.1, 0.1)) + theme_bw() +  
  ylim(-3.5, 0) + xlim(-3, 2) +
  ylab(NULL) +  xlab(NULL) +  
  geom_vline(xintercept = 0, size = 0.25) + 
  geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  # geom_abline(slope = 1, intercept = 0, linetype='dashed', size = 1, color = 'gray') +
  guides(fill = "none") +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_blank(), axis.title.x = element_text(size = 10),  
        axis.title.y = element_text(size = 10), strip.text = element_text(size = 10)) +
  scale_fill_gradient(name = expression("Biomass (Mg ha"^-1*")"), limits = c(10, 280), breaks = c(100,200), low = 'brown', high = 'green', na.value = 'transparent') +
  annotate("text", x = 1, y = -0.5, label = "Neither \nDrought", size = 3) + annotate("text", x = -2.4, y = -0.5, label = "1992-2002 \nOnly", size = 3) +
  annotate("text", x = -2.4, y = -3, label = "Both \nDroughts", size = 3) + annotate("text", x = 1, y = -2, label = "2012-2015 \nOnly", size = 3) +
  facet_wrap( ~ second) #+ scale_alpha(range = c(1, 1), limits = c(20, 2950), na.value = 0.2)

f4 <- ggarrange(p8, p9, ncol = 2, nrow = 1, widths = c(1, 0.95), common.legend = FALSE)
f4

annotate_figure(f4, left = textGrob(label = "SPI48 2012-2015", rot = 90, hjust = 0.5, vjust = 0.3), bottom = textGrob(label = 'SPI48 1999-2002', vjust = 0.1, hjust = 0.5))

ggsave(filename = 'SFig3_biomass_SPI48_grid_scatter.png', device = 'png', height=7, width=14, units = 'cm', dpi=900)

#4 year Max Tempeature SPI48 grid cells
p10 <- ggplot(subset(all.ca.spi48, count >= 20), mapping = aes(x = spi48_09_2002, y = spi48_09_2015, fill = tmax_4yr_2002.mean, group = tmax_4yr_2002.mean)) +
  geom_bin2d(mapping = aes(group = tmax_4yr_2002.mean), binwidth = c(0.1, 0.1)) + theme_bw() + 
  ylim(-3.5, 0) + xlim(-3, 2) +
  ylab(NULL) +  xlab(NULL) + 
  geom_vline(xintercept = 0, size = 0.25) + 
  geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  guides(fill = guide_colorbar(barwidth = 5, barheight = 1, title.position = "top", title.hjust = 0.5, ticks.colour = "black")) +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA), 
        legend.justification = c(1, 0), legend.position = c(0.955, 0.6), legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8), legend.direction = "horizontal", strip.text = element_text(size = 10)) + 
  scale_fill_gradient(name = "Temperature (C)", limits = c(6, 22), low = "blue", high = "red", na.value = 'transparent') +
  facet_wrap(~ both)#+ scale_alpha(range = c(1, 1), limits = c(20, 2950), na.value = 0.2)

p11 <-ggplot(subset(all.ca.spi48, count >= 20), mapping = aes(x = spi48_09_2002, y = spi48_09_2015, fill = tmax_4yr_2015.mean, group = tmax_4yr_2015.mean)) +
  geom_bin2d(mapping = aes(group = tmax_4yr_2015.mean), binwidth = c(0.1, 0.1)) + theme_bw() +  
  ylim(-3.5, 0) + xlim(-3, 2) +
  ylab(NULL) +  xlab(NULL) +  
  geom_vline(xintercept = 0, size = 0.25) + 
  geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  # geom_abline(slope = 1, intercept = 0, linetype='dashed', size = 1, color = 'gray') +
  guides(fill = "none") +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_blank(), axis.title.x = element_text(size = 10),  
        axis.title.y = element_text(size = 10), strip.text = element_text(size = 10)) +
  scale_fill_gradient(name = "Temperature (C)", limits = c(6, 22), low = "blue", high = "red", na.value = 'transparent') +
  annotate("text", x = 1, y = -0.5, label = "Neither \nDrought", size = 3) + annotate("text", x = -2.4, y = -0.5, label = "1992-2002 \nOnly", size = 3) +
  annotate("text", x = -2.4, y = -3, label = "Both \nDroughts", size = 3) + annotate("text", x = 1, y = -2, label = "2012-2015 \nOnly", size = 3) +
  facet_wrap( ~ second) #+ scale_alpha(range = c(1, 1), limits = c(20, 2950), na.value = 0.2)

f5 <- ggarrange(p10, p11, ncol = 2, nrow = 1, widths = c(1, 0.95), common.legend = FALSE)
f5

annotate_figure(f5, left = textGrob(label = "SPI48 2012-2015", rot = 90, hjust = 0.5, vjust = 0.3), bottom = textGrob(label = 'SPI48 1999-2002', vjust = 0.1, hjust = 0.5))

ggsave(filename = 'SFig4_tmax_4yr_SPI48_grid_scatter.png', device = 'png', height=7, width=14, units = 'cm', dpi=900)

#Prepare data for a dNDMI comparison
all.ca.dndmi <- all.ca %>%
  dplyr::mutate(socal = as.integer(USFS_zone == 262), sierra = as.integer(USFS_zone == 261)) %>% #Make new columns that have 0,1 for Sierra and socal to calculate proportions later
  dplyr::mutate(spi48_09_2002.bin = cut(spi48_09_2002, breaks = seq(-3.5, 2, by = 0.1)),
                spi48_09_2015.bin = cut(spi48_09_2015, breaks = seq(-3.5, 2, by = 0.1)),
  ) %>%
  dplyr::group_by(spi48_09_2002.bin, spi48_09_2015.bin) %>%
  # dplyr::mutate(dNDMI_2004.bin = cut(dNDMI_2004, breaks = seq(-0.3, 0.7, by = 0.0075)),
  #               dNDMI_2017.bin = cut(dNDMI_2017, breaks = seq(-0.3, 0.7, by = 0.0075)),
  # ) %>%
  # dplyr::group_by(dNDMI_2004.bin, dNDMI_2017.bin) %>%
  #dplyr::mutate(dNDMI_1994.mean = mean(dNDMI_1994)) %>%
  dplyr::mutate(dNDMI_2004.mean = mean(dNDMI_2004)) %>%
  dplyr::mutate(dNDMI_2017.mean = mean(dNDMI_2017)) %>%
  #dplyr::mutate(PET_4yr_1992.mean = mean(PET_4yr_1992)) %>%
  dplyr::mutate(PET_4yr_2002.mean = mean(PET_4yr_2002)) %>%
  dplyr::mutate(PET_4yr_2015.mean = mean(PET_4yr_2015)) %>%
  dplyr::mutate(biomass_1999.mean = mean(biomass_1999)) %>%
  dplyr::mutate(biomass_2012.mean = mean(biomass_2012)) %>%
  dplyr::mutate(tmax_4yr_2002.mean = mean(tmax_4yr_2002)) %>%
  dplyr::mutate(tmax_4yr_2015.mean = mean(tmax_4yr_2015)) %>%
  #dplyr::mutate(dNDMI_2004.median = median(dNDMI_2004)) %>%
  #dplyr::mutate(dNDMI_2017.median = median(dNDMI_2017)) %>%
  dplyr::mutate(ADS_2004.mean = mean(ADS_2004)) %>%
  dplyr::mutate(ADS_2017.mean = mean(ADS_2017)) %>%
  dplyr::mutate(count = n()) %>%
  dplyr::mutate(socal_count = sum(socal), sierra_count = sum(sierra)) %>%
  dplyr::mutate(socal_prop = (socal_count / count) * 100, sierra_prop = (sierra_count / count) * 100) %>% # Calculate Sierra and SoCal proportions.
  dplyr::mutate(dominant_region = as.integer(socal_prop > sierra_prop)) %>%
  dplyr::mutate(ADS_2004.count = sum(ADS_2004.cat), ADS_2017.count = sum(ADS_2017.cat)) %>%
  dplyr::mutate(ADS_2004.total = count, ADS_2017.total = count) %>%
  dplyr::mutate(ADS_2004.prop = (ADS_2004.count / ADS_2004.total) * 100, ADS_2017.prop = (ADS_2017.count / ADS_2017.total) * 100) #%>%
#ungroup()
# all.ca.dndmi$drought.sequence

#Both droughts dNDMI comparison,doesn't really seem worth it
p12 <-ggplot(filter(all.ca.dndmi, drought.sequence != '1999-2002 Only' & !is.na(drought.sequence) ), 
             mapping = aes(x = dNDMI_2004.mean, y = dNDMI_2017.mean)) +
  geom_point(size = 1, shape = 1) + 
  theme_bw() +  
  ylim(-0.15, 0.05) + xlim(-0.15, 0.05) +
  ylab('dNDMI 2017') +  xlab('dNDMI 2004') +  
  geom_vline(xintercept = 0, size = 0.25) + 
  geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  # geom_abline(slope = 1, intercept = 0, linetype='dashed', size = 1, color = 'gray') +
  #guides(fill = "none") +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.x = element_text(size = 10),  
        axis.title.y = element_text(size = 10), strip.text = element_text(size = 10)) +
  stat_smooth(method = 'lm', formula = y ~ x) +
  stat_cor(aes(label = paste(..rr.label..,  sep = "~")), label.x = 0.01, label.y = 0.03, size = 3) + facet_grid(~drought.sequence)
# scale_fill_gradient2(limits = c(10,120), breaks = c(10,50,100), 
#                      midpoint = 60, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent')
# scale_fill_gradient(name = "Temperature (C)", limits = c(6, 22), low = "blue", high = "red", na.value = 'transparent') +
# annotate("text", x = 1, y = -0.5, label = "Neither \nDrought", size = 3) + annotate("text", x = -2.4, y = -0.5, label = "1992-2002 \nOnly", size = 3) +
# annotate("text", x = -2.4, y = -3, label = "Both \nDroughts", size = 3) + annotate("text", x = 1, y = -2, label = "2012-2015 \nOnly", size = 3) +
# facet_wrap( ~ second) #+ scale_alpha(range = c(1, 1), limits = c(20, 2950), na.value = 0.2)
p12

ggsave(filename = 'SFig11_dNDMI_scatter.png', device = 'png', height=7, width=14, units = 'cm', dpi=900)

#What is the sample size?
length(filter(all.ca.dndmi, drought.sequence != '1999-2002 Only' & !is.na(drought.sequence)))

#Single Panel dNDMI comparison
p13 <-ggplot(filter(all.ca.dndmi, drought.sequence != '1999-2002 Only' & !is.na(drought.sequence)), # 
             mapping = aes(x = dNDMI_2004.mean, y = dNDMI_2017.mean)) +
  geom_point(size = 1, shape = 1) + 
  theme_bw() +  
  ylim(-0.15, 0.05) + xlim(-0.15, 0.05) +
  ylab('dNDMI 2017') +  xlab('dNDMI 2004') +  
  geom_vline(xintercept = 0, size = 0.25) + 
  geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  # geom_abline(slope = 1, intercept = 0, linetype='dashed', size = 1, color = 'gray') +
  #guides(fill = "none") +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.x = element_text(size = 10),  
        axis.title.y = element_text(size = 10), strip.text = element_text(size = 10)) +
  stat_smooth(method = 'lm', formula = y ~ x) +
  stat_cor(aes(label = paste(..rr.label..,  sep = "~")), label.x = 0.01, label.y = 0.03, size = 3) #+ facet_grid(~drought.sequence)
# scale_fill_gradient2(limits = c(10,120), breaks = c(10,50,100), 
#                      midpoint = 60, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent')
# scale_fill_gradient(name = "Temperature (C)", limits = c(6, 22), low = "blue", high = "red", na.value = 'transparent') +
# annotate("text", x = 1, y = -0.5, label = "Neither \nDrought", size = 3) + annotate("text", x = -2.4, y = -0.5, label = "1992-2002 \nOnly", size = 3) +
# annotate("text", x = -2.4, y = -3, label = "Both \nDroughts", size = 3) + annotate("text", x = 1, y = -2, label = "2012-2015 \nOnly", size = 3) +
# facet_wrap( ~ second) #+ scale_alpha(range = c(1, 1), limits = c(20, 2950), na.value = 0.2)
p13

ggsave(filename = 'SFig12_dNDMI_scatter.png', device = 'png', height=7, width = 8, units = 'cm', dpi=900)