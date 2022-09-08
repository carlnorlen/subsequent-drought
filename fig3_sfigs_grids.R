#Author: Carl A. Norlen
#Date Created: November 11, 2019
#Date Edited: September 8, 2022
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

#Add Categorical ADS mortality data data 
all.ca <- all.ca %>% mutate(ADS_2004.cat = case_when(
  (ADS_2004) >= 8 ~ 1, #Mortality 
  (ADS_2004) < 8 ~ 0), #No Mortality
  ADS_2017.cat = case_when(
    (ADS_2017) >= 8 ~ 1, #Mortality 
    (ADS_2017) < 8 ~ 0)) #No Mortality

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

#Calculate proportion of Both Droughts in Socal
both.socal <- all.ca.spi48 %>% filter(drought.sequence == 'Both Droughts') %>% select(socal) %>% sum()
both.count <- all.ca.spi48 %>% filter(drought.sequence == 'Both Droughts') %>% select(socal) %>% count()
both.socal / both.count

#Calculate proportion of 2012-2015 Only in Sierra Nevada
second.socal <- all.ca.spi48 %>% filter(drought.sequence == '2012-2015 Only') %>% select(sierra) %>% sum()
second.count <- all.ca.spi48 %>% filter(drought.sequence == '2012-2015 Only') %>% select(sierra) %>% count()
second.socal / second.count

#Add columns for labeling plots.
all.ca.spi48$both <- 'Response During 1st Period'#'1999-2002'
all.ca.spi48$second <- 'Response During 2nd Period'#'2012-2015'

#Plot dNDMI 2004 with SPI48 2002 by SPI48 2015 grid
p1 <- ggplot(subset(all.ca.spi48, count >= 20), mapping = aes(x = spi48_09_2002, y = spi48_09_2015, fill = dNDMI_2004.mean, group = dNDMI_2004.mean)) +
	  geom_bin2d(mapping = aes(group = dNDMI_2004.mean), binwidth = c(0.1, 0.1)) + theme_bw() + 
	  ylim(-3.5, 0) + xlim(-3, 2) +
	  ylab("SPI48 During 2nd Period") +  xlab('SPI48 During 1st Period') + 
  geom_vline(xintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
    guides(fill = guide_colorbar(barwidth = 5, barheight = 1, title.position = "top", title.hjust = 0.5, ticks.colour = "black", reverse = T), alpha = "none") +
    theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.x = element_text(size = 10), 
          axis.title.y = element_text(size = 10), plot.title = element_text(size = 10, hjust = 0.5), legend.background = element_rect(colour = NA, fill = NA), 
          legend.justification = c(1, 0), legend.position = c(0.895, 0.67), legend.text = element_text(size = 6), 
          legend.title = element_text(size = 8), legend.direction = "horizontal", strip.text = element_text(size = 10, face = 'bold'),
          plot.margin = unit(c(0,0,0,0), "pt")) +  
	  scale_fill_gradient2(name = "Die-off (dNDMI)", limits = c(-0.16, 0.07), midpoint = 0, low = "#D41159", mid = "lightyellow1", high = "#1A85FF") +
    facet_wrap(~ both) + 
    annotate(geom="text", x=-1, y=-0.52, label="Less\nDie-off", color="black", size = 2) + 
    annotate(geom="text", x=1.9, y=-0.52, label="More\nDie-off", color="black", size = 2) +
    annotate(geom="text", x = -2.9, y = -3.5, label="bold(Drier)", size = 2, parse = TRUE) + 
    annotate(geom="text", x = 1.75, y = -3.5, label ="bold(Wetter)", size = 2, parse = TRUE) 

#Plot dNDMI 2017 with SPI48 2002 by SPI48 2015 grid
p2 <-ggplot(subset(all.ca.spi48, count >= 20), mapping = aes(x = spi48_09_2002, y = spi48_09_2015, fill = dNDMI_2017.mean, group = dNDMI_2017.mean)) +
	  geom_bin2d(mapping = aes(group = dNDMI_2017.mean), binwidth = c(0.1, 0.1)) + theme_bw() + 
	  ylim(-3.5, 0) + xlim(-3, 2) +
	  ylab(NULL) +  xlab('SPI48 During 1st Period') +  
  # geom_vline(xintercept = 0, size = 0.25) + 
  # geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
    guides(fill = "none", alpha = "none") +  
    theme(axis.text.x = element_text(size = 8), axis.text.y = element_blank(), axis.title.x = element_text(size = 10),  
          axis.title.y = element_blank(), strip.text = element_text(size = 10, face = 'bold'), plot.margin = unit(c(0,0,0,20), "pt")) +  
	  scale_fill_gradient2(name = "Die-off (dNDMI)", limits = c(-0.16, 0.07), midpoint = 0, low = "#D41159", mid = "lightyellow1", high = "#1A85FF") +
  annotate("text", x = -0.5, y = -0.5, label = "Neither \nDrought", size = 3) + annotate("text", x = -2.4, y = -0.5, label = "1st Drought \nOnly", size = 3) +
  annotate("text", x = -2.4, y = -3, label = "Both \nDroughts", size = 3) + annotate("text", x = 1, y = -2, label = "2nd Drought \nOnly", size = 3) + 
  facet_wrap( ~ second) 

#Combine the two figure panels into one	  
f1 <- ggarrange(p1, p2, ncol = 2, nrow = 1, widths = c(1, 0.975), labels = c('a)', 'b)'), align = 'h', common.legend = FALSE)
f1

#Save the figure as a png
ggsave(filename = 'Fig3_spi48_dNDMI_SPI48_grid_scatter.png', device = 'png', height=7, width=15, units = 'cm', dpi=900)

#Plot 2004 ADS mortality proportion for SPI48 2002 versus SPI48 2015
p3 <- ggplot(subset(all.ca.spi48, count >= 20), mapping = aes(x = spi48_09_2002, y = spi48_09_2015, fill = ADS_2004.prop, group = ADS_2004.prop)) +
  geom_bin2d(mapping = aes(group = ADS_2004.prop), binwidth = c(0.1, 0.1)) + theme_bw() + 
  ylim(-3.5, 0) + xlim(-3, 2) +
  ylab(NULL) +  xlab(NULL) + 
  # geom_vline(xintercept = 0, size = 0.25) + 
  # geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  guides(fill = guide_colorbar(barwidth = 5, barheight = 1, title.position = "top", title.hjust = 0.5, ticks.colour = "black")) +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA), 
        legend.justification = c(1, 0), legend.position = c(0.92, 0.6), legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8), legend.direction = "horizontal", strip.text = element_text(size = 10)) + 
  scale_fill_gradient(name = "Mortality (% Grid Cells)", limits = c(0, 100), high = "#D41159", low = "lightyellow1", na.value = 'transparent') +
  facet_wrap(~ both)

#Plot 2017 ADS mortality proportion for SPI48 2002 versus SPI48 2015
p4 <-ggplot(subset(all.ca.spi48, count >= 20), mapping = aes(x = spi48_09_2002, y = spi48_09_2015, fill = ADS_2017.prop, group = ADS_2017.prop)) +
  geom_bin2d(mapping = aes(group = ADS_2017.prop), binwidth = c(0.1, 0.1)) + theme_bw() +  
  ylim(-3.5, 0) + xlim(-3, 2) +
  ylab(NULL) +  xlab(NULL) +  
  # geom_vline(xintercept = 0, size = 0.25) + 
  # geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  guides(fill = "none") +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_blank(), axis.title.x = element_text(size = 10),  
        axis.title.y = element_text(size = 10), strip.text = element_text(size = 10)) +
  scale_fill_gradient(name = "Mortality (% Grid Cells)", limits = c(0, 100), high = "#D41159", low = "lightyellow1", na.value = 'transparent') +
  annotate("text", x = 1, y = -0.5, label = "Neither \nDrought", size = 3) + annotate("text", x = -2.4, y = -0.5, label = "1st Drought \nOnly", size = 3) +
  annotate("text", x = -2.4, y = -3, label = "Both \nDroughts", size = 3) + annotate("text", x = 1, y = -2, label = "2nd Drought \nOnly", size = 3) + 
  facet_wrap( ~ second) 

#Combine the two figure panels into one	
f2 <- ggarrange(p3, p4, ncol = 2, nrow = 1, widths = c(1, 0.95), common.legend = FALSE)

#Add shared Y label and x-label
annotate_figure(f2, left = textGrob(label = "SPI48 2012-2015", rot = 90, hjust = 0.5, vjust = 0.3), bottom = textGrob(label = 'SPI48 1999-2002', vjust = 0.1, hjust = 0.5))

#Save the figure as a png
ggsave(filename = 'SFig1_ADS_prop_SPI48_grid_scatter.png', device = 'png', height=7, width=14, units = 'cm', dpi=900)

#Plot 1999-2002 Pr-ET (mm/4yr) separate by SPI48 2002 versus SPI48 2015
p5 <- ggplot(subset(all.ca.spi48, count >= 20), mapping = aes(x = spi48_09_2002, y = spi48_09_2015, fill = PET_4yr_2002.mean, group = PET_4yr_2002.mean)) +
  geom_bin2d(mapping = aes(group = PET_4yr_2002.mean), binwidth = c(0.1, 0.1)) + theme_bw() + 
  ylim(-3.5, 0) + xlim(-3, 2) +
  ylab(NULL) +  xlab(NULL) + 
  # geom_vline(xintercept = 0, size = 0.25) + 
  # geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  guides(fill = guide_colorbar(barwidth = 5, barheight = 1, title.position = "top", title.hjust = 0.5, ticks.colour = "black")) +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA), 
        legend.justification = c(1, 0), legend.position = c(0.92, 0.6), legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8), legend.direction = "horizontal", strip.text = element_text(size = 10)) + 
  scale_fill_gradient2(name = expression("Pr-ET (mm 4yr"^-1*")"), limits = c(-1700, 3000), midpoint = 0, low = "#D41159", mid = "lightyellow1", high = "#1A85FF") +
  facet_wrap(~ both)

#Plot 2012-2015 Pr-ET (mm/4yr) separate by SPI48 2002 versus SPI48 2015
p6 <-ggplot(subset(all.ca.spi48, count >= 20), mapping = aes(x = spi48_09_2002, y = spi48_09_2015, fill = PET_4yr_2015.mean, group = PET_4yr_2015.mean)) +
  geom_bin2d(mapping = aes(group = PET_4yr_2015.mean), binwidth = c(0.1, 0.1)) + theme_bw() +  
  ylim(-3.5, 0) + xlim(-3, 2) +
  ylab(NULL) +  xlab(NULL) +  
  # geom_vline(xintercept = 0, size = 0.25) + 
  # geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  guides(fill = "none") +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_blank(), axis.title.x = element_text(size = 10),  
        axis.title.y = element_text(size = 10), strip.text = element_text(size = 10)) +
  scale_fill_gradient2(name = expression("Pr-ET (mm 4yr"^-1*")"), limits = c(-1700, 3000), midpoint = 0, low = "#D41159", mid = "lightyellow1", high = "#1A85FF") +
  annotate("text", x = 1, y = -0.5, label = "Neither \nDrought", size = 3) + annotate("text", x = -2.4, y = -0.5, label = "1st Drought \nOnly", size = 3) +
  annotate("text", x = -2.4, y = -3, label = "Both \nDroughts", size = 3) + annotate("text", x = 1, y = -2, label = "2nd Drought \nOnly", size = 3) + 
  facet_wrap( ~ second)

#Combine the two figure panels into one	
f3 <- ggarrange(p5, p6, ncol = 2, nrow = 1, widths = c(1, 0.95), common.legend = FALSE)

#Add shared x-axis and y-axis labels
annotate_figure(f3, left = textGrob(label = "SPI48 2012-2015", rot = 90, hjust = 0.5, vjust = 0.3), bottom = textGrob(label = 'SPI48 1999-2002', vjust = 0.1, hjust = 0.5))

#Save the figure as a png
ggsave(filename = 'SFig2_spi48_PrET_4yr_SPI48_grid_scatter.png', device = 'png', height=7, width=14, units = 'cm', dpi=900)

#Plot 1999 Biomass (Mg/ha) for SPI48 2002 versus SPI48 2015
p7 <- ggplot(subset(all.ca.spi48, count >= 20), mapping = aes(x = spi48_09_2002, y = spi48_09_2015, fill = biomass_1999.mean, group = biomass_1999.mean)) +
  geom_bin2d(mapping = aes(group = biomass_1999.mean), binwidth = c(0.1, 0.1)) + theme_bw() + 
  ylim(-3.5, 0) + xlim(-3, 2) +
  ylab(NULL) +  xlab(NULL) + 
  # geom_vline(xintercept = 0, size = 0.25) + 
  # geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  guides(fill = guide_colorbar(barwidth = 5, barheight = 1, title.position = "top", title.hjust = 0.5, ticks.colour = "black")) +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA), 
        legend.justification = c(1, 0), legend.position = c(0.96, 0.6), legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8), legend.direction = "horizontal", strip.text = element_text(size = 10)) + 
  scale_fill_gradient(name = expression("Biomass (Mg ha"^-1*")"), limits = c(10, 280), breaks = c(100,200), low = 'brown', high = 'green', na.value = 'transparent') +
  facet_wrap(~ both)

#Plot 2012 Biomass (Mg/ha) for SPI48 2002 versus SPI48 2015
p8 <-ggplot(subset(all.ca.spi48, count >= 20), mapping = aes(x = spi48_09_2002, y = spi48_09_2015, fill = biomass_2012.mean, group = biomass_2012.mean)) +
  geom_bin2d(mapping = aes(group = biomass_2012.mean), binwidth = c(0.1, 0.1)) + theme_bw() +  
  ylim(-3.5, 0) + xlim(-3, 2) +
  ylab(NULL) +  xlab(NULL) +  
  # geom_vline(xintercept = 0, size = 0.25) + 
  # geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  guides(fill = "none") +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_blank(), axis.title.x = element_text(size = 10),  
        axis.title.y = element_text(size = 10), strip.text = element_text(size = 10)) +
  scale_fill_gradient(name = expression("Biomass (Mg ha"^-1*")"), limits = c(10, 280), breaks = c(100,200), low = 'brown', high = 'green', na.value = 'transparent') +
  annotate("text", x = 1, y = -0.5, label = "Neither \nDrought", size = 3) + annotate("text", x = -2.4, y = -0.5, label = "1st Drought \nOnly", size = 3) +
  annotate("text", x = -2.4, y = -3, label = "Both \nDroughts", size = 3) + annotate("text", x = 1, y = -2, label = "2nd Drought \nOnly", size = 3) + 
  facet_wrap( ~ second) 

#Combine the two figure panels into one	
f4 <- ggarrange(p7, p8, ncol = 2, nrow = 1, widths = c(1, 0.95), common.legend = FALSE)

#Add shared x-axis and y-axis labels
annotate_figure(f4, left = textGrob(label = "SPI48 2012-2015", rot = 90, hjust = 0.5, vjust = 0.3), bottom = textGrob(label = 'SPI48 1999-2002', vjust = 0.1, hjust = 0.5))

#Save the figure as a png
ggsave(filename = 'SFig3_biomass_SPI48_grid_scatter.png', device = 'png', height=7, width=14, units = 'cm', dpi=900)

#Plot 1999-2002 4 year Max Temperature for SPI48 2002 vs. SPI48 2015
p9 <- ggplot(subset(all.ca.spi48, count >= 20), mapping = aes(x = spi48_09_2002, y = spi48_09_2015, fill = tmax_4yr_2002.mean, group = tmax_4yr_2002.mean)) +
  geom_bin2d(mapping = aes(group = tmax_4yr_2002.mean), binwidth = c(0.1, 0.1)) + theme_bw() + 
  ylim(-3.5, 0) + xlim(-3, 2) +
  ylab(NULL) +  xlab(NULL) + 
  # geom_vline(xintercept = 0, size = 0.25) + 
  # geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  guides(fill = guide_colorbar(barwidth = 5, barheight = 1, title.position = "top", title.hjust = 0.5, ticks.colour = "black")) +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA), 
        legend.justification = c(1, 0), legend.position = c(0.955, 0.6), legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8), legend.direction = "horizontal", strip.text = element_text(size = 10)) + 
  scale_fill_gradient(name = "Temperature (C)", limits = c(6, 22), low = "blue", high = "red", na.value = 'transparent') +
  facet_wrap(~ both)

#Plot 2012-2015 4 year Max Temperature for SPI48 2002 vs. SPI48 2015
p10 <-ggplot(subset(all.ca.spi48, count >= 20), mapping = aes(x = spi48_09_2002, y = spi48_09_2015, fill = tmax_4yr_2015.mean, group = tmax_4yr_2015.mean)) +
  geom_bin2d(mapping = aes(group = tmax_4yr_2015.mean), binwidth = c(0.1, 0.1)) + theme_bw() +  
  ylim(-3.5, 0) + xlim(-3, 2) +
  ylab(NULL) +  xlab(NULL) +  
  # geom_vline(xintercept = 0, size = 0.25) + 
  # geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  guides(fill = "none") +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_blank(), axis.title.x = element_text(size = 10),  
        axis.title.y = element_text(size = 10), strip.text = element_text(size = 10)) +
  scale_fill_gradient(name = "Temperature (C)", limits = c(6, 22), low = "blue", high = "red", na.value = 'transparent') +
  annotate("text", x = 1, y = -0.5, label = "Neither \nDrought", size = 3) + annotate("text", x = -2.4, y = -0.5, label = "1st Drought \nOnly", size = 3) +
  annotate("text", x = -2.4, y = -3, label = "Both \nDroughts", size = 3) + annotate("text", x = 1, y = -2, label = "2nd Drought \nOnly", size = 3) + 
  facet_wrap( ~ second) 

#Combine the two figure panels into one	
f5 <- ggarrange(p9, p10, ncol = 2, nrow = 1, widths = c(1, 0.95), common.legend = FALSE)

#Add shared x-axis and y-axis labels
annotate_figure(f5, left = textGrob(label = "SPI48 2012-2015", rot = 90, hjust = 0.5, vjust = 0.3), bottom = textGrob(label = 'SPI48 1999-2002', vjust = 0.1, hjust = 0.5))

#Save the figure as a png
ggsave(filename = 'SFig4_tmax_4yr_SPI48_grid_scatter.png', device = 'png', height=7, width=14, units = 'cm', dpi=900)

#Create a figure to compare SPI48 for the two droughts and figure out the drought sequences experienced by the two regions.
p11 <-ggplot(subset(all.ca.spi48, count >= 20), mapping = aes(x = spi48_09_2002, y = spi48_09_2015, fill = sierra_prop, group = sierra_prop)) +
  geom_bin2d(mapping = aes(group = sierra_prop), binwidth = c(0.1, 0.1)) + theme_bw() + 
  ylim(-3.5, 0) + xlim(-3, 2) +
  ylab('SPI48 2012-2015') +  xlab('SPI48 1999-2002') +  
  # geom_vline(xintercept = 0, size = 0.25) + 
  # geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 0.5, color = 'black', linetype='dashed') +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, title.vjust = 0.5, barwidth = 4, barheight = 1, ticks.colour = "black"), alpha = FALSE) +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), plot.title = element_text(size = 12, hjust = 0.5)) + #Presentation text sizes.
  scale_fill_gradient2(name = "Region", limits = c(0, 100), midpoint = 50,  breaks = c(0, 50, 100), labels = c('Southern \nCalifornia', '50/50 \nMix', 'Sierra \nNevada'),
                       low = "black", mid = "cornsilk", high = "dark grey")

#Add annotations and move the legend into the corner of the figure.
p12 <- p11 +
  theme(
    legend.background = element_rect(colour = NA, fill = NA), # This removes the white square behind the legend
    legend.justification = c(1, 0),
    legend.position = c(0.88, 0.6),
    legend.text = element_text(size = 4),
    legend.title = element_text(size = 6),
    legend.direction = "horizontal")

#Save the figure as a png
ggsave(filename = 'SFig5_spi48_sierra_socal_SPI48_grid_scatter.png', device = 'png', height=6, width=7, units = 'cm', dpi=900)