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