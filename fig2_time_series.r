#Author: Carl Norlen
#Date Created: February 6, 2020
#Date Updated: April 26, 2022
#Purpose: Create Figure 2 time series for publication

#Load required scripts for teh script
p <- c('ggpubr', 'tidyr', 'dplyr', 'ggplot2', 'patchwork')
lapply(p,require,character.only=TRUE)

setwd('C:/Users/can02/mystuff/subsequent-drought')

#Directory for loading data
land_dir <- "D:\\Large_Files\\Landsat"

#Load Time Series data sets
first <- read.csv(file.path(land_dir, 'dNDMI_NDMI_PET_Temp_ADS_trajectories_first_drought_full_region_300m_v9.csv'))
second <- read.csv(file.path(land_dir, 'dNDMI_NDMI_PET_Temp_ADS_trajectories_second_drought_full_region_300m_v9.csv'))

#Add the group name for each time series
first$drought <- '2nd Drought Only'
second$drought <- 'Both Droughts'

#Add sample size for each time series
first$count <- as.numeric(8923)
second$count <- as.numeric(49222)

#Combine the time series data into on data frame
time.series <- rbind(first, second)

#Rename the system.time_start column
names(time.series)[names(time.series) == 'system.time_start'] <- 'time_start' 

#Convert time_start to a Date object		
time.series$time_start <- as.Date(time.series$time_start)

#Create a year column
time.series$year <- format(time.series$time_start, '%Y')

#Convert acres to hectares
time.series$tpa_sum <- time.series$tpa_sum * 2.41705 * 2.25 # convert acres to hectares and then convert 150 m pixels (2.25 hectares) into number of dead trees
# time.series$tpa <- time.series$tpa_sum / (time.series$tpa_count * 2.25)
time.series$tpa_mean <- time.series$tpa_mean * 2.41705 
time.series$tpa_stdDev <- time.series$tpa_stdDev * 2.41705 
# time.series$tpa_stdError <- time.series$tpa_stdDev / sqrt(time.series$tpa_count)
time.series$tpa_p100 <- time.series$tpa_p100 * 2.41705 
time.series$tpa_p50 <- time.series$tpa_p50 * 2.41705
time.series$tpa_p75 <- time.series$tpa_p75 * 2.41705 
time.series$tpa_p25 <- time.series$tpa_p25 * 2.41705
time.series$tpa_low_area <- (time.series$tpa_mid_sum / time.series$tpa_count) * 100 
time.series$biomass_mean[time.series$biomass_mean == 0] <- NA

#Make the drought/Region a factor
time.series$drought <- factor(time.series$drought)

#Create an ADS proportion time series
p1 <- ggplot() +
  geom_rect(data = data.frame(xmin = as.Date('1998-10-01'), xmax = as.Date('2002-09-30'), ymin = -Inf, ymax = Inf), 
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf), 
            fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  geom_line(data = time.series, mapping = aes(x = time_start, y = tpa_low_area, color = drought, linetype = drought), size = 0.5, na.rm = TRUE) + 
  theme_bw() + guides(color = guide_legend(title = 'Drought\nSequence', nrow = 2, title.position = 'top'), linetype = "none") +
  scale_linetype_manual(values=c("solid", "twodash")) +
  geom_hline(yintercept = 0, linetype='dashed') +
  scale_color_manual(values=c("#E66100", "#5D3A9B"), labels = c('Both\nDroughts', '2nd Drought\nOnly')) +
  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10, margin = margin(t=0,b=0,r=0,l=0)), 
        axis.title.x = element_blank(), legend.position = c(0.15, 0.6), legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(fill = NA), axis.text.x = element_blank(), plot.margin = unit(c(0,0,5,0), "pt"), 
        legend.title = element_text(size = 8), legend.text = element_text(size = 6)) + 
  annotate("text", x = as.Date('2000-10-01'), y = 30, label = "1st\nPeriod", size = 2.5, fontface = 2) +
  annotate("text", x = as.Date('2013-10-01'), y = 30, label = "2nd\nPeriod", size = 2.5, fontface = 2) +
  ylab(expression(atop(NA, atop(textstyle('Percent with'),textstyle('Die-off (ADS)')))))

#Create a dNDMI time series
p2 <- ggplot() + 
	  geom_rect(data = data.frame(xmin = as.Date('1998-10-01'), xmax = as.Date('2002-09-30'), ymin = -Inf, ymax = Inf), 
	  fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
      geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf), 
	  fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
	  geom_line(data = time.series, mapping = aes(x = time_start, y = dNDMI_mean, color = drought, linetype = drought), size = 0.5) +
	  geom_errorbar(data=time.series, mapping=aes(x=time_start, ymin=dNDMI_mean - (dNDMI_stdDev / sqrt(count) * 1.96), ymax=dNDMI_mean + (dNDMI_stdDev / sqrt(count) * 1.96), color = drought, linetype = drought),
	                size = 0.4, width = 100, alpha = 0.6) +
    theme_bw() + geom_hline(yintercept = 0, linetype='dashed') + 
    scale_linetype_manual(values=c("solid", "twodash")) +
    scale_color_manual(values=c("#E66100", "#5D3A9B")) + scale_y_reverse() +
	  theme(axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10, margin = margin(t=0,b=0,r=0,l=0)), axis.title.x=element_blank(), legend.position = 'none', 
	  axis.text.x=element_blank(), plot.margin = unit(c(0,0,5,0), "pt"), legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
	  ylab('Die-off\n(dNDMI)') + geom_text(data = data.frame(label = "95% CI", x = as.Date('2018-12-31'), y = -0.019, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2) +
     annotate("text", x = as.Date('1992-10-01'), y = -0.043, label = "More Die-off", size = 2, fontface = 2) +
    annotate("text", x = as.Date('1992-10-01'), y = 0.043, label = "Less Die-off", size = 2, fontface = 2)

#Create a Pr-ET time series graph
p3 <- ggplot() +
	  geom_rect(data = data.frame(xmin = as.Date('1998-10-01'), xmax = as.Date('2002-09-30'), ymin = -Inf, ymax = Inf), 
	  fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
      geom_rect(data = data.frame(xmin = as.Date('2011-10-01'), xmax = as.Date('2015-09-30'), ymin = -Inf, ymax = Inf), 
	  fill = "red", alpha = 0.3, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
	  geom_line(data = time.series, mapping = aes(x = time_start, y = PET_mean, color = drought, linetype = drought), size = 0.5) +
    geom_errorbar(data=time.series, mapping=aes(x=time_start, ymin=PET_mean - (PET_stdDev / sqrt(count) * 1.96), ymax=PET_mean + (PET_stdDev / sqrt(count) * 1.96), color = drought, linetype = drought), 
                  size = 0.4, width = 100, alpha = 0.6) +
    theme_bw() + 
	  geom_hline(yintercept = 0, linetype='dashed') + 
    scale_linetype_manual(values=c("solid", "twodash")) +
    scale_color_manual(values=c("#E66100", "#5D3A9B")) +
	  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.x = element_text(size = 10), legend.position = 'none', 
	  axis.title.y = element_text(size = 10, margin = margin(t=0,b=0,r=0,l=0)), plot.margin = unit(c(0,0,0,0), "pt"), legend.title = element_text(size = 8), legend.text = element_text(size = 6)) +
	  xlab('Year') + ylab(expression(atop(NA, atop(textstyle('Pr-ET'),textstyle(paste('(mm ',yr^{-1},')')))))) + 
    annotate("text", x = as.Date('1992-10-01'), y = 1100, label = "Water Surplus", size = 2, fontface = 2) +
    annotate("text", x = as.Date('1992-10-01'), y = -100, label = "Water Deficit", size = 2, fontface = 2)

f1 <- ggarrange(p1, p2, p3, ncol = 1, nrow = 3, common.legend = FALSE, heights = c(0.9, 0.9,1), align = "v", labels = c('a)', 'b)', 'c)'))
f1
#Save and export the figure as a .png file
ggsave(filename = 'Fig2_drought_time_series.png', height=13, width=11, units = 'cm', dpi=900)
