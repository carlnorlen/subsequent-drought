#Author: Carl Norlen
#Date Created: November 11, 2019
#Date Editted: May 25, 2021
#Purpose: Create a chart showing the relationship between annual NDVI from Landsat and ET from flux towers

p <- c('dplyr','tidyr','ggplot2','ggpubr','viridis','segmented', 'patchwork','RColorBrewer', 'broom', 'svglite', 'ggpmisc')
#If necessary install packages
# install.packages(p,repo='https://cloud.r-project.org/')
# Load packages
lapply(p,require,character.only=TRUE)

dir_input <- "D:\\Large_Files\\tower_data\\Input"
dir_output <- "D:\\Large_Files\\tower_data\\Output"

## Navigate to the correct directory and run the program
#cd /C/Users/can02/mystuff/Goulden_Lab/Forest_Dieback/dieback/figure_set/flux_tower
#cd /C/Users/can02/mystuff/Goulden_Lab/Forest_Dieback/dieback/final_figure_set
#Run the script: R < sfig5_ndvi_flux_tower.R --vanilla

site1 <- read.csv(file.path(dir_output, "Grass_Annual_EClos_Hinojo.csv"), header = TRUE, na.strings = "NaN")

site2 <- read.csv(file.path(dir_output, "James_Annual_EClos_Hinojo.csv"), header = TRUE, na.strings = "NaN")

site3 <- read.csv(file.path(dir_output, "LowDesert_Annual_EClos_Hinojo.csv"), header = TRUE, na.strings = "NaN")

site4 <- read.csv(file.path(dir_output, "P301_Annual_EClos_Hinojo.csv"), header = TRUE, na.strings = "NaN")

site5 <- read.csv(file.path(dir_output, "PinyonBurn_Annual_EClos_Hinojo.csv"), header = TRUE, na.strings = "NaN")

site6 <- read.csv(file.path(dir_output, "Pinyon_Annual_EClos_Hinojo.csv"), header = TRUE, na.strings = "NaN")

site7 <- read.csv(file.path(dir_output, "SJER_Annual_EClos_Hinojo.csv"), header = TRUE, na.strings = "NaN")

site8 <- read.csv(file.path(dir_output, "Sage_Annual_EClos_Hinojo.csv"), header = TRUE, na.strings = "NaN")

site9 <- read.csv(file.path(dir_output, "Shorthair_Annual_EClos_Hinojo.csv"), header = TRUE, na.strings = "NaN")

site10 <- read.csv(file.path(dir_output, "Soaproot_Annual_EClos_Hinojo.csv"), header = TRUE, na.strings = "NaN")

flux.sites <- rbind(site1, site2, site3, site4, site5, site6, site7, site8, site9, site10)

flux.sites$Site <- recode(.x=flux.sites$ID, 'Grassland US-SCg' = 'US-SCg', 'OakPine Forest US-SCf' = 'US-SCf', 'Desert US-SCd' = 'US-SCd', 'Sierran Mixed Conifer US-CZ3' = 'US-CZ3', 'PinyonJuniper US-SCw' = 'US-SCw', 'OakPine Woodland US-CZ1' = 'US-CZ1', 'Coastal Sage US-SCs' = 'US-SCs',
							  'Subalpine Forest US-CZ4' = 'US-CZ4', 'Ponderosa Pine Forest US-CZ2' = 'US-CZ2', 'Desert Chaparral US-SCc' = 'US-SCc')

#Remove years with missing data and when data wasn't working
flux.sites <- subset(flux.sites, n_days >= 300 & ET > 0)

#Load Landsat NDVI data
ndvi.sites <- read.csv(file.path(dir_input, "UCIupwind_pixels_NDVI_met_30m.csv"), header = TRUE, na.strings = "NaN")

ndvi.sites.group <- ndvi.sites %>% group_by(Site,year) %>%
					summarize(NDVI.mean = mean(NDVI))
ndvi.sites.group <- as.data.frame(ndvi.sites.group)
ndvi.sites.group$wYEAR <- ndvi.sites.group$year
							  
sites.join <- inner_join(ndvi.sites.group,flux.sites,by=c("Site","wYEAR"))

# Create the exponential model.
# Estimate the rest parameters using a linear model
model.0 <- lm(log(ET) ~ NDVI.mean, data=sites.join)  
alpha.0 <- exp(coef(model.0)[1])
beta.0 <- coef(model.0)[2]

# Starting parameters
start <- list(alpha = alpha.0, beta = beta.0)

nlsFit <-
  nls(formula = ET~alpha*exp(NDVI.mean*beta),
      start = start,
      data = sites.join)
summary(nlsFit)

#### Create scatter plot with exponential fit
sites.join$Site <- factor(sites.join$Site)
p1 <- ggplot(data = sites.join, mapping = aes(x = NDVI.mean, y = ET)) + 
	  scale_shape_manual(values=1:10) + theme_bw() +
	  geom_point(mapping = aes(color = Site, shape = Site), size = 2) + 
	  geom_smooth(method = nls, method.args = list(formula = y ~ alpha*exp(x*beta), start = start), se=FALSE, color = 'black') + 
	  stat_cor(aes(label = paste(..rr.label.., expression('ET = 125.5345 * e'^'(2.6148 * NDVI)'), sep = "~`,`~")), size = 3.5, color = 'black', r.accuracy = 0.001, p.accuracy = 0.001) +
	  theme(axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9), axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10), legend.title = element_text(size = 10), legend.text = element_text(size = 8)) +
	  xlab('NDVI') + ylab(expression('ET (mm yr'^-1*')'))

f1 <- ggarrange(p1, ncol = 1, nrow = 1, common.legend = TRUE, legend = 'bottom')

ggsave(filename = 'SupFig8_NDVI_ET_scaling.png', height=10, width=12, units = 'cm', dpi=900)
ggsave(filename = 'SupFig8_NDVI_ET_scaling.svg', height=10, width=12, units = 'mm', dpi=900)