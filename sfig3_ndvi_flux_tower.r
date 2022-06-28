#Author: Carl A. Norlen
#Date Created: November 11, 2019
#Date Edited: June 28, 2022
#Purpose: Create a chart showing the relationship between annual NDVI from Landsat and ET from flux towers

p <- c('dplyr','tidyr','ggplot2','ggpubr','viridis','segmented', 'patchwork','RColorBrewer', 'broom', 'svglite', 'ggpmisc')
#If necessary install packages

# Load packages
lapply(p,require,character.only=TRUE)

dir_input <- "D:\\Large_Files\\tower_data\\Input"
dir_output <- "D:\\Large_Files\\tower_data\\Output"

## Navigate to the correct directory and run the program
setwd("C:\\Users\\can02\\mystuff\\subsequent-drought")

#Load annual data from Flux Tower sites
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

#Combine all the flux tower data into one dataframe
flux.sites <- rbind(site1, site2, site3, site4, site5, site6, site7, site8, site9, site10)

#Add site labels for the flux tower sites
flux.sites$veg.type <- recode(.x=flux.sites$ID, 'Grassland US-SCg' = 'non-forest', 'OakPine Forest US-SCf' = 'forest', 'Desert US-SCd' = 'non-forest', 'Sierran Mixed Conifer US-CZ3' = 'forest', 'PinyonJuniper US-SCw' = 'forest', 'OakPine Woodland US-CZ1' = 'forest', 'Coastal Sage US-SCs' = 'non-forest',
							  'Subalpine Forest US-CZ4' = 'forest', 'Ponderosa Pine Forest US-CZ2' = 'forest', 'Desert Chaparral US-SCc' = 'non-forest')

flux.sites$Site <- recode(.x=flux.sites$ID, 'Grassland US-SCg' = 'US-SCg', 'OakPine Forest US-SCf' = 'US-SCf', 'Desert US-SCd' = 'US-SCd', 'Sierran Mixed Conifer US-CZ3' = 'US-CZ3', 'PinyonJuniper US-SCw' = 'US-SCw', 'OakPine Woodland US-CZ1' = 'US-CZ1', 'Coastal Sage US-SCs' = 'US-SCs',
                              'Subalpine Forest US-CZ4' = 'US-CZ4', 'Ponderosa Pine Forest US-CZ2' = 'US-CZ2', 'Desert Chaparral US-SCc' = 'US-SCc')

flux.sites$eco.type <- recode(.x=flux.sites$ID, 'Grassland US-SCg' = 'Grassland US-SCg', 'OakPine Forest US-SCf' = 'Oak Pine Forest US-SCf', 'Desert US-SCd' = 'Desert US-SCd', 'Sierran Mixed Conifer US-CZ3' = 'Sierran Mixed Conifer US-CZ3', 'PinyonJuniper US-SCw' = 'Pinyon Juniper US-SCw', 'OakPine Woodland US-CZ1' = 'Oak Pine Woodland US-CZ1', 'Coastal Sage US-SCs' = 'Coastal Sage US-SCs',
                              'Subalpine Forest US-CZ4' = 'Subalpine Forest US-CZ4', 'Ponderosa Pine Forest US-CZ2' = 'Ponderosa Pine Forest US-CZ2', 'Desert Chaparral US-SCc' = 'Desert Chaparral US-SCc')
#Remove years with missing data and when data wasn't working
flux.sites <- subset(flux.sites, n_days >= 300 & ET > 0)

#Load Landsat data for flux tower sites
ndvi.sites <- read.csv(file.path(dir_input, "UCIupwind_pixels_NDVI_met_30m.csv"), header = TRUE, na.strings = "NaN")

#Calculate the mean NDVI for the 9 upwind Landsat pixels at each flux tower site
ndvi.sites.group <- ndvi.sites %>% group_by(Site,year) %>%
					summarize(NDVI.mean = mean(NDVI))

#Make the Landsat data a data frame
ndvi.sites.group <- as.data.frame(ndvi.sites.group)

#Make a column for wYEAR
ndvi.sites.group$wYEAR <- ndvi.sites.group$year

#Join the Landsat and Flux tower data frames by Site and wYEAR
sites.join <- inner_join(ndvi.sites.group,flux.sites,by=c("Site","wYEAR"))

# Create an exponential model.
# Estimate the rest parameters using a linear model
model.0 <- lm(log(ET) ~ NDVI.mean, data=sites.join)  
alpha.0 <- exp(coef(model.0)[1])
beta.0 <- coef(model.0)[2]

# Create the model starting parameters
start <- list(alpha = alpha.0, beta = beta.0)

#Create the exponential fit between ET and NDVI
nlsFit <-
  nls(formula = ET~alpha*exp(NDVI.mean*beta),
      start = start,
      data = sites.join)

#Create the exponential fit between ET and NDVI, for forested sites
nlsFit.forest <-
  nls(formula = ET~alpha*exp(NDVI.mean*beta),
      start = start,
      data = sites.join %>% filter(Site %in% c('US-CZ1', 'US-CZ2', 'US-CZ3', 'US-CZ4', 'US-SCw', 'US-SCf')))
nlsFit.forest

#### Create scatter plot with exponential fit
sites.join$Site <- factor(sites.join$Site)
sites.join$eco.type <- factor(sites.join$eco.type)

#### Create scatter plot with exponential fit for just forested sites
p1 <- ggplot() + 
  scale_shape_manual(values=1:10) + theme_bw() +
  scale_color_manual(values = c('dark green', 'black'), labels = c('Forest', 'Shrub/Grass')) + 
  guides(color = 'none', shape = guide_legend(title = 'Site', ncol = 2)) +
  geom_point(data = sites.join, #%>% filter(Site %in% c('US-CZ1', 'US-CZ2', 'US-CZ3', 'US-CZ4', 'US-SCw', 'US-SCf')), 
             mapping = aes(x = NDVI.mean, y = ET, color = veg.type, shape = eco.type), size = 2) + 
  #Add the full ET line
  geom_smooth(data = sites.join, 
              mapping = aes(x = NDVI.mean, y = ET), linetype = 'dashed',
              method = nls, method.args = list(formula = y ~ alpha*exp(x*beta), start = start), se=FALSE, color = 'black') + 
  geom_text_npc(data = data.frame(label = "All Ecosystems:", y = 0.96, x = 0.02), mapping = aes(npcx=x, npcy=y, label = label), 
                size = 3.5, color = 'black') +
  stat_cor(data = sites.join, label.x.npc = 0.24, label.y.npc = 1,
           aes(x = NDVI.mean, y = ET, label = paste(..rr.label.., expression('ET = 125.5345 * e'^'(2.6148 * NDVI)'), sep = "~`,`~")), 
           size = 3.5, color = 'black', r.accuracy = 0.001, p.accuracy = 0.001) +
  #Add the forest only ET line
  geom_smooth(data = sites.join %>% filter(veg.type == 'forest'), 
              mapping = aes(x = NDVI.mean, y = ET), linetype = 'dotdash',
              method = nls, method.args = list(formula = y ~ alpha*exp(x*beta), start = start), se=FALSE, color = 'dark green') + 
  geom_text_npc(data = data.frame(label = "Forest Only:", y = 0.82, x = 0.02), mapping = aes(npcx=x, npcy=y, label = label), 
                size = 3.5, color = 'dark green') +
  stat_cor(data = sites.join %>% filter(veg.type == 'forest'), label.x.npc = 0.18, label.y.npc = 0.85,
           aes(x = NDVI.mean, y = ET, label = paste(..rr.label.., expression('ET = 132.223 * e'^'(2.579 * NDVI)'), sep = "~`,`~")), 
           size = 3.5, r.accuracy = 0.001, p.accuracy = 0.001, color = 'dark green') +
  theme(axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9), axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10), 
        legend.position = c(0.22, 0.55), legend.background = element_rect(colour = NA, fill = NA), legend.title.align = 0.5,
        legend.key = element_rect(fill = NA), legend.title = element_text(size = 6), legend.text = element_text(size = 5)) +
  xlab('NDVI') + ylab(expression('ET (mm yr'^-1*')'))
p1

#Add the legend to the bottom of the figure
f1 <- ggarrange(p1, ncol = 1, nrow = 1, common.legend = FALSE)
f1
#Save the figure as a .PNG file
ggsave(filename = 'SupFig13_Forest_NDVI_ET_scaling.png', height=10, width=16, units = 'cm', dpi=900)
