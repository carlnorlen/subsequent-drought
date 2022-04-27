#Author: Carl Norlen
#Date Created: February 6, 2020
#Date Updated: April 26, 2022
#Purpose: Create figure 1 map for publication

#Load required packages
p <- c('ggalt', 'ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggmap', 'ggplot2', 'magrittr', 'raster', 'rgdal', 'sp', 
       'sf', 'RStoolbox', 'gtools', 'tigris', 'patchwork', 'rlist', 'ggspatial', 'svglite', 'grid')
lapply(p,require,character.only=TRUE)

setwd('C:/Users/can02/mystuff/subsequent-drought')

# # # File Directories
socal_dir <- "D:\\Large_Files\\socal"
dir_usfs <- "D:\\Large_Files\\USFS\\data\\subsections"
dir_in <- "D:\\Large_Files\\Landsat"

#Load CSV file with data for creating figures
all.ca <- read.csv(file.path(dir_in, "Regression_all_socal_300m_v23.csv"))

#Convert from units of deads trees /acre to dead trees/hectare
all.ca$ADS_2004 <- all.ca$ADS_2004 * 2.41705
all.ca$ADS_2017 <- all.ca$ADS_2017 * 2.41705

#Calculation of Difference in SPI48
all.ca$dSPI48 <- abs(all.ca$spi48_09_2015 - all.ca$spi48_09_2002)

#Setting variable for ESPG 5070, proj4 crs
c <- crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

#Read in a raster of general WGS 84 crs in PROJ4 code format
wg <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")

#Increase the memory limit for R. Helps with spatially explicit analyses.
memory.limit(32000)

#Add California Boundary shape file
us_states_20m <- states(cb = TRUE, resolution = "20m", class = "sf")
us_states_20m <- st_transform(us_states_20m, c)
ca_20m <- us_states_20m[us_states_20m$NAME == "California", ]
ca_20m <- st_as_sf(ca_20m)
ca_20m <- st_transform(ca_20m, c)

#Select just Western CONUS states
west_st_20m <- subset(us_states_20m, STUSPS == "CA" | STUSPS == "NV") #
west_st_20m <- st_as_sf(west_st_20m)
west_st_20m <- st_transform(west_st_20m, c)

#Create an extend polygon based on just California and Nevada
west.ext <- as(extent(west_st_20m), 'SpatialPolygons')
crs(west.ext) <- c

#Select USFS EcoRegion for South Sierra Nevada
usfs.regions <- st_read(file.path(dir_usfs, 'S_USA.EcomapSubsections.shp'))
usfs.sierra <- subset(usfs.regions, MAP_UNIT_S == 'M261Ep' | MAP_UNIT_S == 'M261Eq' | MAP_UNIT_S == 'M261Es' | MAP_UNIT_S == 'M261Eu' | MAP_UNIT_S == 'M261Er' | MAP_UNIT_S == 'M261Eo') # | MAP_UNIT_S == 'M261Ev') #MAP_UNIT_S == 'M261Et' | 

#Merge Sierra Nevada polygons into one larger polygon
usfs.sierra.union <- usfs.sierra %>% st_union()

#Select Southern California USFS Polygons
usfs.socal <- subset(usfs.regions, MAP_UNIT_S == 'M262Bh' | MAP_UNIT_S == 'M262Bg' | MAP_UNIT_S == 'M262Bd' | MAP_UNIT_S == 'M262Be' | MAP_UNIT_S == 'M262Bf' | MAP_UNIT_S =='M262Bo' | 
                                   MAP_UNIT_S =='M262Bi' | MAP_UNIT_S =='M262Bm' | MAP_UNIT_S == 'M262Bl' | MAP_UNIT_S =='M262Bc' | MAP_UNIT_S == 'M262Bp' | MAP_UNIT_S == 'M262Ba' | MAP_UNIT_S == 'M262Bb')

#Merge Southern California polygons into a larger polygon
usfs.socal.union <- usfs.socal %>% st_union()

#Add SPI48 layers for different years and a combined layer based on SPI48 exposure
spi48_2015.300m <- raster(file.path(socal_dir, 'Drought_second_300m_v5.tif'))
spi48_2002.300m <- raster(file.path(socal_dir, 'Drought_first_300m_v5.tif'))
spi48.300m.both <- spi48_2002.300m + spi48_2015.300m 
spi48.300m.both.crop <- crop(spi48.300m.both, west.ext)
spi48.300m.m <- spi48.300m.both.crop == 0
spi48.300m.both.mask <- mask(spi48.300m.both.crop, mask = spi48.300m.m, maskvalue = 1)

#Do binning by SPI48 2002 and SPI48 2015
all.ca.spi48 <- all.ca %>%
  mutate(socal = as.integer(USFS_zone == 262), sierra = as.integer(USFS_zone == 261)) %>% #Make new columns that have 0,1 for Sierra and socal to calculate proportions later
  mutate(spi48_09_2002.bin = cut(spi48_09_2002, breaks = seq(-3.5, 2, by = 0.1)),
         spi48_09_2015.bin = cut(spi48_09_2015, breaks = seq(-3.5, 2, by = 0.1)),
  ) %>%
  group_by(spi48_09_2002.bin, spi48_09_2015.bin) %>%
  mutate(dNDMI_1994.mean = mean(dNDMI_1994)) %>%
  mutate(dNDMI_2004.mean = mean(dNDMI_2004)) %>%
  mutate(dNDMI_2017.mean = mean(dNDMI_2017)) %>%
  mutate(PET_4yr_1992.mean = mean(PET_4yr_1992)) %>%
  mutate(PET_4yr_2002.mean = mean(PET_4yr_2002)) %>%
  mutate(PET_4yr_2015.mean = mean(PET_4yr_2015)) %>%
  mutate(dNDMI_2004.median = median(dNDMI_2004)) %>%
  mutate(dNDMI_2017.median = median(dNDMI_2017)) %>%
  mutate(ADS_2004.mean = mean(ADS_2004)) %>%
  mutate(ADS_2017.mean = mean(ADS_2017)) %>%
  mutate(count = n()) %>%
  mutate(socal_count = sum(socal), sierra_count = sum(sierra)) %>%
  mutate(socal_prop = (socal_count / count) * 100, sierra_prop = (sierra_count / count) * 100) %>% # Calculate Sierra and SoCal proportions.
  mutate(dominant_region = as.integer(socal_prop > sierra_prop)) %>%
  ungroup()

#Map Showing SPI 48 month Drought mask for 2002 and 2015 droughts
p1 <- ggplot() +
	  ggR(img = spi48.300m.both.mask, layer = 1, maxpixels = 1e10, geom_raster = TRUE, ggLayer = TRUE, forceCat = TRUE) +
	  scale_fill_manual(values = c("1" = "#E1BE6A","2" = "#5D3A9B","3" = "#E66100"), breaks = c("1", "2", "3"), name = expression(atop(NA,atop(textstyle('Severe Drought'),textstyle('(SPI48'<= -1.5*')')))), 
	                    labels = c("1st Drought \nOnly", "2nd Drought \nOnly", "Both \nDroughts"), na.value = NA) +
	  geom_sf(data = ca_20m, color='black', size = 0.2, fill=NA) +
	  geom_sf(data = usfs.sierra.union, color='black', size = 0.6,  fill='black', alpha = 0.2) +
	  geom_sf(data = usfs.socal.union, color='black', size = 0.6,  fill='black', alpha = 0.2) +
	  coord_sf() +
    xlab('Longitude') + ylab('Latitude') +
	  annotation_scale(location = "bl", height = unit(0.1, "cm"), width_hint = 0.2) +
	  annotation_north_arrow(location = "br", which_north = "true",
	  height = unit(1, "cm"), width = unit(1, "cm"),
      pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"),
      style = north_arrow_minimal) + theme_bw() + guides(fill = guide_legend(title.position = "top")) +
	  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
	  legend.key = element_rect(fill = NA), legend.text=element_text(size=8), legend.title = element_text(size=8),  plot.margin = unit(c(0,5,0,10), "pt")) 

#Add annotations to map of SPI48 exposure
p2 <- p1 + 
    theme(
    legend.justification = c(1, 0),
    legend.position = c(0.89, 0.6),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.direction = "vertical") +
  guides(fill = guide_legend(ncol = 1, nrow = 3,
                             title.position = "top", title.hjust = 0.5), override.aes = list(fill = 0.4))

p2    
    
#Create a figure of the sample size comparing SPI48 2002 versus SPI48 2015
p3 <- ggplot(all.ca.spi48, mapping = aes(x = spi48_09_2002, y = spi48_09_2015, fill = count, group = count)) +
  geom_bin2d(mapping = aes(group = count, alpha = ..count..), binwidth = c(0.1, 0.1)) + theme_bw() + 
  theme(legend.position="bottom", legend.text = element_text(size=6)) + 
  ylim(-3.5, 0) + xlim(-3, 2) +
  ylab('SPI48 During 2nd Period (2012-2015)') +  xlab('SPI48 During 1st Period (1999-2002)') + 
  # geom_vline(xintercept = 0, size = 0.5) + 
  # geom_hline(yintercept = 0, size = 0.5) +
  geom_vline(xintercept = -1.5, size = 1, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 1, color = 'black', linetype='dashed') +
  guides(fill = guide_colorbar(barwidth = 5, barheight = 1, title.position = "top", title.hjust = 0.5, title.vjust = 0.5, ticks.colour = "black"), alpha = FALSE) +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), legend.background = element_rect(colour = NA, fill = NA), 
        legend.justification = c(1, 0), legend.position = c(0.99, 0.6), legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8), legend.direction = "horizontal", strip.text = element_text(size = 12),
        plot.margin = unit(c(10,0,10,10), "pt")) +  
  scale_fill_gradient2(name = "Total Area\n(# Grid Cells)", limits = c(0,2950), midpoint = 1475, low = "cornflowerblue", 
                       mid = "yellow", high = "red", na.value = 'transparent') +
  coord_cartesian(clip = 'off') + #Turn off clipping
  scale_alpha(range = c(1, 1), limits = c(20, 2950), na.value = 0.4)

#Add annotations and move the legend into the corner of the figure.
p4 <- p3 + annotate("text", x = 1, y = -0.5, label = "Neither \nDrought") + annotate("text", x = -2.4, y = -0.5, label = "1st Drought \nOnly") +
  annotate("text", x = -2.4, y = -3, label = "Both \nDroughts") + annotate("text", x = 1, y = -2, label = "2nd Drought \nOnly") + 
  annotation_custom(grob = textGrob(label = "Drier", hjust = 0, gp = gpar(fontface = 'bold', fontsize = 8)),
    ymin = -3.5, ymax = -3.5,      # Vertical position of the textGrob
    xmin = -3.1, xmax = -3.1) +     # Horizontal position
  annotation_custom(grob = textGrob(label = "Wetter", hjust = 0, gp = gpar(fontface = 'bold', fontsize = 8)),
                  ymin = -3.5, ymax = -3.5,      # Vertical position of the textGrob
                  xmin = 1.75, xmax = 1.75 ) +     # Horizontal position
  theme(
    legend.background = element_rect(colour = NA, fill = NA), # This removes the white square behind the legend
    legend.justification = c(1, 0),
    legend.position = c(0.62, 0.7),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.direction = "horizontal") 

#Combine the two figures into one plot, add letters to label sub-plots
f1 <- ggarrange(p2, p4, ncol = 2, nrow = 1, common.legend = FALSE, align = 'h', heights = c(1,1), widths = c(1, 1.5), labels = c('a)', 'b)')) #,  plot.margin = unit(c(0,3,0,0), "pt")

#Save the figure as a .png file
ggsave(filename = 'Fig1_both_droughts_SPI48_300m_map.png', height=10, width= 20, units = 'cm', dpi=900)
