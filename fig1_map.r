#Author: Carl Norlen
#Date Created: February 6, 2020
#Date Updated: May 20, 2021
#Purpose: Create map figures for publication

#Navigate to directory: cd /C/Users/Carl/mystuff/Goulden_Lab/Forest_Dieback/dieback/figure_set/final_figures_redo
#Navigate to directory: cd /C/Users/can02/mystuff/Goulden_Lab/Forest_Dieback/dieback/final_figure_set_redo
#Run the script: R < fig1_map.r --vanilla
#Load required packages
p <- c('ggalt', 'ggpubr', 'viridis', 'tidyr', 'dplyr', 'ggmap', 'ggplot2', 'magrittr', 'raster', 'rgdal', 'sp', 'sf', 'RStoolbox', 'gtools', 'tigris', 'patchwork', 'rlist', 'ggspatial', 'svglite')
lapply(p,require,character.only=TRUE)
# install.packages(c('ggalt'),repo='https://cran.r-project.org/')

# library(ggalt)
# land_dir <- "C:\\Users\\Carl\\mystuff\\Large_Files\\Landsat"
# socal_dir <- "C:\\Users\\Carl\\mystuff\\Large_Files\\socal"
# landfire_dir <- "C:\\Users\\Carl\\mystuff\\Large_Files\\LANDFIRE"
# frap_dir <- "C:\\Users\\Carl\\mystuff\\Large_Files\\FRAP\\raster"
# wrcc_dir <- "C:\\Users\\Carl\\mystuff\\Large_Files\\WRCC"
# data_dir <- "C:\\Users\\Carl\\mystuff\\Large_Files\\WRCC\\All"
# data_30m_dir <- "C:\\Users\\Carl\\mystuff\\Large_Files\\WRCC\\SPI48_30m"
# # dir_out <- "C:\\Users\\Carl\\mystuff\\Large_Files\\WRCC\\Output"
# dir_out <- "C:\\Users\\Carl\\mystuff\\Large_Files\\WRCC\\SPI48_30m"
# work_dir <- "C:\\Users\\Carl\\mystuff\\Goulden_Lab\\Forest_Dieback\\dieback\\figure_set\\map"
# dir_ca <- "C:\\Users\\Carl\\mystuff\\Large_Files\\TIGER\\ca-state-boundary"
# dir_usgs <- "C:\\Users\\Carl\\mystuff\\Large_Files\\USGS"

setwd('C:/Users/can02/mystuff/Goulden_Lab/Forest_Dieback/dieback/final_figure_set_redo')

# # # File Directories
land_dir <- "D:\\Large_Files\\Landsat"
socal_dir <- "D:\\Large_Files\\socal"
landfire_dir <- "D:\\Large_Files\\LANDFIRE"
frap_dir <- "D:\\Large_Files\\FRAP\\raster"
wrcc_dir <- "D:\\Large_Files\\WRCC"
data_dir <- "D:\\Large_Files\\WRCC\\All"
data_30m_dir <- "D:\\Large_Files\\WRCC\\SPI48_30m"
dir_out <- "D:\\Large_Files\\WRCC\\SPI48_30m"
work_dir <- "C:\\Users\\can02\\mystuff\\Goulden_Lab\\Forest_Dieback\\dieback\\figure_set\\final_figures_redo"
dir_ca <- "D:\\Large_Files\\TIGER\\ca-state-boundary"
dir_usgs <- "D:\\Large_Files\\USGS\\data"
dir_usfs <- "D:\\Large_Files\\USFS\\data\\subsections"
spi_dir <- "D:\\Large_Files\\WRCC\\All"

#Directory for drought monitor polygons
dir_usdm <- "D:\\Large_Files\\Drought_Monitor\\equal_drought"

#CSV version of landsat data directory
dir_in <- "D:\\Large_Files\\Landsat"

all.ca <- read.csv(file.path(dir_in, "Regression_all_socal_300m_v23.csv"))

all.ca$ADS_2004 <- all.ca$ADS_2004 * 2.41705
all.ca$ADS_2017 <- all.ca$ADS_2017 * 2.41705

all.ca$dSPI48 <- abs(all.ca$spi48_09_2015 - all.ca$spi48_09_2002)

# all.ca.both <- subset(all.ca, spi48_09_2002 <= -1.5 & spi48_09_2015 <= -1.5 & dSPI48 <= 0.5 & USFS_zone == 262)
# 
# all.ca.second <- subset(all.ca, spi48_09_2015 <= -1.5 & spi48_09_2002 > spi48_09_2015 & dSPI48 > 0.5 & USFS_zone == 261) # & latitude <= 37.65 & latitude >= 35.69)

all.ca$dBiomass_1994 <- all.ca$biomass_1994 - all.ca$biomass_1989

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

#Load USDM data
usdm.august2002.dataset <- st_read(file.path(dir_usdm, 'USDM_20020827.shp'))
usdm.august2002.dataset <- st_transform(usdm.august2002.dataset, c)
usdm.august2002.dataset <- subset(usdm.august2002.dataset, DM >= 3)
usdm.august2002.dataset <- st_crop(usdm.august2002.dataset, west.ext)

usdm.august2015.dataset <- st_read(file.path(dir_usdm, 'USDM_20150825.shp'))
usdm.august2015.dataset <- st_transform(usdm.august2015.dataset, c)
usdm.august2015.dataset <- subset(usdm.august2015.dataset, DM >= 2)
usdm.august2015.dataset <- st_crop(usdm.august2015.dataset, west.ext)

#USDM overlap polygon
usdm.both.dataset <- st_read(file.path(dir_usdm, 'USDM_DM234_082002_082015.shp'))
usdm.both.dataset <- st_transform(usdm.both.dataset, c)
usdm.both.dataset <- subset(usdm.both.dataset, DM >= 3 & DM_1 >= 3)
usdm.both.dataset <- st_crop(usdm.both.dataset, west.ext)

#Select USFS EcoRegions
usfs.regions <- st_read(file.path(dir_usfs, 'S_USA.EcomapSubsections.shp'))
usfs.sierra <- subset(usfs.regions, MAP_UNIT_S == 'M261Ep' | MAP_UNIT_S == 'M261Eq' | MAP_UNIT_S == 'M261Es' | MAP_UNIT_S == 'M261Eu' | MAP_UNIT_S == 'M261Er' | MAP_UNIT_S == 'M261Eo') # | MAP_UNIT_S == 'M261Ev') #MAP_UNIT_S == 'M261Et' | 
usfs.sierra.union <- usfs.sierra %>% st_union()
usfs.socal <- subset(usfs.regions, MAP_UNIT_S == 'M262Bh' | MAP_UNIT_S == 'M262Bg' | MAP_UNIT_S == 'M262Bd' | MAP_UNIT_S == 'M262Be' | MAP_UNIT_S == 'M262Bf' | MAP_UNIT_S =='M262Bo' | 
                                   MAP_UNIT_S =='M262Bi' | MAP_UNIT_S =='M262Bm' | MAP_UNIT_S == 'M262Bl' | MAP_UNIT_S =='M262Bc' | MAP_UNIT_S == 'M262Bp' | MAP_UNIT_S == 'M262Ba' | MAP_UNIT_S == 'M262Bb')
usfs.socal.union <- usfs.socal %>% st_union()

#Loading in US Drought Monitor Data
drought.west <- raster(file.path(wrcc_dir,"SPI48_9_2002_2015_drought_mask_west_conus_cropped.tif"))
drought.socal <- raster(file.path(wrcc_dir, 'SPI48_9_2002_2015_drought_mask_cropped_v2.tif'))

#Remapped LANDFIRE (SPI48 <= -1.5 & Within 0.5)
spi48_2015.300m <- raster(file.path(socal_dir, 'Drought_second_300m_v5.tif'))
spi48_2002.300m <- raster(file.path(socal_dir, 'Drought_first_300m_v5.tif'))
spi48.300m.both <- spi48_2002.300m + spi48_2015.300m 
spi48.300m.both.crop <- crop(spi48.300m.both, west.ext)
spi48.300m.m <- spi48.300m.both.crop == 0
spi48.300m.both.mask <- mask(spi48.300m.both.crop, mask = spi48.300m.m, maskvalue = 1)

#Do binning by SPI48 and Pr-ET over four years.
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
  # mutate(tmax.anom.1992.mean = mean(tmax.anom.1992)) %>%
  # mutate(tmax.anom.2002.mean = mean(tmax.anom.2002)) %>%
  # mutate(tmax.anom.2015.mean = mean(tmax.anom.2015)) %>%
  mutate(ADS_2004.mean = mean(ADS_2004)) %>%
  mutate(ADS_2017.mean = mean(ADS_2017)) %>%
  # mutate(dDieback.mean = mean(dDieback)) %>%
  # mutate(dDieback_ADS.mean = mean(dDieback_ADS)) %>%
  mutate(count = n()) %>%
  mutate(socal_count = sum(socal), sierra_count = sum(sierra)) %>%
  mutate(socal_prop = (socal_count / count) * 100, sierra_prop = (sierra_count / count) * 100) %>% # Calculate Sierra and SoCal proportions.
  mutate(dominant_region = as.integer(socal_prop > sierra_prop)) %>%
  ungroup()

all.ca.spi48 %>% dplyr::select(USFS_zone, socal_prop, sierra_prop, dominant_region)

#Map Showing SPI 48 month Drought mask for 2002 and 2015 droughts
p1 <- ggplot() +
	  ggR(img = spi48.300m.both.mask, layer = 1, maxpixels = 1e10, geom_raster = TRUE, ggLayer = TRUE, forceCat = TRUE) +
	  scale_fill_manual(values = c("1" = "#E1BE6A","2" = "#5D3A9B","3" = "#E66100"), breaks = c("1", "2", "3"), name = expression('SPI48'<= -1.5*''), 
	                    labels = c("1999-2002 \nOnly", "2012-2015 \nOnly", "Both \nDroughts"), na.value = NA) +
	  geom_sf(data = ca_20m, color='black', size = 0.2, fill=NA) +
	  geom_sf(data = usfs.sierra.union, color='dark gray', size = 0.6,  fill='dark gray', alpha = 0.4) +
	  geom_sf(data = usfs.socal.union, color='black', size = 0.6,  fill='black', alpha = 0.2) +
	  coord_sf() +
    xlab('Longitude') + ylab('Latitude') +
	  annotation_scale(location = "bl", height = unit(0.1, "cm"), width_hint = 0.2) +
	  annotation_north_arrow(location = "br", which_north = "true",
	  height = unit(1, "cm"), width = unit(1, "cm"),
      pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"),
      style = north_arrow_minimal) + theme_bw() + guides(fill = guide_legend(title.position = "top")) +
	  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
	  legend.key = element_rect(fill = NA), legend.text=element_text(size=8), legend.title = element_text(size=8))

#Add annotations to map of SPI48 exposure
p2 <- p1 + #annotate("text", x = -120, y = 35, label = "Sierra \nNevada") + annotate(x = -115, y = 33, label = "Southern \nCalifornia") +
  theme(
    #legend.background = element_rect(colour = NA, fill = NA), # This removes the white square behind the legend
    # axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.85, 0.65),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.direction = "vertical") +
  guides(fill = guide_legend(ncol = 1, nrow = 3,
                             title.position = "top", title.hjust = 0.5), override.aes = list(fill = 0.4))
p2

#Create a figure to compare SPI48 for the two droughts and figure out the drought sequences experienced by the two regions.
p3 <-ggplot(subset(all.ca.spi48, count >= 20), mapping = aes(x = spi48_09_2002, y = spi48_09_2015, fill = sierra_prop, group = sierra_prop)) +
  geom_bin2d(mapping = aes(group = sierra_prop), binwidth = c(0.1, 0.1)) + theme_bw() + 
  # theme(legend.position="bottom", legend.text = element_text(size=6)) + 
  ylim(-3.5, 0) + xlim(-3, 2) +
  ylab('SPI48 2012-2015') +  xlab('SPI48 1999-2002') +  
  geom_vline(xintercept = 0, size = 0.5) + 
  geom_hline(yintercept = 0, size = 0.5) +
  geom_vline(xintercept = -1.5, size = 1, color = 'black', linetype='dashed') +
  geom_hline(yintercept = -1.5, size = 1, color = 'black', linetype='dashed') +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, title.vjust = 0.5, barwidth = 4, barheight = 1, ticks.colour = "black"), alpha = FALSE) +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), plot.title = element_text(size = 12, hjust = 0.5)) + #Presentation text sizes.
  # scale_fill_manual(name = "Dominant \nRegion", values = c('0' = 'dark gray', '1' = 'black'), breaks = c('0', '1'), labels = c('Sierra Nevada \n(100%)', 'Southern \nCalifornia'), na.value = NA) 
  scale_fill_gradient2(name = "Region", limits = c(0, 100), midpoint = 50,  breaks = c(0, 50, 100), labels = c('Southern \nCalifornia', '50/50 \nMix', 'Sierra \nNevada'),
                     low = "black", mid = "cornsilk", high = "dark grey") #+ scale_alpha(range = c(1,1), limits = c(20, 2950), na.value = 0.2)

#Add annotations and move the legend into the corner of the figure.
p4 <- p3 + annotate("text", x = 1, y = -0.5, label = "Neither \nDrought") + annotate("text", x = -2.4, y = -0.5, label = "1992-2002 \nOnly") +
  annotate("text", x = -2.4, y = -3, label = "Both \nDroughts") + annotate("text", x = 1, y = -2, label = "2012-2015 \nOnly") + 
  theme(
    legend.background = element_rect(colour = NA, fill = NA), # This removes the white square behind the legend
    # axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.65, 0.7),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.direction = "horizontal") 
p4
#Combine the two figures into one plot
f1 <- ggarrange(p2, p4, ncol = 2, nrow = 1, common.legend = FALSE, labels = c('a', 'b'), heights = c(1,1), widths = c(1, 1.5))
f1

ggsave(filename = 'Fig1_both_droughts_SPI48_300m_map.png', height=10, width= 20, units = 'cm', dpi=900)
# ggsave(filename = 'SupFig2_both_droughts_SPI48_300m_map.svg', height=12, width= 8, units = 'cm', dpi=900, device = 'svg')

all.usfs <- read.csv(file.path(dir_in, "Sierra_ecoregion_summary_300m.csv"))
all.usfs %>% dplyr::select(MAP_UNIT_S)

p5 <-ggplot(all.usfs, mapping = aes(x = spi48_09_2002_mean, y = spi48_09_2015_mean, color = as.factor(USFS_zone_mode), shape = as.factor(MAP_UNIT_S))) +
  geom_point() + theme_bw() + 
  theme(legend.position="bottom", legend.text = element_text(size=6)) + 
  ylim(-3.5, 0) + xlim(-3, 0.5) +
  ylab('SPI48 2012-2015') +  xlab('SPI48 1999-2002') +  
  geom_vline(xintercept = 0, linetype='dashed') + 
  geom_hline(yintercept = 0, linetype='dashed') +
  geom_vline(xintercept = -1.5, size = 1, color = 'black') +
  geom_hline(yintercept = -1.5, size = 1, color = 'black') +
  guides(color=guide_legend(title.position = "top", title.hjust = 0.5, ncol=2), shape = guide_legend(ncol = 4, nrow = 5, title.position = "top", title.hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), plot.title = element_text(size = 12, hjust = 0.5),legend.background = element_rect(colour = NA, fill = NA), # This removes the white square behind the legend
        legend.justification = c(1, 0),legend.position = c(0.9, 0.6),legend.text = element_text(size = 4), legend.title = element_text(size = 6), legend.box = "horizontal") + #Presentation text sizes.
  scale_shape_manual(values=1:19, name = 'USFS Subsection') +
  scale_color_manual(name = "Region", breaks = c(261,262), labels = c('Sierra \nNevada', 'Southern \nCalifornia'),
                        values = c('261' = 'dark gray', '262' = 'black'))

p5
ggsave(filename = 'SupFig4_FIA_subsections_both_droughts_SPI48_300m_map.png', height=12, width= 13, units = 'cm', dpi=900)

