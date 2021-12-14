#Author: Carl Norlen
#Date Created: February 6, 2020
#Date Updated: November 9, 2021
#Purpose: Create supplementary maps for publication

# cd /C/Users/Carl/mystuff/Goulden_Lab/Forest_Dieback/dieback/figure_set/final_figures_redo
# cd /C/Users/can02/mystuff/Goulden_Lab/Forest_Dieback/dieback/final_figure_set_redo
#Run the script: R < sfig1_maps.r --vanilla
p <- c('ggpubr', 'tidyr', 'dplyr', 'ggmap', 'ggplot2', 'magrittr', 'raster', 'rgdal', 'sp', 'sf', 'RStoolbox', 
       'ncdf4', 'gtools', 'tigris', 'patchwork', 'rlist', 'ggspatial', 'svglite')
# install.packages(p,repo='https://cran.r-project.org/')

# install.packages(c('ggmap'),repo='https://cran.r-project.org/')
lapply(p,require,character.only=TRUE)

setwd('C:/Users/can02/mystuff/Goulden_Lab/Forest_Dieback/dieback/final_figure_set_redo')

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

#ESPG 5070, proj4 crs
c <- crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

#Read in a raster of general WGS 84 crs in PROJ4 code format
wg <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")

#Increase the memory limit for R. Helps with spatially explicit analyses.
memory.limit(32000)

# # #Select USFS EcoRegions
# usfs.regions <- st_read(file.path(dir_usfs, 'S_USA.EcomapSubsections.shp'))
# usfs.sierra <- subset(usfs.regions, MAP_UNIT_S == 'M261Ep' | MAP_UNIT_S == 'M261Eq' | MAP_UNIT_S == 'M261Es' | MAP_UNIT_S == 'M261Eu' | MAP_UNIT_S == 'M261Er') #   | MAP_UNIT_S == 'M261Eo' 
# usfs.sierra.union <- usfs.sierra %>% st_union()
# usfs.socal <- subset(usfs.regions, MAP_UNIT_S == 'M262Bh' | MAP_UNIT_S == 'M262Bg' | MAP_UNIT_S == 'M262Bd' | MAP_UNIT_S == 'M262Be' | MAP_UNIT_S == 'M262Bf' | MAP_UNIT_S =='M262Bo' | MAP_UNIT_S =='M262Bi' | MAP_UNIT_S =='M262Bm' | MAP_UNIT_S == 'M262Bl' | MAP_UNIT_S =='M262Bc' | MAP_UNIT_S == 'M262Bp')
# usfs.socal.union <- usfs.socal %>% st_union()
#Select USFS EcoRegions
usfs.regions <- st_read(file.path(dir_usfs, 'S_USA.EcomapSubsections.shp'))
usfs.sierra <- subset(usfs.regions, MAP_UNIT_S == 'M261Ep' | MAP_UNIT_S == 'M261Eq' | MAP_UNIT_S == 'M261Es' | MAP_UNIT_S == 'M261Eu' | MAP_UNIT_S == 'M261Er' | MAP_UNIT_S == 'M261Eo') # | MAP_UNIT_S == 'M261Ev') #MAP_UNIT_S == 'M261Et' | 
usfs.sierra.union <- usfs.sierra %>% st_union()
usfs.socal <- subset(usfs.regions, MAP_UNIT_S == 'M262Bh' | MAP_UNIT_S == 'M262Bg' | MAP_UNIT_S == 'M262Bd' | MAP_UNIT_S == 'M262Be' | MAP_UNIT_S == 'M262Bf' | MAP_UNIT_S =='M262Bo' | 
                       MAP_UNIT_S =='M262Bi' | MAP_UNIT_S =='M262Bm' | MAP_UNIT_S == 'M262Bl' | MAP_UNIT_S =='M262Bc' | MAP_UNIT_S == 'M262Bp' | MAP_UNIT_S == 'M262Ba' | MAP_UNIT_S == 'M262Bb')
usfs.socal.union <- usfs.socal %>% st_union()


#Landsat dNDMI
dndmi.2004 <- raster(file.path(socal_dir, 'dNDMI_2004_bigger_region_300m_v4.tif'))
crs(dndmi.2004) <- c
dndmi.2004.m <- is.na(dndmi.2004) #is this actually a good way to mask this?
dndmi.2004.mask <- mask(dndmi.2004, mask = dndmi.2004.m, maskvalue = 1)

dndmi.2017 <- raster(file.path(socal_dir, 'dNDMI_2017_bigger_region_300m_v4.tif'))
crs(dndmi.2017) <- c
dndmi.2017.m <- is.na(dndmi.2017)
dndmi.2017.mask <- mask(dndmi.2017, mask = dndmi.2017.m, maskvalue = 1)


#Landsat four-year Pr-ET
PrET.2002 <- raster(file.path(socal_dir, 'PrET_2002_bigger_region_300m_v4.tif'))
crs(PrET.2002) <- c
PrET.2002.m <- is.na(PrET.2002)
PrET.2002.mask <- mask(PrET.2002, mask = PrET.2002.m, maskvalue = 1)

PrET.2015 <- raster(file.path(socal_dir, 'PrET_2015_bigger_region_300m_v4.tif'))
crs(PrET.2015) <- c
PrET.2015.m <- is.na(PrET.2015)
PrET.2015.mask <- mask(PrET.2015, mask = PrET.2015.m, maskvalue = 1)

#Landsat Biomass
biomass.1999 <- raster(file.path(socal_dir, 'biomass_1999_bigger_region_300m.tif'))
crs(biomass.1999) <- c
biomass.1999.m <- is.na(biomass.1999)
biomass.1999.mask <- mask(biomass.1999, mask = biomass.1999.m, maskvalue = 1)

biomass.2012 <- raster(file.path(socal_dir, 'biomass_2012_bigger_region_300m.tif'))
crs(biomass.2012) <- c
biomass.2012.m <- is.na(biomass.2012)
biomass.2012.mask <- mask(biomass.2012, mask = biomass.2012.m, maskvalue = 1)

#Landsat tmax
tmax.2002 <- raster(file.path(socal_dir, 'tmax_2002_bigger_region_300m.tif'))
crs(tmax.2002 ) <- c
tmax.2002.m <- is.na(tmax.2002)
tmax.2002.mask <- mask(tmax.2002, mask = tmax.2002.m, maskvalue = 1)

tmax.2015 <- raster(file.path(socal_dir, 'tmax_2015_bigger_region_300m.tif'))
crs(tmax.2015) <- c
tmax.2015.m <- is.na(tmax.2015)
tmax.2015.mask <- mask(tmax.2015, mask = tmax.2015.m, maskvalue = 1)

#ADS Die-off
ads.2004 <- raster(file.path(socal_dir, 'ADS_2004_bigger_region_300m.tif'))
crs(ads.2004) <- c
ads.2004.reclass <- reclassify(ads.2004, c(-Inf,5,0,5,Inf,1), right = FALSE)
ads.2004.m <- is.na(ads.2004.reclass) #is this actually a good way to mask this?
ads.2004.mask <- mask(ads.2004.reclass, mask = ads.2004.m, maskvalue = 1)

ads.2017 <- raster(file.path(socal_dir, 'ADS_2017_bigger_region_300m.tif'))
crs(ads.2017) <- c
ads.2017.reclass <- reclassify(ads.2017, c(-Inf,5,0,5,Inf,1), right = FALSE)
ads.2017.m <- is.na(ads.2017.reclass)
ads.2017.mask <- mask(ads.2017.reclass, mask = ads.2017.m, maskvalue = 1)

#Drought masks, considering region
drought.region.both <- raster(file.path(socal_dir, 'Drought_both_bigger_region_300m_v5.tif'))
drought.region.both.calc <- drought.region.both * 2
crs(drought.region.both.calc) <- c

drought.region.second <- raster(file.path(socal_dir, 'Drought_second_bigger_region_300m_v5.tif'))
crs(drought.region.second) <- c

drought.region.all <- drought.region.both.calc + drought.region.second
drought.region.all.m <- drought.region.all == 0
drought.region.all.mask <- mask(drought.region.all, mask = drought.region.all.m, maskvalue = 1)

#Create Southern California region polygon
socal.ext <- as(extent(-120.5, -115.5, 32.2, 38.9), 'SpatialPolygons')
crs(socal.ext) <- wg
socal.ext <- spTransform(socal.ext, c)

#Import hillshade base map for California
base.ca <- raster(file.path(dir_usgs, 'hillshade_bigger_region_300m_v2.tif'))
crs(base.ca) <- c
base.ca.m <- base.ca == 0
base.ca.mask <- mask(base.ca, mask = base.ca.m, maskvalue = 1)

#Add California Boundary shape file
us_states_20m <- states(cb = TRUE, resolution = "20m", class = "sf")
us_states_20m <- st_transform(us_states_20m, c)
ca_20m <- us_states_20m[us_states_20m$NAME == "California", ]
ca_20m_crop <- st_crop(ca_20m, socal.ext)

# facet$both <- '1999-2002'
# facet$second <- '2012-2015'

#Die-off (dNDMI) for 1999-2002 drought
p1 <- ggplot() + 
	  ggR(img = base.ca.mask, layer = 1, maxpixels = 1e12, geom_raster = FALSE, ggLayer = TRUE, forceCat = TRUE, sat = 0.0, hue = 0.4, alpha = 1.0) + #sat = 1.0 and hue = 0.1 is orange # current color is gray sat = 0# green is sat = 0.5 and hue = 0.4
    geom_sf(data = ca_20m_crop, color='black', size = 0.2, fill=NA) +
    ggR(img = dndmi.2004.mask, layer = 1, maxpixels = 1e10, geom_raster = TRUE, ggLayer = TRUE, forceCat = FALSE) + #Less Strict Drougth Mask Pixels, Error when only this is present.
    geom_sf(data = usfs.sierra.union, color='black', size = 0.3,  fill='dark gray', alpha = 0) +
    geom_sf(data = usfs.socal.union, color='black', size = 0.3,  fill='black', alpha = 0) +
    coord_sf() + xlab(NULL) + ylab(NULL) + #xlab('Longitude') + ylab('Latitude') +
    guides(fill = FALSE) +  
    annotation_scale(location = "bl", height = unit(0.1, "cm"), width_hint = 0.2) +
	  annotation_north_arrow(location = "bl", which_north = "true",
	  height = unit(1, "cm"), width = unit(1, "cm"),
      pad_x = unit(0.1, "cm"), pad_y = unit(0.5, "cm"),
      style = north_arrow_minimal) + 
    theme_bw() + theme(axis.title.y = element_text(size=10), axis.text.y = element_text(size=8), axis.title.x = element_text(size=10), legend.background = element_rect(colour = NA, fill = NA),
                     axis.text.x = element_text(size=8), legend.justification = c(1, 0), legend.position = c(0.45, 0.01), legend.text = element_text(size = 6), 
                     legend.title = element_text(size = 8), legend.direction = "horizontal") +
	  scale_fill_gradient2(name = "Die-off (dNDMI)", limits = c(-0.25, 0.1), midpoint = 0, low = "#D41159", mid = "lightyellow1", high = "#1A85FF", na.value = NA) +
    ggtitle("1999-2002")

#Die-off (dNDMI) for 2012-2015 drought
p2 <- ggplot() + 
	  ggR(img = base.ca.mask, layer = 1, maxpixels = 1e12, geom_raster = FALSE, ggLayer = TRUE, forceCat = TRUE, sat = 0.0, hue = 0.4, alpha = 1.0)+ # sat = 1.0 and hue = 0.1 is orange # current color is gray sat = 0# green is sat = 0.5 and hue = 0.4
    geom_sf(data = ca_20m_crop, color='black', size = 0.2, fill=NA) +  
    ggR(img = dndmi.2017.mask, layer = 1, maxpixels = 1e10, geom_raster = TRUE, ggLayer = TRUE, forceCat = FALSE) + #Less Strict Drougth Mask Pixels, Error when only this is present.
    geom_sf(data = usfs.sierra.union, color='black', size = 0.3,  fill='black', alpha = 0) +
    geom_sf(data = usfs.socal.union, color='black', size = 0.3,  fill='black', alpha = 0) +
	  coord_sf() + xlab(NULL) + ylab(NULL) + #xlab('Longitude') + ylab('Latitude') + 
    guides(fill = guide_colorbar(barwidth = 5, barheight = 1, title.position = "top", title.hjust = 0.5, ticks.colour = "black")) +
	  theme_bw() + theme(axis.title.y = element_text(size=10), axis.text.y = element_text(size=8), axis.title.x = element_text(size=10), legend.background = element_rect(colour = NA, fill = NA),
	                     axis.text.x = element_text(size=8), legend.justification = c(1, 0), legend.position = c(0.45, 0.01), legend.text = element_text(size = 6), 
	                     legend.title = element_text(size = 8), legend.direction = "horizontal") +
	  scale_fill_gradient2(name = "Die-off (dNDMI)", limits = c(-0.25, 0.1), midpoint = 0, low = "#D41159", mid = "lightyellow1", high = "#1A85FF", na.value = NA) +
    ggtitle("2012-2015")

f1 <- ggarrange(p1, p2, ncol = 2, nrow = 1, common.legend = FALSE, labels = c('a', 'b'))
# f1
ggsave(filename = 'SupFig1_dNDMI_Maps.png', height=12.5, width= 16, units = 'cm', dpi=900)

#Drought mask when excluding cross region pixels
p4 <- ggplot() + 
	  ggR(img = base.ca.mask, layer = 1, maxpixels = 1e12, geom_raster = FALSE, ggLayer = TRUE, forceCat = TRUE, sat = 0.0, hue = 0.4, alpha = 1.0) +  
	  geom_sf(data = ca_20m_crop, color='black', size = 0.2, fill=NA) + 
	  ggR(img = drought.region.all.mask, layer = 1, maxpixels = 1e10, geom_raster = TRUE, ggLayer = TRUE, forceCat = TRUE) + #Less Strict Drougth Mask Pixels, Error when only this is present.
	  geom_sf(data = usfs.sierra.union, color='black', size = 0.3, fill='black', alpha = 0) +
	  geom_sf(data = usfs.socal.union, color='black', size = 0.3, fill='black', alpha = 0) +
	  coord_sf() + #guides(fill = guide_colorsteps(even.steps = TRUE, barwidth = 5, barheight = 1, title.position = "top", title.hjust = 0.5)) +
	  annotation_scale(location = "bl", height = unit(0.1, "cm"), width_hint = 0.2) + xlab(NULL) + ylab(NULL) + #xlab('Longitude') + ylab('Latitude') +
	  annotation_north_arrow(location = "bl", which_north = "true",
	  height = unit(1, "cm"), width = unit(1, "cm"),
      pad_x = unit(0.1, "cm"), pad_y = unit(0.5, "cm"),
      style = north_arrow_minimal) + 
	  #The plot.margin order is top, right, bottom, left
	  scale_fill_manual(values = c("1" = "#5D3A9B","2" = "#E66100"), name = 'Drought Sequence', labels = c("2012-2015 \nOnly", "Both \nDroughts"), na.value = NA, na.translate = F) +
    theme_bw() + guides( fill = guide_legend(title.position = "top")) +
    theme(axis.title.y = element_text(size=10), axis.text.y = element_text(size=8), axis.title.x = element_text(size=10), 
                     axis.text.x = element_text(size=8), legend.text = element_text(size = 6), #legend.background = element_rect(colour = NA, fill = NA),
                     legend.title = element_text(size = 7), legend.direction = "vertical", legend.position = c(0.8, 0.88))

ggsave(filename = 'SupFig2_Drought_Maps.png', height=12.5, width= 8, units = 'cm', dpi=900)

# four-year Pr-ET for 1999-2002 drought
p5 <- ggplot() + 
	  ggR(img = base.ca.mask, layer = 1, maxpixels = 1e12, geom_raster = FALSE, ggLayer = TRUE, forceCat = TRUE, sat = 0.0, hue = 0.4, alpha = 1.0) +  
	  geom_sf(data = ca_20m_crop, color='black', size = 0.2, fill=NA) + 
	  ggR(img = PrET.2002.mask, layer = 1, maxpixels = 1e10, geom_raster = TRUE, ggLayer = TRUE, forceCat = FALSE) + #Less Strict Drougth Mask Pixels, Error when only this is present.
	  geom_sf(data = usfs.sierra.union, color='black', size = 0.3, fill='black', alpha = 0) +
	  geom_sf(data = usfs.socal.union, color='black', size = 0.3, fill='black', alpha = 0) +
	  coord_sf() + guides(fill = FALSE) + xlab(NULL) + ylab(NULL) + #xlab('Longitude') + ylab('Latitude') +
	  annotation_scale(location = "bl", height = unit(0.1, "cm"), width_hint = 0.2) +
	  annotation_north_arrow(location = "bl", which_north = "true",
	  height = unit(1, "cm"), width = unit(1, "cm"),
      pad_x = unit(0.1, "cm"), pad_y = unit(0.5, "cm"),
      style = north_arrow_minimal) + 
	  #The plot.margin order is top, right, bottom, left
	  theme_bw() + theme(axis.title.y = element_text(size=10), axis.text.y = element_text(size=8), axis.title.x = element_text(size=10), axis.text.x = element_text(size=8)) +
	  scale_fill_gradient2(name = 'four-year Pr-ET', limits = c(-2500, 4000), midpoint = 0, low = "#D41159", mid = "lightyellow1", high = "#1A85FF", na.value = NA) +
	  # scale_fill_viridis(name = 'four-year Pr-ET', direction = -1, option = "plasma", limits = c(-3000, 4000), na.value = NA) + 
	  ggtitle("1999-2002")

#four-year Pr-ET for 2012-2015 drought
p6 <- ggplot() + 
	  ggR(img = base.ca.mask, layer = 1, maxpixels = 1e12, geom_raster = FALSE, ggLayer = TRUE, forceCat = TRUE, sat = 0.0, hue = 0.4, alpha = 1.0)+ 
	  geom_sf(data = ca_20m_crop, color='black', size = 0.2, fill=NA) + 
	  ggR(img = PrET.2015.mask, layer = 1, maxpixels = 1e10, geom_raster = TRUE, ggLayer = TRUE, forceCat = FALSE) + #Less Strict Drougth Mask Pixels, Error when only this is present.
	  geom_sf(data = usfs.sierra.union, color='black', size = 0.3, fill='black', alpha = 0) +
	  geom_sf(data = usfs.socal.union, color='black', size = 0.3, fill='black', alpha = 0) +
	  coord_sf() + xlab(NULL) + ylab(NULL) + #xlab('Longitude') + ylab('Latitude') +
    guides(fill = guide_colorbar(barwidth = 5, barheight = 1, title.position = "top", title.hjust = 0.5, ticks.colour = "black")) +
	  theme_bw() + theme(axis.title.y = element_text(size=10), axis.text.y = element_text(size=8), axis.title.x = element_text(size=10), 
	                     legend.background = element_rect(colour = NA, fill = NA),
	                     axis.text.x = element_text(size=8), legend.justification = c(1, 0), legend.position = c(0.47, 0.01), legend.text = element_text(size = 6), 
	                     legend.title = element_text(size = 7), legend.direction = "horizontal") +
	  scale_fill_gradient2(name = 'four-year Pr-ET', limits = c(-2500, 4000), midpoint = 0, low = "#D41159", mid = "lightyellow1", high = "#1A85FF", na.value = NA) +
	  # scale_fill_viridis(name = 'four-year Pr-ET', direction = -1, option = "plasma", limits = c(-3000, 4000), na.value = NA) + 
	  ggtitle("2012-2015")

f3 <- ggarrange(p5, p6, ncol = 2, nrow = 1, common.legend = FALSE, labels = c('a', 'b'))
# f3
ggsave(filename = 'SupFig3_PrET_Maps.png', height=12.5, width= 16, units = 'cm', dpi=900)
# ggsave(filename = 'SupFig3_Drought_Maps.svg', height=12.5, width= 16, units = 'cm', dpi=900, device = 'svg')

# Biomass for 1999-2002 drought
p7 <- ggplot() + 
  ggR(img = base.ca.mask, layer = 1, maxpixels = 1e12, geom_raster = FALSE, ggLayer = TRUE, forceCat = TRUE, sat = 0.0, hue = 0.4, alpha = 1.0) +  
  geom_sf(data = ca_20m_crop, color='black', size = 0.2, fill=NA) + 
  ggR(img = biomass.1999.mask, layer = 1, maxpixels = 1e10, geom_raster = TRUE, ggLayer = TRUE, forceCat = FALSE) + #Less Strict Drougth Mask Pixels, Error when only this is present.
  geom_sf(data = usfs.sierra.union, color='black', size = 0.3, fill='black', alpha = 0) +
  geom_sf(data = usfs.socal.union, color='black', size = 0.3, fill='black', alpha = 0) +
  coord_sf() + guides(fill = FALSE) + xlab(NULL) + ylab(NULL) + #xlab('Longitude') + ylab('Latitude') +
  annotation_scale(location = "bl", height = unit(0.1, "cm"), width_hint = 0.2) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.1, "cm"), pad_y = unit(0.5, "cm"),
                         style = north_arrow_minimal) + 
  #The plot.margin order is top, right, bottom, left
  theme_bw() + theme(axis.title.y = element_text(size=10), axis.text.y = element_text(size=8), axis.title.x = element_text(size=10), axis.text.x = element_text(size=8)) +
  scale_fill_gradient(name = 'Biomass', limits = c(0, 460), low = "brown", high = "green", na.value = NA) +
  ggtitle("1999-2002")

#Biomass for 2012-2015 drought
p8 <- ggplot() + 
  ggR(img = base.ca.mask, layer = 1, maxpixels = 1e12, geom_raster = FALSE, ggLayer = TRUE, forceCat = TRUE, sat = 0.0, hue = 0.4, alpha = 1.0)+ 
  geom_sf(data = ca_20m_crop, color='black', size = 0.2, fill=NA) + 
  ggR(img = biomass.2012.mask, layer = 1, maxpixels = 1e10, geom_raster = TRUE, ggLayer = TRUE, forceCat = FALSE) + #Less Strict Drougth Mask Pixels, Error when only this is present.
  geom_sf(data = usfs.sierra.union, color='black', size = 0.3, fill='black', alpha = 0) +
  geom_sf(data = usfs.socal.union, color='black', size = 0.3, fill='black', alpha = 0) +
  coord_sf() + xlab(NULL) + ylab(NULL) + #xlab('Longitude') + ylab('Latitude') +
  guides(fill = guide_colorbar(barwidth = 5, barheight = 1, title.position = "top", title.hjust = 0.5, ticks.colour = "black")) +
  theme_bw() + theme(axis.title.y = element_text(size=10), axis.text.y = element_text(size = 8), axis.title.x = element_text(size=10), 
                     legend.background = element_rect(colour = NA, fill = NA),
                     axis.text.x = element_text(size=8), legend.justification = c(1, 0), legend.position = c(0.47, 0.01), legend.text = element_text(size = 6), 
                     legend.title = element_text(size = 7), legend.direction = "horizontal") +
  scale_fill_gradient(name = 'Biomass', limits = c(0, 460), low = "brown", high = "green", na.value = NA) + 
  ggtitle("2012-2015")

f4 <- ggarrange(p7, p8, ncol = 2, nrow = 1, common.legend = FALSE, labels = c('a', 'b'))
f4
ggsave(filename = 'SupFig4_Biomass_Maps.png', height=12.5, width= 16, units = 'cm', dpi=900)

# Temp for 1999-2002 drought
p9 <- ggplot() + 
  ggR(img = base.ca.mask, layer = 1, maxpixels = 1e12, geom_raster = FALSE, ggLayer = TRUE, forceCat = TRUE, sat = 0.0, hue = 0.4, alpha = 1.0) +  
  geom_sf(data = ca_20m_crop, color='black', size = 0.2, fill=NA) + 
  ggR(img = tmax.2002.mask, layer = 1, maxpixels = 1e10, geom_raster = TRUE, ggLayer = TRUE, forceCat = FALSE) + #Less Strict Drougth Mask Pixels, Error when only this is present.
  geom_sf(data = usfs.sierra.union, color='black', size = 0.3, fill='black', alpha = 0) +
  geom_sf(data = usfs.socal.union, color='black', size = 0.3, fill='black', alpha = 0) +
  coord_sf() + guides(fill = FALSE) + xlab(NULL) + ylab(NULL) + #xlab('Longitude') + ylab('Latitude') +
  annotation_scale(location = "bl", height = unit(0.1, "cm"), width_hint = 0.2) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.1, "cm"), pad_y = unit(0.5, "cm"),
                         style = north_arrow_minimal) + 
  #The plot.margin order is top, right, bottom, left
  theme_bw() + theme(axis.title.y = element_text(size=10), axis.text.y = element_text(size=8), axis.title.x = element_text(size=10), axis.text.x = element_text(size=8)) +
  scale_fill_gradient(name = 'Temperature', limits = c(5, 26), low = "blue", high = "red", na.value = NA) +
  ggtitle("1999-2002")

#Temp for 2012-2015 drought
p10 <- ggplot() + 
  ggR(img = base.ca.mask, layer = 1, maxpixels = 1e12, geom_raster = FALSE, ggLayer = TRUE, forceCat = TRUE, sat = 0.0, hue = 0.4, alpha = 1.0)+ 
  geom_sf(data = ca_20m_crop, color='black', size = 0.2, fill=NA) + 
  ggR(img = tmax.2015.mask, layer = 1, maxpixels = 1e10, geom_raster = TRUE, ggLayer = TRUE, forceCat = FALSE) + #Less Strict Drougth Mask Pixels, Error when only this is present.
  geom_sf(data = usfs.sierra.union, color='black', size = 0.3, fill='black', alpha = 0) +
  geom_sf(data = usfs.socal.union, color='black', size = 0.3, fill='black', alpha = 0) +
  coord_sf() + xlab(NULL) + ylab(NULL) + #xlab('Longitude') + ylab('Latitude') +
  guides(fill = guide_colorbar(barwidth = 5, barheight = 1, title.position = "top", title.hjust = 0.5, ticks.colour = "black")) +
  theme_bw() + theme(axis.title.y = element_text(size=10), axis.text.y = element_text(size=8), axis.title.x = element_text(size=10), 
                     legend.background = element_rect(colour = NA, fill = NA),
                     axis.text.x = element_text(size=8), legend.justification = c(1, 0), legend.position = c(0.47, 0.01), legend.text = element_text(size = 6), 
                     legend.title = element_text(size = 7), legend.direction = "horizontal") +
  scale_fill_gradient(name = 'Temperature', limits = c(5, 26), low = "blue", high = "red", na.value = NA) + 
  ggtitle("2012-2015")

f5 <- ggarrange(p9, p10, ncol = 2, nrow = 1, common.legend = FALSE, labels = c('a', 'b'))
f5
ggsave(filename = 'SupFig5_Temperature_Maps.png', height=12.5, width= 16, units = 'cm', dpi=900)

# Die-off (ADS) for 1999-2002 drought
p11 <- ggplot() + 
  ggR(img = base.ca.mask, layer = 1, maxpixels = 1e12, geom_raster = FALSE, ggLayer = TRUE, forceCat = TRUE, sat = 0.0, hue = 0.4, alpha = 1.0) +  
  geom_sf(data = ca_20m_crop, color='black', size = 0.2, fill=NA) + 
  ggR(img = ads.2004.mask, layer = 1, maxpixels = 1e10, geom_raster = TRUE, ggLayer = TRUE, forceCat = TRUE) + #Less Strict Drougth Mask Pixels, Error when only this is present.
  geom_sf(data = usfs.sierra.union, color='black', size = 0.3, fill='black', alpha = 0) +
  geom_sf(data = usfs.socal.union, color='black', size = 0.3, fill='black', alpha = 0) +
  coord_sf() + guides(fill = FALSE) + xlab(NULL) + ylab(NULL) + #xlab('Longitude') + ylab('Latitude') +
  annotation_scale(location = "bl", height = unit(0.1, "cm"), width_hint = 0.2) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.1, "cm"), pad_y = unit(0.5, "cm"),
                         style = north_arrow_minimal) + 
  #The plot.margin order is top, right, bottom, left
  theme_bw() + theme(axis.title.y = element_text(size=10), axis.text.y = element_text(size=8), axis.title.x = element_text(size=10), axis.text.x = element_text(size=8)) +
  scale_fill_manual(values = c("1" = "#D41159","0" = "lightyellow1"), name = 'ADS', labels = c("No Mortality", "Mortality"), na.value = NA, na.translate = F) +
  #guides( fill = guide_legend(title.position = "top")) +
  ggtitle("1999-2002")

#Die-off (ADS) 2012-2015 drought
p12 <- ggplot() +
  ggR(img = base.ca.mask, layer = 1, maxpixels = 1e12, geom_raster = FALSE, ggLayer = TRUE, forceCat = TRUE, sat = 0.0, hue = 0.4, alpha = 1.0)+
  geom_sf(data = ca_20m_crop, color='black', size = 0.2, fill=NA) +
  ggR(img = ads.2017.mask, layer = 1, maxpixels = 1e10, geom_raster = TRUE, ggLayer = TRUE, forceCat = TRUE) + #Less Strict Drougth Mask Pixels, Error when only this is present.
  geom_sf(data = usfs.sierra.union, color='black', size = 0.3, fill='black', alpha = 0) +
  geom_sf(data = usfs.socal.union, color='black', size = 0.3, fill='black', alpha = 0) +
  coord_sf() + xlab(NULL) + ylab(NULL) + #xlab('Longitude') + ylab('Latitude') +
  # guides(fill = guide_colorbar(barwidth = 5, barheight = 1, title.position = "top", title.hjust = 0.5, ticks.colour = "black")) +
  guides(fill = guide_legend(title.hjust = 0.5, title.position = "top")) +
  theme_bw() + theme(axis.title.y = element_text(size=10), axis.text.y = element_text(size=8), axis.title.x = element_text(size=10),
                     legend.background = element_rect(colour = NA, fill = NA),
                     axis.text.x = element_text(size=8), legend.justification = c(1, 0), legend.position = c(0.37, 0.01),
                     legend.text = element_text(size = 6), legend.title = element_text(size = 7), legend.direction = "vertical") +
  scale_fill_manual(values = c("1" = "#D41159","0" = "lightyellow1"), name = 'Die-off (ADS)', labels = c("No Mortality", "Mortality"), na.value = NA, na.translate = F) +
  ggtitle("2012-2015")
# p12
f6 <- ggarrange(p11, p12, ncol = 2, nrow = 1, common.legend = FALSE, labels = c('a', 'b'))
# f6
ggsave(filename = 'SupFig6_ADS_Maps.png', height=12.5, width= 16, units = 'cm', dpi=900)
