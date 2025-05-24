//Author: Carl A. Norlen

//Purpose: Create a hillshade base map

//Define polygon to use a region of interest for the study.
var socal = ee.Geometry.Rectangle(-120.5, 38.9, -115.5, 32.2);

//Add USGS National DEM Dataset
var dem = ee.Image("USGS/NED");

//Create a mask for for all DEM values that are not equal to zero.
var mask = dem.neq(0);

//Apply the mask to the DEM
var dem_masked = dem.updateMask(mask);

//Create the terrain map
var terrain = ee.Terrain.products(dem_masked);

//Calculate the hillshade to export
var hillshade = terrain.select('hillshade').reproject({crs: 'EPSG: 5070', scale: 30})
                                           .reproject({crs: 'EPSG: 5070', scale: 300});

//Export with CONUS Albers projection
Export.image.toDrive({image: hillshade, description: 'hillshade_bigger_region_300m_v2', folder: 'CECS', region: socal, 
                      scale: 300, crs: 'EPSG:5070', maxPixels: 1e13, fileFormat: 'GeoTIFF'});