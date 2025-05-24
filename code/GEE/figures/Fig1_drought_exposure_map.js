//Author: Carl A. Norlen
//Purpose: Script to download regridded SPI48 maps for Figure 1 of publication.

//Add functions for downscaling data layers
var resolution = require('users/cnorlen/subsequent_drought:resolution');

//Define a function for reprojectin data in CONUS Albers projection at 30-m
var reproject = function(image) {
                 return image.reproject({crs: 'EPSG: 5070', scale: 30});
};

//Define polygon to use a region of interest for the study
var socal = ee.Geometry.Rectangle(-124.5, 42.5, -114, 32.2); 

//Add SPI48 drougth metric
var spi9 = ee.ImageCollection('users/cnorlen/California_Data/SPI48_9');
            //.map(resolution.albers);

//Interpolate the SPI48 4km data set to 30 m
var spi9_30m = spi9.map(resolution.down_30m)
                   .map(reproject);
                   
//Select SPI48_9 for 2002
var spi_2002 = spi9_30m.filter(ee.Filter.date('2002-09-01')).first()
                                                             .divide(100); //Convert SPI to decimal

//Select SPI48_9 for 2015
var spi_2015 = spi9_30m.filter(ee.Filter.date('2015-09-01')).first()
                                                             .divide(100);

//Combine the SPI48 data into one Image Collection
var spi_data = ee.Image(spi_2002.select([0],['spi48_09_2002']))
                             .addBands(spi_2015.select([0],['spi48_09_2015']));
                             //.addBands(spi_1992.select([0],['spi48_09_1992']));

//Re-grid the SPI48 data from 30-m to 300-m
var spi_300m = spi_data.reproject({crs: 'EPSG: 5070', scale: 30})
                          .reduceResolution({reducer: ee.Reducer.mean(), maxPixels: 1024})
                          .reproject({crs: 'EPSG: 5070', scale: 300});
Map.addLayer(spi_300m);
//Create California Perimeter mask
//Add the California Perimeter
var region = ee.FeatureCollection('TIGER/2018/States')
              .filter(ee.Filter.eq('NAME', 'California'));

//Convert the California perimeter into a raster
var CA_raster = region.reduceToImage(['ALAND'],ee.Reducer.first()).rename('region'); 

//Create a mask out of the rasterized California perimeter
var CAmask = CA_raster.gte(1);

//Define locations with drought in 2012-2015 according to SPI48
var drought_2nd = spi_300m.select('spi48_09_2015').lte(-1.5).multiply(2).updateMask(CAmask).unmask(-9999);
Map.addLayer(drought_2nd, {}, '2nd Drought');

//Define locations with drought in 1999-2002 according to SPI48
var drought_1st = spi_300m.select('spi48_09_2002').lte(-1.5).updateMask(CAmask).unmask(-9999);
Map.addLayer(drought_1st, {}, '1st Drought');

//Export the drought masks as .TIFF images
Export.image.toDrive({image: drought_1st, description: 'Drought_first_300m_v7', 
                      folder: 'final_socal_data', region: socal, scale: 300, 
                      crs: 'EPSG:5070', maxPixels: 1e13, formatOptions: {
    cloudOptimized: true
  }});
                      
Export.image.toDrive({image: drought_2nd, description: 'Drought_second_300m_v7', 
                      folder: 'final_socal_data', region: socal, scale: 300, 
                      crs: 'EPSG:5070', maxPixels: 1e13, formatOptions: {
    cloudOptimized: true
  }});   