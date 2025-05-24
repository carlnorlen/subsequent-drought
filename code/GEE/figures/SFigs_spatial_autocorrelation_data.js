//Author: Carl A. Norlen

//Purpose: Extract data for create datasets for Figure 3 and Figure 5

//Add functions that are good for creating mosaics/composites of Landsat data.
var composites = require('users/cnorlen/subsequent_drought:functions/composites');

//Add functions for creating masks
var mask = require('users/cnorlen/subsequent_drought:functions/mask');

//Add functions for downscaling data layers
var resolution = require('users/cnorlen/subsequent_drought:functions/resolution');

//Add functions for converting data formats and downloading data
var download = require('users/cnorlen/subsequent_drought:functions/download');

//Add biomass ImageCollection
var biomass = require('users/cnorlen/subsequent_drought:functions/biomass');

//Add functions for joining data sets
var join = require('users/cnorlen/subsequent_drought:functions/join');
//print(san_bern_gab);

//Add functions for joining data sets
var pet = require('users/cnorlen/subsequent_drought:functions/pet');

//Functions for filtering by Landfire veg type
//var landfire = require('users/cnorlen/subsequent_drought:functions/landfire');

//Function for adding ADS mortality 
var ads = require('users/cnorlen/subsequent_drought:functions/ads');

//Function for adding eMapR tree cover 
var frap = require('users/cnorlen/subsequent_drought:functions/frap');

//Function for adding PRISM_climatology 
var climatology = require('users/cnorlen/subsequent_drought:functions/climatology');

//Function for adding PRISM_climatology 
var climate = require('users/cnorlen/subsequent_drought:functions/climate');

//Add palettes package
var palettes = require('users/gena/packages:palettes');

//Define polygon to use a region of interest for the study.
var socal = ee.Geometry.Rectangle(-122.5, 40.9, -115.5, 32.2); 


//FeatureCollection that includes EcoRegions for the CONUS.
var region = ee.FeatureCollection('TIGER/2018/States')
              .filter(ee.Filter.eq('NAME', 'California'));

//Add a 5-km buffer to              
var add_buffer = function(f) {
  return f.buffer(50000,2000);
};

//Create a California perimeter with a 5-km buffer added
var region_buffer = region.map(add_buffer);

//Function for reprojecting to 30 meters in Albers projection
var reproject = function(image) {
                 return image.reproject({crs: 'EPSG: 5070', scale: 30});
};

//Load different years of Vegetation type data.
var type = ee.ImageCollection('users/cnorlen/California_Data/LANDFIRE/EV_Type');

//Select the LANDFIRE Vegetation Type for 2001
var type_2001 = type.filter(ee.Filter.date('2001-01-01')).max()
                    .select([0],['type_2001']);

//Select the LANDFIRE Vegetation Type for 2008                    
var type_2008 = type.filter(ee.Filter.date('2008-01-01')).max()
                    .select([0],['type_2008']);

//Add Biomass and Tree Cover datasets
var land_biomass = biomass.ca_biomass;

//Add Lat Lon Image
var coordinates = ee.Image.pixelLonLat();

//Add USGS DEM elevationa and aspect
var elevation = ee.Image("USGS/NED");
var aspect = ee.Terrain.aspect(elevation);

//Add climatology data
var precip_climate = climatology.precip_climate;
var temp_climate = climatology.temp_climate;

//Add the USFS ADS Time Sereis
var adsTS = ads.TS;

//Calculate the ADS mortality for 1999-2005
var ads_2004 = adsTS.filter(ee.Filter.calendarRange({start: 1999, end: 2005, field: 'year'})).max();

//Calculate the ADS mortality for 2012-2018
var ads_2017 = adsTS.filter(ee.Filter.calendarRange({start: 2012, end: 2018, field: 'year'})).max();

//Select eMapR biomass layers for other years
//Biomass 1999
var biomass_1999 = land_biomass.filter(ee.Filter.date('1999-06-01T00:00'))
                               .select(['biomass'],['biomass_1999'])
                               .first();

//Biomass 2004
var biomass_2004 = land_biomass.filter(ee.Filter.date('2004-06-01T00:00'))
                               .select(['biomass'],['biomass_2004'])
                               .first();

//Biomass 2012
var biomass_2012 = land_biomass.filter(ee.Filter.date('2012-06-01T00:00'))
                               .select(['biomass'],['biomass_2012'])
                               .first();
                               
//Biomass 2017
var biomass_2017 = land_biomass.filter(ee.Filter.date('2017-06-01T00:00'))
                               .select(['biomass'],['biomass_2017'])
                               .first();

//Add NDMI data set                                
var NDMI = ee.ImageCollection('projects/ca-ecs/L578_Composites/byYear_NDMI_v2')
                .select('NDMI');  

//Add a year field to the NDMI Image Collecgtion and reproject the images.
var NDMIyears = NDMI.map(join.addYear)
                    .map(reproject);

// Calculate dNDMI for (1999  -2001) - (2002 - 2004)
var NDMI1999_01 = NDMIyears.filter(ee.Filter.calendarRange({start: 1997, end: 1999, field: 'year'})).mean();
var NDMI2002_04 = NDMIyears.filter(ee.Filter.calendarRange({start: 2003, end: 2004, field: 'year'})).mean();
var dNDMI_2004 = NDMI2002_04.subtract(NDMI1999_01);

// Calculate dNDMI for (2010 - 2012) - (2014 - 2016)
var NDMI2010_12 = NDMIyears.filter(ee.Filter.calendarRange({start: 2009, end: 2011, field: 'year'})).mean(); 
var NDMI2015_17 = NDMIyears.filter(ee.Filter.calendarRange({start: 2016, end: 2017, field: 'year'})).mean();
var dNDMI_2017 = NDMI2015_17.subtract(NDMI2010_12);

//Add annual ET data to the script.
var ET = pet.annualTS.select('ET').map(reproject);

//Add temperature Time Series data
var temp = climate.tempTS;

//Select the Pr-ET, ET, ppt, and NDVI data
var PET = pet.annualTS.select(['PET', 'ET', 'ppt','NDVI']).map(reproject);

//Calculate four-year Pr-ET
var PET_4year_sum = ee.List.sequence(0, 30).map(function(n) {
  var start = ee.Date('1984-10-01').advance(n, 'year');
  var end = ee.Date('1988-09-30').advance(n, 'year');
  return PET.select(['PET','ET', 'ppt'])
        .filterDate(start, end)
        .reduce({reducer:ee.Reducer.sum(),parallelScale:4})
        .select(['PET_sum','ET_sum','ppt_sum'],['PET','ET', 'ppt'])
        .set('system:time_start', start.millis())
        .set('system:time_end', end.millis());
});

//Calculate the four-year Tmax
var tmax_4year_mean = ee.List.sequence(0, 30).map(function(n) {
  var start = ee.Date('1984-10-01').advance(n, 'year');
  var end = ee.Date('1988-09-30').advance(n, 'year');
  return temp.select(['tmax'])
        .filterDate(start, end)
        .reduce({reducer:ee.Reducer.mean(),parallelScale:4})
        .select([0],['tmax'])
        .set('system:time_start', start.millis())
        .set('system:time_end', end.millis());
});

//Convert the four yoear data sets into Image Collection
var PET_4year = ee.ImageCollection(PET_4year_sum).select('PET');
var ppt_4year = ee.ImageCollection(PET_4year_sum).select('ppt');
var ET_4year = ee.ImageCollection(PET_4year_sum).select('ET');
var tmax_4year = ee.ImageCollection(tmax_4year_mean).select('tmax');

//Select P-ET, precip, temp 4 year, start: 1988-10-01, end: 1992-09-30
var PET_4yr_1992 = PET_4year.filterDate('1988-10-01').first();
var ppt_4yr_1992 = ppt_4year.filterDate('1988-10-01').first();
var ET_4yr_1992 = ET_4year.filterDate('1988-10-01').first();
var tmax_4yr_1992 = tmax_4year.filterDate('1988-10-01').first();


//Select P-ET 4 year, precip, temp start: 1998-10-01, end: 2002-09-30
var PET_4yr_2002 = PET_4year.filterDate('1998-10-01').first();
var ppt_4yr_2002 = ppt_4year.filterDate('1998-10-01').first();
var ET_4yr_2002 = ET_4year.filterDate('1998-10-01').first();
var tmax_4yr_2002 = tmax_4year.filterDate('1998-10-01').first();

//Select P-ET 4 year, precip, temp start: '2004-10-01', end: '2009-09-30'
var PET_4yr_2009 = PET_4year.filterDate('2004-10-01').first();
var ppt_4yr_2009 = ppt_4year.filterDate('2004-10-01').first();
var ET_4yr_2009 = ET_4year.filterDate('2004-10-01').first();
var tmax_4yr_2009 = tmax_4year.filterDate('2004-10-01').first();

//Select P-ET 4 year, precip, temp start: '2011-10-01', end: '2015-09-30'
var PET_4yr_2015 = PET_4year.filterDate('2011-10-01').first();
var ppt_4yr_2015 = ppt_4year.filterDate('2011-10-01').first();
var ET_4yr_2015 = ET_4year.filterDate('2011-10-01').first();
var tmax_4yr_2015 = tmax_4year.filterDate('2011-10-01').first();

//Add SPI48 drougth metric
var spi9 = ee.ImageCollection('users/cnorlen/California_Data/SPI48_9');

//Interpolate the SPI48 4km data set to 30 m
var spi9_30m = spi9.map(resolution.down_30m)
                   .map(reproject);

//Select SPI48_9 for 2002
var spi_2002 = spi9_30m.filter(ee.Filter.date('2002-09-01')).first()
                                                            .divide(100); //Convert SPI to decimal

//Select SPI48_9 for 2015
var spi_2015 = spi9_30m.filter(ee.Filter.date('2015-09-01')).first()
                                                            .divide(100);

//USFS Eco Map Subsections, can be used for subsetting FIA data.
var usfs = ee.FeatureCollection('projects/ca-ecs/USFS/EcomapSubsections');

//Sierra Nevada Subsections startw with M261, Southern California subsections start with M262
var sierra = usfs.filter(//Sierra Nevada
                         ee.Filter.or(
                         ee.Filter.eq('MAP_UNIT_S', 'M261Ep'),ee.Filter.eq('MAP_UNIT_S', 'M261Eq'), 
                         ee.Filter.eq('MAP_UNIT_S', 'M261Es'),ee.Filter.eq('MAP_UNIT_S', 'M261Eu'), 
                         ee.Filter.eq('MAP_UNIT_S', 'M261Er'), 
                         ee.Filter.eq('MAP_UNIT_S', 'M261Eo'), 
                         //South Sierra 
                         ee.Filter.eq('MAP_UNIT_S', 'M262Bh'),ee.Filter.eq('MAP_UNIT_S', 'M262Bg'), 
                         ee.Filter.eq('MAP_UNIT_S', 'M262Bp'),ee.Filter.eq('MAP_UNIT_S', 'M262Be'),
                         ee.Filter.eq('MAP_UNIT_S', 'M262Bm'),ee.Filter.eq('MAP_UNIT_S', 'M262Bd'),
                         ee.Filter.eq('MAP_UNIT_S', 'M262Bl'),ee.Filter.eq('MAP_UNIT_S', 'M262Bc'),
                         ee.Filter.eq('MAP_UNIT_S', 'M262Bi'),ee.Filter.eq('MAP_UNIT_S', 'M262Bo'),
                         ee.Filter.eq('MAP_UNIT_S', 'M262Bf'),
                         ee.Filter.eq('MAP_UNIT_S', 'M262Ba'), ee.Filter.eq('MAP_UNIT_S', 'M262Bb')
                         )); 

//Create a rasterized version of USFS EcoRegions
var numberParse = function(feature) {
   return feature.set('MAP_UNIT_S', ee.Number.parse(ee.String(feature.get('MAP_UNIT_S')).slice(1,4)));
};
var USFS_zone_num = sierra.map(numberParse, true); //Second argument is drop nulls
var USFS_zone = USFS_zone_num.reduceToImage(['MAP_UNIT_S'],ee.Reducer.first()).rename('USFS_zone'); 

//Define a function for masking by USFS EcoRegions
var addUSFSmask = function(image) {
             var USFSmask = USFS_zone.eq(261).or(USFS_zone.eq(262)); //Select Sierra Nevada or Southern California Regions
             return image.updateMask(USFSmask);
};

//Combine data sets to be exported
var full_data = ee.Image(PET_4yr_2002.select([0],['PET_4yr_2002'])
                             .addBands(PET_4yr_2015.select([0],['PET_4yr_2015']))
                             .addBands(ppt_4yr_2002.select([0],['ppt_4yr_2002']))
                             .addBands(ppt_4yr_2015.select([0],['ppt_4yr_2015']))
                             .addBands(tmax_4yr_2002.select([0],['tmax_4yr_2002']))
                             .addBands(tmax_4yr_2015.select([0],['tmax_4yr_2015']))
                             .addBands(ET_4yr_2002.select([0],['ET_4yr_2002']))
                             .addBands(ET_4yr_2015.select([0],['ET_4yr_2015']))
                             .addBands(spi_2002.select([0],['spi48_09_2002']))
                             .addBands(spi_2015.select([0],['spi48_09_2015']))
                             .addBands(type_2001.select([0],['veg_type_2001']))
                             .addBands(type_2008.select([0],['veg_type_2008']))
                             .addBands(NDMI2002_04.select([0],['NDMI_2004']))
                             .addBands(NDMI2015_17.select([0],['NDMI_2017']))
                             .addBands(dNDMI_2004.select([0],['dNDMI_2004']))
                             .addBands(dNDMI_2017.select([0],['dNDMI_2017']))
                             .addBands(ads_2004.select([0],['ADS_2004']))
                             .addBands(ads_2017.select([0],['ADS_2017']))
                             .addBands(biomass_1999.select([0],['biomass_1999']))
                             .addBands(biomass_1999.select([0],['biomass_2004']))
                             .addBands(biomass_2012.select([0],['biomass_2012']))
                             .addBands(biomass_2017.select([0],['biomass_2017']))
                             .addBands(precip_climate.select([0],['ppt_climate']))
                             .addBands(temp_climate.select([0],['tmax_climate']))
                             .addBands(coordinates)
                             .addBands(elevation)
                             .addBands(aspect)
                             .addBands(USFS_zone)
                             );

//Add a mask for wildfire history
var fire_all = mask.addFireMask(full_data);

//Add a USFS mask
var usfs_all = addUSFSmask(fire_all);

//Add a Conifer mask
var conifer_all = mask.addConiferMask_LF(usfs_all);
print(conifer_all);

//Regrid the data from 30-m to 300-m resolution
var conifer_300m = conifer_all.reproject({crs: 'EPSG: 5070', scale: 30})
                        .reduceResolution({reducer: ee.Reducer.median()
                        .combine({reducer2: ee.Reducer.mode(), sharedInputs: true})
                        .combine({reducer2: ee.Reducer.count(), sharedInputs: true})
                        .combine({reducer2: ee.Reducer.sum(), sharedInputs: true})
                        .combine({reducer2: ee.Reducer.mean(), sharedInputs: true}), maxPixels: 1024})
                        .reproject({crs: 'EPSG: 5070', scale: 300});

//Do the task for first drought mask
var mask_count = conifer_300m.select('veg_type_2001_count').gte(40); //Selects all regridded pixels with 40 or more input pixels.

//Make the partially masked data opaque
var conifer_opaq = conifer_300m.updateMask(mask_count)
                               .updateMask(1);  //Changes partially transparent pixels to opaque.

//Remap the veg data to oak versus conifer
var remap_2001 = conifer_opaq.remap({from: [2014,2019,2027,2028,2029,2030,2031,2033,2114],
                                to: [1,2,2,2,1,1,2,2,1], bandName: 'veg_type_2001_mode'}) //1: Oaks, 2:Conifers,
                          .select([0],['veg_type_2001_remapped']);
var remap_2008 = conifer_opaq.remap({from: [2014,2019,2027,2028,2029,2030,2031,2033,2114,2131],
                                to: [1,2,2,2,1,1,2,2,1,3], bandName: 'veg_type_2008_mode'}) //1: Oaks, 2: Conifers, 3: Shrub/Grass
                          .select([0],['veg_type_2008_remapped']);
                          
//Rename the band names for exporting                          
var full = conifer_opaq.addBands(remap_2001).addBands(remap_2008) //Remapped LANDFIRE Vegetation
//Original Band Names
                    .select(['PET_4yr_2002_mean','PET_4yr_2015_mean','spi48_09_2002_mean','spi48_09_2015_mean','veg_type_2001_mode',
'veg_type_2008_mode','dNDMI_2004_mean','dNDMI_2017_mean','ADS_2004_mean', 'ADS_2017_mean', 'biomass_1999_mean',
'biomass_2004_mean','biomass_2012_mean','biomass_2017_mean', 'elevation_mean', 'aspect_median', 'longitude_mean', 'latitude_mean',
'ppt_climate_mean', 'tmax_climate_mean', 'ppt_4yr_2002_mean', 'ppt_4yr_2015_mean', 'tmax_4yr_2002_mean', 'tmax_4yr_2015_mean', 
'ET_4yr_2002_mean', 'ET_4yr_2015_mean', 'USFS_zone_mode'],
//New Band Names
['PET_4yr_2002','PET_4yr_2015','spi48_09_2002','spi48_09_2015','veg_type_2001',
'veg_type_2008','dNDMI_2004','dNDMI_2017',
'ADS_2004', 'ADS_2017', 'biomass_1999','biomass_2004','biomass_2012','biomass_2017', 'elevation', 'aspect', 'longitude', 'latitude',
'ppt_climate', 'tmax_climate','ppt_4yr_2002', 'ppt_4yr_2015','tmax_4yr_2002', 'tmax_4yr_2015',  
'ET_4yr_2002', 'ET_4yr_2015','USFS_zone']);

//Convert images to data tables for exporting
var feature_all = download.makeFeature(full, socal, 300, 1e13, 16);

//Conifer Export
//var conifer_30m;

//Regrid the data from 30-m to 300-m resolution
var conifer_90m = conifer_all.reproject({crs: 'EPSG: 5070', scale: 30})
                        .reduceResolution({reducer: ee.Reducer.median()
                        .combine({reducer2: ee.Reducer.mode(), sharedInputs: true})
                        .combine({reducer2: ee.Reducer.count(), sharedInputs: true})
                        .combine({reducer2: ee.Reducer.sum(), sharedInputs: true})
                        .combine({reducer2: ee.Reducer.mean(), sharedInputs: true}), maxPixels: 1024})
                        .reproject({crs: 'EPSG: 5070', scale: 900});

//Do the task for first drought mask
var mask_count_90m = conifer_90m.select('veg_type_2001_count').gte(360); //Selects all regridded pixels with 40 or more input pixels.

//Make the partially masked data opaque
var conifer_opaq_90m = conifer_90m.updateMask(mask_count_90m)
                               .updateMask(1);  //Changes partially transparent pixels to opaque.

//Remap the veg data to oak versus conifer
var remap_2001_90m = conifer_opaq.remap({from: [2014,2019,2027,2028,2029,2030,2031,2033,2114],
                                to: [1,2,2,2,1,1,2,2,1], bandName: 'veg_type_2001_mode'}) //1: Oaks, 2:Conifers,
                          .select([0],['veg_type_2001_remapped']);
var remap_2008_90m = conifer_opaq.remap({from: [2014,2019,2027,2028,2029,2030,2031,2033,2114,2131],
                                to: [1,2,2,2,1,1,2,2,1,3], bandName: 'veg_type_2008_mode'}) //1: Oaks, 2: Conifers, 3: Shrub/Grass
                          .select([0],['veg_type_2008_remapped']);
   
//print(conifer_opaq_90m);                      
//Rename the band names for exporting                          
var full_90m = conifer_opaq_90m.addBands(remap_2001).addBands(remap_2008) //Remapped LANDFIRE Vegetation
//Original Band Names
                    .select(['PET_4yr_2002_mean','PET_4yr_2015_mean','spi48_09_2002_mean','spi48_09_2015_mean','veg_type_2001_mode',
'veg_type_2008_mode','dNDMI_2004_mean','dNDMI_2017_mean','ADS_2004_mean', 'ADS_2017_mean', 'biomass_1999_mean',
'biomass_2004_mean','biomass_2012_mean','biomass_2017_mean', 'elevation_mean', 'aspect_median', 'longitude_mean', 'latitude_mean',
'ppt_climate_mean', 'tmax_climate_mean', 'ppt_4yr_2002_mean', 'ppt_4yr_2015_mean', 'tmax_4yr_2002_mean', 'tmax_4yr_2015_mean', 
'ET_4yr_2002_mean', 'ET_4yr_2015_mean', 'USFS_zone_mode'],
//New Band Names
['PET_4yr_2002','PET_4yr_2015','spi48_09_2002','spi48_09_2015','veg_type_2001',
'veg_type_2008','dNDMI_2004','dNDMI_2017',
'ADS_2004', 'ADS_2017', 'biomass_1999','biomass_2004','biomass_2012','biomass_2017', 'elevation', 'aspect', 'longitude', 'latitude',
'ppt_climate', 'tmax_climate','ppt_4yr_2002', 'ppt_4yr_2015','tmax_4yr_2002', 'tmax_4yr_2015',  
'ET_4yr_2002', 'ET_4yr_2015','USFS_zone']);

//Convert the 900 meter data to Feature
var feature_900m = download.makeFeature(full_90m,//.reproject({crs: 'EPSG: 5070', scale: 30}), 
                                       socal, 900, 1e13, 8);

//Load the 30 meter confier data
var full_30m = ee.Image('users/cnorlen/conifer_all_bands_30m');
                                       

//Define reduces for exporting
var reducer =  ee.Reducer.percentile([25,50,75]) 
                        .combine({reducer2: ee.Reducer.mean(), sharedInputs: true})
                        .combine({reducer2: ee.Reducer.stdDev(), sharedInputs: true})
                        .combine({reducer2: ee.Reducer.mode(), sharedInputs: true})
                        .combine({reducer2: ee.Reducer.count(), sharedInputs: true});

//Export the 300m data as a .CSV file
Export.table.toDrive({collection: feature_all,
                      description: 'Regression_all_socal_300m_v23',
                      folder: 'final_socal_data',
                      fileFormat: 'CSV'});

//Export the 30 m data as a .CSV file
Export.table.toDrive({collection: feature_900m,
                      description: 'Regression_all_socal_900m',
                      folder: 'final_socal_data',
                      fileFormat: 'CSV'});

//Select individual data layers to export as Tiff images.
var dieoff_ndmi = full.select('dNDMI_2004');
var ads_2004_export = full.select('ADS_2004');
var ads_2017_export = full.select('ADS_2017');
var dif = full.select('spi48_09_2015').subtract(full.select('spi48_09_2002')).abs();
var dNDMI_2004_export = full.select('dNDMI_2004');
var dNDMI_2017_export = full.select('dNDMI_2017');
var PrET_2002_export = full.select('PET_4yr_2002');
var PrET_2015_export = full.select('PET_4yr_2015');
var biomass_1999_export = full.select('biomass_1999');
var biomass_2012_export = full.select('biomass_2012');
var tmax_4yr_2002_export = full.select('tmax_4yr_2002');
var tmax_4yr_2015_export = full.select('tmax_4yr_2015');

//Select 30 meter layer
var dNDMI_2004_export_30m = conifer_all.reproject({crs: 'EPSG: 5070', scale: 30})
                                       .select('dNDMI_2004');

//Export drougth data sets
var drought_both = full.select('spi48_09_2015').lte(-1.5).and(full.select('spi48_09_2002').lte(-1.5).and(dif.lte(0.5))); //.and(full.select('USFS_zone').eq(262));
var drought_second = full.select('spi48_09_2015').lte(-1.5).and(full.select('spi48_09_2015').lt(full.select('spi48_09_2002')).and(dif.gt(0.5))); //.and(full.select('USFS_zone').eq(261));

//Export 30 m raster with CONUS Albers projection
Export.image.toAsset({image:conifer_all.reproject({crs: 'EPSG: 5070', scale: 30}), description: 'conifer_all_bands_30m', 
                      region: socal, scale: 30, 
                      crs: 'EPSG:5070', maxPixels: 1e13});

Export.image.toDrive({image: dNDMI_2004_export_30m, description: 'dNDMI_2004_bigger_region_30m', 
                      folder: 'final_socal_data', region: socal, scale: 30, 
                      crs: 'EPSG:5070', maxPixels: 1e13});
                      
//Export with CONUS Albers projection
Export.image.toDrive({image: dNDMI_2004_export, description: 'dNDMI_2004_bigger_region_300m_v4', 
                      folder: 'final_socal_data', region: socal, scale: 300, 
                      crs: 'EPSG:5070', maxPixels: 1e13});
Export.image.toDrive({image: dNDMI_2017_export, description: 'dNDMI_2017_bigger_region_300m_v4', 
                      folder: 'final_socal_data', region: socal, scale: 300, 
                      crs: 'EPSG:5070', maxPixels: 1e13});     
                      
Export.image.toDrive({image: ads_2004_export, description: 'ADS_2004_bigger_region_300m', 
                      folder: 'final_socal_data', region: socal, scale: 300, 
                      crs: 'EPSG:5070', maxPixels: 1e13});
Export.image.toDrive({image: ads_2017_export, description: 'ADS_2017_bigger_region_300m', 
                      folder: 'final_socal_data', region: socal, scale: 300, 
                      crs: 'EPSG:5070', maxPixels: 1e13});                       

Export.image.toDrive({image: PrET_2002_export, description: 'PrET_2002_bigger_region_300m_v4', 
                      folder: 'final_socal_data', region: socal, scale: 300, 
                      crs: 'EPSG:5070', maxPixels: 1e13});
Export.image.toDrive({image: PrET_2015_export, description: 'PrET_2015_bigger_region_300m_v4', 
                      folder: 'final_socal_data', region: socal, scale: 300, 
                      crs: 'EPSG:5070', maxPixels: 1e13}); 

Export.image.toDrive({image: tmax_4yr_2002_export, description: 'tmax_2002_bigger_region_300m', 
                      folder: 'final_socal_data', region: socal, scale: 300, 
                      crs: 'EPSG:5070', maxPixels: 1e13});
Export.image.toDrive({image: tmax_4yr_2015_export, description: 'tmax_2015_bigger_region_300m', 
                      folder: 'final_socal_data', region: socal, scale: 300, 
                      crs: 'EPSG:5070', maxPixels: 1e13});  

Export.image.toDrive({image: biomass_1999_export, description: 'biomass_1999_bigger_region_300m', 
                      folder: 'final_socal_data', region: socal, scale: 300, 
                      crs: 'EPSG:5070', maxPixels: 1e13});
Export.image.toDrive({image: biomass_2012_export, description: 'biomass_2012_bigger_region_300m', 
                      folder: 'final_socal_data', region: socal, scale: 300, 
                      crs: 'EPSG:5070', maxPixels: 1e13});

Export.image.toDrive({image: drought_both, description: 'Drought_both_bigger_region_300m_v5', 
                      folder: 'final_socal_data', region: socal, scale: 300, 
                      crs: 'EPSG:5070', maxPixels: 1e13});
Export.image.toDrive({image: drought_second, description: 'Drought_second_bigger_region_300m_v5', 
                      folder: 'final_socal_data', region: socal, scale: 300, 
                      crs: 'EPSG:5070', maxPixels: 1e13});   
                      
