//Author: Carl A. Norlen

//Purpose: Extract a time series data set for California 

//Add functions that are good for creating mosaics/composites of Landsat data.
var composites = require('users/cnorlen/subsequent_drought:functions/composites');

//Add functions for creating masks
var mask = require('users/cnorlen/subsequent_drought:functions/mask');

//Add functions for calculating vegetation indices.
var veg_indices = require('users/cnorlen/subsequent_drought:functions/veg_indices');

//Add functions for downscaling data layers
var resolution = require('users/cnorlen/subsequent_drought:functions/resolution');

//Add functions for converting data formats and downloading data
var download = require('users/cnorlen/subsequent_drought:functions/download');

//Add functions for joining data sets
var join = require('users/cnorlen/subsequent_drought:functions/join');

//Add functions adding biomass layers
var biomass = require('users/cnorlen/subsequent_drought:functions/biomass');

//Add functions for getting climate data (temp and precip)
var climate = require('users/cnorlen/subsequent_drought:functions/climate');

//Add functions for getting USFS ADS data
var ads = require('users/cnorlen/subsequent_drought:functions/ads');

//Add functions for getting Pr-ET data
var pet = require('users/cnorlen/subsequent_drought:functions/pet');

//Define polygon to use a region of interest for the study.
var socal = ee.Geometry.Rectangle(-120.5, 38.9, -115.5, 32.2);
Map.addLayer(socal);

//Add a date extract from a start_date string
var addDate = function(image) {
    var begin = ee.String(image.get('start_date'));
    var begin_date = ee.Date.parse('M/d/YYYY', begin);
    var end = ee.String(image.get('end_date'));
    var end_date = ee.Date.parse('M/d/YYYY', end);
    return image.set('system:time_start', begin_date.millis())
                .set('system:time_end', end_date.millis());
};

//Define a function to make data in an image a=in float format
var toFloat = function(image) {
                              return image.toFloat();
};

//Add ADS Time Series
var adsTS = ads.TS;

//Add the Pr-ET data
var PET = pet.annualTS.map(join.addEndYear);


//Function to calculate 4 yr Pr-ET
var PET_4yr_sum = ee.List.sequence(0, 31).map(function(n) {
  var start = ee.Date('1984-10-01').advance(n, 'year');
  var end = ee.Date('1985-09-30').advance(ee.Number(3).add(n), 'year');
  return PET.select(['PET','ET','ppt'])
        .filterDate(start, end)
        .reduce({reducer:ee.Reducer.sum(),parallelScale:16})
        .select([0,1,2],['PET','ET','ppt'])
        .set('system:time_start', start.millis())
        .set('system:time_end', end.millis());
});

//Make four-year Pr-ET time series an Image Collection
var PET_4yr = ee.ImageCollection(PET_4yr_sum)
                .map(join.addEndYear); //Add an end year to the four-year Pr-ET

//Add annual NDMI data
var NDMI = ee.ImageCollection('projects/ca-ecs/L578_Composites/byYear_NDMI_v2').select([0],['NDMI']).map(join.addYear);

//Calculating a time series of dNDMI for 1984-1997
var dNDMI_1 = ee.List.sequence(0, 14).map(function(n) {
  var pre_start = ee.Date('1984-08-01').advance(n, 'year');
  var pre_end = ee.Date('1986-10-31').advance(n, 'year');
  var post_start = ee.Date('1990-08-01').advance(n, 'year');
  var post_end = ee.Date('1991-10-31').advance(n, 'year');
  var NDMI_pre = NDMI.filterDate(pre_start, pre_end).mean();
  var NDMI_post = NDMI.filterDate(post_start, post_end).mean();
  var dNDMI = NDMI_post.subtract(NDMI_pre);
  return dNDMI.select(['NDMI'], ['dNDMI'])
              .set('system:time_start', post_end.millis());
});

//Convert to an Image Collection
var dNDMI_1 = ee.ImageCollection(dNDMI_1);

//Calculating a time series of dNDMI for 1998-2019
var dNDMI_2 = ee.List.sequence(0, 13).map(function(n) {
  var pre_start = ee.Date('1998-08-01').advance(n, 'year');
  var pre_end = ee.Date('2000-10-31').advance(n, 'year');
  var post_start = ee.Date('2005-08-01').advance(n, 'year');
  var post_end = ee.Date('2006-10-31').advance(n, 'year');
  var NDMI_pre = NDMI.filterDate(pre_start, pre_end).mean();
  var NDMI_post = NDMI.filterDate(post_start, post_end).mean();
  var dNDMI = NDMI_post.subtract(NDMI_pre);
  return dNDMI.select(['NDMI'], ['dNDMI'])
              .set('system:time_start', post_end.millis());
});

//Convert to an Image Collection
var dNDMI_2 = ee.ImageCollection(dNDMI_2);

//Merging the dNDMI into a time series
var dNDMI = dNDMI_1.merge(dNDMI_2).map(join.addYear);

//Add the temperature time series and add a year property for joining
var temp = climate.tempTS.map(join.addEndYear);

//Create dummy biomass data for 2018 and 2019
var biomass_2018 = ee.Image([0]).rename('biomass')
                                .set('system:time_start', ee.Date('2018-06-01').millis())
                                .set('system:time_end', ee.Date('2018-08-31').millis());
var biomass_2019 = ee.Image([0]).rename('biomass')
                                .set('system:time_start', ee.Date('2019-06-01').millis())
                                .set('system:time_end', ee.Date('2019-08-31').millis());

//Make the dummy bimoass into an Image Collection
var biomass_2 = ee.ImageCollection([biomass_2018, biomass_2019]);

//Combined the time series into one image collection. 
var biomass = biomass.ca_biomass.merge(biomass_2)
                                   .select([0],['biomass'])
                                   .map(join.addYear);

//Combine the Pr-ET and Temperature time series
var all_combined = join.simpleJoin(PET,temp,'year');

//Next add LT NDMI
all_combined = join.simpleJoin(NDMI, all_combined, 'year');

//Add Biomass time series
all_combined = join.simpleJoin(all_combined, biomass, 'year');

//Add dNDMI time series
all_combined = join.simpleJoin(all_combined, dNDMI, 'year');

//Add ADS time series
all_combined = join.simpleJoin(all_combined, adsTS, 'year');

//Reproject the data into CONUS Albers projection
var reproject = function(image) {
                 return image.reproject({crs: 'EPSG: 5070', scale: 30});
};
                    
//Reproject the data from 30-m to 300-m
var regrid300m = function(image) {
                                  var reprojected = image.reproject({crs: 'EPSG: 5070', scale: 30})
                                                         .reduceResolution({reducer: ee.Reducer.median()
                                                         .combine({reducer2: ee.Reducer.mode(), sharedInputs: true})
                                                         .combine({reducer2: ee.Reducer.count(), sharedInputs: true})
                                                         .combine({reducer2: ee.Reducer.mean(), sharedInputs: true}) 
                                                         .combine({reducer2: ee.Reducer.stdDev(), sharedInputs: true}), maxPixels: 1024})
                                                         .reproject({crs: 'EPSG: 5070', scale: 300});
                                  return reprojected;
};

//Select pixels that had add least 40 pixels to be regridded
var countMask = function(image) {
                         var mask_count = image.select('type_2001_count')
                                                   .gte(40); //Selects all regridded pixels with 13 or more input pixels.
                         var masked = image.updateMask(mask_count)
                                           .updateMask(1);  //Changes partially transparent pixels to opaque.
                         return masked;
};

//USFS Eco Map Subsections, can be used for subsetting FIA data.
var usfs = ee.FeatureCollection('projects/ca-ecs/USFS/EcomapSubsections');

//Sierra Nevada Subsections startw with M261, Southern California subsections start with M262
var sierra = usfs.filter(//Sierra Nevada USFS Subsections
                         ee.Filter.or(
                         ee.Filter.eq('MAP_UNIT_S', 'M261Ep'), //South Sierra Nevada
                         ee.Filter.eq('MAP_UNIT_S', 'M261Es'),ee.Filter.eq('MAP_UNIT_S', 'M261Eu'), 
                         ee.Filter.eq('MAP_UNIT_S', 'M261Er'), 
                         ee.Filter.eq('MAP_UNIT_S', 'M261Eo'), ee.Filter.eq('MAP_UNIT_S', 'M261Eq'), 
                         //Southern California USFS Subsections
                         ee.Filter.eq('MAP_UNIT_S', 'M262Bh'),ee.Filter.eq('MAP_UNIT_S', 'M262Bg'), 
                         ee.Filter.eq('MAP_UNIT_S', 'M262Bp'),ee.Filter.eq('MAP_UNIT_S', 'M262Be'),
                         ee.Filter.eq('MAP_UNIT_S', 'M262Bm'),ee.Filter.eq('MAP_UNIT_S', 'M262Bd'),
                         ee.Filter.eq('MAP_UNIT_S', 'M262Bl'),ee.Filter.eq('MAP_UNIT_S', 'M262Bc'),
                         ee.Filter.eq('MAP_UNIT_S', 'M262Bi'),ee.Filter.eq('MAP_UNIT_S', 'M262Bo'),
                         ee.Filter.eq('MAP_UNIT_S', 'M262Bf'),
                         ee.Filter.eq('MAP_UNIT_S', 'M262Ba'), ee.Filter.eq('MAP_UNIT_S', 'M262Bb')
                         ));

//Create USFS function
//Rasterize US Forest Service polygons
var numberParse = function(feature) {
   return feature.set('MAP_UNIT_S', ee.Number.parse(ee.String(feature.get('MAP_UNIT_S')).slice(1,4)));
};
var USFS_zone_num = sierra.map(numberParse, true); //Second argument is drop nulls
var USFS_zone = USFS_zone_num.reduceToImage(['MAP_UNIT_S'],ee.Reducer.first()).rename('USFS_zone'); 

//Define a function for masking for only USFS regions that I selected earlier
var addUSFSmask = function(image) {
             var USFSmask = USFS_zone.eq(262).or(USFS_zone.eq(261)); //Select Sierra Nevada (261) or Southern California Regions (262)
             return image.updateMask(USFSmask);
};

//Convert data type to a Float
var toFloat = function(image) {
                              return image.toFloat();
};

//Mask the entire regridded region
var all_regrid = all_combined
                    .map(addUSFSmask)
                    .map(mask.addConiferMask_LF)
                    .map(mask.addFireMask)
                    //.map(addType) //Is this necessary?
                    .map(regrid300m)
                    .map(countMask)
                    .map(toFloat);

//Create categorical ADS values
var adsCalc = function(img) {var tpa = img.select('tpa');
                             var ads1 = tpa.gte(3).rename('tpa_low');
                             var ads2 = tpa.gte(5).rename('tpa_mid');
                             var ads3 = tpa.gte(8).rename('tpa_high');
                             return img.addBands(ads1)
                                       .addBands(ads2)
                                       .addBands(ads3);
};

//Rename the variables with _mean removed
var all_filter = all_regrid.select(['NDMI_mean', 'dNDMI_mean', 'biomass_mean', 'PET_mean', 'ET_mean', 'ppt_mean', 'tmax_mean', 'tpa_max_mean'], 
                                   ['NDMI', 'dNDMI', 'biomass', 'PET', 'ET', 'ppt', 'tmax', 'tpa']) 
                           .map(adsCalc);

//Add SPI data
var spi9 = ee.ImageCollection('users/cnorlen/California_Data/SPI48_9');

//Select pixels that had add least 40 pixels to be regridded
var countSPImask = function(image) {
                         var mask_count = image.select('b1_count')
                                                   //.mean()
                                                   .gte(40); //Selects all regridded pixels with 13 or more input pixels.
                         var masked = image.updateMask(mask_count)
                                           .updateMask(1);  //Changes partially transparent pixels to opaque.
                         return masked;
};

//Reproject data and add new masks
var spi9_30m = spi9.map(resolution.down_30m)
                   .map(reproject)
                   .map(addUSFSmask)
                   .map(mask.addConiferMask_LF)
                   .map(mask.addFireMask)
                   .map(regrid300m)
                   .map(countSPImask)
                   .map(toFloat)
                   .select(['b1_mean'],['b1']);

//Select SPI48 for 1999-2002
var spi_2002 = spi9_30m.filter(ee.Filter.date('2002-09-01')).first()
                                                            .divide(100);
//Select SPI48 for 2012-2015
var spi_2015 = spi9_30m.filter(ee.Filter.date('2015-09-01')).first()
                                                            .divide(100);

//Calculate SPI48 difference
var dif = spi_2002.subtract(spi_2015).abs();

//Define a function apply a mask for both droughts
var addSPImaskBoth = function(image) {
                  var mask = spi_2002.lte(-1.5).and(spi_2015.lte(-1.5)).and(dif.lte(0.5)); //original value was -150
                  return image.updateMask(mask);
};

//Define a function to apply a mask for 2012-2015 Only droughts
var addSPImask2nd = function(image) {
                  var mask = spi_2015.lte(-1.5).and(spi_2002.gt(-1.5)).and(dif.gt(0.5)).and(spi_2002.gt(spi_2015));//; //original value was -150
                  return image.updateMask(mask);
};

//Do the both drought SPI mask
var first_drought_filter = all_filter.map(addSPImaskBoth);

//Do the second drought SPI mask          
var second_drought_filter = all_filter.map(addSPImask2nd);

//Select the reducers for getting the imte series.
var reducer = ee.Reducer.percentile([0,25,50,75,100])
                        .combine({reducer2: ee.Reducer.mean(), sharedInputs: true})
                        .combine({reducer2: ee.Reducer.stdDev(), sharedInputs: true})
                        .combine({reducer2: ee.Reducer.count(), sharedInputs: true})
                        .combine({reducer2: ee.Reducer.sum(), sharedInputs: true});

//Calculate the reduced time series for Both Droughts
var first_drought_export = download.reduceImageCollection(first_drought_filter, ee.Date('1991-08-01'), 1, 'year', 29, 'system:time_start', reducer, socal, 300, 1e13, 16);

//Calculate the reduced time series for 2012-2015 Only
var second_drought_export = download.reduceImageCollection(second_drought_filter, ee.Date('1991-08-01'), 1, 'year', 29, 'system:time_start', reducer, socal, 300, 1e13, 16);

//Export the time series .CSV files
//First drought export
Export.table.toDrive({collection: first_drought_export, 
                      description: 'dNDMI_NDMI_PET_Temp_ADS_trajectories_first_drought_full_region_300m_v10',
                      folder: 'final_socal_data',
                      fileFormat: 'CSV'});

//Second drought export
Export.table.toDrive({collection: second_drought_export, 
                      description: 'dNDMI_NDMI_PET_Temp_ADS_trajectories_second_drought_full_region_300m_v10',
                      folder: 'final_socal_data',
                      fileFormat: 'CSV'});