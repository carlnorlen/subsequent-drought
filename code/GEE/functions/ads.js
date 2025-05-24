//Author: Carl A. Norlen
//Purpose: Create annual time series of USFS Aerial Detection Survey Data (ADS)

//Add functions that are good for creating mosaics/composites of Landsat data.
var composites = require('users/cnorlen/Scripts:composites');

//Add functions for joining image collections together
var join = require('users/cnorlen/Scripts:join');

//Define a function for extracting the date from a text string property of the ADS data
var addDate = function(image) {
    var begin = ee.String(image.get('start_date'));
    var begin_date = ee.Date.parse('M/d/YYYY', begin);
    var end = ee.String(image.get('end_date'));
    var end_date = ee.Date.parse('M/d/YYYY', end);
    return image.set('system:time_start', begin_date.millis())
                .set('system:time_end', end_date.millis());
};

//Define a function that converts the data to Float format
var toFloat = function(image) {
                              return image.toFloat();
};

//Reproject the raster image in Albers projection at 30-m
var reproject = function(image) {
                 return image.reproject({crs: 'EPSG: 5070', scale: 30});
};

//Load the first part of the ADS data set
var ads1 = ee.ImageCollection('projects/ca-ecs/Rasterized/ads1');

//Load the second part of the ADS data set
var ads2 = ee.ImageCollection('projects/ca-ecs/Rasterized/ads2');

//Load the flown area part of the ADS data set
var flown = ee.ImageCollection('projects/ca-ecs/Rasterized/flown');//.map(addDate)

//Load the ADS feature for 2017
var ads_2017 = ee.FeatureCollection('projects/ca-ecs/ADS_features/ADS_2017');

//Rasterize the ADS feature for 2017
var ads_2017_raster = ads_2017.reduceToImage(['TPA1'],ee.Reducer.first())
                              .rename('tpa')
                              .set('start_date', '9/1/2017')
                              .set('end_date', '10/31/2017'); 

//Load the ADS feature for 2019
var ads_2019 = ee.FeatureCollection('projects/ca-ecs/ADS_features/ADS_2017');

//Rasterize the ADS feature for 2019
var ads_2019_raster = ads_2019.reduceToImage(['MORT_TPA'],ee.Reducer.first())
                              .rename('tpa')
                              .set('start_date', '9/1/2019')
                              .set('end_date', '10/31/2019'); 

//Combine the 2017 and 2019 ADS data into an Image Collection
var ads3 = ee.ImageCollection.fromImages([ads_2017_raster, ads_2019_raster]);

//Merge all of the ADS data, add a date property, and rename the bands as 'tpa'
var ads = ads1.merge(ads2).merge(ads3).merge(flown)                                                            // .select([0],['tph']);
                          .map(addDate)
                          .select([0], ['tpa']);

//Combine the ADS data into a Time Series with on band per image
var ads_combine = composites.temporalReduce(ads, ee.Reducer.max(), ee.Date('1985-09-01'), ee.Date('1985-10-31'), 35, 1, 'year', 4);

//Unmask ADS data and add -9999 for masked areas
var fixMask = function(img) {
                             return img.unmask(-9999);
                             //var mask = combine.neq(-1);
                             //return img.updateMask(mask);
};

//Create California Perimeter mask
//Add the California Perimeter
var region = ee.FeatureCollection('TIGER/2018/States')
              .filter(ee.Filter.eq('NAME', 'California'));

//Convert the California perimeter into a raster
var CA_raster = region.reduceToImage(['ALAND'],ee.Reducer.first()).rename('region'); 

//Create a mask out of the rasterized California perimeter
var CAmask = function(image) {var mask = CA_raster.gte(1);
                              return image.updateMask(mask);
};

//Create the ADS time series
var ads_fill = composites.gapFillcollect(ads_combine, ['tpa_max']).map(join.addYear)
                                                                  .map(reproject)
                                                                  .map(toFloat)
                                                                  .map(fixMask) //Add -9999 for no data areas
                                                                  .map(CAmask); //Add CA perimeter mask

//Export the ADS time series to another script                                                                
exports.TS = ads_fill;