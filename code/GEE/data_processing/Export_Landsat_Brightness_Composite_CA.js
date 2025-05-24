//Author: Carl A. Norlen
//Purpose: Create composites of Landsat Brightness mean and standard deviation

//Importing Scripts with functions
//Import scripts for masking data
var mask = require('users/cnorlen/subsequent_drought:functions/mask');

//Add functions for calculating veg indices, and merging data L7 and L8
var veg_ind_calc = require('users/cnorlen/subsequent_drought:functions/veg_indices');

//Batch export functions form, GEE Tools package create by fitoprincipe GEE user.
var batch = require('users/fitoprincipe/geetools:batch');

// Specify the start and end dates
var startDate = '1984-05-01';
var endDate   = '2019-12-31';

//Specify the region
var ca = ee.FeatureCollection('TIGER/2018/States')
              .filter(ee.Filter.eq('NAME', 'California'));

//Add a 50 km buffer with a 2km error margin
var add_buffer = function(f) {
  return f.buffer(50000,2000);
};

//Create a California perimeter with a 5-km added buffer
var region = ca.map(add_buffer);

//Get Landsat 5 data
var dataset_LT05 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
                  .filterDate(startDate, '2012-04-30')
                  .filter(ee.Filter.bounds(region))
                  .map(mask.cloudMaskL457)
                  .select('B1','B2','B3','B4','B5','B6','B7');

//Get Landsat 7 data
var dataset_LE07 = ee.ImageCollection('LANDSAT/LE07/C01/T1_SR')
                  .filterDate('2000-01-01', endDate)
                  .filter(ee.Filter.bounds(region))
                  .map(mask.cloudMaskL457)
                  .select('B1','B2','B3','B4','B5','B6','B7');

//Get Landsat 8 data
var dataset_LE08 = ee.ImageCollection('LANDSAT/LC08/C01/T1_SR')
                  .filterDate('2013-05-01', endDate)
                  .filter(ee.Filter.bounds(region))
                  .map(mask.maskL8sr)
                  .map(veg_ind_calc.harmonizationRoy)
                  .select('B1','B2','B3','B4','B5','B6','B7');

//Merge Landsat 5, 7, and 8
var collection = dataset_LT05.merge(dataset_LE07).merge(dataset_LE08)
                             .map(veg_ind_calc.addVIs); //Apply function to calculate vegetation indices (VIs)

//Create mean brightness composite
var bright_mean = (collection.select('brightness')).mean().rename('brightness_mean');

//Create Brightness Std. Dev. composite
var bright_sd = (collection.select('brightness')).reduce(ee.Reducer.stdDev()).rename('brightness_stdDev');

//Combine the two images in a list
var images = [bright_mean, bright_sd]; 

//Make the list of images an Image Collection
var collect = ee.ImageCollection(images);

//Export the two images as a GEE Asset
batch.Download.ImageCollection.toAsset(collect, 
                                       'L578_Composites/allYear_VI_v2', 
                                      {name: 'allYear_VI_v2', 
                                      scale: 30, 
                                      maxPixels: 1e13, 
                                      region: region});