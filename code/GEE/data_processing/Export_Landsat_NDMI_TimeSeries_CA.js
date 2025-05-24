//Author: Carl A. Norlen
//Purpose: Create annual time series of NDMI calculate for Aug-Oct

//Importing Scripts with functions
//Functions for batch exporting of Image Collection
var batch = require('users/fitoprincipe/geetools:batch');

//Functions for loading Landsat 5,7, and 8 data
var landsat = require('users/cnorlen/subsequent_drought/functions:All_Landsat');

//Functions for combining Landsat data into temporal composites
var composites = require('users/cnorlen/subsequent_drought/functions:composites');

//Functions for calcuation Vegetation Indices
var veg_indices = require('users/cnorlen/subsequent_drought/functions:veg_indices');

//Functions for masking Spatial data
var mask = require('users/cnorlen/subsequent_drought/functions:mask');

//Add a California state perimeter
var region = ee.FeatureCollection('TIGER/2018/States')
              .filter(ee.Filter.eq('NAME', 'California'));

//Define a function that adds a 5-km +/- 2-km buffer to a polygon
var add_buffer = function(f) {
  return f.buffer(50000,2000);
};

//Make a new California perimeter with a 5-km buffer added
var region_buffer = region.map(add_buffer);

//Define the start and end date for the data processing
var startDate = '1984-05-01';

var endDate   = '2019-12-31';

//Add data for Landsat 5, 7, and 8.
var L578 = landsat.L578(startDate, endDate, region_buffer)
                  .map(veg_indices.addTC); //Calculate Tasselled Cap (TC) indices

//Mask the Image Collection with a simple mask based on TC Brightness
var L578_mask = mask.addTmaskLiteCA(L578);

//Calculate and select NDMI band
var NDMI = L578_mask.map(veg_indices.addNDMI)
               .select('NDMI');

//Create a composite for Landsat 5, 7, 8 starting in 1984. Average the scenes over three month periods (seasons).
//Argument are: ImageCollection to export, reducer, first composite start date, first composite end date, 
// # of images, time increment, time unit, tile scale (bigger makes parallel processing easier)
var ndmiTS = composites.temporalReduce(NDMI, ee.Reducer.mean(), ee.Date('1984-08-01'), ee.Date('1984-10-31'), 36, 1, 'year', 16);

//Export the annual NDMI time series
batch.Download.ImageCollection.toAsset(ndmiTS.select([0],['NDMI']),
                                       'L578_Composites/byYear_NDMI_v2', 
                                        {name: 'byYear_NDMI_v2', scale: 30, 
                                        maxPixels: 1e13, region: region_buffer});