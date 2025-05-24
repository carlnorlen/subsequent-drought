//Author: Carl A. Norlen
//Purpose: Create annual time series of NDVI calculated for each water year Oct - Sept

//Add functions for exporting Image Collections
var batch = require('users/fitoprincipe/geetools:batch');

//Add functions for adding Landsat data
var landsat = require('users/cnorlen/subsequent_drought/functions:All_Landsat');

//Add functions for creating composites
var composites = require('users/cnorlen/subsequent_drought/functions:composites');

//Add functions for calucating vegetation indices
var veg_indices = require('users/cnorlen/subsequent_drought/functions:veg_indices');

//Add functions for masking data
var mask = require('users/cnorlen/subsequent_drought/functions:mask');

//Add California perimeter
var region = ee.FeatureCollection('TIGER/2018/States')
              .filter(ee.Filter.eq('NAME', 'California'));

//Define function for adding a 5-km perimeter to a polygon             
var add_buffer = function(f) {
  return f.buffer(50000,2000);
};

//Create California polygon with an added 5-km perimeter
var region_buffer = region.map(add_buffer);

//Define the start and end dates for the Landsat data to extract
var startDate = '1984-05-01';
var endDate   = '2019-12-31';

// Get the Landsat 5, 7, and 8 data.
var L578 = landsat.L578(startDate, endDate, region_buffer)
                  .map(veg_indices.addTC); //Add Tasselled Cap Indices to each image

// Add a mask based on TC Brightness
var L578_mask = mask.addTmaskLiteCA(L578);

//Calculate and select NDVI band
var NDVI = L578_mask.map(veg_indices.addNDVI)
               .select('NDVI');

//Create a composite for Landsat 5, 7, 8starting in 1984. Average the scenes over three month periods (seasons).
var ndviTS = composites.temporalReduce(NDVI, ee.Reducer.mean(), ee.Date('1984-10-01'), ee.Date('1985-09-30'), 35, 1, 'year', 16);

//Export the annual NDVI time series as a GEE Assett
batch.Download.ImageCollection.toAsset(ndviTS.select([0],['NDVI']),
                                       'L578_Composites/byYear_NDVI_v2', 
                                        {name: 'byYear_NDVI_v2', scale: 30, 
                                        maxPixels: 1e13, region: region_buffer});