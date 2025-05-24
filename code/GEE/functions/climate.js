//Author: Carl A. Norlen
//Purpose: Prepare annual climate data for analysis

//Add functions for creating masks
var mask = require('users/cnorlen/Scripts:mask');

//Add functions for calculating vegetation indices.
var veg_indices = require('users/cnorlen/Scripts:veg_indices');

//Add functions for downscaling data layers
var resolution = require('users/cnorlen/Scripts:resolution');

//Add functions for joining functions
var join = require('users/cnorlen/Scripts:join');

//Get the PRISM data and select the precip. layer.    
var prism = ee.ImageCollection('OREGONSTATE/PRISM/AN81m')
                  .filter(ee.Filter.date('1984-10-01', '2019-09-30'));

//Select temperature max and min from the PRISM data set
var temp = prism.select(['tmax','tmin']);

//Select precipitation data from the PRISM data set
var precip = prism.select('ppt');

//Calculate the mean temperature max and mean for each water year
var year_mean = ee.List.sequence(0, 34).map(function(n) {
  var start = ee.Date('1984-10-01').advance(n, 'year');
  var end = ee.Date('1985-09-30').advance(n, 'year');
  return temp
        .filterDate(start, end)
        .mean()
        .select([0,1],['tmax','tmin'])
        .set('system:time_start', start.millis())
        .set('system:time_end', end.millis());
});

//Calculate the total annual precipitation for each water year
var year_sum = ee.List.sequence(0, 34).map(function(n) {
  var start = ee.Date('1984-10-01').advance(n, 'year');
  var end = ee.Date('1985-09-30').advance(n, 'year');
  return precip
        .filterDate(start, end)
        .reduce({reducer:ee.Reducer.sum(),parallelScale:16})
        .select([0],['ppt'])
        .set('system:time_start', start.millis())
        .set('system:time_end', end.millis());
});

// Make the annual ppt values into an Image Collection
var collection1 = ee.ImageCollection(year_sum);

//Make the annual temperature values into an Image Collection
var collection2 = ee.ImageCollection(year_mean);

//Downscale precipitation to 30 meter resolution.
var precip_30m = collection1.map(resolution.down_30m);

//Downscale temperature to 30 meter resolution.
var temp_30m = collection2.map(resolution.down_30m);

    ///30 meter Time Series Exports
exports.precipTS = precip_30m;

exports.tempTS = temp_30m;

// Long term climate , time invariant
exports.addPRISMnormals = function(image) {
  var PRISM_ppt = ee.ImageCollection("OREGONSTATE/PRISM/Norm81m") // 1981-2010 monthly mean 'normals'
      .select('ppt')
      .reduce(ee.Reducer.sum())
      .rename('clm_precip_sum'); 
  var PRISM_ppt_30m = resolution.down_30m(PRISM_ppt);
  var PRISM_temp = ee.ImageCollection("OREGONSTATE/PRISM/Norm81m") // 1981-2010 monthly mean 'normals'
      .select('tmean')
      .reduce(ee.Reducer.mean())
      .rename('clm_temp_mean'); 
  var PRISM_temp_30m = resolution.down_30m(PRISM_temp);
  var PRISM_vpd = ee.ImageCollection("OREGONSTATE/PRISM/Norm81m") // 1981-2010 monthly mean 'normals'
      .select('vpdmax')
      .reduce(ee.Reducer.mean())
      .rename('clm_vpd_mean'); 
  var PRISM_vpd_30m = resolution.down_30m(PRISM_vpd);
  return image.addBands(PRISM_ppt_30m)
              .addBands(PRISM_temp_30m)
              .addBands(PRISM_vpd_30m);
};
