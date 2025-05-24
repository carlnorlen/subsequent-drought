//Author: Carl A. Norlen

//Purpose: Calculate Pr-ET from ET and precip data

//Add functions for calculating vegetation indices.
var veg_indices = require('users/cnorlen/subsequent_drought:functions/veg_indices');

//Add functions for downscaling data layers
var resolution = require('users/cnorlen/subsequent_drought:functions/resolution');

//Add functions for joining functions
var join = require('users/cnorlen/subsequent_drought:functions/join');

//Add functions for joining functions
var climate = require('users/cnorlen/subsequent_drought:functions/climate');

//Add NDVI composite data
var NDVI = ee.ImageCollection('projects/ca-ecs/L578_Composites/byYear_NDVI_v2');

//Add an ET band
var ET_NDVI = NDVI.map(veg_indices.addET);

//Add a Precipitation to Time Series 
var precip_30m = climate.precipTS;

//Join ET and Precip data together
// The primary data set is ET
var mod1 = ET_NDVI;

// The secondary collection is annual total precipitation values from PRISM
var mod2 = precip_30m;

// Join the two image collections
var final_col = join.simpleJoin(mod1, mod2, 'system:time_start')
                    .select('ET','ppt','NDVI');

//Add P-ET to the ImageCollection. It requires ET and precip. to calculate.
var annualTS = final_col.map(veg_indices.addPET);

//Export the Image Collection of Pr-ET time series
exports.annualTS = annualTS;