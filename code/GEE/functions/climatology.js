//Author: Carl A. Norlen
//Purpose: Prepare climatology data (1981-2010)for analysis

//Add functions for creating masks
var mask = require('users/cnorlen/Scripts:mask');

//Add functions for calculating vegetation indices.
var veg_indices = require('users/cnorlen/Scripts:veg_indices');

//Add functions for downscaling data layers
var resolution = require('users/cnorlen/Scripts:resolution');

//Add functions for joining functions
var join = require('users/cnorlen/Scripts:join');

//Get the PRISM data   
var prism = ee.ImageCollection("OREGONSTATE/PRISM/Norm81m");

//Select the temperature max and precipitation layers
var prism_30m = prism.select(['tmax','ppt'])
                     .map(resolution.down_30m); //down-scale to 30 m

//Get the annual climatology mean for temperature
var temp = prism_30m.select('tmax')
                    .mean();

//Get the annual climatology sum for precipitation
var precip = prism_30m.select('ppt')
                      .sum();
//Export the temperature
exports.temp_climate = temp;

//Export the percip.
exports.precip_climate = precip;