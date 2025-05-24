//Author: Carl A. Norlen, Kyle Hemes

///////////////////// NDVI-ET SCALING /////////////////////
//Goal: Produce scaled ET monthly and annual image stack

///////////////////// CALL FUNCTIONS /////////////////////

// Our functions
//Add function for creating Image Collection formats
var composite = require('users/cnorlen/subsequent_drought/functions:composites');

//Add functions for joining Image Collections
var join = require('users/cnorlen/subsequent_drought/functions:join');

//Add functions for downloading data
var download = require('users/cnorlen/subsequent_drought/functions:download');

//Add fucntion for add climate data
var climate = require('users/cnorlen/subsequent_drought/functions:climate');

//Add functions for adding Landsat data
var landsat = require('users/cnorlen/subsequent_drought/functions:All_Landsat');

//Add functions to calculation vegetation indices
var veg_indices = require('users/cnorlen/subsequent_drought/functions:veg_indices');

///////////////////// ADD GEOMETRY /////////////////////

//var ameriflux_siteLocations = ee.FeatureCollection('projects/ca-ecs/Flux_sites/CA_fluxsites');
var UCI_upwind = ee.FeatureCollection('projects/ca-ecs/Flux_sites/UCI_LSpixellocations_siteID_2');

//Add a polygon of a California perimeter
var region = ee.FeatureCollection('TIGER/2018/States')
              .filter(ee.Filter.eq('NAME', 'California'));
              
//Add a 5-km perimeter to the state perimeter
var add_buffer = function(f) {
  return f.buffer(50000,2000);
};

//Make a California perimeter with 5-km added
var region_buffer = region.map(add_buffer);

///////////////////// GET NDVI /////////////////////

// Specify the start and end dates
var startDate = '1984-05-01';
var endDate   = '2019-12-31';
var reducer = ee.Reducer.mean();
var parallel = 16;
           
//Add the NDVI time sereis data
var NDVI = ee.ImageCollection('projects/ca-ecs/L578_Composites/byYear_NDVI_v2').map(join.addYear);

///////////////////// ADD MET /////////////////////

// Add the Temp and Precip data
var temp = climate.tempTS.map(join.addEndYear);
var precip = climate.precipTS.map(join.addEndYear);

//Add the NDVI, precip, and temp data
var joinField = 'year';
var NDVI_yearly_met = join.simpleJoin(NDVI, temp, joinField);
NDVI_yearly_met = join.simpleJoin(NDVI_yearly_met, precip, joinField);


///////////////////// EXPORT FOR UCI SITE SCALING /////////////////////

//Function for exporting data by pixel
var byPixel = function(image) { // function for each annual image to extract each pixel

var sampleRegionImages = image.unmask().sampleRegions(
  {collection: UCI_upwind,
   scale: 30,
   properties:['Pixel #','Site'],
   tileScale: 8
}) 
;
var mapped = sampleRegionImages.map(function(ftr) { // map over each feature and set year (why doesnt this work?)
  return ftr.set('year',image.get('year'));
});
return mapped;
};

//Create the byPixel exported data
var byPixel_NDVI_yearly_met = NDVI_yearly_met.map(byPixel).flatten();

//Export the NDVI byPixle data as .CSV file
Export.table.toDrive({
  'collection': byPixel_NDVI_yearly_met,
  'description': 'UCIupwind_pixels_NDVI_met_30m',
  'folder': 'CECS', 
  'fileFormat': 'CSV'
});