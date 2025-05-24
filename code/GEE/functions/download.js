//Author: Carl A. Norlen
//Purpose: variety of scripts of downloading data and changing the format of data.

//A script for changing an image into a FeatureCollection to export as a CSV.
//The arguments are: image to reduce, geometry the image is within, scale of pixels, max # of pixels, tile scale (larger mean less memory needed for parallel procesing)
exports.makeFeature = function(image, geometry, scale, maxPixels, tileScale) {
                                    var extract = image.reduceRegion({
                                    reducer: ee.Reducer.toCollection(image.bandNames()), 
                                    geometry: geometry, //Trying this out
                                    scale: scale,
                                    maxPixels: maxPixels,
                                    tileScale: tileScale
                                    }).get("features");
                                    return ee.FeatureCollection(extract)};

//Extract time series data for a specific region
exports.reduceImageCollection = function(collection, start, interval, unit, count, xProperty, reducer, region, scale, maxPixels, tileScale) 
{// Create a sequence of numbers, one for each time interval.
  var sequence = ee.List.sequence(0, ee.Number(count).subtract(1));

  var originalStartDate = ee.Date(start);

  //Function that selects Images from the Image Collection and reduces each to 
  var series = function(i) {
      // Get the start date of the current sequence.
      var startDate = originalStartDate.advance(ee.Number(interval).multiply(i), unit);
      //Select an image from the Image collection
      var image = collection.filterDate(startDate).first();
      //Define a function to reduce an image and convert to an array
      var reduce = function(img) {return img.reduceRegion({reducer: reducer,
                          geometry: region,
                          scale: scale,
                          maxPixels: maxPixels,
                          tileScale: tileScale});
                    };
      //Apply the function
      var dict = reduce(image);
      //Get the date for the image
      var date = ee.Date(image.get(xProperty));
      //Add the date to the dictionary
      var combine = ee.Dictionary(dict.set(xProperty, date));
      //Make the dictionary into a Feature
      return ee.Feature(null, combine);
      };
      
  //Map the series function over the series    
  var values = ee.FeatureCollection(sequence.map(series));
  
  //Return the final ImageCollection
  return values;
};