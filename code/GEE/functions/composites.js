/* Creates a collection of mosaics with a given temporal interval.
 *
 * collection - the collection from which to make composites.
 * start - the date of the first composite (either a string or an ee.Date)
 * count - the number of composites to make
 * interval - The time between composites, in units of "units".
 * units - The units of step (day, week, month, year; see ee ee.Date.advance)
 */

//Combine images for a set time period into an Image Colleciton
exports.temporalReduce = function(collection, reducer, start, end, count, interval, units, parallel) {
  // Create a sequence of numbers, one for each time interval.
  var sequence = ee.List.sequence(0, ee.Number(count).subtract(1));

  var originalStartDate = ee.Date(start);
  
  var originalEndDate = ee.Date(end);

  return ee.ImageCollection(sequence.map(function(i) { 
    // Get the start date of the current sequence.
    var startDate = originalStartDate.advance(ee.Number(interval).multiply(i), units);

    // Get the end date of the current sequence.
    var endDate = originalEndDate.advance(ee.Number(interval).multiply(i), units);
    
    var collection_filtered = collection.filterDate(startDate, endDate);
    
    //Reduce images for selected time period
    return collection_filtered
                     .reduce({reducer: reducer, parallelScale : parallel})
                     .toFloat()
                     .set('system:time_start', startDate.millis())
                     .set('system:time_end', endDate.millis());
  }));
};

//Create dummy images for Image Colelction sets with missing data in some years
exports.gapFillcollect = function(inCollection, bands) {
  
  var gapFill = function(image) {
  
    // Count the number of bands entered in the band field. Storage as an ee.Number() data type.
    var count = ee.Number(ee.List(bands).length());
  
    //Create a list with length equal to the number of bands in 'band' and values equal to 0.
    var sequence = ee.List.repeat({value: ee.Number(0).toInt8(), count: count});
  
    //Create a dummy Image to fill in missing years.
    //Make the image based on array of the sequence list (each pixel has an Array of values)
    var dummyImage = ee.Image(ee.Array(sequence))
                        //Flatten the Image pixel array based on the 'band field and mask 0 values
                                   .arrayFlatten([bands]).mask(ee.Image(0))
                        //Rename the band names based on the 'band' field
                                   .select(ee.List.sequence(0, ee.Number(ee.List(bands).length()).subtract(1)),bands)
                                   //Add start and end dates to the image
                                   .set('system:time_start', image.get('system:time_start'))
                                   .set('system:time_end', image.get('system:time_end'));
    var filled = ee.Algorithms.If((image.bandNames().length()).neq(count), dummyImage, image);
    return filled;
    };
  var collection_filled = inCollection.map(gapFill);
  return ee.ImageCollection(collection_filled);
};
