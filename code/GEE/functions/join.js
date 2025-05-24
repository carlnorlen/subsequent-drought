//Author: Carl A. Norlen

//Purpose: A variety of functions for joing ImageCollection datasets toghether

//Add a property to an image that is the year of the image
exports.addYear = function(image) {
                  return image.set('year', (image.date()).get('year'));
                };

//Add a property to an image that is the year of the image plus one                
exports.addLastYear = function(image) {
                  return image.set('year', (image.date()).get('year').add(1));
                };

//Add a property to an image that is a year from the time_end            
exports.addEndYear = function(image) {
                  return image.set('year', (ee.Date(image.get('system:time_end')).get('year')));
                };

//A function to apply a simple Join to two image collectoins based on a common property.
//The function includes the bands from both image collections in the output.
exports.simpleJoin = function(mod1, mod2, joinField) {
  var filter = ee.Filter.equals({
    leftField: joinField,
    rightField: joinField
  });
  // Create the join.
  var simpleJoin = ee.Join.simple();
  // Apply join.
  var mod1join = ee.ImageCollection(simpleJoin.apply(mod1, mod2, filter));
  var mod2join = ee.ImageCollection(simpleJoin.apply(mod2, mod1, filter));

  var final_col = mod1join.map(function(img){
    // Create a collection with 1 image
    var temp = ee.ImageCollection(ee.List([img]));
    // Apply join to collection 2
    // Resulting collection will have 1 image with exact same date as img
    var join = simpleJoin.apply(mod2join, temp, filter);
    // Get resulting image
    var i2 = ee.Image(join.first());

    return img.addBands(i2);
  });
  return final_col};

//A function to applies a simple Join to two image collectoins based on a common property.
//The function includes the bands from both image collections in the output.
exports.innerJoinFeature = function(mod1, mod2, joinField) {
  var filter = ee.Filter.equals({
    leftField: joinField,
    rightField: joinField
  });
  // Create the join.
  var simpleJoin = ee.Join.inner();
  // Apply join.
  var mod1join = ee.FeatureCollection(simpleJoin.apply(mod1, mod2, filter));
  //var mod2join = ee.FeatureCollection(simpleJoin.apply(mod2, mod1, filter));

  var cleanJoin = function(pair) {
  var f1 = ee.Feature(pair.get('primary'));
  var f2 = ee.Feature(pair.get('secondary'));
  return f1.set(f2.toDictionary());
  };

  var final_col = mod1join.map(cleanJoin);
  return final_col};

//Function to unstack bands from an ImageStack into an ImageCollection.
//Creates one band for each year.
exports.unStack = function(image, start, end, new_band) {
    var bands = ee.List(image.bandNames());
    //Create list of bands into a number
    var count = ee.Number(bands.reduce(ee.Reducer.count()));
    var sequence = ee.List.sequence(0, ee.Number(count).subtract(1));
    var originalStartDate = ee.Date(start);
    var originalEndDate = ee.Date(end);
    var add_date = function(number) {var band = bands.get(number);
                                     var startDate = originalStartDate.advance(number, 'year');
                                     var endDate = originalEndDate.advance(number, 'year');
                                     var year_image = image.select([band],[new_band])
                                                           .set('system:time_start', startDate.millis())
                                                           .set('system:time_end', endDate.millis());
                                     return year_image;
    };
    var collection = ee.ImageCollection(sequence.map(add_date));
    return collection;
                                              
};
