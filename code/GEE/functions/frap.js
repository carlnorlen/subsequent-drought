//Author: Carl A. Norlen
//Purpose: Add a rasterized FRAP Image Collection data sets.

//Data set of rasterized FRAP data
var frap = ee.ImageCollection('projects/ca-ecs/Rasterized/frap_18');

//Function to date FRAP file names and convert them into a system:time_start property
var remapFRAP = function(image) {
    var name = ee.String(image.get('system:index'));
    var year = ee.Number.parse(name.slice(5));
    return image.remap([1],[year]).toInt()
                .set('system:time_start', ee.Date.parse('YYYY', name.slice(5)));
};

//Export the FRAP Image Collection with the new system:time_start property.
exports.frap_date = frap.map(remapFRAP);