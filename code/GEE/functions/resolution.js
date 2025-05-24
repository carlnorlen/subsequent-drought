//Author: Carl A. Norlen

//Purpose: Functions to change the resolution or projection of raster data sets

//Function to reproject to EPSG: 5070.
exports.reproject = function(image) {
                 return image.reproject({crs: 'EPSG: 5070', scale: 30});
};


// Function to resample a data layer with bilinear interpolation
exports.down_30m = function(image) {
  var image_bilin = image.reproject({crs: image.projection(), scale: 4000})
                         .resample('bilinear')
                         .reproject({crs: image.projection(), scale: 30});
  return image_bilin
    .copyProperties(image, ['system:time_start'])
    .copyProperties(image, ['system:time_end']);
};

//Function to reproject data into CONUS Albers
exports.albers = function(img) {
                     return img.reproject({crs: 'EPSG:5070', scale: 30})
                            .set('system:time_start', img.get('system:time_start'));
                     };