//Author: Carl A. Norlen

//Purpose: Define functions for masking raster data

//Add functions to calculate vegetationt indices
var landsat = require('users/cnorlen/subsequent_drought:functions/veg_indices');
var resolution = require('users/cnorlen/subsequent_drought:functions/resolution');

//Add function to get annual FRAP data.
var frap = require('users/cnorlen/subsequent_drought:functions/frap');

//Export a cloud mask for Landsat 4, 5, or 7
exports.cloudMaskL457 = function(image) {
  var qa = image.select('QA_PIXEL');
  // If the cloud bit (5) is set and the cloud confidence (7) is high
  // or the cloud shadow bit is set (3), then it's a bad pixel
  // or the snow bit is set (4), then it's a bad pixel.
  var cloud_snow = qa.bitwiseAnd(1 << 5)
                  .and(qa.bitwiseAnd(1 << 7))
                  .or(qa.bitwiseAnd(1 << 3))
                  .or(qa.bitwiseAnd(1 << 4));
  // Remove edge pixels that don't occur in all bands
  //Select the pixels that are not flagged as clouds, snow, or shadow by passing cloud_snow.not()
  return image.updateMask(cloud_snow.not());
};

//Export a cloud mask for Landsat 8
exports.maskL8sr = function(image) {
  // Get the pixel QA band.
  var qa = image.select('QA_PIXEL');
    // If the cloud bit (5) is set and the cloud confidence (7) is high
    //or Cirrus confidence (9) is high
  // or the cloud shadow bit is set (3), then it's a bad pixel
  // or the snow bit is set (4), then it's a bad pixel.
  // Both flags should be set to zero, indicating clear conditions.
  var cloud_snow = qa.bitwiseAnd(1 << 5) //Cloud Mask
                     .and(qa.bitwiseAnd(1 << 7) //High Cloud Confidence
                     .or(qa.bitwiseAnd(1 << 9))) //High Cirrus Confidence
                  .or(qa.bitwiseAnd(1 << 3)) // Cloud Shadow
                  .or(qa.bitwiseAnd(1 << 4)); //Snow
  return image.updateMask(cloud_snow.not());
};

//Trying to fix TmaskLite
exports.addTmaskLiteCA = function(collection) {
                        var bright_mean = ee.Image('projects/ca-ecs/L578_Composites/allYear_VI_v2/0');
                        var bright_sd = ee.Image('projects/ca-ecs/L578_Composites/allYear_VI_v2/1');
                        var addMask = function(image) {
                                              var bright = image.select('brightness');
                                              var mask2 = bright.gte(bright_mean.add(bright_sd.multiply(2))) //Make a mask for pixels that are 2 St Dev brighter
                                                                .or(bright.lte(bright_mean.subtract(bright_sd.multiply(2)))); //or darker than average
                                              return image.updateMask(mask2.not()); //Apply the mask to the Image for all pixels not indicated
                       };
                        var masked = collection.map(addMask);
                        return (ee.ImageCollection(masked));
                       };
                       
//A LANDFIRE Based Conifer Vegetation mask
exports.addConiferMask_LF = function(image) {
  //Image Collection of LANDFIRE EVT values
  var type = ee.ImageCollection('users/cnorlen/California_Data/LANDFIRE/EV_Type');
  
  //LANDFIRE EVT for 2001
  var vegtype = type.filter(ee.Filter.date('2001-01-01')).max();
  //Selects all the Conifer layers and returns a one for each
  //LANDFIRE Codes: 2027 - Douglass Fir / Ponderosa Pine, 2028: Douglass Fir
  var vegmask = //2010: Western Larch, 2013: Bur Oak
                       vegtype.eq(2015).or(vegtype.eq(2016)).or(vegtype.eq(2018)) //2015: Redwood, 2016: Juniper-Pinyon Woodland, 2017: Western Juniper, 2018: Grand Fir
                       .or(vegtype.eq(2019)).or(vegtype.eq(2020)).or(vegtype.eq(2022)) //2019: Juniper-Pinyon Woodland, 2020: Bristlecone Pine, 2021: Port Orford Cedar, 2022: Knobcone Pine,
                       .or(vegtype.eq(2025)) //2023: Western Live Oak, 2024: Western Live Oak, 2025: Juniper-Pinyon Woodland, 2026: Western Live Oak,
                       .or(vegtype.eq(2027)).or(vegtype.eq(2028)) //2027 - Sierra Nevada Mixed Conifer (Douglass Fir / Ponderosa Pine), 2028: White fir (Douglass-Fir)
                       .or(vegtype.eq(2031)).or(vegtype.eq(2032)).or(vegtype.eq(2033)).or(vegtype.eq(2034)) //2031: Jeffrey Pine, 2032: Red Fir, 2033: Subalpine, 2034: Knobcone pine
                       //.or(vegtype.eq(2035)).or(vegtype.eq(2037)).or(vegtype.eq(2038)) //Pacific Region; 2035: Pacific Douglass Fir, 2036: Sitka spruce, 2037: Douglass Fir-Western Hemlock, 2038: Mountain Hemlock,
                       //.or(vegtype.eq(2039)).or(vegtype.eq(2040)).or(vegtype.eq(2041)).or(vegtype.eq(2042)) //Pacific Region; 2039: Douglass Fir-Western Hemlock, 2040: Western Hemlock, 2041: Mountain Hemlock, 2042: Coastal True Fir Hemlock,
                       .or(vegtype.eq(2045)).or(vegtype.eq(2046)) //California Region; 2043: Douglas-Fir-Tanoak-Pacific Madrone, 2044: Mountain Hemlock, 2045: Interior Douglas-Fir, 2046: Whitebark Pine (subalpine)
                       .or(vegtype.eq(2047)).or(vegtype.eq(2049)).or(vegtype.eq(2050)) //Northern Rockies; 2047: Grand Fire, 2048: White Spruce, 2049: Limber Pine, 2050: Lodgepole Pine,
                       .or(vegtype.eq(2051)).or(vegtype.eq(2052)).or(vegtype.eq(2053)).or(vegtype.eq(2054)) //Southern Rockies; 2051: Interior Douglass-firi, 2052: White Fir, 2053: Interior Ponderosa Pine, 2054: Interior Ponderosa Pine,
                       .or(vegtype.eq(2055)).or(vegtype.eq(2056)).or(vegtype.eq(2057)).or(vegtype.eq(2058)) //Rocky Mountain / Sierra Nevada ; 2055: Engelmann Spruce-Subalpine Fir, 2056: Engelmann Spruce-Subalpine Fir, 2057:Bristlecone Pine, 2058: Lodgepole pine,
                       .or(vegtype.eq(2059)).or(vegtype.eq(2060)) // Varies; 2059: Juniper-Pinyon Woodland, 2060: Interior Ponderosa Pine, 2061: Aspen / Mixed conifer
                       .or(vegtype.eq(2115)).or(vegtype.eq(2117)) // Varies; 2112: Blue Oak Woodland, 2115: Juniper-Pinyon Woodland, 2116: Sideoats Grama-Sumac-Juniper, 2117:  Interior Ponderosa Pine
                       .or(vegtype.eq(2119)) //2119: JUniper-Pinyon Woodland
                       .or(vegtype.eq(2165)).or(vegtype.eq(2166)).or(vegtype.eq(2167)).or(vegtype.eq(2170)) //2165: Ponderosa Pine-Shrubland, 2166: Interior Douglas-Fir, 2167: Lodgepole Pine, 2170: Knobcone Pine,
                       .or(vegtype.eq(2172)).or(vegtype.eq(2173)).or(vegtype.eq(2175)) //2172: White Fir, 2173: Lodgepole Pine, 2174: Coastal True Fir-Hemlock, 2175: Engelmann Spruce-Subalpine Fir
                       .or(vegtype.eq(2177)).or(vegtype.eq(2179)).or(vegtype.eq(2200)) //2177: Knobcone Pine, 2178: Western Redcedar-Western Hemlock, 2179: Interior Ponderosa Pine, 2200: Pacific Douglas-Fir,
                       .or(vegtype.eq(2204)) //2201: Oregon White Oak, 2202: Western Juniper, 2203: Western Juniper, 2204: Western Hemlock
                       .or(vegtype.eq(2206)).or(vegtype.eq(2208)) //2205: Coastal True Fir-Hemlock, 2206: Pacific Douglas-Fir, 2207: Western Redcedar, 2208: White Fir
                       .or(vegtype.eq(2227)).or(vegtype.eq(2229)) //2227: Interior Douglas-Fir, 2228: Western Larch, 2229: Whitebark pine (subalpine), 2230: Blue Oak-Digger Pine
                       .or(vegtype.eq(2231)).or(vegtype.eq(2232)); //2231: Sierra Nevada Mixed Conifer, 2232: Grand Fir
  var mask = image.updateMask(vegmask);
  return mask;
};

//Function to mask pixels that burned since 1980 according to FRAP.
var addFireMask = function(image) {
  var firedata = frap.frap_date.filter(ee.Filter.date(ee.Date('1980-01-01'), ee.Date('2019-12-31')))
                               .max()
                               .reproject({crs: 'EPSG: 5070', scale: 30});
  var firemask = (firedata.unmask()).eq(0);
  return image.updateMask(firemask);
};
exports.addFireMask = addFireMask;