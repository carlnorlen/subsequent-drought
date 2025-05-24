//Author: Carl A. Norlen
//Purpose: Define functions for calcuating vegetation indices

//Define function for adding NDVI to a Landsat image
exports.addNDVI = function(image) { //Create a function for an image and allow to be exported
  var ndvi = image.normalizedDifference(['B4', 'B3']).rename('NDVI'); //Calculate NDVI for the image and renames 'NDVI'
  return image.addBands(ndvi).toFloat(); //add the NDVI band to the original image and return it.
};

//Define a function to calculate NDMI
//Bands are based on L5 naming conventions
exports.addNDMI = function(image) {
  var ndmi = image.normalizedDifference(['B4','B5']).rename('NDMI').toFloat();
  return image.addBands(ndmi);
};
  
//Define function to calcuate ET from NDVI
exports.addET = function(image) {
  var ET = image.expression( //Equation calculated from NDVI, ET relationship, based on Goulden & Bales, 2019
    '125.5345 * exp(2.6148 * NDVI)', {
      'NDVI': image.select('NDVI')}).rename('ET');
  return image.addBands(ET);
};

//Define a function to calculate Pr-ET by subtracting ET from ppt in the same image collection
exports.addPET= function(image) {
  var pet = image.select('ppt').subtract(image.select('ET')).rename('PET');
  return image.addBands(pet.select('PET'));
};

//Define function for calculating Tasseled Cap Indices from Landsat (bands are according to Landsat 5 conventions)
//Citation: Christ and Cicone, 1984 IEEE Transactions of Geosciences and Remote Sensing
exports.addTC = function(img) {
	var b = ee.Image(img).select(['B1', 'B2', 'B3', 'B4', 'B5', 'B7']);
	var brt_coeffs = ee.Image([0.3037, 0.2793, 0.4743, 0.5585, 0.5082, 0.1863]);
	var grn_coeffs = ee.Image([-0.2848, -0.2435, -0.5436, 0.7243, 0.0840, -0.1800]);
	var wet_coeffs = ee.Image([0.1509, 0.1973, 0.3279, 0.3406, -0.7112, -0.4572]);
	var four_coeffs = ee.Image([-0.8242, 0.0849, 0.4392, -0.0580, 0.2012, -0.2768]);
	var five_coeffs = ee.Image([-0.3280, 0.0549, 0.1075, 0.1855, -0.4357, 0.8085]);
	var six_coeffs = ee.Image([0.1084, -0.9022, 0.4120, 0.0573, -0.0251, 0.0238]);

	var sum = ee.call("Reducer.sum");
	var brightness = ee.Image(b.multiply(brt_coeffs)).reduce(sum).rename('brightness');
	var greenness = ee.Image(b.multiply(grn_coeffs)).reduce(sum).rename('greenness');
	var wetness = ee.Image(b.multiply(wet_coeffs)).reduce(sum).rename('wetness');
	var fourth = ee.Image(b.multiply(four_coeffs)).reduce(sum).rename('fourth');
	var fifth = ee.Image(b.multiply(five_coeffs)).reduce(sum).rename('fifth');
	var sixth = ee.Image(b.multiply(six_coeffs)).reduce(sum).rename('sixth');

	return img.addBands(brightness)
            .addBands(greenness)
            .addBands(wetness)
            .addBands(fourth)
            .addBands(fifth)
            .addBands(sixth);
};

//Define function for harmonizing L8 to L7
//Slope and intercept derived from: Roy, D.P., Kovalskyy, V., Zhgang, H.K., Vermote, E.F., Yan, L., Kumar, S.S, Egorov, A., 2016, Characterization of Landsat-7 to Landsat-8 reflective wavelength and normalized difference vegetation index continuity, Remote Sensing of Environment, 185, 57-70.(http://dx.doi.org/10.1016/j.rse.2015.12.024); Table 2 - reduced major axis (RMA) regression coefficients
var harmonizationRoy = function(oli) {
  //var slopes = ee.Image.constant([0.9785, 0.9542, 0.9825, 1.0073, 1.0171, 1, 0.9949]);        // RMA - create an image of slopes per band for L8 TO L7 regression line - David Roy
  //var itcp = ee.Image.constant([-0.0095, -0.0016, -0.0022, -0.0021, -0.0030, 0, 0.0029]);     // RMA - create an image of y-intercepts per band for L8 TO L7 regression line - David Roy
  var slopes = ee.Image.constant([0.885, 0.9317, 0.9372, 0.8339, 0.8639, 1, 0.9165]);  // least squares OLI to ETM+
  var itcp = ee.Image.constant([0.0183, 0.0123, 0.0123, 0.0448, 0.0306, 0, 0.0116]);   // least squares OLI to ETM+
  var y = oli.select(['SR_B2','SR_B3','SR_B4','SR_B5','SR_B6','ST_B10','SR_B7'],['SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'ST_B6', 'SR_B7']) // select OLI bands 2-7 and rename them to match L7 band names
             .resample('bicubic')                                                          // ...resample the L8 bands using bicubic
             .subtract(itcp.multiply(10000)).divide(slopes)                                // ...multiply the y-intercept bands by 10000 to match the scale of the L7 bands then apply the line equation - subtract the intercept and divide by the slope
             .set('system:time_start', oli.get('system:time_start'));                       // ...set the output system:time_start metadata to the input image time_start otherwise it is null
             //.set('system:time_end', oli.get('system:time_end'));                          // ...set the output system:time_end metadata to the input image time_end otherwise it is null
  return y.toShort();                                                                     // return the image as 16-bit data (aka short) to match the type of the other data
};
exports.harmonizationRoy = harmonizationRoy;

// Define a function that adds multiple vegetation indices.
exports.addVIs = function(image) { // function that computes VIs for any image
  
  // EVI parameters. Landsat 8, EVI = 2.5 * ((Band 5 – Band 4) / (Band 5 + 6 * Band 4 – 7.5 * Band 2 + 1))
  // In Landsat 4-7, EVI = 2.5 * ((Band 4 – Band 3) / (Band 4 + 6 * Band 3 – 7.5 * Band 1 + 1)).
  // account for scale factor in Landsat data
  var L  = 1; // scaling factor applied
  var C1 = 6;
  var C2 = 7.5;
  var G  = 2.5;
  var brt_coeffs = ee.Image([0.3037, 0.2793, 0.4743, 0.5585, 0.5082, 0.1863]);
	var grn_coeffs = ee.Image([-0.2848, -0.2435, -0.5436, 0.7243, 0.0840, -0.1800]);
	var wet_coeffs = ee.Image([0.1509, 0.1973, 0.3279, 0.3406, -0.7112, -0.4572]);
	var four_coeffs = ee.Image([-0.8242, 0.0849, 0.4392, -0.0580, 0.2012, -0.2768]);
	var five_coeffs = ee.Image([-0.3280, 0.0549, 0.1075, 0.1855, -0.4357, 0.8085]);
	var six_coeffs = ee.Image([0.1084, -0.9022, 0.4120, 0.0573, -0.0251, 0.0238]);
  
  // Parameters we need (LS8)
  var scaleFac  = 0.0001; // Landsat scaling factor - only necessary to apply to VIs that dont divide bands (e.g. NIRv)
  var scaleFac_thermal = 0.01; // thermal scaling factor
  var blue   = image.select('B1').multiply(scaleFac);
  var green   = image.select('B2').multiply(scaleFac);
  var red  = image.select('B3').multiply(scaleFac);
  var NIR   = image.select('B4').multiply(scaleFac);
  var SWIR1 = image.select('B5').multiply(scaleFac);
  var therm = image.select('B6').multiply(scaleFac_thermal);
  var SWIR2 = image.select('B7').multiply(scaleFac);
  
  //Image with bands selected for tasseled cap calculation
	var b = ee.Image(image).select(['B1', 'B2', 'B3', 'B4', 'B5', 'B7']);
  
	var sum = ee.call("Reducer.sum");
	var brightness = ee.Image(b.multiply(brt_coeffs)).reduce(sum).toFloat().rename('brightness');
	var greenness = ee.Image(b.multiply(grn_coeffs)).reduce(sum).toFloat().rename('greenness');
	var wetness = ee.Image(b.multiply(wet_coeffs)).reduce(sum).toFloat().rename('wetness');
	var fourth = ee.Image(b.multiply(four_coeffs)).reduce(sum).toFloat().rename('fourth');
	var fifth = ee.Image(b.multiply(five_coeffs)).reduce(sum).toFloat().rename('fifth');
	var sixth = ee.Image(b.multiply(six_coeffs)).reduce(sum).toFloat().rename('sixth');
  
  // Compute the bands and add them in
  //var aeri  = AER.subtract(blue).divide(AER.add(blue)).rename('aerosol');
  var ndvi = image.normalizedDifference(['B4', 'B3']).toFloat().rename('NDVI');
  //var ndvi  = NIR.subtract(red).divide(NIR.add(red)).rename('NDVI');
  var nirv  = NIR.multiply(ndvi).toFloat().rename('NIRv'); // multiplied by scaling factor to get units right
  var ndmi  = image.normalizedDifference(['B4', 'B5']).toFloat().rename('NDMI');
  var nbr = image.normalizedDifference(['B4', 'B7']).toFloat().rename('NBR');
  var thermal = therm.toFloat().rename('Thermal_SR');
  var evi  = ((NIR.subtract(red))
              .divide(NIR.add(red.multiply(C1)).subtract(blue.multiply(C2)).add(L)))
              .multiply(G).toFloat().rename('EVI'); 

  // Store the results of all new VIs and thermal indices
  return image.addBands([ndvi,evi,nirv,ndmi,nbr,thermal,brightness,greenness,wetness,fourth,fifth,sixth]); 
};

//Define a function that adds an elevation, aspect, and slope Band to an image
exports.addElev = function(image) {
  var elevation = ee.Image('USGS/NED').select('elevation');
  var aspect = ee.Terrain.aspect(elevation).rename('aspect');
  var slope = ee.Terrain.slope(elevation).rename('slope');
  return image.addBands(elevation)
              .addBands(aspect)
              .addBands(slope);
};   
