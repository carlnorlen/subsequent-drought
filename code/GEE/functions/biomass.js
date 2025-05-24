//Author: Carl A. Norlen
//Purpose: Create an Image Collection for Biomass from eMapR

//Add functions for joining data sets
var join = require('users/cnorlen/Scripts:join');

//Add the biomass data
//California Statwide: Biomass (Mg/ha)
var emapr_bm = ee.Image('projects/ca-ecs/eMapR/eMapR_biomass_CA_ARD_all');

    /// Unstack Biomass (Mg/ha)
var emapr_bm_annual = join.unStack(emapr_bm, '1984-06-01', '1984-09-30', 'biomass'); 

    ///Rename and cast as an image collection
var ca_biomass = emapr_bm_annual;

    ///Export the California statewide ImageCollection
exports.ca_biomass = ca_biomass;