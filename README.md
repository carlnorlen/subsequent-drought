# Datasets used in "Recent tree mortality dampens semi-arid forest die-off during subsequent drought
---

These data sets and scripts allow for the creation of all figures and supplementary figures and tables cited in 
Norlen, C.A., Goulden, M.L. (2023) "Recent tree mortality dampens semi-arid forest die-off during subsequent drought" AGU Advances

## Description of the data and file structure
Shape File of USFS Ecological Subsections used to create Manuscript Figure 1a
  *  S_USA.EcomapSubsections.shp, S_USA.EcomapSubsections.prj, S_USA.EcomapSubsections.dbf, S_USA.EcomapSubsections.shx

Geotiffs used to create Manuscript Figure 1a
  *  Drought_first_300m_v7, Drought_second_300m_v7 (1 = drought, 0 = no drought)
  
CSV files used to Data sets for creation of Manuscript Figure 2. The files include dNDMI (unitless), Pr-ET (mm/year), and tpa max (trees/acre) as a time series for the two drought sequences.
  * dNDMI_NDMI_PET_Temp_ADS_trajectories_second_drought_full_region_300m_v9.csv (data for the 2nd Drought Only sequence)
  * dNDMI_NDMI_PET_Temp_ADS_trajectories_first_drought_full_region_300m_v9.csv (data for the Boht Droughts sequence)  

CSV file with data set for four-year Evapotranspiration (mm/4yr), four year precipitation (mm/4yr), four-year temperature (C), biomass (Mg/ha), tpa_max (max trees/acre), dNDMI (unit less), Precipitation minus ET (mm/yr), elevation (meters), latitude, longitude, fortyeight month standardized precipitation index (unit less), and four year Precipitation minus ET (mm/4yr) for each grid cell used in the following scripts: fig5_correlation_simple.r, fig3_sfigs_grids.r, and  sfig4_spatial_autocorrelation.r 
to create Figures 1b, 3, 5, S2, S3, S5, S8, S10, S12, S14, S15, S16 to S20, and Tables S1, S2, and S6.
  * Regression_all_socal_300m_v23_v3.csv

Geotiffs used to create Manuscript Figures S1, S6, S7, S9, S11, and S13.
  *  dNDMI_2004_bigger_region_300m_v4.tif, dNDMI_2017_bigger_region_300m_v4.tif (unit less)
  *  biomass_1999_bigger_region_300m.tif, biomass_2012_bigger_region_300m.tif (Mg/ha)
  *  PrET_2002_bigger_region_300m_v4.tif, PrET_2015_bigger_region_300m_v4.tif (mm/4yr)
  *  tmax_2002_bigger_region_300m.tif, tmax_2015_bigger_region_300m.tif (C)
  *  ADS_2004_bigger_region_300m.tif, ADS_2017_bigger_region_300m.tif (trees/acre)
  *  Drought_second_bigger_region_300m_v5.tif, Drought_both_bigger_region_300m_v5.tif
  *  hillshade_bigger_region_300m_v2.tif
  
CSV files of annual data from 10 eddy covariance tower locations used to create Figure S4. Each file is for one of the 10 eddy covariance sites. Each file contains the following variables: wYEAR (water year), Evapotranspiration (ET, mm/yr), n_days (number of days with data), and ID (site description).
  * Pinyon_Annual_EClos_Hinojo.csv
  * P301_Annual_EClos_Hinojo.csv
  * LowDesert_Annual_EClos_Hinojo.csv
  * James_Annual_EClos_Hinojo.csv
  * Grass_Annual_EClos_Hinojo.csv
  * PinyonBurn_Annual_EClos_Hinojo.csv
  * Sage_Annual_EClos_Hinojo.csv
  * Shorthair_Annual_EClos_Hinojo.csv
  * SJER_Annual_EClos_Hinojo.csv
  * Soaproot_Annual_EClos_Hinojo.csv

CSV file of annual NDVI (unit less) for each of the 10 eddy covariance sites used to create Figure S4.
  * UCIupwind_pixels_NDVI_met_30m.csv

## Sharing/Access information

Data was derived from these publicly available sources:
  * Landsat data on Google Earth Engine (GEE): https://developers.google.com/earth-engine/datasets/catalog/landsat
  * 48-month Standardized Precipitation Index (SPI48) data: https://wrcc.dri.edu/wwdt/archive.php
  * California state perimeter: https://developers.google.com/earth-engine/datasets/catalog/TIGER_2016_States
  * California Fire Perimeters: https://frap.fire.ca.gov/mapping/gis-data/
  * LANDFIRE Existing Vegetation Type: https://www.landfire.gov/viewer/
  * USFS Ecological Subsections: https://data.fs.usda.gov/geodata/edw/datasets.php?xmlKeyword=ecomap+subsection
  * USFS Aerial Detection Surveys: https://www.fs.usda.gov/detail/r5/forest-grasslandhealth/?cid=fsbdev3_046696
  * USFS Forest Inventory and Analysis data for California (2019 version): https://apps.fs.usda.gov/fia/datamart/
  * Precipitation data: https://developers.google.com/earth-engine/datasets/catalog/OREGONSTATE_PRISM_AN81m   
  * Temperature data: https://developers.google.com/earth-engine/datasets/catalog/OREGONSTATE_PRISM_AN81m
  * USGS Digital Elevation Model: https://developers.google.com/earth-engine/datasets/catalog/USGS_NED.
  * Eddy Covariance Data: https://www.ess.uci.edu/~california/
  * Above ground biomass data: http://emapr.ceoas.oregonstate.edu/pages/data/viz/index.html

## Code/Software
The code shared with this submission were written in R 4.02 and run using RStudio.
The code requires the tidyverse, sf, RSQlite, rFIA, RSToolbox, patchwork, ggpubr, kableExtra, and gstat packages.

R script used to create Figure 1 of the manuscript.
  * fig1_map.r
  
R script used to create Figure 2 of the manuscript. 
  * fig2_time_series.r
  
R script used to create Figure 3 of the manuscript, and Figures S2, S3, S5, S8, S10, S12 and S19. 
  * fig3_sfigs_grids.r
  
R script used to create Figure 4 of the manuscript, Figure S6, and Tables S3 to S5. 
  * fig4_sfig_FIA_analysis.r
  
R script used to create Figure 5 of the manuscript, Tables S1, S2, and S6, and Figures S14, S15, S18, and S20.
  * fig5_correlation_simple.r
  
R script used to create Figures S1, S6, S7, S9, S11, and S13. 
  * sfig1_maps.r
  
R script used to create Figure S4.
  * sfig3_ndvi_flux_tower
  
R script used to create Figures S16, S17.
  * sfig4_spatial_autocorrelation