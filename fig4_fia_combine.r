#Author: Carl Norlen
#Date Created: November 11, 2019
#Date Edited: March 31, 2022
#Purpose: Create bar graphs for manuscript FIA analysis 

# Specify necessary packages
p <- c("RSQLite","dbplyr","ggplot2","dplyr","tidyr", "ggpubr", "RColorBrewer",  
	   'gt', 'gtsummary', 'webshot', 'kableExtra', 'broom')

# Load packages
lapply(p,require,character.only=TRUE)

#Set working directory
setwd('C:/Users/can02/mystuff/subsequent-drought')

#Add Data Sets
sql_dir <- 'D:\\Large_Files\\FIA\\SQLite_FIADB_CA\\2019_version' #Downloaedd from FIA DataMart
fiaCA <- file.path(sql_dir, 'FIADB_CA.db')
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = fiaCA)  

#Add extension functions, allows more math functions
initExtension(db)				

#Get data for the Both Droughts sequence
#Query for FIA live trees, 2015-2019
q1 <- dbSendQuery(db, "SELECT 
t.tree, -- tree identified code
(t.carbon_ag * 2)*(t.tpa_unadj) live_biomass, -- biomass (lbs) of tree
t.tpa_unadj count, --trees/acre
t.dia, -- DBH in inches
t.ht, --Total live tree height in feet
t.actualht, --The measure height in feet
t.agentcd, --tree damage
t.mortyr, --mortality year
t.plot, t.statuscd, t.invyr, r.common_name, t.spcd, c.fortypcd, c.fldtypcd, rft.meaning
FROM 
cond c,
plot p,
tree t, -- tree table must be included for tree-level estimates
ref_species r,
ref_forest_type rft
WHERE p.cn = c.plt_cn
AND t.plt_cn = c.plt_cn
AND t.condid = c.condid
AND c.cond_status_cd = 1 
AND t.statuscd = 1 --1 means live trees, 2 means dead trees
AND t.spcd = r.spcd
AND t.dia >= 5.0 -- Tree diameters to include (5 inches is cut-off for tree plots)
AND rft.value = c.fldtypcd
AND (P.ECOSUBCD = 'M262Bd' OR P.ECOSUBCD = 'M262Be' OR P.ECOSUBCD = 'M262Bg' OR P.ECOSUBCD = 'M262Bh' OR P.ECOSUBCD = 'M262Bf' OR P.ECOSUBCD = 'M262Bo' OR P.ECOSUBCD = 'M262Bi' 
OR P.ECOSUBCD = 'M262Bm' OR P.ECOSUBCD = 'M262Bl' OR P.ECOSUBCD = 'M262Bc' OR P.ECOSUBCD = 'M262Bp' OR P.ECOSUBCD = 'M261Es') -- Only the San Gabriel and San Gorgonio/Bernardino Eco Regions, Bottom row includes other SoCal regions 
AND (p.invyr >= 2015 AND p.invyr <= 2019) --Inventory Years included
AND (c.dstrbcd1 = 0 OR c.dstrbcd1 = 10 OR c.dstrbcd1 = 11 OR c.dstrbcd1 = 12 OR c.dstrbcd1 = 54 OR c.dstrbcd1 = 70) --Disturbance codes
")

#Retrieve the queried data
live.2017 <- dbFetch(q1, n = -1)

#Query for dead trees: 2015 to 2019
q2 <- dbSendQuery(db, "SELECT 
t.tree, -- tree identified code
(t.carbon_ag * 2)* t.tpa_unadj live_biomass, -- biomass (lbs) per tree
t.tpa_unadj count, --trees/acre
t.dia, -- DBH in inches
t.ht, --Total live tree height in feet
t.actualht, --The measure height in feet
t.agentcd, --tree damage
t.mortyr, --mortality year
t.plot, t.statuscd, t.invyr, r.common_name, t.spcd, c.fortypcd, c.fldtypcd, rft.meaning
FROM 
cond c,
plot p,
tree t, -- tree table must be included for tree-level estimates
ref_species r,
ref_forest_type rft
WHERE p.cn = c.plt_cn
AND t.plt_cn = c.plt_cn
AND t.condid = c.condid
AND c.cond_status_cd = 1 
AND t.statuscd = 2 --1 means live trees, 2 means dead trees
AND t.spcd = r.spcd
AND t.dia >= 5.0 
AND rft.value = c.fldtypcd
AND (P.ECOSUBCD = 'M262Bd' OR P.ECOSUBCD = 'M262Be' OR P.ECOSUBCD = 'M262Bg' OR P.ECOSUBCD = 'M262Bh' OR P.ECOSUBCD = 'M262Bf' OR P.ECOSUBCD = 'M262Bo' OR P.ECOSUBCD = 'M262Bi' 
OR P.ECOSUBCD = 'M262Bm' OR P.ECOSUBCD = 'M262Bl' OR P.ECOSUBCD = 'M262Bc' OR P.ECOSUBCD = 'M262Bp' OR P.ECOSUBCD = 'M261Es') -- Only the San Gabriel and San Gorgonio/Bernardino Eco Regions, Bottom row includes other SoCal regions
AND (p.invyr >= 2015 AND p.invyr <= 2019)
AND (c.dstrbcd1 = 0 OR c.dstrbcd1 = 10 OR c.dstrbcd1 = 11 OR c.dstrbcd1 = 12 OR c.dstrbcd1 = 54 OR c.dstrbcd1 = 70)
AND (t.mortyr != 2001 OR t.mortyr != 2002 OR t.mortyr != 2003 OR t.mortyr != 2004 OR t.mortyr != 2005 OR t.mortyr != 2006 OR t.mortyr != 2007 OR t.mortyr != 2008 OR t.mortyr != 2009 OR t.mortyr != 2010 OR t.mortyr != 2011 OR t.mortyr != 2012)
")

#Retrieve the queried data
dead.2017 <- dbFetch(q2, n = -1)

#Query for live trees 2002 - 2006
q3 <- dbSendQuery(db, "SELECT 
t.tree, -- tree identity code
(t.carbon_ag * 2)* t.tpa_unadj live_biomass, -- biomass (lbs) per tree
t.tpa_unadj count, --trees/acre
t.dia, -- DBH in inches
t.ht, --Total live tree height in feet
t.actualht, --The measure height in feet
t.agentcd, --tree damage
t.mortyr, --mortality year
t.plot, t.statuscd, p.invyr, r.common_name, t.spcd, c.fortypcd, c.fldtypcd, rft.meaning--t.spcd, r.major_spgrpcd
FROM 
cond c,
plot p,
tree t, -- tree table must be included for tree-level estimates
ref_species r,
ref_forest_type rft
WHERE p.cn = c.plt_cn
AND t.plt_cn = c.plt_cn
AND t.condid = c.condid
AND c.cond_status_cd = 1 --possibly add 2 (non-forest)
AND t.statuscd = 1 --1 means live trees, 2 means dead trees
AND t.spcd = r.spcd
AND t.dia >= 5.0 -- additional where_clause from ref_pop_attribute table
AND rft.value = c.fldtypcd
AND (P.ECOSUBCD = 'M262Bd' OR P.ECOSUBCD = 'M262Be' OR P.ECOSUBCD = 'M262Bg' OR P.ECOSUBCD = 'M262Bh' OR P.ECOSUBCD = 'M262Bf' OR P.ECOSUBCD = 'M262Bo' OR P.ECOSUBCD = 'M262Bi' 
OR P.ECOSUBCD = 'M262Bm' OR P.ECOSUBCD = 'M262Bl' OR P.ECOSUBCD = 'M262Bc' OR P.ECOSUBCD = 'M262Bp' OR P.ECOSUBCD = 'M261Es')  -- Only the San Gabriel and San Gorgonio/Bernardino Eco Regions, Bottom row includes other SoCal regions	
AND (p.invyr >= 2002 AND p.invyr <= 2006)
AND (c.dstrbcd1 = 0 OR c.dstrbcd1 = 10 OR c.dstrbcd1 = 11 OR c.dstrbcd1 = 12 OR c.dstrbcd1 = 54 OR c.dstrbcd1 = 70) --10, 11, 12 are insect, 54 is drought, 0 is no disturbance, 70 is unknown
")

#Retrieve the queried data
live.2007 <- dbFetch(q3, n = -1)

#Query for dead trees 2002 - 2006
q4 <- dbSendQuery(db, "SELECT 
t.tree, -- tree identified code
(t.carbon_ag * 2)* t.tpa_unadj live_biomass, -- biomass (lbs) per tree
t.tpa_unadj count, --trees/acre
t.dia, -- DBH in inches
t.ht, --Total live tree height in feet
t.actualht, --The measure height in feet
t.agentcd, --tree damage
t.mortyr, --mortality year
t.plot, t.statuscd, p.invyr, r.common_name, t.spcd, c.fortypcd, c.fldtypcd, rft.meaning
FROM 
cond c,
plot p,
tree t, -- tree table must be included for tree-level estimates
ref_species r,
ref_forest_type rft
WHERE p.cn = c.plt_cn
AND t.plt_cn = c.plt_cn
AND t.condid = c.condid
AND c.cond_status_cd = 1 --1 means forest
AND t.statuscd = 2 --1 means live trees, 2 means dead trees
AND t.spcd = r.spcd
AND t.dia >= 5.0 
AND rft.value = c.fldtypcd
AND (P.ECOSUBCD = 'M262Bd' OR P.ECOSUBCD = 'M262Be' OR P.ECOSUBCD = 'M262Bg' OR P.ECOSUBCD = 'M262Bh' OR P.ECOSUBCD = 'M262Bf' OR P.ECOSUBCD = 'M262Bo' OR P.ECOSUBCD = 'M262Bi' 
OR P.ECOSUBCD = 'M262Bm' OR P.ECOSUBCD = 'M262Bl' OR P.ECOSUBCD = 'M262Bc' OR P.ECOSUBCD = 'M262Bp' OR P.ECOSUBCD = 'M261Es') -- Only the San Gabriel and San Gorgonio/Bernardino Eco Regions, Bottom row includes other SoCal regions	
AND (p.invyr >= 2002 AND p.invyr <= 2006)
AND (c.dstrbcd1 = 0 OR c.dstrbcd1 = 10 OR c.dstrbcd1 = 11 OR c.dstrbcd1 = 12 OR c.dstrbcd1 = 54 OR c.dstrbcd1 = 70) --10, 11, 12 are insect, 54 is drought, 0 is no disturbance, 70 is unknown
")

#Retrieve the queried data
dead.2007 <- dbFetch(q4, n = -1)

#Columns to remove later
drop.cols <- c('INVYR', 'FORTYPCD','MEANING') 

#Combine the live and dead trees for the 2014-2019 surveys.
trees.2017 <- rbind(live.2017, dead.2017)

#Add simple conifer versus broadleaf veg-type names
trees.2017$veg_type <- recode(.x=trees.2017$COMMON_NAME, 'California black oak' = 'oak', 'California juniper' = 'conifer', 'California live oak' = 'oak', 'California sycamore' = 'broadleaf', 'Coulter pine' = 'conifer', 'chinkapin oak' = 'oak', 'Jeffrey pine' = 'conifer',
							  'bigcone Douglas-fir' = 'conifer', 'bigleaf maple' = 'broadleaf', 'canyon live oak' = 'oak', 'curlleaf mountain-mahogany' = 'broadleaf', 'incense-cedar' = 'conifer', 'interior live oak' = 'oak', 'limber pine' = 'conifer', 
							  'lodgepole pine' = 'conifer', 'ponderosa pine' = 'conifer', 'singleleaf pinyon' = 'conifer', 'sugar pine' = 'conifer', 'Utah juniper' = 'conifer', 'western juniper' = 'conifer', 'white alder' = 'broadleaf', 'white fir' = 'conifer', 'California laurel' = 'broadleaf',
							  'California-laurel' = 'broadleaf', 'Oregon ash' = 'broadleaf', 'Douglas-fir' = 'conifer', 'honey mesquite' = 'broadleaf', 'desert ironwood' = 'broadleaf', 'California red fir' = 'conifer', 'California buckeye' = 'broadleaf', 'Engelmann oak' = 'oak', 'grand fir' = 'conifer')

#Combine the live and dead columns for the 2002-2006 surveys
trees.2007 <- rbind(live.2007, dead.2007)

#Add simple conifer versus broadleaf veg-type names
trees.2007$veg_type <- recode(.x=trees.2007$COMMON_NAME, 'California black oak' = 'oak', 'California juniper' = 'conifer', 'California live oak' = 'oak', 'California sycamore' = 'broadleaf', 'Coulter pine' = 'conifer', 'chinkapin oak' = 'oak', 'Jeffrey pine' = 'conifer',
							  'bigcone Douglas-fir' = 'conifer', 'bigleaf maple' = 'broadleaf', 'canyon live oak' = 'oak', 'curlleaf mountain-mahogany' = 'broadleaf', 'incense-cedar' = 'conifer', 'interior live oak' = 'oak', 'limber pine' = 'conifer', 
							  'lodgepole pine' = 'conifer', 'ponderosa pine' = 'conifer', 'singleleaf pinyon' = 'conifer', 'sugar pine' = 'conifer', 'Utah juniper' = 'conifer', 'western juniper' = 'conifer', 'white alder' = 'broadleaf', 'white fir' = 'conifer', 'California laurel' = 'broadleaf',
							  'California-laurel' = 'broadleaf', 'Oregon ash' = 'broadleaf', 'Douglas-fir' = 'conifer', 'honey mesquite' = 'broadleaf', 'desert ironwood' = 'broadleaf', 'California red fir' = 'conifer', 'California buckeye' = 'broadleaf', 'Engelmann oak' = 'oak', 'grand fir' = 'conifer')

#Add forest type names of conifer or other
trees.2007$forest_type <- recode(.x=trees.2007$MEANING, 'Canyon live oak' = 'other', 'California mixed conifer' = 'conifer', 'Pinyon / juniper woodland'= 'conifer', 'Jeffrey pine'= 'conifer', 'Lodgepole pine'= 'conifer', 'Limber pine'= 'conifer', 'Other hardwoods' = 'other',
									   'Cercocarpus (mountain brush) woodland' = 'other', 'White fir' = 'conifer', 'California black oak' = 'other', 'Bigcone Douglass-fir' = 'conifer', 'Willow' = 'other', 'Western juniper' = 'conifer', 'Coast live oak' = 'other', 'Incense-cedar' = 'conifer',
									   'Interior live oak' = 'other', 'Juniper woodland' = 'conifer', 'Red alder' = 'other', 'Ponderosa pine' = 'conifer', 'Nonstocked' = 'other', 'Oregon ash' = 'other', 'Coulter pine' = 'conifer')

#Add forest type names of conifer or other
trees.2017$forest_type <- recode(.x=trees.2017$MEANING, 'Canyon live oak' = 'other', 'California mixed conifer' = 'conifer', 'Pinyon / juniper woodland'= 'conifer', 'Jeffrey pine'= 'conifer', 'Lodgepole pine'= 'conifer', 'Limber pine'= 'conifer', 'Other hardwoods' = 'other',
									   'Cercocarpus (mountain brush) woodland' = 'other', 'White fir' = 'conifer', 'California black oak' = 'other', 'Bigcone Douglass-fir' = 'conifer', 'Willow' = 'other', 'Western juniper' = 'conifer', 'Coast live oak' = 'other', 'Incense-cedar' = 'conifer',
									   'Interior live oak' = 'other', 'Juniper woodland' = 'conifer', 'Red alder' = 'other', 'Ponderosa pine' = 'conifer', 'Nonstocked' = 'other', 'Oregon ash' = 'other', 'Coulter pine' = 'conifer')

#Change dead and live tree to 0 = live and 1 = dead
trees.2017$STATUSCD[trees.2017$STATUSCD == 1] <- 0
trees.2017$STATUSCD[trees.2017$STATUSCD == 2] <- 1
trees.2007$STATUSCD[trees.2007$STATUSCD == 1] <- 0
trees.2007$STATUSCD[trees.2007$STATUSCD == 2] <- 1

#Convert height in feet to meters.
trees.2007$ACTUALHT <- trees.2007$ACTUALHT * (1/3.281) #Measured height
trees.2007$HT <- trees.2007$HT * (1/3.281) #Corrected height for broken tops
trees.2017$ACTUALHT <- trees.2017$ACTUALHT * (1/3.281) #Measured height.
trees.2017$HT <- trees.2017$HT * (1/3.281) #Corrected height for broken tops.

#Convert biomass lbs to Mg
trees.2007$live_biomass <- trees.2007$live_biomass * (1/2205) 
trees.2017$live_biomass <- trees.2017$live_biomass * (1/2205)

#Convert DIA inches to centimeters. (1 in = 2.54 cm)
trees.2007$DIA <- trees.2007$DIA * (2.54)
trees.2017$DIA <- trees.2017$DIA * (2.54) 

#Convert from trees per acre to trees per hectare
trees.2007$count <- trees.2007$count * (1/0.404686) 
trees.2017$count <- trees.2017$count * (1/0.404686) 

#Make forest status code (Tree dead or alive) a factor
trees.2007$STATUSCD <- factor(trees.2007$STATUSCD)

#Add a column with the time period that matches the remote sensing data
trees.2007$drought <- '1999-2002'
trees.2017$drought <- '2012-2015'

#Combine all of the data into one data frame
trees.all <- rbind(trees.2007, trees.2017)

#Add a string column for whether the trees area live or dead
trees.all$Tree_Status[trees.all$STATUSCD == 0] <- 'Live'
trees.all$Tree_Status[trees.all$STATUSCD == 1] <- 'Dead'

#Calculate basal area from DIA. Convert to m^2 / ha from cm^2 / ha)
trees.all$basal_area <- (((trees.all$DIA / 2)^2) * pi)*(1/10000) * trees.all$count 

#Change tree names to other tree or pine/fir
trees.all$tree_type <- recode(.x=trees.all$COMMON_NAME, 'California black oak' = 'other tree', 'California juniper' = 'other tree', 'California live oak' = 'other tree', 'California sycamore' = 'other tree', 'Coulter pine' = 'pine/fir', 'chinkapin oak' = 'other tree', 'Jeffrey pine' = 'pine/fir',
                              'bigcone Douglas-fir' = 'pine/fir', 'bigleaf maple' = 'other tree', 'canyon live oak' = 'other tree', 'curlleaf mountain-mahogany' = 'other tree', 'incense-cedar' = 'other tree', 'interior live oak' = 'other tree', 'limber pine' = 'pine/fir', 
                              'lodgepole pine' = 'pine/fir', 'ponderosa pine' = 'pine/fir', 'singleleaf pinyon' = 'pine/fir', 'sugar pine' = 'pine/fir', 'Utah juniper' = 'other tree', 'western juniper' = 'other tree', 'white alder' = 'other tree', 'white fir' = 'pine/fir', 'California laurel' = 'other tree',
                              'California-laurel' = 'other tree', 'Oregon ash' = 'other tree', 'Douglas-fir' = 'pine/fir', 'honey mesquite' = 'other tree', 'desert ironwood' = 'other tree', 'California red fir' = 'pine/fir', 'California buckeye' = 'other tree', 'Engelmann oak' = 'other tree', 'grand fir' = 'pine/fir', 'western white pine' = 'pine/fir',
                              'ash spp.' = 'other tree', 'blue oak' = 'other tree', 'gray or California foothill pine' = 'pine/fir')

#Sort into tree diameter groups
trees.all <- trees.all %>% mutate(DIA.group = case_when(
  DIA >= 65 ~ '65+ cm',
  DIA >= 45 & DIA < 65 ~ '45-64.9 cm', 
  DIA >= 25 & DIA <= 45 ~ '25-44.9 cm',
  DIA < 25 ~ '12.7-24.9 cm'))

#Make the new DIA.group column a factor
trees.all$DIA.group = with(trees.all, factor(DIA.group, levels = c('12.7-24.9 cm', '25-44.9 cm', '45-64.9 cm', '65+ cm')))

#Make the grouped data a data drame
trees.all <- as.data.frame(trees.all)

#Group data by plot and drought
trees.all.group <- trees.all %>% group_by(PLOT, drought) %>%
  summarize(count.plot = sum(count), 
            biomass.plot = sum(live_biomass),
            BasalArea.plot = sum(basal_area),
            DIA.plot.mean = sum(DIA*count) / sum(count),
            DIA.plot.stdDev = sqrt(sum((DIA - (sum(DIA*count)/sum(count)))^2 * count) / sum(count)))

#Make the grouped data a dataframe
trees.all.group <- as.data.frame(trees.all.group)

#Group by plot, drought and tree status
trees.all.status <- trees.all %>% group_by(PLOT, drought, Tree_Status) %>%
  summarize(count.plot = sum(count),  
            biomass.plot = sum(live_biomass),
            BasalArea.plot = sum(basal_area),
            DIA.plot.mean = sum(DIA*count) / sum(count), 
            DIA.plot.stdDev = sqrt(sum((DIA - (sum(DIA*count)/sum(count)))^2 * count) / sum(count)))

#Make the grouped data a dataframe
trees.all.status <- as.data.frame(trees.all.status)

#Select just the live trees
trees.all.live <- subset(trees.all.status, Tree_Status == 'Live')

#Change the column names
colnames(trees.all.live) <- c("PLOT", "drought", "Tree_Status", "count.live", "biomass.live", "BasalArea.live", "DIA.mean.live", "DIA.stdDev.live")

#Select just the dead trees
trees.all.dead <- subset(trees.all.status, Tree_Status == 'Dead')

#Change the column names
colnames(trees.all.dead) <- c("PLOT", "drought", "Tree_Status", "count.dead", "biomass.dead", "BasalArea.dead", "DIA.mean.dead", "DIA.stdDev.dead")

#Select just the plots from the data frame
plots.both <- trees.all.group %>% dplyr::select(PLOT, drought)

#Remove tree status column
rmv <- c('Tree_Status')
trees.all.live <- dplyr::select(trees.all.live, -rmv)

#Join the plots and live tree data frames
join.all.live <- left_join(plots.both, trees.all.live, by = c('PLOT', 'drought'))

#Remove uneccessary columns
trees.all.dead <- dplyr::select(trees.all.dead, -rmv)

#Join the dead trees data with the plots
join.all.dead <- left_join(plots.both, trees.all.dead, by = c('PLOT', 'drought'))

#Combine the dead and live tree data back together
join.all <- left_join(join.all.live, join.all.dead, by = c('PLOT', 'drought'))

#Filling in live plot variables with zero when there is no data
join.all$count.live[is.na(join.all$count.live)] <- 0
join.all$biomass.live[is.na(join.all$biomass.live)] <- 0
join.all$DIA.mean.live[is.na(join.all$DIA.mean.live)] <- 0
join.all$DIA.stdDev.live[is.na(join.all$DIA.stdDev.live)] <- 0
join.all$BasalArea.live[is.na(join.all$BasalArea.live)] <- 0

#Filling in dead conifer plot variables with zero when there is no data
join.all$count.dead[is.na(join.all$count.dead)] <- 0
join.all$biomass.dead[is.na(join.all$biomass.dead)] <- 0
join.all$DIA.mean.dead[is.na(join.all$DIA.mean.dead)] <- 0
join.all$DIA.stdDev.dead[is.na(join.all$DIA.stdDev.dead)] <- 0
join.all$BasalArea.dead[is.na(join.all$BasalArea.dead)] <- 0

#Join data tables 
trees.all.join <- left_join(trees.all.group, join.all, by = c('PLOT', 'drought'))

#Calculate mortality percentages with different data
trees.all.join$mort.count <- (trees.all.join$count.dead / (trees.all.join$count.plot)) * 100
trees.all.join$mort.biomass <- (trees.all.join$biomass.dead / (trees.all.join$biomass.plot)) * 100
trees.all.join$mort.BasalArea <- (trees.all.join$BasalArea.dead / (trees.all.join$BasalArea.plot)) * 100

#Create tree diameter grouping
# trees.all.DIA <- subset(trees.all, forest_type == 'conifer') %>% group_by(DIA.group, PLOT, drought) %>%
#   summarize(count = sum(count), 
#             biomass = sum(live_biomass),
#             basal_area = sum(basal_area)
#   )

#Create tree diameter and plot group
trees.group <- subset(trees.all, tree_type == 'pine/fir') %>% group_by(DIA.group, PLOT, Tree_Status, drought) %>% summarize(
  biomass = sum(live_biomass),
  count = sum(count),
  basal_area = sum(basal_area)
)

#Make the grouped data a dataframe
trees.group <- as.data.frame(trees.group)

#Get the totals by plot by DIA group and drougth
DIA.sum <- subset(trees.all, tree_type == 'pine/fir') %>% group_by(DIA.group, PLOT, drought) %>% summarize(
  biomass = sum(live_biomass),
  count = sum(count),
  basal_area = sum(basal_area)
)

DIA.sum <- as.data.frame(trees.sum)

#Select the plots from the data
trees.select <- trees.group %>% dplyr::select(DIA.group, PLOT, drought)

#Get the data for the full plot
plot.group <- subset(trees.all, tree_type == 'pine/fir') %>% group_by(PLOT, Tree_Status, drought) %>% summarize(
  biomass = sum(live_biomass),
  count = sum(count),
  basal_area = sum(basal_area)
)

#Make the grouped data a data frame
plot.group <- as.data.frame(plot.group)

#Get summary data by plot
plot.summary <- plot.group %>% group_by(Tree_Status, drought) %>% summarize(
  biomass.mean = mean(biomass),
  biomass.sd = sd(biomass),
  biomass.se = sd(biomass) / sqrt(length(biomass)),
  count.mean = mean(count),
  count.sd = sd(count),
  count.se = sd(count) / sqrt(length(count)),
  basal_area.mean = mean(basal_area),
  basal_area.sd = sd(basal_area),
  basal_area.se = sd(basal_area) / sqrt(length(basal_area))
)

#Make the grouped data a dataframe
plot.summmary <- as.data.frame(plot.summary)

#Create tree diameter and plot group
trees.DIA <- subset(trees.all, tree_type == 'pine/fir') %>% group_by(DIA.group, PLOT, Tree_Status, drought) %>% summarize(
  biomass = sum(live_biomass),
  count = sum(count),
  basal_area = sum(basal_area)
)

trees.DIA <- as.data.frame(trees.DIA)
trees.DIA
#Get the totals by plot by DIA group and drougth
# trees.sum <- subset(trees.all, tree_type == 'pine/fir') %>% group_by(DIA.group, PLOT, drought) %>% summarize(
#   biomass = sum(live_biomass),
#   count = sum(count),
#   basal_area = sum(basal_area)
# )
# 
# trees.select <- trees.group %>% select(DIA.group, PLOT, drought)
# 
# #Get the data for the full plot
# plot.group <- subset(trees.all, tree_type == 'pine/fir') %>% group_by(PLOT, Tree_Status, drought) %>% summarize(
#   biomass = sum(live_biomass),
#   count = sum(count),
#   basal_area = sum(basal_area)
# )
# 
# plot.group <- as.data.frame(plot.group)
# 
# #Get summary data by plot
# plot.summary <- plot.group %>% group_by(Tree_Status, drought) %>% summarize(
#   biomass.mean = mean(biomass),
#   biomass.sd = sd(biomass),
#   biomass.se = sd(biomass) / sqrt(length(biomass)),
#   count.mean = mean(count),
#   count.sd = sd(count),
#   count.se = sd(count) / sqrt(length(count)),
#   basal_area.mean = mean(basal_area),
#   basal_area.sd = sd(basal_area),
#   basal_area.se = sd(basal_area) / sqrt(length(basal_area))
# )
# 
# plot.summmary <- as.data.frame(plot.summary)

#Summarize by tree type
trees.type <- trees.all %>% group_by(tree_type, PLOT, Tree_Status, drought) %>% summarize(
  biomass = sum(live_biomass),
  count = sum(count),
  basal_area = sum(basal_area)
)

trees.type <- as.data.frame(trees.type)

#Full plot summaries
# trees.sum <- subset(trees.all, tree_type == 'pine/fir') %>% group_by(DIA.group, PLOT, drought) %>% summarize(
#   biomass = sum(live_biomass),
#   count = sum(count),
#   basal_area = sum(basal_area)
# )

#Make sure to include all the zeros
#Create the DIA small group
DIA.live.small <- subset(trees.DIA, Tree_Status == 'Live' & DIA.group == '12.7-24.9 cm')
colnames(DIA.live.small) <- c("DIA.group", "PLOT", "Tree_Status", "drought", "biomass.live", "count.live", "basal_area.live")
join.DIA.live.small <- left_join(plots.both, DIA.live.small, by = c('PLOT', 'drought'))
join.DIA.live.small$DIA.group <- '12.7-24.9 cm'
join.DIA.live.small$Tree_Status <- 'Live'

#Create the DIA mid group
DIA.live.mid <- subset(trees.DIA, Tree_Status == 'Live' & DIA.group == '25-44.9 cm')
colnames(DIA.live.mid) <- c("DIA.group", "PLOT", "Tree_Status", "drought", "biomass.live", "count.live", "basal_area.live")
join.DIA.live.mid <- left_join(plots.both, DIA.live.mid, by = c('PLOT', 'drought'))
join.DIA.live.mid$DIA.group <- '25-44.9 cm'
join.DIA.live.mid$Tree_Status <- 'Live'

#Create the DIA large group
DIA.live.large <- subset(trees.DIA, Tree_Status == 'Live' & DIA.group == '45-64.9 cm')
colnames(DIA.live.large) <- c("DIA.group", "PLOT", "Tree_Status", "drought", "biomass.live", "count.live", "basal_area.live")
join.DIA.live.large <- left_join(plots.both, DIA.live.large, by = c('PLOT', 'drought'))
join.DIA.live.large$DIA.group <- '45-64.9 cm'
join.DIA.live.large$Tree_Status <- 'Live'

#Create the DIA largest group
DIA.live.largest <- subset(trees.DIA, Tree_Status == 'Live' & DIA.group == '65+ cm')
colnames(DIA.live.largest) <- c("DIA.group", "PLOT", "Tree_Status", "drought", "biomass.live", "count.live", "basal_area.live")
join.DIA.live.largest <- left_join(plots.both, DIA.live.largest, by = c('PLOT', 'drought'))
join.DIA.live.largest$DIA.group <- '65+ cm'
join.DIA.live.largest$Tree_Status <- 'Live'

DIA.live <- rbind(join.DIA.live.small, join.DIA.live.mid, join.DIA.live.large, join.DIA.live.largest) 
rmv <- c('Tree_Status')
DIA.live <- select(DIA.live, -rmv)

#Dead plots
#Create the DIA small group
DIA.dead.small <- subset(trees.DIA, Tree_Status == 'Dead' & DIA.group == '12.7-24.9 cm')
colnames(DIA.dead.small) <- c("DIA.group", "PLOT", "Tree_Status", "drought", "biomass.dead", "count.dead", "basal_area.dead")
join.DIA.dead.small <- left_join(plots.both, DIA.dead.small, by = c('PLOT', 'drought'))
join.DIA.dead.small$DIA.group <- '12.7-24.9 cm'
join.DIA.dead.small$Tree_Status <- 'dead'

#Create the DIA mid group
DIA.dead.mid <- subset(trees.DIA, Tree_Status == 'Dead' & DIA.group == '25-44.9 cm')
colnames(DIA.dead.mid) <- c("DIA.group", "PLOT", "Tree_Status", "drought", "biomass.dead", "count.dead", "basal_area.dead")
join.DIA.dead.mid <- left_join(plots.both, DIA.dead.mid, by = c('PLOT', 'drought'))
join.DIA.dead.mid$DIA.group <- '25-44.9 cm'
join.DIA.dead.mid$Tree_Status <- 'Dead'

#Create the DIA large group
DIA.dead.large <- subset(trees.DIA, Tree_Status == 'Dead' & DIA.group == '45-64.9 cm')
colnames(DIA.dead.large) <- c("DIA.group", "PLOT", "Tree_Status", "drought", "biomass.dead", "count.dead", "basal_area.dead")
join.DIA.dead.large <- left_join(plots.both, DIA.dead.large, by = c('PLOT', 'drought'))
join.DIA.dead.large$DIA.group <- '45-64.9 cm'
join.DIA.dead.large$Tree_Status <- 'dead'

#Create the DIA largest group
DIA.dead.largest <- subset(trees.DIA, Tree_Status == 'Dead' & DIA.group == '65+ cm')
colnames(DIA.dead.largest) <- c("DIA.group", "PLOT", "Tree_Status", "drought", "biomass.dead", "count.dead", "basal_area.dead")
join.DIA.dead.largest <- left_join(plots.both, DIA.dead.largest, by = c('PLOT', 'drought'))
join.DIA.dead.largest$DIA.group <- '65+ cm'
join.DIA.dead.largest$Tree_Status <- 'Dead'

#Bind the dead DIA groups together
DIA.dead <- rbind(join.DIA.dead.small, join.DIA.dead.mid, join.DIA.dead.large, join.DIA.dead.largest) #, join.DIA.dead.large)
DIA.dead <- select(DIA.dead, -rmv)
DIA.join <- left_join(DIA.sum, DIA.live, by = c('PLOT', 'drought', 'DIA.group'))
DIA.all <- left_join(DIA.join, DIA.dead, by = c('PLOT', 'drought', 'DIA.group'))

#Filling in live plot variables
DIA.all$count.live[is.na(DIA.all$count.live)] <- 0
DIA.all$biomass.live[is.na(DIA.all$biomass.live)] <- 0
DIA.all$basal_area.live[is.na(DIA.all$basal_area.live)] <- 0

#Filling in dead plot variables
DIA.all$count.dead[is.na(DIA.all$count.dead)] <- 0
DIA.all$biomass.dead[is.na(DIA.all$biomass.dead)] <- 0
DIA.all$basal_area.dead[is.na(DIA.all$basal_area.dead)] <- 0
DIA.all$basal_area.mort <- DIA.all$basal_area.dead / DIA.all$basal_area * 100
DIA.all <- as.data.frame(DIA.all)

#Summary statistics across plots by DIA
DIA.summary <- DIA.all %>% group_by(DIA.group, drought) %>% summarize(
  biomass.mean = mean(biomass),
  biomass.sd = sd(biomass),
  biomass.se = sd(biomass) / sqrt(length(biomass)),
  count.mean = mean(count),
  count.sd = sd(count),
  count.se = sd(count) / sqrt(length(count)),
  basal_area.mean = mean(basal_area),
  basal_area.sd = sd(basal_area),
  basal_area.se = sd(basal_area) / sqrt(length(basal_area)),
  mortality.mean = mean(basal_area.mort),
  mortality.sd = sd(basal_area.mort),
  mortality.se = sd(basal_area.mort) / sqrt(length(basal_area.mort)),
  sample.size = length(count)
)
DIA.summary <- as.data.frame(DIA.summary)

#Summarize by tree type
trees.type <- trees.all %>% group_by(tree_type, PLOT, Tree_Status, drought) %>% summarize(
  biomass = sum(live_biomass),
  count = sum(count),
  basal_area = sum(basal_area)
)

#Make the grouped data a dataframe
trees.type <- as.data.frame(trees.type)

#Full plot summaries
type.sum <- trees.all %>% group_by(tree_type, PLOT, drought) %>% summarize(
  biomass = sum(live_biomass),
  count = sum(count),
  basal_area = sum(basal_area)
)

#Adding in zero rows for Tree Types analysis
type.live.pine <- subset(trees.type, Tree_Status == 'Live' & tree_type == 'pine/fir')
colnames(type.live.pine) <- c("tree_type", "PLOT", "Tree_Status", "drought", "biomass.live", "count.live", "basal_area.live")
join.type.live.pine <- left_join(plots.both, type.live.pine, by = c('PLOT', 'drought'))
join.type.live.pine$Tree_Status <- 'Live'

#Select out the live other trees
type.live.other <- subset(trees.type, Tree_Status == 'Live' & tree_type == 'other tree')
colnames(type.live.other) <- c("tree_type", "PLOT", "Tree_Status", "drought", "biomass.live", "count.live", "basal_area.live")
join.type.live.other <- left_join(plots.both, type.live.other, by = c('PLOT', 'drought'))
join.type.live.other$Tree_Status <- 'Live'

#Join the columns together and remove uneccessary columns
type.live <- rbind(join.type.live.pine, join.type.live.other)
type.live <- dplyr::select(type.live, -rmv)

#Select out the dead pine data
type.dead.pine <- subset(trees.type, Tree_Status == 'Dead' & tree_type == 'pine/fir')
colnames(type.dead.pine) <- c("tree_type", "PLOT", "Tree_Status", "drought", "biomass.dead", "count.dead", "basal_area.dead")
join.type.dead.pine <- left_join(plots.both, type.dead.pine, by = c('PLOT', 'drought'))
join.type.dead.pine$Tree_Status <- 'Dead'

#Select out the dead other tree data
type.dead.other <- subset(trees.type, Tree_Status == 'Dead' & tree_type == 'other tree')
colnames(type.dead.other) <- c("tree_type", "PLOT", "Tree_Status", "drought", "biomass.dead", "count.dead", "basal_area.dead")
join.type.dead.other <- left_join(plots.both, type.dead.other, by = c('PLOT', 'drought'))
join.type.dead.other$Tree_Status <- 'Dead'

#Combine the dead pine and other tree data together
type.dead <- rbind(join.type.dead.pine, join.type.dead.other)
type.dead <- dplyr::select(type.dead, -rmv)

#Join the overall, dead, and live data together
type.join <- left_join(type.sum, type.live, by = c('PLOT', 'drought', 'tree_type'))
type.all <- left_join(type.join, type.dead, by = c('PLOT', 'drought', 'tree_type'))

#Filling in live plot variables
type.all$count.live[is.na(type.all$count.live)] <- 0
type.all$biomass.live[is.na(type.all$biomass.live)] <- 0
type.all$basal_area.live[is.na(type.all$basal_area.live)] <- 0

#Filling in dead plot variables
type.all$count.dead[is.na(type.all$count.dead)] <- 0
type.all$biomass.dead[is.na(type.all$biomass.dead)] <- 0
type.all$basal_area.dead[is.na(type.all$basal_area.dead)] <- 0
type.all$basal_area.mort <- type.all$basal_area.dead / type.all$basal_area * 100
type.all <- as.data.frame(type.all)

#Summaries by tree types
type.summary <- type.all %>% group_by(tree_type, drought) %>% summarize(
  biomass.mean = mean(biomass),
  biomass.sd = sd(biomass),
  biomass.se = sd(biomass) / sqrt(length(biomass)),
  count.mean = mean(count),
  count.sd = sd(count),
  count.se = sd(count) / sqrt(length(count)),
  basal_area.mean = mean(basal_area),
  basal_area.sd = sd(basal_area),
  basal_area.se = sd(basal_area) / sqrt(length(basal_area)),
  basal_area.dead.mean = mean(basal_area.dead),
  basal_area.dead.sd = sd(basal_area.dead),
  basal_area.dead.se = sd(basal_area.dead) / sqrt(length(basal_area.dead)),
  mortality.mean = mean(basal_area.mort),
  mortality.sd = sd(basal_area.mort),
  mortality.se = sd(basal_area.mort) / sqrt(length(basal_area.mort)),
  sample.size = length(count)
)

#Convert into a data frame
type.summary <- as.data.frame(type.summary)

#Get data for the 2012-2015 Only Drought Sequence
#Query for FIA live trees, 2015-2019
q5 <- dbSendQuery(db, "SELECT 
t.tree, -- tree identified code
(t.carbon_ag * 2)*(t.tpa_unadj) live_biomass, -- biomass (lbs) of tree
t.tpa_unadj count, --trees/acre
t.dia, -- DBH in inches
t.ht, --Total live tree height in feet
t.actualht, --The measure height in feet
t.agentcd, --tree damage
t.mortyr, --mortality year
t.plot, t.statuscd, t.invyr, r.common_name, t.spcd, c.fortypcd, c.fldtypcd, rft.meaning
FROM 
cond c,
plot p,
tree t, -- tree table must be included for tree-level estimates
ref_species r,
ref_forest_type rft
WHERE p.cn = c.plt_cn
AND t.plt_cn = c.plt_cn
AND t.condid = c.condid
AND c.cond_status_cd = 1 --2 means forest
AND t.statuscd = 1 --1 means live trees, 2 means dead trees
AND t.spcd = r.spcd
AND t.dia >= 5.0 -- additional where_clause from ref_pop_attribute table
AND rft.value = c.fldtypcd
AND (P.ECOSUBCD = 'M261Ep' OR P.ECOSUBCD = 'M261Eq' OR P.ECOSUBCD = 'M261Eu' OR P.ECOSUBCD = 'M261Er' OR P.ECOSUBCD = 'M261Eo' OR P.ECOSUBCD = 'M262Bb' OR P.ECOSUBCD = 'M262Ba') --South Sierra Nevada
AND (p.invyr >= 2015 AND p.invyr <= 2019)
AND (c.dstrbcd1 = 0 OR c.dstrbcd1 = 10 OR c.dstrbcd1 = 11 OR c.dstrbcd1 = 12 OR c.dstrbcd1 = 54 OR c.dstrbcd1 = 70)
")

live.second.2017 <- dbFetch(q5, n = -1)

#Query for FIA dead trees, 2015-2019
q6 <- dbSendQuery(db, "SELECT 
t.tree, -- tree identified code
(t.carbon_ag * 2)* t.tpa_unadj live_biomass, -- biomass (lbs) per tree
t.tpa_unadj count, --trees/acre
t.dia, -- DBH in inches
t.ht, --Total live tree height in feet
t.actualht, --The measure height in feet
t.agentcd, --tree damage
t.mortyr, --mortality year
t.plot, t.statuscd, t.invyr, r.common_name, t.spcd, c.fortypcd, c.fldtypcd, rft.meaning--t.spcd, r.major_spgrpcd
FROM 
cond c,
plot p,
tree t, -- tree table must be included for tree-level estimates
ref_species r,
ref_forest_type rft
WHERE p.cn = c.plt_cn
AND t.plt_cn = c.plt_cn
AND t.condid = c.condid
AND c.cond_status_cd = 1 --1 means forest
AND t.statuscd = 2 --1 means live trees, 2 means dead trees
AND t.spcd = r.spcd
AND t.dia >= 5.0 -- additional where_clause from ref_pop_attribute table
AND rft.value = c.fldtypcd
AND (P.ECOSUBCD = 'M261Ep' OR P.ECOSUBCD = 'M261Eq' OR P.ECOSUBCD = 'M261Eu' OR P.ECOSUBCD = 'M261Er' OR P.ECOSUBCD = 'M261Eo' OR P.ECOSUBCD = 'M262Bb' OR P.ECOSUBCD = 'M262Ba') --OR P.ECOSUBCD = 'M261Es'  South Sierra Nevada
AND (p.invyr >= 2015 AND p.invyr <= 2019)
AND (c.dstrbcd1 = 0 OR c.dstrbcd1 = 10 OR c.dstrbcd1 = 11 OR c.dstrbcd1 = 12 OR c.dstrbcd1 = 54 OR c.dstrbcd1 = 70)
AND (t.mortyr != 2001 AND t.mortyr != 2002 AND t.mortyr != 2003 AND t.mortyr != 2004 AND t.mortyr != 2005 AND t.mortyr != 2006 AND t.mortyr != 2007 AND t.mortyr != 2008)
")

#Retrieve the queried data
dead.second.2017 <- dbFetch(q6, n = -1)

#Query for FIA live trees 2002 - 2006
q7 <- dbSendQuery(db, "SELECT 
t.tree, -- tree identified code
(t.carbon_ag * 2)* t.tpa_unadj live_biomass, -- biomass (lbs) per tree
t.tpa_unadj count, --trees/acre
t.dia, -- DBH in inches
t.ht, --Total live tree height in feet
t.actualht, --The measure height in feet
t.agentcd, --tree damage
t.mortyr, --mortality year
t.plot, t.statuscd, p.invyr, r.common_name, t.spcd, c.fortypcd, c.fldtypcd, rft.meaning--t.spcd, r.major_spgrpcd
FROM 
cond c,
plot p,
tree t, -- tree table must be included for tree-level estimates
--seedling s, --seedling table must be included for seedling level estimates
ref_species r,
ref_forest_type rft
WHERE p.cn = c.plt_cn
AND t.plt_cn = c.plt_cn
AND t.condid = c.condid
AND c.cond_status_cd = 1 --1 means forest
AND t.statuscd = 1 --1 means live trees, 2 means dead trees
AND t.spcd = r.spcd
AND t.dia >= 5.0 -- additional where_clause from ref_pop_attribute table
AND rft.value = c.fldtypcd
--AND (P.COUNTYCD = 65 OR P.COUNTYCD = 71 OR P.COUNTYCD = 37 OR P.COUNTYCD = 73 OR P.COUNTYCD = 59)
AND (P.ECOSUBCD = 'M261Ep' OR P.ECOSUBCD = 'M261Eq' OR P.ECOSUBCD = 'M261Eu' OR P.ECOSUBCD = 'M261Er' OR P.ECOSUBCD = 'M261Eo' OR P.ECOSUBCD = 'M262Bb' OR P.ECOSUBCD = 'M262Ba') --OR P.ECOSUBCD = 'M261Es'  South Sierra Nevada  
AND (p.invyr >= 2002 AND p.invyr <= 2006)
--AND (c.dstrbcd1 != 30 OR c.dstrbcd1 != 31 OR c.dstrbcd1 != 32) --this seems to cause problems for 1994 periodic plots, this removes plots with a burn disturbance code
AND (c.dstrbcd1 = 0 OR c.dstrbcd1 = 10 OR c.dstrbcd1 = 11 OR c.dstrbcd1 = 12 OR c.dstrbcd1 = 54 OR c.dstrbcd1 = 70) --10, 11, 12 are insect, 54 is drought, 0 is no disturbance, 70 is unknown
")

#Retrieve the queried data
live.second.2007 <- dbFetch(q7, n = -1)

#Query for FIA dead trees 2002 - 2006
q8 <- dbSendQuery(db, "SELECT 
t.tree, -- tree identified code
(t.carbon_ag * 2)* t.tpa_unadj live_biomass, -- biomass (lbs) per tree
t.tpa_unadj count, --trees/acre
t.dia, -- DBH in inches
t.ht, --Total live tree height in feet
t.actualht, --The measure height in feet
t.agentcd, --tree damage
t.mortyr, --mortality year
t.plot, t.statuscd, p.invyr, r.common_name, t.spcd, c.fortypcd, c.fldtypcd, rft.meaning
FROM --cond c_past, 
cond c,
plot p,
tree t, -- tree table must be included for tree-level estimates
ref_species r,
ref_forest_type rft
WHERE p.cn = c.plt_cn
AND t.plt_cn = c.plt_cn
AND t.condid = c.condid
AND c.cond_status_cd = 1 --1 means forest
AND t.statuscd = 2 --1 means live trees, 2 means dead trees
AND t.spcd = r.spcd
AND t.dia >= 5.0 -- additional where_clause from ref_pop_attribute table
AND rft.value = c.fldtypcd
AND (P.ECOSUBCD = 'M261Ep' OR P.ECOSUBCD = 'M261Eq' OR P.ECOSUBCD = 'M261Eu' OR P.ECOSUBCD = 'M261Er' OR P.ECOSUBCD = 'M261Eo' OR P.ECOSUBCD = 'M262Bb' OR P.ECOSUBCD = 'M262Ba') --OR P.ECOSUBCD = 'M261Es'  South Sierra Nevada
AND (p.invyr >= 2002 AND p.invyr <= 2006)
AND (c.dstrbcd1 = 0 OR c.dstrbcd1 = 10 OR c.dstrbcd1 = 11 OR c.dstrbcd1 = 12 OR c.dstrbcd1 = 54 OR c.dstrbcd1 = 70) --10, 11, 12 are insect, 54 is drought, 0 is no disturbance, 70 is unknown
AND (t.mortyr != '') --Seeing what happens when I remove this
")

#Retrieve the queried data
dead.second.2007 <- dbFetch(q8, n = -1)

#Columns to drop
drop.cols <- c('INVYR', 'FORTYPCD','MEANING') 

#Combine the live and dead trees for the 2011-2017 surveys.
trees.second.2017 <- rbind(live.second.2017, dead.second.2017)

#Create column with just conifer versus broadleaf
trees.second.2017$veg_type <- recode(.x=trees.second.2017$COMMON_NAME, 'California black oak' = 'oak', 'California juniper' = 'conifer', 'California live oak' = 'oak', 'California sycamore' = 'broadleaf', 'Coulter pine' = 'conifer', 'chinkapin oak' = 'oak', 'Jeffrey pine' = 'conifer',
                              'bigcone Douglas-fir' = 'conifer', 'bigleaf maple' = 'broadleaf', 'canyon live oak' = 'oak', 'curlleaf mountain-mahogany' = 'broadleaf', 'incense-cedar' = 'conifer', 'interior live oak' = 'oak', 'limber pine' = 'conifer', 
                              'lodgepole pine' = 'conifer', 'ponderosa pine' = 'conifer', 'singleleaf pinyon' = 'conifer', 'sugar pine' = 'conifer', 'Utah juniper' = 'conifer', 'western juniper' = 'conifer', 'white alder' = 'broadleaf', 'white fir' = 'conifer', 'California laurel' = 'broadleaf',
                              'California-laurel' = 'broadleaf', 'Oregon ash' = 'broadleaf', 'Douglas-fir' = 'conifer', 'honey mesquite' = 'broadleaf', 'desert ironwood' = 'broadleaf', 'California red fir' = 'conifer', 'California buckeye' = 'broadleaf', 'Engelmann oak' = 'oak', 'grand fir' = 'conifer')

#Combine the dead and live tree samples for 2002-2006 into one data frame
trees.second.2007 <- rbind(live.second.2007, dead.second.2007)

#Separate into broadleaf, conifer, oak vegetation types
trees.second.2007$veg_type <- recode(.x=trees.second.2007$COMMON_NAME, 'California black oak' = 'oak', 'California juniper' = 'conifer', 'California live oak' = 'oak', 'California sycamore' = 'broadleaf', 'Coulter pine' = 'conifer', 'chinkapin oak' = 'oak', 'Jeffrey pine' = 'conifer',
                              'bigcone Douglas-fir' = 'conifer', 'bigleaf maple' = 'broadleaf', 'canyon live oak' = 'oak', 'curlleaf mountain-mahogany' = 'broadleaf', 'incense-cedar' = 'conifer', 'interior live oak' = 'oak', 'limber pine' = 'conifer', 
                              'lodgepole pine' = 'conifer', 'ponderosa pine' = 'conifer', 'singleleaf pinyon' = 'conifer', 'sugar pine' = 'conifer', 'Utah juniper' = 'conifer', 'western juniper' = 'conifer', 'white alder' = 'broadleaf', 'white fir' = 'conifer', 'California laurel' = 'broadleaf',
                              'California-laurel' = 'broadleaf', 'Oregon ash' = 'broadleaf', 'Douglas-fir' = 'conifer', 'honey mesquite' = 'broadleaf', 'desert ironwood' = 'broadleaf', 'California red fir' = 'conifer', 'California buckeye' = 'broadleaf', 'Engelmann oak' = 'oak', 'grand fir' = 'conifer')

#Seperate into conifer and other forest types
trees.second.2007$forest_type <- recode(.x=trees.second.2007$MEANING, 'Canyon live oak' = 'other', 'California mixed conifer' = 'conifer', 'Pinyon / juniper woodland'= 'conifer', 'Jeffrey pine'= 'conifer', 'Lodgepole pine'= 'conifer', 'Limber pine'= 'conifer', 'Other hardwoods' = 'other',
                                 'Cercocarpus (mountain brush) woodland' = 'other', 'White fir' = 'conifer', 'California black oak' = 'other', 'Bigcone Douglass-fir' = 'conifer', 'Willow' = 'other', 'Western juniper' = 'conifer', 'Coast live oak' = 'other', 'Incense-cedar' = 'conifer',
                                 'Interior live oak' = 'other', 'Juniper woodland' = 'conifer', 'Red alder' = 'other', 'Ponderosa pine' = 'conifer', 'Nonstocked' = 'other', 'Oregon ash' = 'other', 'Coulter pine' = 'conifer')

#Sepearate into conifer and other forest types
trees.second.2017$forest_type <- recode(.x=trees.second.2017$MEANING, 'Canyon live oak' = 'other', 'California mixed conifer' = 'conifer', 'Pinyon / juniper woodland'= 'conifer', 'Jeffrey pine'= 'conifer', 'Lodgepole pine'= 'conifer', 'Limber pine'= 'conifer', 'Other hardwoods' = 'other',
                                 'Cercocarpus (mountain brush) woodland' = 'other', 'White fir' = 'conifer', 'California black oak' = 'other', 'Bigcone Douglass-fir' = 'conifer', 'Willow' = 'other', 'Western juniper' = 'conifer', 'Coast live oak' = 'other', 'Incense-cedar' = 'conifer',
                                 'Interior live oak' = 'other', 'Juniper woodland' = 'conifer', 'Red alder' = 'other', 'Ponderosa pine' = 'conifer', 'Nonstocked' = 'other', 'Oregon ash' = 'other', 'Coulter pine' = 'conifer')

#Update tree status so live = 0, dead = 1
trees.second.2017$STATUSCD[trees.second.2017$STATUSCD == 1] <- 0
trees.second.2017$STATUSCD[trees.second.2017$STATUSCD == 2] <- 1
trees.second.2007$STATUSCD[trees.second.2007$STATUSCD == 1] <- 0
trees.second.2007$STATUSCD[trees.second.2007$STATUSCD == 2] <- 1

#Convert height in feet to meters.
trees.second.2007$ACTUALHT <- trees.second.2007$ACTUALHT * (1/3.281) #2002-2006 sample of trees. Measured tree height.
trees.second.2007$HT <- trees.second.2007$HT * (1/3.281) #2015-2019 sample of trees. Tree height corrected for broken tops.
trees.second.2017$ACTUALHT <- trees.second.2017$ACTUALHT * (1/3.281) #2002-2006 sample of trees. Measured tree height.
trees.second.2017$HT <- trees.second.2017$HT * (1/3.281) #2015-2019 sample of trees. Tree hieght corrected for broken tops.

#Convert lbs to Mg
trees.second.2007$live_biomass <- trees.second.2007$live_biomass * (1/2205) #2002-2006 sample of trees
trees.second.2017$live_biomass <- trees.second.2017$live_biomass * (1/2205) #2015-2019 sample of trees

#Convert inches to centimeters. (1 in = 2.54 cm)
trees.second.2007$DIA <- trees.second.2007$DIA * (2.54) #2002-2006 sample of trees
trees.second.2017$DIA <- trees.second.2017$DIA * (2.54) #2015-2019 sample of trees

#Convert from trees per acre to trees per hectare
trees.second.2007$count <- trees.second.2007$count * (1/0.404686) #2002-2006 sample of trees
trees.second.2017$count <- trees.second.2017$count * (1/0.404686) #2015-2019 sample of trees

#Year lables for the tree samples
trees.second.2007$drought <- '1999-2002'
trees.second.2017$drought <- '2012-2015'

#Combine samples for the two time periods for the 2012-2015 Only drought sequence
trees.second <- rbind(trees.second.2007, trees.second.2017)

#Add a string column for live and dead trees
trees.second$Tree_Status[trees.second$STATUSCD == 0] <- 'Live'
trees.second$Tree_Status[trees.second$STATUSCD == 1] <- 'Dead'

#Calculate basal area. Convert to m^2 / ha (from cm^2 / ha)
trees.second$basal_area <- (((trees.second$DIA / 2)^2) * pi)*(1/10000) * trees.second$count 

#Change tree names to other tree or pine/fir
trees.second$tree_type <- recode(.x=trees.second$COMMON_NAME, 'California black oak' = 'other tree', 'California juniper' = 'other tree', 'California live oak' = 'other tree', 'California sycamore' = 'other tree', 'Coulter pine' = 'pine/fir', 'chinkapin oak' = 'other tree', 'Jeffrey pine' = 'pine/fir',
							  'bigcone Douglas-fir' = 'pine/fir', 'bigleaf maple' = 'other tree', 'canyon live oak' = 'other tree', 'curlleaf mountain-mahogany' = 'other tree', 'incense-cedar' = 'other tree', 'interior live oak' = 'other tree', 'limber pine' = 'pine/fir', 
							  'lodgepole pine' = 'pine/fir', 'ponderosa pine' = 'pine/fir', 'singleleaf pinyon' = 'pine/fir', 'sugar pine' = 'pine/fir', 'Utah juniper' = 'other tree', 'western juniper' = 'other tree', 'white alder' = 'other tree', 'white fir' = 'pine/fir', 'California laurel' = 'other tree',
							  'California-laurel' = 'other tree', 'Oregon ash' = 'other tree', 'Douglas-fir' = 'pine/fir', 'honey mesquite' = 'other tree', 'desert ironwood' = 'other tree', 'California red fir' = 'pine/fir', 'California buckeye' = 'other tree', 'Engelmann oak' = 'other tree', 'grand fir' = 'pine/fir', 'western white pine' = 'pine/fir',
							  "western white pine" = 'pine/fir', "whitebark pine" = 'pine/fir', "mountain hemlock" = "pine/fir", "gray or California foothill pine" = "pine/fir", "foxtail pine" = 'pine/fir', "blue oak" = 'other tree', "California white oak" = 'other tree', "quaking aspen" = 'other tree', 
							  "giant sequoia" = 'other tree', "Unknown dead conifer" = 'pine/fir', "ash spp." = 'other tree', "black cottonwood" = 'other tree', "California torreya (nutmeg)" = 'other tree', "Oregon white oak" = 'other tree', "Port-Orford-cedar" = 'other tree', "Pacific dogwood" = 'other tree')


#Sort into tree diameter groups
trees.second <- trees.second %>% mutate(DIA.group = case_when(
  DIA >= 65 ~ '65+ cm',
  DIA >= 45 & DIA < 65 ~ '45-64.9 cm', 
  DIA >= 25 & DIA <= 45 ~ '25-44.9 cm',
  DIA < 25 ~ '12.7-24.9 cm'))

#Make the new DIA.group column a factor
trees.second$DIA.group = with(trees.second, factor(DIA.group, levels = c('12.7-24.9 cm', '25-44.9 cm', '45-64.9 cm', '65+ cm')))

#Convert into a data frame
trees.second <- as.data.frame(trees.second)
summary(trees.second)
trees.second %>% filter(COMMON_NAME == "ponderosa pine") %>% count()
trees.second %>% filter(COMMON_NAME == "ponderosa pine") %>% summary()
trees.all %>% filter(COMMON_NAME == "ponderosa pine") %>% summary()
trees.all %>% group_by(COMMON_NAME) %>% count() %>% tail()

Species.All.Death.Summary <- trees.all %>% filter(drought == '1999-2002') %>% group_by(COMMON_NAME, Tree_Status) %>% summarize(DIA.mean = sum(DIA*count)/sum(count), count.all = sum(count))

Species.Second.Death.Summary <- trees.second %>% filter(drought == '2012-2015') %>% group_by(COMMON_NAME, Tree_Status) %>% summarize(DIA.mean = sum(DIA*count)/sum(count), sum(count))

#Add the DIA grouping

#Group data by plot and drought
trees.second.group <- trees.second %>% group_by(PLOT, drought) %>%
							 summarize(count.plot = sum(count), 
									   biomass.plot = sum(live_biomass),
									   BasalArea.plot = sum(basal_area),
									   DIA.plot.mean = sum(DIA*count) / sum(count),
									   DIA.plot.stdDev = sqrt(sum((DIA - (sum(DIA*count)/sum(count)))^2 * count) / sum(count)))

#Convert into a data frame
trees.second.group <- as.data.frame(trees.second.group)

#Get the totals by plot by DIA group and drougth
DIA.second.sum <- subset(trees.second, tree_type == 'pine/fir') %>% group_by(PLOT, drought, DIA.group) %>% summarize(
  biomass = sum(live_biomass),
  count = sum(count),
  basal_area = sum(basal_area)
)

DIA.second.sum <- as.data.frame(DIA.second.sum)
DIA.second.sum

#Create tree diameter and plot group
trees.second.DIA <- subset(trees.second, tree_type == 'pine/fir') %>% group_by(DIA.group, PLOT, Tree_Status, drought) %>% summarize(
  biomass = sum(live_biomass),
  count = sum(count),
  basal_area = sum(basal_area)
)

trees.second.DIA <- as.data.frame(trees.second.DIA)
trees.second.DIA

#Group by plot, drought and tree status
trees.second.status <- trees.second %>% group_by(PLOT, drought, Tree_Status) %>%
							 summarize(count.plot = sum(count),  
									   biomass.plot = sum(live_biomass),
									   BasalArea.plot = sum(basal_area),
									   DIA.plot.mean = sum(DIA*count) / sum(count), 
									   DIA.plot.stdDev = sqrt(sum((DIA - (sum(DIA*count)/sum(count)))^2 * count) / sum(count)))

#Convert into a data frame
trees.second.status <- as.data.frame(trees.second.status)

#Select the live tree data
trees.second.live <- subset(trees.second.status, Tree_Status == 'Live')
colnames(trees.second.live) <- c("PLOT", "drought", "Tree_Status", "count.live", "biomass.live", "BasalArea.live", "DIA.mean.live", "DIA.stdDev.live")

#Select the dead tree data
trees.second.dead <- subset(trees.second.status, Tree_Status == 'Dead')
colnames(trees.second.dead) <- c("PLOT", "drought", "Tree_Status", "count.dead", "biomass.dead", "BasalArea.dead", "DIA.mean.dead", "DIA.stdDev.dead")

# Select a list of the plots by the sampling year
plots.second.both <- trees.second.group %>% dplyr::select(PLOT, drought)

#Remove tree status column
rmv <- c('Tree_Status')
trees.second.live <- dplyr::select(trees.second.live, -rmv)

#Join data tables
join.second.live <- left_join(plots.second.both, trees.second.live, by = c('PLOT', 'drought'))

#Remove uneccessary columns
trees.second.dead <- dplyr::select(trees.second.dead, -rmv)
join.second.dead <- left_join(plots.second.both, trees.second.dead, by = c('PLOT', 'drought'))
join.second <- left_join(join.second.live, join.second.dead, by = c('PLOT', 'drought'))

#Filling in live plot variables
join.second$count.live[is.na(join.second$count.live)] <- 0
join.second$biomass.live[is.na(join.second$biomass.live)] <- 0
join.second$DIA.mean.live[is.na(join.second$DIA.mean.live)] <- 0
join.second$DIA.stdDev.live[is.na(join.second$DIA.stdDev.live)] <- 0
join.second$BasalArea.live[is.na(join.second$BasalArea.live)] <- 0

#Filling in dead conifer plot variables
join.second$count.dead[is.na(join.second$count.dead)] <- 0
join.second$biomass.dead[is.na(join.second$biomass.dead)] <- 0
join.second$DIA.mean.dead[is.na(join.second$DIA.mean.dead)] <- 0
join.second$DIA.stdDev.dead[is.na(join.second$DIA.stdDev.dead)] <- 0
join.second$BasalArea.dead[is.na(join.second$BasalArea.dead)] <- 0

#Join data tables
trees.second.join <- left_join(trees.second.group, join.second, by = c('PLOT', 'drought'))
# trees.second.join

#Calculate mortality percentages with different data
trees.second.join$mort.count <- (trees.second.join$count.dead / (trees.second.join$count.plot)) * 100
trees.second.join$mort.biomass <- (trees.second.join$biomass.dead / (trees.second.join$biomass.plot)) * 100
trees.second.join$mort.BasalArea <- (trees.second.join$BasalArea.dead / (trees.second.join$BasalArea.plot)) * 100

#Get the data for the full plot
plot.second.group <- subset(trees.second, tree_type == 'pine/fir') %>% group_by(PLOT, Tree_Status, drought) %>% summarize(
									   biomass = sum(live_biomass),
									   count = sum(count),
									   basal_area = sum(basal_area)
		)

#Convert into a data frame
plot.second.group <- as.data.frame(plot.second.group)

#Get summary data by plot
plot.second.summary <- plot.second.group %>% group_by(Tree_Status, drought) %>% summarize(
																			  biomass.mean = mean(biomass),
																			  biomass.sd = sd(biomass),
																			  biomass.se = sd(biomass) / sqrt(length(biomass)),
																			  count.mean = mean(count),
																			  count.sd = sd(count),
																			  count.se = sd(count) / sqrt(length(count)),
																			  basal_area.mean = mean(basal_area),
																			  basal_area.sd = sd(basal_area),
																			  basal_area.se = sd(basal_area) / sqrt(length(basal_area))
		)

#Convert into a data frame
plot.second.summmary <- as.data.frame(plot.second.summary)

#Make sure to include all the zeros
#Create the DIA small group
DIA.second.live.small <- subset(trees.second.DIA, Tree_Status == 'Live' & DIA.group == '12.7-24.9 cm')
colnames(DIA.second.live.small) <- c("DIA.group", "PLOT", "Tree_Status", "drought", "biomass.live", "count.live", "basal_area.live")
join.DIA.second.live.small <- left_join(plots.second.both, DIA.second.live.small, by = c('PLOT', 'drought'))
join.DIA.second.live.small$DIA.group <- '12.7-24.9 cm'
join.DIA.second.live.small$Tree_Status <- 'Live'

#Create the DIA mid group
DIA.second.live.mid <- subset(trees.second.DIA, Tree_Status == 'Live' & DIA.group == '25-44.9 cm')
colnames(DIA.second.live.mid) <- c("DIA.group", "PLOT", "Tree_Status", "drought", "biomass.live", "count.live", "basal_area.live")
join.DIA.second.live.mid <- left_join(plots.second.both, DIA.second.live.mid, by = c('PLOT', 'drought'))
join.DIA.second.live.mid$DIA.group <- '25-44.9 cm'
join.DIA.second.live.mid$Tree_Status <- 'Live'

#Create the DIA large group
DIA.second.live.large <- subset(trees.second.DIA, Tree_Status == 'Live' & DIA.group == '45-64.9 cm')
colnames(DIA.second.live.large) <- c("DIA.group", "PLOT", "Tree_Status", "drought", "biomass.live", "count.live", "basal_area.live")
join.DIA.second.live.large <- left_join(plots.second.both, DIA.second.live.large, by = c('PLOT', 'drought'))
join.DIA.second.live.large$DIA.group <- '45-64.9 cm'
join.DIA.second.live.large$Tree_Status <- 'Live'

#Create the DIA largest group
DIA.second.live.largest <- subset(trees.second.DIA, Tree_Status == 'Live' & DIA.group == '65+ cm')
colnames(DIA.second.live.largest) <- c("DIA.group", "PLOT", "Tree_Status", "drought", "biomass.live", "count.live", "basal_area.live")
join.DIA.second.live.largest <- left_join(plots.second.both, DIA.second.live.largest, by = c('PLOT', 'drought'))
join.DIA.second.live.largest$DIA.group <- '65+ cm'
join.DIA.second.live.largest$Tree_Status <- 'Live'

DIA.second.live <- rbind(join.DIA.second.live.small, join.DIA.second.live.mid, join.DIA.second.live.large, join.DIA.second.live.largest) 
rmv <- c('Tree_Status')
DIA.second.live <- select(DIA.second.live, -rmv)

#Dead plots
#Create the DIA small group
DIA.second.dead.small <- subset(trees.second.DIA, Tree_Status == 'Dead' & DIA.group == '12.7-24.9 cm')
colnames(DIA.second.dead.small) <- c("DIA.group", "PLOT", "Tree_Status", "drought", "biomass.dead", "count.dead", "basal_area.dead")
join.DIA.second.dead.small <- left_join(plots.second.both, DIA.second.dead.small, by = c('PLOT', 'drought'))
join.DIA.second.dead.small$DIA.group <- '12.7-24.9 cm'
join.DIA.second.dead.small$Tree_Status <- 'dead'

#Create the DIA mid group
DIA.second.dead.mid <- subset(trees.second.DIA, Tree_Status == 'Dead' & DIA.group == '25-44.9 cm')
colnames(DIA.second.dead.mid) <- c("DIA.group", "PLOT", "Tree_Status", "drought", "biomass.dead", "count.dead", "basal_area.dead")
join.DIA.second.dead.mid <- left_join(plots.second.both, DIA.second.dead.mid, by = c('PLOT', 'drought'))
join.DIA.second.dead.mid$DIA.group <- '25-44.9 cm'
join.DIA.second.dead.mid$Tree_Status <- 'Dead'

#Create the DIA large group
DIA.second.dead.large <- subset(trees.second.DIA, Tree_Status == 'Dead' & DIA.group == '45-64.9 cm')
colnames(DIA.second.dead.large) <- c("DIA.group", "PLOT", "Tree_Status", "drought", "biomass.dead", "count.dead", "basal_area.dead")
join.DIA.second.dead.large <- left_join(plots.second.both, DIA.second.dead.large, by = c('PLOT', 'drought'))
join.DIA.second.dead.large$DIA.group <- '45-64.9 cm'
join.DIA.second.dead.large$Tree_Status <- 'Dead'

#Create the DIA largest group
DIA.second.dead.largest <- subset(trees.second.DIA, Tree_Status == 'Dead' & DIA.group == '65+ cm')
colnames(DIA.second.dead.largest) <- c("DIA.group", "PLOT", "Tree_Status", "drought", "biomass.dead", "count.dead", "basal_area.dead")
join.DIA.second.dead.largest <- left_join(plots.second.both, DIA.second.dead.largest, by = c('PLOT', 'drought'))
join.DIA.second.dead.largest$DIA.group <- '65+ cm'
join.DIA.second.dead.largest$Tree_Status <- 'Dead'

#Bind the dead DIA groups together
DIA.second.dead <- rbind(join.DIA.second.dead.small, join.DIA.second.dead.mid, join.DIA.second.dead.large, join.DIA.second.dead.largest) #, join.DIA.dead.large)
DIA.second.dead <- select(DIA.second.dead, -rmv)
DIA.second.join <- left_join(DIA.second.sum, DIA.second.live, by = c('PLOT', 'drought', 'DIA.group'))
# DIA.second.join
# DIA.second.sum
# DIA.second.dead
# DIA.second.live
# 
# DIA.second.join
DIA.second.all <- left_join(DIA.second.join, DIA.second.dead, by = c('PLOT', 'drought', 'DIA.group'))

#Filling in live plot variables
DIA.second.all$count.live[is.na(DIA.second.all$count.live)] <- 0
DIA.second.all$biomass.live[is.na(DIA.second.all$biomass.live)] <- 0
DIA.second.all$basal_area.live[is.na(DIA.second.all$basal_area.live)] <- 0

#Filling in dead plot variables
DIA.second.all$count.dead[is.na(DIA.second.all$count.dead)] <- 0
DIA.second.all$biomass.dead[is.na(DIA.second.all$biomass.dead)] <- 0
DIA.second.all$basal_area.dead[is.na(DIA.second.all$basal_area.dead)] <- 0
DIA.second.all$basal_area.mort <- DIA.second.all$basal_area.dead / DIA.second.all$basal_area * 100
DIA.second.all <- as.data.frame(DIA.second.all)
DIA.second.all

#Summary statistics across plots by DIA
DIA.second.summary <- DIA.second.all %>% group_by(DIA.group, drought) %>% summarize(
  biomass.mean = mean(biomass),
  biomass.sd = sd(biomass),
  biomass.se = sd(biomass) / sqrt(length(biomass)),
  count.mean = mean(count),
  count.sd = sd(count),
  count.se = sd(count) / sqrt(length(count)),
  basal_area.mean = mean(basal_area),
  basal_area.sd = sd(basal_area),
  basal_area.se = sd(basal_area) / sqrt(length(basal_area)),
  mortality.mean = mean(basal_area.mort),
  mortality.sd = sd(basal_area.mort),
  mortality.se = sd(basal_area.mort) / sqrt(length(basal_area.mort)),
  sample.size = length(count)
)
DIA.second.summary <- as.data.frame(DIA.second.summary)
DIA.second.all
DIA.second.summary
#Add the drought exposure sequence to the script.
DIA.summary$sequence <- 'Both Droughts'
DIA.second.summary$sequence <- '2012-2015 Only'
DIA.summary.all <- rbind(DIA.summary, DIA.second.summary)
DIA.summary.all

# Combining the type data for the two drought sequence regions
DIA.all$sequence <- 'Both Droughts'
DIA.second.all$sequence <- '2012-2015 Only'
DIA.both.all <- rbind(DIA.all, DIA.second.all)

#Make DIA.group a factor
DIA.both.all$DIA.group <- factor(DIA.both.all$DIA.group, levels = c('12.7-24.9 cm', '25-44.9 cm', '45-64.9 cm', '65+ cm'))

#Summarize by tree type
trees.second.type <- trees.second %>% group_by(tree_type, PLOT, Tree_Status, drought) %>% summarize(
									   biomass = sum(live_biomass),
									   count = sum(count),
									   basal_area = sum(basal_area)
		)

#Convert into a data frame
trees.second.type <- as.data.frame(trees.second.type)

#Full plot summaries by tree type
type.second.sum <- trees.second %>% group_by(tree_type, PLOT, drought) %>% summarize(
									   biomass = sum(live_biomass),
									   count = sum(count),
									   basal_area = sum(basal_area)
		)

#Full plot summaries without tree type
plot.second.sum <- trees.second %>% group_by(PLOT, drought, Tree_Status) %>% summarize(
  biomass = sum(live_biomass),
  count = sum(count),
  basal_area = sum(basal_area)
)

head(plot.second.sum)

#Adding in zero rows for Tree Types analysis
#Add the live pine tree data by plot
type.second.live.pine <- subset(trees.second.type, Tree_Status == 'Live' & tree_type == 'pine/fir')
colnames(type.second.live.pine) <- c("tree_type", "PLOT", "Tree_Status", "drought", "biomass.live", "count.live", "basal_area.live")
join.type.second.live.pine <- left_join(plots.second.both, type.second.live.pine, by = c('PLOT', 'drought'))
join.type.second.live.pine$Tree_Status <- 'Live'

#Add the live other tree data by plot
type.second.live.other <- subset(trees.second.type, Tree_Status == 'Live' & tree_type == 'other tree')
colnames(type.second.live.other) <- c("tree_type", "PLOT", "Tree_Status", "drought", "biomass.live", "count.live", "basal_area.live")
join.type.second.live.other <- left_join(plots.second.both, type.second.live.other, by = c('PLOT', 'drought'))
join.type.second.live.other$Tree_Status <- 'Live'

#Add the total live tree data by plot
type.second.live.total <- subset(trees.second.type, Tree_Status == 'Live')
colnames(type.second.live.other) <- c("tree_type", "PLOT", "Tree_Status", "drought", "biomass.live", "count.live", "basal_area.live")
join.type.second.live.other <- left_join(plots.second.both, type.second.live.other, by = c('PLOT', 'drought'))
join.type.second.live.other$Tree_Status <- 'Live'


type.second.live <- rbind(join.type.second.live.pine, join.type.second.live.other)
type.second.live <- dplyr::select(type.second.live, -rmv)

type.second.dead.pine <- subset(trees.second.type, Tree_Status == 'Dead' & tree_type == 'pine/fir')
colnames(type.second.dead.pine) <- c("tree_type", "PLOT", "Tree_Status", "drought", "biomass.dead", "count.dead", "basal_area.dead")
join.type.second.dead.pine <- left_join(plots.second.both, type.second.dead.pine, by = c('PLOT', 'drought'))
join.type.second.dead.pine$Tree_Status <- 'Dead'

type.second.dead.other <- subset(trees.second.type, Tree_Status == 'Dead' & tree_type == 'other tree')
colnames(type.second.dead.other) <- c("tree_type", "PLOT", "Tree_Status", "drought", "biomass.dead", "count.dead", "basal_area.dead")
join.type.second.dead.other <- left_join(plots.second.both, type.second.dead.other, by = c('PLOT', 'drought'))
join.type.second.dead.other$Tree_Status <- 'Dead'

type.second.dead <- rbind(join.type.second.dead.pine, join.type.second.dead.other)
type.second.dead <- dplyr::select(type.second.dead, -rmv)

#Add a row for each plot of with the total live and dead biomass

type.second.join <- left_join(type.second.sum, type.second.live, by = c('PLOT', 'drought', 'tree_type'))
type.second.all <- left_join(type.second.join, type.second.dead, by = c('PLOT', 'drought', 'tree_type'))
head(type.second.all)

#Filling in live plot variables
type.second.all$count.live[is.na(type.second.all$count.live)] <- 0
type.second.all$biomass.live[is.na(type.second.all$biomass.live)] <- 0
type.second.all$basal_area.live[is.na(type.second.all$basal_area.live)] <- 0

#Filling in dead plot variables
type.second.all$count.dead[is.na(type.second.all$count.dead)] <- 0
type.second.all$biomass.dead[is.na(type.second.all$biomass.dead)] <- 0
type.second.all$basal_area.dead[is.na(type.second.all$basal_area.dead)] <- 0
type.second.all$basal_area.mort <- type.second.all$basal_area.dead / type.second.all$basal_area * 100
type.second.all <- as.data.frame(type.second.all)

head(type.second.all)

#Summaries by tree types across plots
type.second.summary <- type.second.all %>% group_by(tree_type, drought) %>% summarize(
																			  biomass.mean = mean(biomass),
																			  biomass.sd = sd(biomass),
																			  biomass.se = sd(biomass) / sqrt(length(biomass)),
																			  count.mean = mean(count),
																			  count.sd = sd(count),
																			  count.se = sd(count) / sqrt(length(count)),
																			  basal_area.mean = mean(basal_area),
																			  basal_area.sd = sd(basal_area),
																			  basal_area.se = sd(basal_area) / sqrt(length(basal_area.dead)),
																			  basal_area.dead.mean = mean(basal_area.dead),
																			  basal_area.dead.sd = sd(basal_area.dead),
																			  basal_area.dead.se = sd(basal_area.dead) / sqrt(length(basal_area.dead)),
																			  mortality.mean = mean(basal_area.mort),
																			  mortality.sd = sd(basal_area.mort),
																			  mortality.se = sd(basal_area.mort) / sqrt(length(basal_area.mort)),
																			  sample.size = length(count)
																			  )

#Make the summary by tree type a data frame
type.second.summary <- as.data.frame(type.second.summary)


#Add the drought exposure sequence to the script.
type.summary$sequence <- 'Both Droughts'
type.second.summary$sequence <- '2012-2015 Only'
type.summary.all <- rbind(type.summary, type.second.summary)

# Combining the type data for the two drought sequence regions
type.all$sequence <- 'Both Droughts'
type.second.all$sequence <- '2012-2015 Only'
type.both.all <- rbind(type.all, type.second.all)

#Make tree_type a factor
type.both.all$tree_type <- factor(type.both.all$tree_type, levels = c("pine/fir", "other tree"))

#Bar chart with FIA Mortality (%) for the two drought sequences and time periods
#Letters to indicate significant differences
p1_texta <- data.frame(label = c("a", "b", "b","b", "b", "b", "a", "b"),
                      sequence   = c('Both Droughts', 'Both Droughts', 'Both Droughts', 'Both Droughts', 
                                     '2012-2015 Only', '2012-2015 Only', '2012-2015 Only', '2012-2015 Only'),
                      # tree_type = c('pine/fir', 'other tree', 'pine/fir', 'other tree', 
                      #               'pine/fir', 'other tree', 'pine/fir', 'other tree'),
                      y     = c(4.05, 1.2, 1.85, 0.85, 0.4, 0.35, 3.55,1.4),
                      x     = c(0.75, 1.25, 1.75, 2.25, 0.75, 1.25, 1.75, 2.25)
)

#Letters to indicate sample sizes
p1_textb <- data.frame(label = c("n = 44", "n = 26", "n = 199", "n = 192"),
                        sequence   = c('Both Droughts', 'Both Droughts', '2012-2015 Only', '2012-2015 Only'),
                        y     = c(34.5, 11, 3, 17.5),
                        x     = c(1, 2, 1, 2)
)

#Create the mortality (%) bar chart
p1 <- ggbarplot(filter(type.both.all), x = "drought", y = "basal_area.dead", fill = "tree_type", color = 'sequence', 
           ylab = 'Mortality (%)', xlab = "Time Period", order = c("1999-2002", "2012-2015"), position = position_dodge(), #stat = "density",
           add = "mean_se" , error.plot = "errorbar", alpha = 0.8) + 
       guides(color = "none", fill = guide_legend('Tree \nType')) + theme_bw() +
       scale_color_manual(values = c("black", "black"), name = 'Drought \nSequence', 
                         # labels = c("pine/fir" = "Needleleaf \nConifer", "other tree" = 'Other \nTrees'),
                         labels = c("Both Droughts" =  "Both \nDroughts", "2012-2015 Only" = "2012-2015 \nOnly"),
                         aesthetics = "color") +
       theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
             legend.position = c(0.75, 0.45), legend.text = element_text(size = 6), legend.title = element_text(size = 8),
             legend.direction = "vertical", axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10),
             axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10)) +
       geom_text(data = p1_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
       # geom_text(data = p1_textb, mapping = aes(x = x, y = y, label = label), size = 3) +
       geom_text(data = data.frame(label = "Mean \n+/- SE", y = 3.5, x = 1, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2) + 
       facet_grid(~ factor(sequence, levels = c('Both Droughts', '2012-2015 Only')))
p1
#Save figure as a .png file
ggsave(filename = 'Fig4_needleleaf_conifer_mortality_barplot.png', height=7, width=12, units = 'cm', dpi=900)

#Basal area total bar plot
#Add letters to indicate significant differences
p2_text <- data.frame(label = c("a", "b", "b","b", "b", "b", "b", "a"),
                       sequence   = c('Both Droughts', 'Both Droughts', '2012-2015 Only', '2012-2015 Only'),
                       y     = c(19.5, 16.5, 31.5, 32.5),
                       x     = c(1, 2, 1, 2)
)

#Add sample size text labels
p2_number <- data.frame(label = c("n = 44", "n = 26", "n = 199", "n = 192"),
                         sequence   = c('Both Droughts', 'Both Droughts', '2012-2015 Only', '2012-2015 Only'),
                         y     = c(16.7, 13.5, 28.7, 29.5),
                         x     = c(1, 2, 1, 2)
)

#Create the basal area bar chart figure
p2 <-   ggbarplot(filter(type.both.all, tree_type == "pine/fir"), x = "drought", y = "basal_area", fill = "sequence",
           ylab = expression('Basal Area (m'^2*' ha'^-1*')'), xlab = "Time Period", order = c("1999-2002", "2012-2015"),
           add = "mean_se", error.plot = "errorbar", alpha = 0.8, position = position_dodge()) +
  guides(color = "none") + theme_bw() +
  scale_fill_manual(values = c("#E66100", "#5D3A9B"), name = 'Drought \nSequence',
                    labels = c("Both Droughts" =  "Both \nDroughts", "2012-2015 Only" = "2012-2015 \nOnly"),
                    aesthetics = "fill") +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0), legend.position = c(0.48, 0.55),
        legend.text = element_text(size = 6), legend.title = element_text(size = 8), legend.direction = "vertical", 
        axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10)) +
  geom_text(data = p2_text, mapping = aes(x = x, y = y, label = label), size = 5) +
  geom_text(data = p2_number, mapping = aes(x = x, y = y, label = label), size = 3) + 
  geom_text(data = data.frame(label = "Mean +/- SE", y = 14.5, x = 1.28, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2) +
  facet_grid(~ factor(sequence, levels = c('Both Droughts', '2012-2015 Only')))
p2
#Save the figure as a .png file
ggsave(filename = 'SFig6_needleleaf_conifer_basal_area_boxplot.png', height=7, width=14, units = 'cm', dpi=900)
# DIA.both.all
#Create a DIA mortality bar chart
# p3 <- ggbarplot(filter(DIA.both.all), #, (sequence == 'Both Droughts' & drought == '1999-2002') | (sequence == '2012-2015 Only' & drought == '2012-2015')), 
#                 x = "drought", y = "basal_area.mort", color = "sequence", fill = 'DIA.group', 
#                 ylab = 'Mortality (%)', xlab = "Time Period", order = c('1999-2002', '2012-2015'), position = position_dodge(), #stat = "density",
#                 add = "mean_se" , error.plot = "errorbar", alpha = 0.8) + 
#   theme_bw() + guides(color = 'none', fill = guide_legend("Diameter \nGroup")) +
#   scale_color_manual(values = c("black", "black"), name = 'Drought \nSequence',
#                     #labels = c("pine/fir" = "Needleleaf \nConifer", "other tree" = 'Other \nTrees'),
#                     labels = c("Both Droughts" =  "Both \nDroughts", "2012-2015 Only" = "2012-2015 \nOnly"),
#                     aesthetics = "color") +
#   theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
#         legend.position = c(0.72, 0.28), legend.text = element_text(size = 6), legend.title = element_text(size = 8),
#         legend.direction = "vertical", axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10),
#         axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10)) +
#   # geom_text(data = p1_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
#   # geom_text(data = p1_textb, mapping = aes(x = x, y = y, label = label), size = 3) +
#   geom_text(data = data.frame(label = "Mean \n+/- SE", y = 35, x = 1.5, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2) + 
#   facet_grid(~ factor(sequence, levels = c('Both Droughts', '2012-2015 Only')))
# p3
# 
# #Save the figure as a .png file
# ggsave(filename = 'SFig7_mortality_by_DBH_boxplot.png', height=7, width=14, units = 'cm', dpi=900)
# 
# p4 <- ggbarplot(filter(type.both.all), 
#                 x = "drought", y = "basal_area", fill = "tree_type",color = "sequence",   
#                 ylab = 'Basal Area', xlab = "Time Period", order = c("1999-2002", "2012-2015"), position = position_dodge(), #stat = "density",
#                 add = "mean_se" , error.plot = "errorbar", alpha = 0.8) + 
#   theme_bw() + guides(color = 'none', fill = guide_legend("Tree \nType")) +
#   scale_color_manual(values = c("black", "black"), 
#                      name = 'Drought \nSequence',
#                     #labels = c("pine/fir" = "Needleleaf \nConifer", "other tree" = 'Other \nTrees'),
#                     labels = c("Both Droughts" =  "Both \nDroughts", "2012-2015 Only" = "2012-2015 \nOnly"),
#                     aesthetics = "color") +
#   theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
#         legend.position = c(0.72, 0.28), legend.text = element_text(size = 6), legend.title = element_text(size = 8),
#         legend.direction = "vertical", axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10),
#         axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10)) +
#   # geom_text(data = p1_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
#   # geom_text(data = p1_textb, mapping = aes(x = x, y = y, label = label), size = 3) +
#   geom_text(data = data.frame(label = "Mean \n+/- SE", y = 35, x = 1.5, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2) + 
#   facet_grid(~ factor(sequence, levels = c('Both Droughts', '2012-2015 Only')))
# p4

# DIA.summary.all
#Doing the normal analysis
type.aov.dead <- aov(data = filter(type.both.all), basal_area.dead ~ drought * sequence * tree_type)
summary(type.aov.dead)

#Tukey Post Hoc analysis of dead basal area by tree species type
type.dead.tHSD <- TukeyHSD(type.aov.dead) 
type.dead.tHSD

#ANOVA of basal area by tree species type 
type.aov.basal.dead <- aov(data = filter(type.both.all, tree_type == "pine/fir"), basal_area.dead ~ drought*sequence)

#Tukey Post Hoc analysis of basal area by tree species type
type.basal.dead.tHSD <- TukeyHSD(type.aov.basal.dead) 

#ANOVA of basal area by tree species type 
type.aov.basal.2 <- aov(data = filter(type.both.all, tree_type == 'pine/fir'), basal_area ~ drought*sequence)

#Tukey Post Hoc analysis of basal area by tree species type
type.basal.tHSD.2 <- TukeyHSD(type.aov.basal.2) 
type.basla.tHSD.2

#ANOVA analysis of DIA for Pine/Fir
DIA.aov.dead <- aov(data = filter(DIA.both.all),  #, (sequence == 'Both Droughts' & drought == '1999-2002') | (sequence == '2012-2015 Only' & drought == '2012-2015')), 
                    basal_area.mort ~ DIA.group * sequence * drought)
summary(DIA.aov.dead)

#Tukey Post Hoc analysis of dead basal area by DIA
DIA.dead.tHSD <- TukeyHSD(DIA.aov.dead)
DIA.dead.tHSD

#ANOVA analysis of tree type basal area for Pine/Fir
type.all.aov <- aov(data = filter(type.both.all, sequence == 'Both Droughts'),  #, (sequence == 'Both Droughts' & drought == '1999-2002') | (sequence == '2012-2015 Only' & drought == '2012-2015')), 
                    basal_area ~ tree_type * drought)
summary(type.all.aov)

#Tukey Post Hoc analysis of dead basal area by DIA
type.all.tHSD <- TukeyHSD(type.all.aov)
type.all.tHSD


#Combine Tukey HSD values
fia.tHSD <- list(type.dead.tHSD, #type.basal.dead.tHSD, 
                 type.basal.tHSD.2)

#Create a data frame
df.fia.tHSD <- as.data.frame(map_df(fia.tHSD, tidy))

#Add a column with variable labels.
df.fia.tHSD$variable <- c('Mortality (%)', 'Mortality (%)', 'Mortality (%)', 'Mortality (%)', 'Mortality (%)', 'Mortality (%)', 'Mortality (%)', 'Mortality (%)',
                      #'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m^2/ha)', 'Mortality (m^2/ha)', 'Mortality (m^2/ha)', 'Mortality (m^2/ha)', 'Mortality (m^2/ha)', 'Mortality (m^2/ha)', 'Mortality (m^2/ha)',
                      'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)')

#Add Estimate 1 for Tukey HSD test
df.fia.tHSD$estimate.1 <- c(#Mortality
  mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '2012-2015'))$basal_area.mort), mean((type.both.all %>% filter(tree_type == 'pine/fir' & sequence == 'Both Droughts'))$basal_area.mort),
  mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '2012-2015' & sequence == '2012-2015 Only'))$basal_area.mort), mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '1999-2002' & sequence == 'Both Droughts'))$basal_area.mort),
  mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '2012-2015' & sequence == 'Both Droughts'))$basal_area.mort), mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '1999-2002' & sequence == 'Both Droughts'))$basal_area.mort),
  mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '2012-2015' & sequence == 'Both Droughts'))$basal_area.mort), mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '2012-2015' & sequence == 'Both Droughts'))$basal_area.mort),
  #Basal Area
  mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '2012-2015'))$basal_area), mean((type.both.all %>% filter(tree_type == 'pine/fir' & sequence == 'Both Droughts'))$basal_area),
  mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '2012-2015' & sequence == '2012-2015 Only'))$basal_area), mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '1999-2002' & sequence == 'Both Droughts'))$basal_area),
  mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '2012-2015' & sequence == 'Both Droughts'))$basal_area), mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '1999-2002' & sequence == 'Both Droughts'))$basal_area),
  mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '2012-2015' & sequence == 'Both Droughts'))$basal_area), mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '2012-2015' & sequence == 'Both Droughts'))$basal_area)
  )

#Add Estimate 2 for Tukey HSD test
df.fia.tHSD$estimate.2 <- c(#Mortality
  mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '1999-2002'))$basal_area.mort), mean((type.both.all %>% filter(tree_type == 'pine/fir' & sequence == '2012-2015 Only'))$basal_area.mort),
  mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '1999-2002' & sequence == '2012-2015 Only'))$basal_area.mort), mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '1999-2002' & sequence == '2012-2015 Only'))$basal_area.mort),
  mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '1999-2002' & sequence == '2012-2015 Only'))$basal_area.mort), mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '2012-2015' & sequence == '2012-2015 Only'))$basal_area.mort),
  mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '2012-2015' & sequence == '2012-2015 Only'))$basal_area.mort), mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '1999-2002' & sequence == 'Both Droughts'))$basal_area.mort),
  #Basal Area
  mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '1999-2002'))$basal_area), mean((type.both.all %>% filter(tree_type == 'pine/fir' & sequence == '2012-2015 Only'))$basal_area),
  mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '1999-2002' & sequence == '2012-2015 Only'))$basal_area), mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '1999-2002' & sequence == '2012-2015 Only'))$basal_area),
  mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '1999-2002' & sequence == '2012-2015 Only'))$basal_area), mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '2012-2015' & sequence == '2012-2015 Only'))$basal_area),
  mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '2012-2015' & sequence == '2012-2015 Only'))$basal_area), mean((type.both.all %>% filter(tree_type == 'pine/fir' & drought == '1999-2002' & sequence == 'Both Droughts'))$basal_area)
  )

#Select variables and put them in order
df.fia.tHSD.label <- df.fia.tHSD %>% dplyr::select(variable, contrast, estimate.1, estimate.2, estimate, conf.low, conf.high, adj.p.value) #, method, alternative)

#Change the column names
colnames(df.fia.tHSD.label) <- c('Variable', 'Comparison', 'Estimate 1', 'Estimate 2', 'Difference', 'Low 95% CI', 'High 95% CI', 'p-value')

#Combined ANOVA and Tukey HSD table
tb1 <- kbl(df.fia.tHSD.label, format = 'html', caption = "Table S3: Field ANOVA and Tukey HSD Results", escape = F, digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb1, width = 10, file = "STable3_FIA_tHSD_test_results.png", zoom = 5.0)

#Calculate the precentage changes based on the Tukey HSD test
df.fia.tHSD$diff.pct <- df.fia.tHSD$estimate / df.fia.tHSD$estimate.1 * 100

df.fia.tHSD$low.pct <- df.fia.tHSD$conf.low / df.fia.tHSD$estimate.1 * 100

df.fia.tHSD$high.pct <- df.fia.tHSD$conf.high / df.fia.tHSD$estimate.1 * 100

#Select variables and put them in order
df.fia.tHSD.sup <- df.fia.tHSD %>% dplyr::select(variable, contrast, estimate.1, estimate.2, estimate, conf.low, conf.high, diff.pct, low.pct, high.pct, adj.p.value) #, method, alternative)

#Change the column names
colnames(df.fia.tHSD.sup) <- c('Variable', 'Comparison', 'Estimate 1', 'Estimate 2', 'Difference', 'Low 95% CI', 'High 95% CI', 'Difference (%)', 'Low (%)', 'High (%)', 'p-value')

#Combined ANOVA and Tukey HSD table. This is the same data table as Sup Table 3, but with percentage changes. It is not included with the manuscript.
tb2 <- kbl(df.fia.tHSD.sup, format = 'html', caption = "Table S7: Field ANOVA and Tukey HSD Results", escape = F, digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb2, width = 10, file = "STable7_FIA_tHSD_test_results.png", zoom = 5.0)

#Save summary statistics by tree species. This data table is not included with the manuscript
tb3 <- type.summary.all %>% dplyr::select("tree_type", "drought", "sequence", "sample.size", "biomass.mean", "biomass.se", "count.mean", "count.se", "basal_area.mean", "basal_area.se",        
                                   "basal_area.dead.mean", "basal_area.dead.se", "mortality.mean", "mortality.se") %>% 
       kbl(caption = "Table S9: Tree Species Summary Statistics") %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb3, width = 5, file = "STable8_Southern_California_mortality_summary_statistics_v1.png", zoom = 4.0) 
