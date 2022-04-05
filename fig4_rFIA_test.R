#Author: Carl Norlen
#Date Created: November 11, 2019
#Date Edited: March 31, 2022
#Purpose: Create bar graphs for manuscript FIA analysis, testing out a new way of calculating the bar charts

# Specify necessary packages
p <- c("RSQLite","dbplyr","ggplot2","dplyr","tidyr", "ggpubr", "RColorBrewer",  
	   'gt', 'gtsummary', 'webshot', 'kableExtra', 'broom', 'rFIA', 'sf')

# install.packages("rFIA")

# Load packages
lapply(p,require,character.only=TRUE)

#Set working directory
setwd('C:/Users/can02/mystuff/subsequent-drought')



#Add Data Sets
fiaCA <- 'D:\\Large_Files\\FIA\\SQLite_FIADB_CA\\2019_CSV' #Downloaedd from FIA DataMart
dir_usfs <- "D:\\Large_Files\\USFS\\data\\subsections"
# fiaCA <- file.path(sql_dir, 'FIADB_CA.db')
ca <- readFIA(fiaCA)
# ca <- getFIA(states = 'CA', dir = 'D:\\Large_Files\\FIA\\SQLite_FIADB_CA\\2019_CSV')
# summary(ca)

#Load the USFS Polygons
# usfs.regions <- st_read(file.path(dir_usfs, 'S_USA.EcomapSubsections.shp'))
# usfs.both <- subset(usfs.regions, MAP_UNIT_S == 'M262Bd' | MAP_UNIT_S == 'M262Be' | MAP_UNIT_S == 'M262Bg' | MAP_UNIT_S == 'M262Bh' | MAP_UNIT_S == 'M262Bf' | MAP_UNIT_S == 'M262Bo' | MAP_UNIT_S == 'M262Bi' 
# | MAP_UNIT_S == 'M262Bm' | MAP_UNIT_S == 'M262Bl' | MAP_UNIT_S == 'M262Bc' | MAP_UNIT_S == 'M262Bp' | MAP_UNIT_S == 'M261Es')			
# usfs.2015.only <- subset(usfs.regions, MAP_UNIT_S == 'M261Ep' | MAP_UNIT_S == 'M261Eq' | MAP_UNIT_S == 'M261Eu' | MAP_UNIT_S == 'M261Er' | MAP_UNIT_S == 'M261Eo' | MAP_UNIT_S == 'M262Bb' | MAP_UNIT_S == 'M262Ba')	
# 
# #Subset the FIA data by the perimeters of USFS polygons for the Both Droughts polygons
# ca.both <- clipFIA(ca, mask = usfs.both, mostRecent = FALSE)
# summary(ca.both)
# 
# #Subset the FIA data by the perimeters of USFS polygons for the 2012-2015 Only Polygons
# ca.2015.only <- clipFIA(ca, mask = usfs.2015.only, mostRecent = FALSE)
# summary(ca.2015.only)
# plotFIA(ca.both)
# plotFIA(ca.2015.only)
# 
# # ca.2015.only <- clipFIA(ca, mask = usfs.2015.only, mostRecent = FALSE)
# # tpa(ca.both)
# # tpa.ca.both
# # tpa.ca.2015.only <- tpa(ca.2015.only)
# # tpa.ca.2015.only
# # tpa.ca.spsc.both <- tpa(ca.both, byPlot = TRUE)
# # tpa.ca.spsc.both
# # mort.both <- growMort(ca.both, byPlot = TRUE)
# # mort.both\
# plotFIA(ca,
#         areaDomain = ECOSUBCD %in% c('M261Ep', 'M261Eq' ,'M261Eu' ,'M261Er' ,'M261Eo' ,'M262Bb' ,'M262Ba') &
#           DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)
# tpa.ca.2015.only <- tpa(ca, byPlot = TRUE,
#                     areaDomain = ECOSUBCD %in% c('M261Ep', 'M261Eq' ,'M261Eu' ,'M261Er' ,'M261Eo' ,'M262Bb' ,'M262Ba') &
#                     DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)
# tpa.ca.both <- tpa(ca, method = 'ANNUAL', treeType = 'all',
#                         areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
#                           DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))
# 
# tpa.spsc.ca.both <- tpa(ca, method = 'ANNUAL', bySpecies = TRUE,
#                    areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
#                      DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))
# 
# tpa.ca.both.mort <- tpa(ca, method = 'ANNUAL', areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
#                            DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70), treeType = "dead")
# tpa.ca.both.mort
# tpa.ca.both.plot.mort <- tpa(ca, byPlot = TRUE, bySpecies = 'TRUE', treeType = "dead", 
#                          areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
#                          DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))
# tpa.ca.both.plot.mort
# tpa.ca.both.plot.all <- tpa(ca, byPlot = TRUE, areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
#                                DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70), treeType = "all")
# annual.BAA.all <- tpa.ca.both.plot.all %>% filter(PROP_FOREST != 0) %>% group_by(YEAR) %>% summarize(BAA.mean = mean(BAA))
# annual.BAA.mort <- tpa.ca.both.plot.mort %>% group_by(MORTYR) %>% summarize(BAA.mean = mean(BAA), BAA.stdDev = sd(BAA), count = n())
# annual.BAA.mort
# 
# tpa.ca.both <- tpa(ca, method = 'ANNUAL', treeType = 'all', 
#                    areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
#                      DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))
# 
# #Create a notin operator
# `%notin%` <- Negate(`%in%`)
# 
# #Doing a combined estimated of the Basal Area
# tpa.ca.combined.both <- tpa(ca, method = 'LMA', treeType = 'all', treeDomain = MORTYR %notin% c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011") & DIA >= 5,
#                    areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
#                      DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))
# tpa.ca.combined.both
# 
# tpa.both.mort.2002 <- tpa(ca, byPlot = TRUE, treeType = 'dead', grpBy = MORTYR, treeDomain = DIA >= 5,
#                             areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
#                               DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))
# tpa.both.mort.2002
# tpa.both.mort.2015 <- tpa(ca, treeType = 'dead', byPlot = TRUE, bySpecies = TRUE, treeDomain = DIA >= 5,
#                           areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
#                             DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))
# tpa.both.mort.2015
# #A different way to calculate the standard error
# tpa.ca.both$BAA_SE <- sqrt(tpa.ca.both$BAA_VAR) / sqrt(tpa.ca.both$nPlots_TREE)
# tpa.ca.both
# 
# tpa.2015.only <- tpa(ca.2015.only)
# tpa.2015.only
# plotFIA(tpa.ca.2015.only %>% filter(PROP_FOREST != 0) %>% group_by(YEAR) %>% summarize(BAA.mean = mean(BAA)), y = BAA.mean, plot.title = 'BAA, 2012-2015 only')
# plotFIA(tpa.ca.both, y = BAA, grp = 'STATUSCD', plot.title = 'BAA, Both Droughts')
# plotFIA(tpa.both.mort.2002 %>% filter(PROP_FOREST != 0) %>% group_by(MORTYR) %>% summarize(BAA.mean = mean(BAA)), x = MORTYR, y = BAA.mean, plot.title = 'BAA, Both Droughts')
# 
# #Do a mortality ggplot2 for the total basal area and the mortality by 
# ggplot() +
# geom_line(data = tpa.both.mort.2002 %>% filter(PROP_FOREST != 0) %>% group_by(MORTYR) %>% summarize(BAA.mean = mean(BAA)), mapping = aes(x = MORTYR, y = BAA.mean)) + 
#   geom_line(data = tpa.ca.both.plot.all %>% filter(PROP_FOREST != 0) %>% group_by(YEAR) %>% summarize(BAA.mean = mean(BAA)), mapping = aes(x = YEAR, y = BAA.mean))
# #Create a figure of Basal Area in FIA plots for the Both Droughts regime
# tpa_plot.both <- tpa(ca, returnSpatial = TRUE, polys = usfs.both, byPlot = TRUE, 
#                        areaDomain = DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)
# # plot(tpa_polys.both, max.plot = 21)
# plotFIA(tpa_plot.both, y = BAA)
# 
# #Create a figure of Basal area in FIA plots for the 2012-2015 Only drought region
# tpa_plot.2015.only <- tpa(ca, returnSpatial = TRUE, polys = usfs.2015.only, byPlot = TRUE, 
#                       areaDomain = DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)
# # plot(tpa_polys.2015.only, max.plot = 21)
# plotFIA(tpa_plot.2015.only, y = BAA)
# 
# #Create Figure of Basal Area in the USFS eco regions for both droughts
# tpa_polys.both <- tpa(ca, returnSpatial = TRUE, polys = usfs.both,  
#                      areaDomain = DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)
# # plot(tpa_polys.both, max.plot = 21)
# plotFIA(tpa_polys.both, y = BAA)
# 
# #Create figure of Basal Area in the USFS eco regions for 2012-2015 Only
# tpa_polys.2015.only <- tpa(ca, returnSpatial = TRUE, polys = usfs.2015.only,
#                           areaDomain = DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)
# # plot(tpa_polys.2015.only, max.plot = 21)
# plotFIA(tpa_polys.2015.only, y = BAA)
# 
# # ca$TREE
# 
# #Try to calculate mortality for both droughts by species group
# tpa.ca.both.plot.all <- tpa(ca, byPlot = TRUE, areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
#                               DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70), treeType = "all")
# annual.BAA.all <- tpa.ca.both.plot.all %>% filter(PROP_FOREST != 0) %>% group_by(YEAR) %>% summarize(BAA.mean = mean(BAA))
# annual.BAA.mort <- tpa.ca.both.plot.mort %>% group_by(MORTYR) %>% summarize(BAA.mean = mean(BAA), BAA.stdDev = sd(BAA), count = n())
# annual.BAA.mort
# 
# tpa.ca.both <- tpa(ca, method = 'ANNUAL', treeType = 'all', 
#                    areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
#                      DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))

#Creating a plot of mortality by tree species in two time periods and drought sequences.
#Create a notin operator
`%notin%` <- Negate(`%in%`)

##Both Droughts regions estimates
#Doing a combined estimated of the Basal Area for Both Droughts during 1999-2002
#Total basal area and tpa estimates by species
tpa.both.all.2002 <- tpa(ca, byPlot = TRUE, treeType = 'all', bySpecies = TRUE, treeDomain = INVYR %in% c("2002", "2003", "2004", "2005", "2006") & DIA >= 5,
                         areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                           DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))
# tpa.both.all.2002

#Dead basal area 
tpa.both.mort.2002 <- tpa(ca, byPlot = TRUE, treeType = 'dead', bySpecies = TRUE, treeDomain = DIA >= 5 & INVYR %in% c("2002", "2003", "2004", "2005", "2006"),
                          areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                            DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))

#Combine multiple inventory years into one plot estimate
mort.both.2002 <- tpa.both.mort.2002 %>% group_by(pltID, YEAR, COMMON_NAME) %>% summarize(BAA.dead = sum(BAA))
all.both.2002 <- tpa.both.all.2002 %>% group_by(pltID, YEAR, COMMON_NAME) %>% summarize(BAA.all = sum(BAA))
join.both.2002 <- left_join(all.both.2002, mort.both.2002, by = c('pltID', 'YEAR','COMMON_NAME'))
# join.both.2002
# join.both.2002[is.na(join.both.2002$BAA.dead), ] <- as.double(0)
# is.double(as.double(0))

#Replace the NAs with 0s
join.both.2002 <- join.both.2002 %>% mutate(BAA.dead = replace(BAA.dead, is.na(BAA.dead), 0))
# join.both.2002 %>% select('pltID') %>% unique()
#Calculate the mortality percentage
join.both.2002$BAA.mort <- join.both.2002$BAA.dead / join.both.2002$BAA.all * 100

#Do the total estimates by species Common Names
mort.sp.both.2002 <- join.both.2002 %>% group_by(COMMON_NAME) %>% summarize (BAA.all.mean = mean(BAA.all), BAA.dead.mean = mean(BAA.dead), BAA.mort.mean = mean(BAA.mort), BAA.mort.sd = sd(BAA.mort), count = n())

#Get a list of species for filtering purposes
sp.both.2002 <- mort.sp.both.2002 %>% filter(count >= 5) %>% pull(COMMON_NAME)
print(sp.both.2002)

#Doing a combined estimated of the Basal Area for Both Droughts during 2012-2015
#Total basal area and tpa estimates by species
tpa.both.all.2015 <- tpa(ca, byPlot = TRUE, treeType = 'all', bySpecies = TRUE, treeDomain = INVYR %in% c("2015", "2016", "2017", "2018", "2019") & 
                           DIA >= 5,
                         areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                           DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))
# tpa.both.all.2002

#Dead basal area 
tpa.both.mort.2015 <- tpa(ca, byPlot = TRUE, treeType = 'dead', bySpecies = TRUE, treeDomain = INVYR %in% c("2015", "2016", "2017", "2018", "2019") & 
                            DIA >= 5 & MORTYR %notin% c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012"),
                          areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                            DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))

#Combine multiple inventory years into one plot estimate
mort.both.2015 <- tpa.both.mort.2015 %>% group_by(pltID, YEAR, COMMON_NAME) %>% summarize(BAA.dead = sum(BAA))
all.both.2015 <- tpa.both.all.2015 %>% group_by(pltID, YEAR, COMMON_NAME) %>% summarize(BAA.all = sum(BAA))
join.both.2015 <- left_join(all.both.2015, mort.both.2015, by = c('pltID', 'YEAR','COMMON_NAME'))

#Replace the NAs with 0s
join.both.2015 <- join.both.2015 %>% mutate(BAA.dead = replace(BAA.dead, is.na(BAA.dead), 0))

#Calculate the mortality percentage
join.both.2015$BAA.mort <- join.both.2015$BAA.dead / join.both.2015$BAA.all * 100

#Do the total estimates by species Common Names
mort.sp.both.2015 <- join.both.2015 %>% group_by(COMMON_NAME) %>% summarize (BAA.all.mean = mean(BAA.all), BAA.dead.mean = mean(BAA.dead), BAA.mort.mean = mean(BAA.mort), BAA.mort.sd = sd(BAA.mort), count = n())

#Get a list of species for filtering purposes
sp.both.2015 <- mort.sp.both.2015 %>% filter(count >= 5) %>% pull(COMMON_NAME)
print(sp.both.2015)

#Combine the 1999-2002 and 2012-2015 plots values
join.both.2002$time.period <- '1999-2002'
join.both.2015$time.period <- '2012-2015'
all.both <- rbind(join.both.2002, join.both.2015)

# all.both <- all.both %>% factor('COMMON_NAME', levels = sp.levels)

##2012-2015 Only regions estimates
#Doing a combined estimated of the Basal Area for Both Droughts during 1999-2002
#Total basal area and tpa estimates by species
tpa.second.all.2002 <- tpa(ca, byPlot = TRUE, treeType = 'all', bySpecies = TRUE, treeDomain = INVYR %in% c("2002", "2003", "2004", "2005", "2006") & DIA >= 5,
                         areaDomain = ECOSUBCD %in% c('M261Ep', 'M261Eq' ,'M261Eu' ,'M261Er' ,'M261Eo' ,'M262Bb' ,'M262Ba') &
                           DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))

# summary(ca$TREE)

#Dead basal area 
tpa.second.mort.2002 <- tpa(ca, byPlot = TRUE, treeType = 'dead', bySpecies = TRUE, treeDomain = DIA >= 5 & MORTYR != '' & INVYR %in% c("2002", "2003", "2004", "2005", "2006"),
                          areaDomain = ECOSUBCD %in% c('M261Ep', 'M261Eq' ,'M261Eu' ,'M261Er' ,'M261Eo' ,'M262Bb' ,'M262Ba') & 
                            DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))

#Combine multiple inventory years into one plot estimate
mort.second.2002 <- tpa.second.mort.2002 %>% group_by(pltID, YEAR, COMMON_NAME) %>% summarize(BAA.dead = sum(BAA))
all.second.2002 <- tpa.second.all.2002 %>% group_by(pltID, YEAR, COMMON_NAME) %>% summarize(BAA.all = sum(BAA))
join.second.2002 <- left_join(all.second.2002, mort.second.2002, by = c('pltID', 'YEAR','COMMON_NAME'))

#Replace the NAs with 0s
join.second.2002 <- join.second.2002 %>% mutate(BAA.dead = replace(BAA.dead, is.na(BAA.dead), 0))
# join.second.2002 %>% select('pltID') %>% unique()
#Calculate the mortality percentage
join.second.2002$BAA.mort <- join.second.2002$BAA.dead / join.second.2002$BAA.all * 100

#Do the total estimates by species Common Names
mort.sp.second.2002 <- join.second.2002 %>% group_by(COMMON_NAME) %>% summarize (BAA.all.mean = mean(BAA.all), BAA.dead.mean = mean(BAA.dead), BAA.mort.mean = mean(BAA.mort), BAA.mort.sd = sd(BAA.mort), count = n())

#Get a list of species for filtering purposes
sp.second.2002 <- mort.sp.second.2002 %>% filter(count >= 5 & COMMON_NAME != 'curlleaf mountain-mahogany') %>% pull(COMMON_NAME)
print(sp.second.2002)

#Doing a combined estimated of the Basal Area for Both Droughts during 2012-2015
#Total basal area and tpa estimates by species
tpa.second.all.2015 <- tpa(ca, byPlot = TRUE, treeType = 'all', bySpecies = TRUE, treeDomain = INVYR %in% c("2015", "2016", "2017", "2018", "2019") & 
                           DIA >= 5,
                         areaDomain = ECOSUBCD %in% c('M261Ep', 'M261Eq' ,'M261Eu' ,'M261Er' ,'M261Eo' ,'M262Bb' ,'M262Ba') &
                           DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))
# tpa.second.all.2002

#Dead basal area 
tpa.second.mort.2015 <- tpa(ca, byPlot = TRUE, treeType = 'dead', bySpecies = TRUE, treeDomain = INVYR %in% c("2015", "2016", "2017", "2018", "2019") & 
                            DIA >= 5 & MORTYR %notin% c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012"),
                          areaDomain = ECOSUBCD %in% c('M261Ep', 'M261Eq' ,'M261Eu' ,'M261Er' ,'M261Eo' ,'M262Bb' ,'M262Ba') &
                            DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))

#Combine multiple inventory years into one plot estimate
mort.second.2015 <- tpa.second.mort.2015 %>% group_by(pltID, YEAR, COMMON_NAME) %>% summarize(BAA.dead = sum(BAA))
all.second.2015 <- tpa.second.all.2015 %>% group_by(pltID, YEAR, COMMON_NAME) %>% summarize(BAA.all = sum(BAA))
join.second.2015 <- left_join(all.second.2015, mort.second.2015, by = c('pltID', 'YEAR','COMMON_NAME'))

#Replace the NAs with 0s
join.second.2015 <- join.second.2015 %>% mutate(BAA.dead = replace(BAA.dead, is.na(BAA.dead), 0))

#Calculate the mortality percentage
join.second.2015$BAA.mort <- join.second.2015$BAA.dead / join.second.2015$BAA.all * 100

#Do the total estimates by species Common Names
mort.sp.second.2015 <- join.second.2015 %>% group_by(COMMON_NAME) %>% summarize (BAA.all.mean = mean(BAA.all), BAA.dead.mean = mean(BAA.dead), BAA.mort.mean = mean(BAA.mort), BAA.mort.sd = sd(BAA.mort), count = n())

#Get a list of species for filtering purposes
sp.second.2015 <- mort.sp.second.2015 %>% filter(count >= 5) %>% pull(COMMON_NAME)
print(sp.second.2015)

#Combine the 1999-2002 and 2012-2015 plots values
join.second.2002$time.period <- '1999-2002'
join.second.2015$time.period <- '2012-2015'
all.second <- rbind(join.second.2002, join.second.2015)

#Combine all the all the data together for drought regions
all.both$sequence <- 'Both Droughts'
all.second$sequence <- '2012-2015 Only'
all.forest <- rbind(all.both, all.second)

#Make a list to order the COMMON_NAME column as a fator
sp.levels <- c("ponderosa pine", "Coulter pine", "Jeffrey pine", "sugar pine", "singleleaf pinyon", "white fir", "bigcone Douglas-fir", 
               "incense-cedar","California juniper","canyon live oak", "California black oak", "interior live oak")

#Convert basal area from sq ft / acre to sq meter / hectare
all.forest$BAA.dead <- all.forest$BAA.dead * (1/4.356)
sp.both.2002[! sp.both.2002 %in% c('curlleaf mountain-mahogany', 'California live oak')]
p1 <- ggbarplot(all.forest %>% filter(COMMON_NAME %in% sp.both.2002[! sp.both.2002 %in% c('curlleaf mountain-mahogany', 'California live oak')]), #, (sequence == 'Both Droughts' & drought == '1999-2002') | (sequence == '2012-2015 Only' & drought == '2012-2015')), 
                x = "time.period", y = "BAA.dead", position = position_dodge(), fill = 'COMMON_NAME', color = "sequence",
                add = "mean_se" , error.plot = "errorbar", alpha = 0.8) + 
  theme_bw() + guides(color = 'none', fill = guide_legend(title = "Tree \nSpecies", ncol = 2)) +
  scale_color_manual(values = c("black", "black"),
                     aesthetics = "color") +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = c(0.76, 0.15), legend.text = element_text(size = 3), legend.title = element_text(size = 5),
        legend.direction = "vertical", axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10)) +
  # geom_text(data = p1_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
  # geom_text(data = p1_textb, mapping = aes(x = x, y = y, label = label), size = 3) +
  geom_text(data = data.frame(label = "Mean \n+/- SE", y = 4, x = 2.25, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2) + 
  facet_grid(~ factor(sequence, levels = c('Both Droughts', '2012-2015 Only')))
p1

ggsave(filename = 'SFig10_species_mortality_barplot.png', height=7, width=16, units = 'cm', dpi=900)

# DIA.summary.all
#Do an ANOVA and Tukey HSD by tree species
sp.aov.dead <- aov(data = all.forest %>% filter(COMMON_NAME %in% sp.both.2002[sp.both.2002 %in% c('ponderosa pine', 'Jeffrey pine', 'singleleaf pinyon')]), BAA.mort ~ time.period * sequence * COMMON_NAME)
summary(sp.aov.dead)

#Tukey Post Hoc analysis of dead basal area by tree species
sp.dead.tHSD <- TukeyHSD(sp.aov.dead) 
sp.dead.tHSD

#Testing out a separation by both Species and DIA group
tpa.dia.both.all.2002 <- tpa(ca, treeType = 'all', byPlot = TRUE, bySpecies = TRUE, bySizeClass = TRUE, treeDomain = INVYR %in% c("2002", "2003", "2004", "2005", "2006") & DIA >= 5,
                         areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                           DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))
tpa.dia.both.all.2002 
plotFIA(tpa.dia.both.all.2002 %>% filter(COMMON_NAME %in% c('ponderosa pine', 'Coulter pine', 'singleleaf pinyon', 'Jeffrey Pine')), x = sizeClass, y = TPA, grp = COMMON_NAME)
# tpa.both.all.2002
tpa.dia.both.all.2002
#Dead basal area 
tpa.dia.both.mort.2002 <- tpa(ca, treeType = 'dead', byPlot = TRUE, bySpecies = TRUE, bySizeClass = TRUE, treeDomain = DIA >= 5 & INVYR %in% c("2002", "2003", "2004", "2005", "2006"),
                          areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                            DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))
plotFIA(tpa.dia.both.mort.2002 %>% filter(COMMON_NAME %in% c('ponderosa pine', 'Coulter pine', 'singleleaf pinyon', 'Jeffrey Pine')), x = sizeClass, y = TPA, grp = COMMON_NAME)

#Do Basal Area distrubutions by species and sizeClass
tpa.dia.both.all.2015 <- tpa(ca, byPlot = TRUE, treeType = 'all', bySpecies = TRUE, bySizeClass = TRUE, treeDomain = INVYR %in% c("2015", "2016", "2017", "2018", "2019") & 
                           DIA >= 5,
                         areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                           DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))
plotFIA(tpa.dia.both.all.2015 %>% filter(COMMON_NAME %in% c('ponderosa pine', 'Coulter pine', 'singleleaf pinyon', 'Jeffrey Pine')), x = sizeClass * 2.54, y = TPA, grp = COMMON_NAME)

#Dead basal area 
tpa.dia.both.mort.2015 <- tpa(ca, treeType = 'dead', byPlot = TRUE, bySpecies = TRUE, bySizeClass = TRUE, treeDomain = INVYR %in% c("2015", "2016", "2017", "2018", "2019") & 
                            DIA >= 5 & MORTYR %notin% c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012"),
                          areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                            DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))
plotFIA(tpa.dia.both.mort.2015 %>% filter(COMMON_NAME %in% c('ponderosa pine', 'Coulter pine', 'singleleaf pinyon', 'Jeffrey Pine')), x = sizeClass, y = TPA, grp = COMMON_NAME)

##Combine two different quieries together
tpa.dia.both.all.2002$time.period <- '1999-2002'
tpa.dia.both.all.2015$time.period <- '2012-2015'
tpa.dia.all <- rbind(tpa.dia.both.all.2002, tpa.dia.both.all.2015)
#Do plots of the total basal area by sizeClass
ggplot() + geom_line(data = tpa.dia.all %>% 
                       filter(COMMON_NAME %in% c('ponderosa pine', 'Coulter pine', 'singleleaf pinyon', 'Jeffrey pine', 'sugar pine', 'white fir', 'bigcone Douglas-fir')) %>% 
                       group_by (COMMON_NAME, sizeClass, time.period) %>% summarize(TPA.mean = mean(TPA), BAA.mean = mean(BAA)), mapping = aes(x = sizeClass * 2.54, y = BAA.mean, color = time.period)) + 
                       xlab('Size Class (cm)') +
                       facet_grid(. ~ COMMON_NAME)


##Combine two different quieries together for tree mortality
tpa.dia.both.mort.2002$time.period <- '1999-2002'
tpa.dia.both.mort.2015$time.period <- '2012-2015'
tpa.dia.mort <- rbind(tpa.dia.both.mort.2002, tpa.dia.both.mort.2015)
#Do plots of the total basal area by sizeClass
ggplot() + geom_line(data = tpa.dia.mort %>% 
                       filter(COMMON_NAME %in% c('ponderosa pine', 'Coulter pine', 'singleleaf pinyon', 'Jeffrey pine', 'sugar pine', 'white fir', 'bigcone Douglas-fir')) %>% 
                       group_by (COMMON_NAME, sizeClass, time.period) %>% summarize(TPA.mean = mean(TPA), BAA.mean = mean(BAA)), mapping = aes(x = sizeClass * 2.54, y = BAA.mean, color = time.period)) + 
  xlab('Size Class (cm)') +
  facet_grid(. ~ COMMON_NAME)
