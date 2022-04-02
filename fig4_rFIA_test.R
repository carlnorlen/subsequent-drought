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
fiaCA <- 'D:\\Large_Files\\FIA\\SQLite_FIADB_CA\\2019_version' #Downloaedd from FIA DataMart
dir_usfs <- "D:\\Large_Files\\USFS\\data\\subsections"
# fiaCA <- file.path(sql_dir, 'FIADB_CA.db')
db <- readFIA(fiaCA)
ca <- getFIA(states = 'CA', dir = 'D:\\Large_Files\\FIA\\SQLite_FIADB_CA\\2019_CSV')
summary(ca)

#Load the USFS Polygons
usfs.regions <- st_read(file.path(dir_usfs, 'S_USA.EcomapSubsections.shp'))
usfs.both <- subset(usfs.regions, MAP_UNIT_S == 'M262Bd' | MAP_UNIT_S == 'M262Be' | MAP_UNIT_S == 'M262Bg' | MAP_UNIT_S == 'M262Bh' | MAP_UNIT_S == 'M262Bf' | MAP_UNIT_S == 'M262Bo' | MAP_UNIT_S == 'M262Bi' 
| MAP_UNIT_S == 'M262Bm' | MAP_UNIT_S == 'M262Bl' | MAP_UNIT_S == 'M262Bc' | MAP_UNIT_S == 'M262Bp' | MAP_UNIT_S == 'M261Es')			
usfs.2015.only <- subset(usfs.regions, MAP_UNIT_S == 'M261Ep' | MAP_UNIT_S == 'M261Eq' | MAP_UNIT_S == 'M261Eu' | MAP_UNIT_S == 'M261Er' | MAP_UNIT_S == 'M261Eo' | MAP_UNIT_S == 'M262Bb' | MAP_UNIT_S == 'M262Ba')	

#Subset the FIA data by the perimeters of USFS polygons for the Both Droughts polygons
ca.both <- clipFIA(ca, mask = usfs.both, mostRecent = FALSE)
summary(ca.both)

#Subset the FIA data by the perimeters of USFS polygons for the 2012-2015 Only Polygons
ca.2015.only <- clipFIA(ca, mask = usfs.2015.only, mostRecent = FALSE)
summary(ca.2015.only)
plotFIA(ca.both)
plotFIA(ca.2015.only)

# ca.2015.only <- clipFIA(ca, mask = usfs.2015.only, mostRecent = FALSE)
# tpa(ca.both)
# tpa.ca.both
# tpa.ca.2015.only <- tpa(ca.2015.only)
# tpa.ca.2015.only
# tpa.ca.spsc.both <- tpa(ca.both, byPlot = TRUE)
# tpa.ca.spsc.both
# mort.both <- growMort(ca.both, byPlot = TRUE)
# mort.both\
plotFIA(ca,
        areaDomain = ECOSUBCD %in% c('M261Ep', 'M261Eq' ,'M261Eu' ,'M261Er' ,'M261Eo' ,'M262Bb' ,'M262Ba') &
          DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)
tpa.ca.2015.only <- tpa(ca, byPlot = TRUE,
                    areaDomain = ECOSUBCD %in% c('M261Ep', 'M261Eq' ,'M261Eu' ,'M261Er' ,'M261Eo' ,'M262Bb' ,'M262Ba') &
                    DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)
tpa.ca.both <- tpa(ca, method = 'ANNUAL', treeType = 'all',
                        areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                          DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))

tpa.spsc.ca.both <- tpa(ca, method = 'ANNUAL', bySpecies = TRUE,
                   areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                     DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))

tpa.ca.both.mort <- tpa(ca, method = 'ANNUAL', areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                           DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70), treeType = "dead")
tpa.ca.both.mort
tpa.ca.both.plot.mort <- tpa(ca, byPlot = TRUE, bySpecies = 'TRUE', treeType = "dead", 
                         areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                         DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))
tpa.ca.both.plot.mort
tpa.ca.both.plot.all <- tpa(ca, byPlot = TRUE, areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                               DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70), treeType = "all")
annual.BAA.all <- tpa.ca.both.plot.all %>% filter(PROP_FOREST != 0) %>% group_by(YEAR) %>% summarize(BAA.mean = mean(BAA))
annual.BAA.mort <- tpa.ca.both.plot.mort %>% group_by(MORTYR) %>% summarize(BAA.mean = mean(BAA), BAA.stdDev = sd(BAA), count = n())
annual.BAA.mort

tpa.ca.both <- tpa(ca, method = 'ANNUAL', treeType = 'all', 
                   areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                     DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))

#Create a notin operator
`%notin%` <- Negate(`%in%`)

#Doing a combined estimated of the Basal Area
tpa.ca.combined.both <- tpa(ca, method = 'LMA', treeType = 'all', treeDomain = MORTYR %notin% c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011") & DIA >= 5,
                   areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                     DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))
tpa.ca.combined.both

tpa.both.mort.2002 <- tpa(ca, byPlot = TRUE, treeType = 'dead', grpBy = MORTYR, treeDomain = DIA >= 5,
                            areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                              DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))
tpa.both.mort.2002
tpa.both.mort.2015 <- tpa(ca, treeType = 'dead', byPlot = TRUE, bySpecies = TRUE, treeDomain = DIA >= 5,
                          areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                            DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))
tpa.both.mort.2015
#A different way to calculate the standard error
tpa.ca.both$BAA_SE <- sqrt(tpa.ca.both$BAA_VAR) / sqrt(tpa.ca.both$nPlots_TREE)
tpa.ca.both

tpa.2015.only <- tpa(ca.2015.only)
tpa.2015.only
plotFIA(tpa.ca.2015.only %>% filter(PROP_FOREST != 0) %>% group_by(YEAR) %>% summarize(BAA.mean = mean(BAA)), y = BAA.mean, plot.title = 'BAA, 2012-2015 only')
plotFIA(tpa.ca.both, y = BAA, grp = 'STATUSCD', plot.title = 'BAA, Both Droughts')
plotFIA(tpa.both.mort.2002 %>% filter(PROP_FOREST != 0) %>% group_by(MORTYR) %>% summarize(BAA.mean = mean(BAA)), x = MORTYR, y = BAA.mean, plot.title = 'BAA, Both Droughts')

#Do a mortality ggplot2 for the total basal area and the mortality by 
ggplot() +
geom_line(data = tpa.both.mort.2002 %>% filter(PROP_FOREST != 0) %>% group_by(MORTYR) %>% summarize(BAA.mean = mean(BAA)), mapping = aes(x = MORTYR, y = BAA.mean)) + 
  geom_line(data = tpa.ca.both.plot.all %>% filter(PROP_FOREST != 0) %>% group_by(YEAR) %>% summarize(BAA.mean = mean(BAA)), mapping = aes(x = YEAR, y = BAA.mean))
#Create a figure of Basal Area in FIA plots for the Both Droughts regime
tpa_plot.both <- tpa(ca, returnSpatial = TRUE, polys = usfs.both, byPlot = TRUE, 
                       areaDomain = DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)
# plot(tpa_polys.both, max.plot = 21)
plotFIA(tpa_plot.both, y = BAA)

#Create a figure of Basal area in FIA plots for the 2012-2015 Only drought region
tpa_plot.2015.only <- tpa(ca, returnSpatial = TRUE, polys = usfs.2015.only, byPlot = TRUE, 
                      areaDomain = DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)
# plot(tpa_polys.2015.only, max.plot = 21)
plotFIA(tpa_plot.2015.only, y = BAA)

#Create Figure of Basal Area in the USFS eco regions for both droughts
tpa_polys.both <- tpa(ca, returnSpatial = TRUE, polys = usfs.both,  
                     areaDomain = DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)
# plot(tpa_polys.both, max.plot = 21)
plotFIA(tpa_polys.both, y = BAA)

#Create figure of Basal Area in the USFS eco regions for 2012-2015 Only
tpa_polys.2015.only <- tpa(ca, returnSpatial = TRUE, polys = usfs.2015.only,
                          areaDomain = DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)
# plot(tpa_polys.2015.only, max.plot = 21)
plotFIA(tpa_polys.2015.only, y = BAA)

# ca$TREE

#Try to calculate mortality for both droughts by species group
tpa.ca.both.plot.all <- tpa(ca, byPlot = TRUE, areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                              DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70), treeType = "all")
annual.BAA.all <- tpa.ca.both.plot.all %>% filter(PROP_FOREST != 0) %>% group_by(YEAR) %>% summarize(BAA.mean = mean(BAA))
annual.BAA.mort <- tpa.ca.both.plot.mort %>% group_by(MORTYR) %>% summarize(BAA.mean = mean(BAA), BAA.stdDev = sd(BAA), count = n())
annual.BAA.mort

tpa.ca.both <- tpa(ca, method = 'ANNUAL', treeType = 'all', 
                   areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                     DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))

#Create a notin operator
`%notin%` <- Negate(`%in%`)

#Doing a combined estimated of the Basal Area
tpa.both.all.2002 <- tpa(ca, byPlot = TRUE, treeType = 'all', bySpecies = TRUE, treeDomain = INVYR %in% c("2002", "2003", "2004", "2005", "2006") & DIA >= 5,
                            areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                              DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))
tpa.both.all.2002

tpa.both.mort.2002 <- tpa(ca, byPlot = TRUE, treeType = 'dead', bySpecies = TRUE, treeDomain = DIA >= 5 & INVYR %in% c("2002", "2003", "2004", "2005", "2006"),
                          areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                            DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70))
mort.both.2002 <- tpa.both.mort.2002 %>% group_by(pltID, YEAR, COMMON_NAME) %>% summarize(BAA.dead = sum(BAA))
all.both.2002 <- tpa.both.all.2002 %>% group_by(pltID, YEAR, COMMON_NAME) %>% summarize(BAA.all = sum(BAA))
join.both.2002 <- left_join(all.both.2002, mort.both.2002, by = c('pltID', 'YEAR','COMMON_NAME'))
join.both.2002
join.both.2002[is.na(join.both.2002$BAA.dead), ] <- as.double(0)
is.double(as.double(0))

#Replace the NAs with 0s
join.both.2002 <- join.both.2002 %>% mutate(BAA.dead = replace(BAA.dead, is.na(BAA.dead), 0))

#Calculate the mortality percentage
join.both.2002$BAA.mort <- join.both.2002$BAA.dead / join.both.2002$BAA.all * 100

mort.sp.2002 <- join.both.2002 %>% group_by(COMMON_NAME) %>% summarize (BAA.mort.mean = mean(BAA.mort), BAA.mort.sd = sd(BAA.mort), count = n())

p3 <- ggbarplot(join.both.2002, #, (sequence == 'Both Droughts' & drought == '1999-2002') | (sequence == '2012-2015 Only' & drought == '2012-2015')), 
                x = "COMMON_NAME", y = "BAA.mort", position = position_dodge(), #stat = "density",
                add = "mean_se" , error.plot = "errorbar", alpha = 0.8) + 
  theme_bw() + #guides(color = 'none', fill = guide_legend("Diameter \nGroup")) +
  # scale_color_manual(values = c("black", "black"), name = 'Drought \nSequence',
  #                    #labels = c("pine/fir" = "Needleleaf \nConifer", "other tree" = 'Other \nTrees'),
  #                    labels = c("Both Droughts" =  "Both \nDroughts", "2012-2015 Only" = "2012-2015 \nOnly"),
  #                    aesthetics = "color") +
  # theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
  #       legend.position = c(0.72, 0.28), legend.text = element_text(size = 6), legend.title = element_text(size = 8),
  #       legend.direction = "vertical", axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10),
  #       axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10)) +
  # geom_text(data = p1_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
  # geom_text(data = p1_textb, mapping = aes(x = x, y = y, label = label), size = 3) +
  geom_text(data = data.frame(label = "Mean \n+/- SE", y = 35, x = 1.5, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2)# + 
  # facet_grid(~ factor(sequence, levels = c('Both Droughts', '2012-2015 Only')))
p3