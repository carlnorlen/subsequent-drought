#Author: Carl Norlen
#Date Created: November 11, 2019
#Date Edited: April 4, 2022
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

#Creating a plot of mortality by tree species in two time periods and drought sequences.
#Create a notin operator
`%notin%` <- Negate(`%in%`)

##Both Droughts regions estimates
#Doing a combined estimated of the Basal Area for Both Droughts during 1999-2002
#Total basal area and tpa estimates by species
tpa.both.all.2002 <- tpa(ca, byPlot = TRUE, treeType = 'live', bySpecies = TRUE, treeDomain = INVYR %in% c("2002", "2003", "2004", "2005", "2006") & DIA >= 5,
                         areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                           DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)
tpa.both.all.2002$COMMON_NAME

#Dead basal area 
tpa.both.mort.2002 <- tpa(ca, byPlot = TRUE, treeType = 'dead', bySpecies = TRUE, treeDomain = DIA >= 5 & INVYR %in% c("2002", "2003", "2004", "2005", "2006"),
                          areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                            DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)

#Combine multiple inventory years into one plot estimate
# tpa.both.mort.sum.2002 <- tpa.both.mort.2002 %>% group_by(pltID, YEAR, COMMON_NAME) %>% summarize(BAA.dead = sum(BAA))
# tpa.both.all.sum.2002 <- tpa.both.all.2002 %>% group_by(pltID, YEAR, COMMON_NAME) %>% summarize(BAA = sum(BAA))
tpa.both.mort.2002 <- tpa.both.mort.2002 %>% select(pltID, YEAR, COMMON_NAME, BAA, TPA) %>% rename(BAA.dead = BAA, TPA.dead = TPA)

join.both.2002 <- left_join(tpa.both.all.2002, tpa.both.mort.2002, by = c('pltID', 'YEAR','COMMON_NAME'))
join.both.2002

#Replace the NAs with 0s
join.both.2002 <- join.both.2002 %>% mutate(BAA.dead = replace(BAA.dead, is.na(BAA.dead), 0))
# # join.both.2002 %>% select('pltID') %>% unique()
# 
# #Calculate the mortality percentage
# join.both.2002$BAA.mort <- join.both.2002$BAA.dead / join.both.2002$BAA * 100
# join.both.2002
#Do the total estimates by species Common Names
# mort.sp.both.2002 <- join.both.2002 %>% group_by(COMMON_NAME) %>% summarize (BAA.mean = mean(BAA), BAA.dead.mean = mean(BAA.dead), BAA.mort.mean = mean(BAA.mort), BAA.mort.sd = sd(BAA.mort), count = n())
# 
# #Get a list of species for filtering purposes
# sp.both.2002 <- mort.sp.both.2002 %>% filter(count >= 5) %>% pull(COMMON_NAME)
# print(sp.both.2002)

#Doing a combined estimated of the Basal Area for Both Droughts during 2012-2015
#Total basal area and tpa estimates by species
tpa.both.all.2015 <- tpa(ca, byPlot = TRUE, treeType = 'live', bySpecies = TRUE, treeDomain = INVYR %in% c("2015", "2016", "2017", "2018", "2019") & DIA >= 5,
                         areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                           DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)
#Dead basal area 
tpa.both.mort.2015 <- tpa(ca, byPlot = TRUE, treeType = 'dead', bySpecies = TRUE, treeDomain = INVYR %in% c("2015", "2016", "2017", "2018", "2019") & 
                            DIA >= 5 & MORTYR %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019"), #%notin% c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012"),
                          areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                            DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)

#Combine multiple inventory years into one plot estimate
# tpa.both.mort.sum.2015 <- tpa.both.mort.2015 %>% group_by(pltID, YEAR, COMMON_NAME) %>% summarize(BAA.dead = sum(BAA))
# tpa.both.all.sum.2015 <- tpa.both.all.2015 %>% group_by(pltID, YEAR, COMMON_NAME) %>% summarize(BAA = sum(BAA))
tpa.both.mort.2015 <- tpa.both.mort.2015 %>% select(pltID, YEAR, COMMON_NAME, BAA, TPA) %>% rename(BAA.dead = BAA, TPA.dead = TPA)

join.both.2015 <- left_join(tpa.both.all.2015, tpa.both.mort.2015, by = c('pltID', 'YEAR','COMMON_NAME'))

#Replace the NAs with 0s
join.both.2015 <- join.both.2015 %>% mutate(BAA.dead = replace(BAA.dead, is.na(BAA.dead), 0))
# 
# #Calculate the mortality percentage
# join.both.2015$BAA.mort <- join.both.2015$BAA.dead / join.both.2015$BAA * 100
# join.both.2015

#Do the total estimates by species Common Names
# mort.sp.both.2015 <- join.both.2015 %>% group_by(COMMON_NAME) %>% summarize (BAA.mean = mean(BAA), BAA.dead.mean = mean(BAA.dead), BAA.mort.mean = mean(BAA.mort), BAA.mort.sd = sd(BAA.mort), count = n())
# 
# #Get a list of species for filtering purposes
# sp.both.2015 <- mort.sp.both.2015 %>% filter(count >= 5) %>% pull(COMMON_NAME)
# print(sp.both.2015)

#Combine the 1999-2002 and 2012-2015 plots values
join.both.2002$time.period <- '1999-2002'
join.both.2015$time.period <- '2012-2015'
all.both <- rbind(join.both.2002, join.both.2015)
all.both
# all.both <- all.both %>% factor('COMMON_NAME', levels = sp.levels)

##2012-2015 Only regions estimates
#Doing a combined estimated of the Basal Area for Both Droughts during 1999-2002
#Total basal area and tpa estimates by species
tpa.second.all.2002 <- tpa(ca, byPlot = TRUE, treeType = 'live', bySpecies = TRUE, treeDomain = INVYR %in% c("2002", "2003", "2004", "2005", "2006") & DIA >= 5,
                         areaDomain = ECOSUBCD %in% c('M261Ep', 'M261Eq' ,'M261Eu' ,'M261Er' ,'M261Eo' ,'M262Bb' ,'M262Ba') &
                           DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)

# summary(ca$TREE)

#Dead basal area 
tpa.second.mort.2002 <- tpa(ca, byPlot = TRUE, treeType = 'dead', bySpecies = TRUE, treeDomain = DIA >= 5 & MORTYR != '' & INVYR %in% c("2002", "2003", "2004", "2005", "2006"),
                          areaDomain = ECOSUBCD %in% c('M261Ep', 'M261Eq' ,'M261Eu' ,'M261Er' ,'M261Eo' ,'M262Bb' ,'M262Ba') & 
                            DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)

#Combine multiple inventory years into one plot estimate
# tpa.second.mort.sum.2002 <- tpa.second.mort.2002 %>% group_by(pltID, YEAR, COMMON_NAME) %>% summarize(BAA.dead = sum(BAA))
# tpa.second.all.sum.2002 <- tpa.second.all.2002 %>% group_by(pltID, YEAR, COMMON_NAME) %>% summarize(BAA = sum(BAA))
tpa.second.mort.2002 <- tpa.second.mort.2002 %>% select(pltID, YEAR, COMMON_NAME, BAA, TPA) %>% rename(BAA.dead = BAA, TPA.dead = TPA)

join.second.2002 <- left_join(tpa.second.all.2002, tpa.second.mort.2002, by = c('pltID', 'YEAR','COMMON_NAME'))
join.second.2002
#Replace the NAs with 0s
join.second.2002 <- join.second.2002 %>% mutate(BAA.dead = replace(BAA.dead, is.na(BAA.dead), 0))
# # join.second.2002 %>% select('pltID') %>% unique()
# #Calculate the mortality percentage
# join.second.2002$BAA.mort <- join.second.2002$BAA.dead / join.second.2002$BAA * 100

#Do the total estimates by species Common Names
# mort.sp.second.2002 <- join.second.2002 %>% group_by(COMMON_NAME) %>% summarize (BAA.mean = mean(BAA), BAA.dead.mean = mean(BAA.dead), BAA.mort.mean = mean(BAA.mort), BAA.mort.sd = sd(BAA.mort), count = n())
# 
# #Get a list of species for filtering purposes
# sp.second.2002 <- mort.sp.second.2002 %>% filter(count >= 5 & COMMON_NAME != 'curlleaf mountain-mahogany') %>% pull(COMMON_NAME)
# print(sp.second.2002)

#Doing a combined estimated of the Basal Area for Both Droughts during 2012-2015
#Total basal area and tpa estimates by species
tpa.second.all.2015 <- tpa(ca, byPlot = TRUE, treeType = 'live', bySpecies = TRUE, treeDomain = INVYR %in% c("2015", "2016", "2017", "2018", "2019") & 
                           DIA >= 5, #Trying to remove any earlier MORTYR values
                         areaDomain = ECOSUBCD %in% c('M261Ep', 'M261Eq' ,'M261Eu' ,'M261Er' ,'M261Eo' ,'M262Bb' ,'M262Ba') &
                           DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)
# tpa.second.all.2002

#Dead basal area 
tpa.second.mort.2015 <- tpa(ca, byPlot = TRUE, treeType = 'dead', bySpecies = TRUE, treeDomain = INVYR %in% c("2015", "2016", "2017", "2018", "2019") & 
                              DIA >= 5 &MORTYR %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                          areaDomain = ECOSUBCD %in% c('M261Ep', 'M261Eq' ,'M261Eu' ,'M261Er' ,'M261Eo' ,'M262Bb' ,'M262Ba') &
                            DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)

#Combine multiple inventory years into one plot estimate
# tpa.second.mort.sum.2015 <- tpa.second.mort.2015 %>% group_by(pltID, YEAR, COMMON_NAME) %>% summarize(BAA.dead = sum(BAA))
# tpa.second.all.sum.2015 <- tpa.second.all.2015 %>% group_by(pltID, YEAR, COMMON_NAME) %>% summarize(BAA = sum(BAA))
tpa.second.mort.2015 <- tpa.second.mort.2015 %>% select(pltID, YEAR, COMMON_NAME, BAA, TPA) %>% rename(BAA.dead = BAA, TPA.dead = TPA)

join.second.2015 <- left_join(tpa.second.all.2015, tpa.second.mort.2015, by = c('pltID', 'YEAR','COMMON_NAME'))

#Replace the NAs with 0s
join.second.2015 <- join.second.2015 %>% mutate(BAA.dead = replace(BAA.dead, is.na(BAA.dead), 0))
# 
# #Calculate the mortality percentage
# join.second.2015$BAA.mort <- join.second.2015$BAA.dead / join.second.2015$BAA * 100
# join.second.2015
#Do the total estimates by species Common Names
# mort.sp.second.2015 <- join.second.2015 %>% group_by(COMMON_NAME) %>% summarize (BAA.mean = mean(BAA), BAA.dead.mean = mean(BAA.dead), BAA.mort.mean = mean(BAA.mort), BAA.mort.sd = sd(BAA.mort), count = n())

#Get a list of species for filtering purposes
# sp.second.2015 <- mort.sp.second.2015 %>% filter(count >= 5) %>% pull(COMMON_NAME)
# print(sp.second.2015)

#Combine the 1999-2002 and 2012-2015 plots values
join.second.2002$time.period <- '1999-2002'
join.second.2015$time.period <- '2012-2015'
all.second <- rbind(join.second.2002, join.second.2015)
all.second
#Combine all the all the data together for drought regions
all.both$sequence <- 'Both Droughts'
all.second$sequence <- '2012-2015 Only'
all.forest <- rbind(all.both, all.second)
all.forest

#Make a list to order the COMMON_NAME column as a factor
sp.levels <- c("ponderosa pine", "Coulter pine", "Jeffrey pine", "sugar pine", "singleleaf pinyon", "white fir", "bigcone Douglas-fir", 
               "incense-cedar","California juniper","canyon live oak", "California black oak", "interior live oak")

#Convert basal area from sq ft / acre to sq meter / hectare
# all.forest$BAA.dead <- all.forest$BAA.dead * (1/4.356)
# sp.both.2002[! sp.both.2002 %in% c('curlleaf mountain-mahogany', 'California live oak')]
# p1 <- ggbarplot(all.forest %>% filter(COMMON_NAME %in% sp.both.2002[! sp.both.2002 %in% c('curlleaf mountain-mahogany', 'California live oak')]), #, (sequence == 'Both Droughts' & drought == '1999-2002') | (sequence == '2012-2015 Only' & drought == '2012-2015')), 
#                 x = "time.period", y = "BAA.dead", position = position_dodge(), fill = 'COMMON_NAME', color = "sequence",
#                 add = "mean_se" , error.plot = "errorbar", alpha = 0.8) + 
#   theme_bw() + guides(color = 'none', fill = guide_legend(title = "Tree \nSpecies", ncol = 2)) +
#   scale_color_manual(values = c("black", "black"),
#                      aesthetics = "color") +
#   theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
#         legend.position = c(0.76, 0.15), legend.text = element_text(size = 3), legend.title = element_text(size = 5),
#         legend.direction = "vertical", axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10),
#         axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10)) +
#   # geom_text(data = p1_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
#   # geom_text(data = p1_textb, mapping = aes(x = x, y = y, label = label), size = 3) +
#   geom_text(data = data.frame(label = "Mean \n+/- SE", y = 4, x = 2.25, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2) + 
#   facet_grid(~ factor(sequence, levels = c('Both Droughts', '2012-2015 Only')))
# p1
# 
# ggsave(filename = 'SFig10_species_mortality_barplot.png', height=7, width=16, units = 'cm', dpi=900)

# DIA.summary.all
#Do an ANOVA and Tukey HSD by tree species
sp.aov.dead <- aov(data = all.forest %>% filter(COMMON_NAME %in% sp.both.2002[sp.both.2002 %in% c('ponderosa pine', 'Jeffrey pine', 'singleleaf pinyon')]), BAA.mort ~ time.period * sequence * COMMON_NAME)
summary(sp.aov.dead)

#Tukey Post Hoc analysis of dead basal area by tree species
sp.dead.tHSD <- TukeyHSD(sp.aov.dead) 
sp.dead.tHSD

#Testing out a separation by Species and DIA group 
#Both Droughts for 1999-2002
#Live Basal Area
# makeClasses(ca, interval = as.numeric(10/2.54))
# ca.size <- makeClasses(ca$TREE$DIA, interval = 10/2.54)
#Create a list of size classes based on the tree table
# makeClasses(ca$TREE$DIA, interval = 10/2.54)
# ca$TREE$DIA
tpa.dia.both.all.2002 <- tpa(ca, treeType = 'live', byPlot = TRUE, bySpecies = TRUE, bySizeClass = TRUE, treeDomain = INVYR %in% c("2002", "2003", "2004", "2005", "2006") & DIA >= 5,
                         areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                           DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)
# tpa.dia.both.all.2002
# makeClasses(ca, )
#Dead basal area 
tpa.dia.both.mort.2002 <- tpa(ca, treeType = 'dead', byPlot = TRUE, bySpecies = TRUE, bySizeClass = TRUE, treeDomain = DIA >= 5 & INVYR %in% c("2002", "2003", "2004", "2005", "2006"),
                          areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                            DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)

#Rename the dead columns for TPA
tpa.dia.both.mort.2002 <- tpa.dia.both.mort.2002 %>% select(pltID, sizeClass, YEAR, COMMON_NAME, BAA, TPA) %>% rename(BAA.dead = BAA, TPA.dead = TPA)

join.dia.both.2002 <- left_join(tpa.dia.both.all.2002, tpa.dia.both.mort.2002, by = c('pltID', 'sizeClass', 'YEAR','COMMON_NAME'))

#Replace the NAs with 0s
join.dia.both.2002 <- join.dia.both.2002 %>% mutate(BAA.dead = replace(BAA.dead, is.na(BAA.dead), 0))
# join.dia.both.2002

#Both Droughts for 2012-2015
#Live Basal Area
tpa.dia.both.all.2015 <- tpa(ca, byPlot = TRUE, treeType = 'live', bySpecies = TRUE, bySizeClass = TRUE, treeDomain = INVYR %in% c("2015", "2016", "2017", "2018", "2019") & 
                           DIA >= 5,
                         areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                           DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)

#Dead basal area 
tpa.dia.both.mort.2015 <- tpa(ca, treeType = 'dead', byPlot = TRUE, bySpecies = TRUE, bySizeClass = TRUE, treeDomain = INVYR %in% c("2015", "2016", "2017", "2018", "2019") & 
                            DIA >= 5 & MORTYR %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                          areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                            DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)

#Rename the dead BAA and TPA columns
tpa.dia.both.mort.2015 <- tpa.dia.both.mort.2015 %>% select(pltID, YEAR, COMMON_NAME, BAA, TPA) %>% rename(BAA.dead = BAA, TPA.dead = TPA)

join.dia.both.2015 <- left_join(tpa.dia.both.all.2015, tpa.dia.both.mort.2015, by = c('pltID', 'YEAR','COMMON_NAME'))

#Replace the NAs with 0s
join.dia.both.2015 <- join.dia.both.2015 %>% mutate(BAA.dead = replace(BAA.dead, is.na(BAA.dead), 0))
 

##Combine two different live basal area queries together
join.dia.both.2002$time.period <- '1999-2002'
join.dia.both.2015$time.period <- '2012-2015'
join.dia.both <- rbind(join.dia.both.2002, join.dia.both.2015)
join.dia.both$sequence <- 'Both Droughts'

#2012-2015 Only
#1999-2002 Data
#Live Basal Area
tpa.dia.second.all.2002 <- tpa(ca, treeType = 'live', byPlot = TRUE, bySpecies = TRUE, bySizeClass = TRUE, treeDomain = INVYR %in% c("2002", "2003", "2004", "2005", "2006") & DIA >= 5,
                             areaDomain = ECOSUBCD %in% c('M261Ep', 'M261Eq' ,'M261Eu' ,'M261Er' ,'M261Eo' ,'M262Bb' ,'M262Ba') &
                               DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)

#Dead basal area 
tpa.dia.second.mort.2002 <- tpa(ca, treeType = 'dead', byPlot = TRUE, bySpecies = TRUE, bySizeClass = TRUE, treeDomain = DIA >= 5 & INVYR %in% c("2002", "2003", "2004", "2005", "2006") & MORTYR != '',
                              areaDomain = ECOSUBCD %in% c('M261Ep', 'M261Eq' ,'M261Eu' ,'M261Er' ,'M261Eo' ,'M262Bb' ,'M262Ba') &
                                DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)

#Rename the dead columns for TPA
tpa.dia.second.mort.2002 <- tpa.dia.second.mort.2002 %>% select(pltID, sizeClass, YEAR, COMMON_NAME, BAA, TPA) %>% rename(BAA.dead = BAA, TPA.dead = TPA)

join.dia.second.2002 <- left_join(tpa.dia.second.all.2002, tpa.dia.second.mort.2002, by = c('pltID', 'sizeClass', 'YEAR','COMMON_NAME'))

#Replace the NAs with 0s
join.dia.second.2002 <- join.dia.second.2002 %>% mutate(BAA.dead = replace(BAA.dead, is.na(BAA.dead), 0))

#2012-2015 Data
#Live Basal Area
tpa.dia.second.all.2015 <- tpa(ca, byPlot = TRUE, treeType = 'live', bySpecies = TRUE, bySizeClass = TRUE, treeDomain = INVYR %in% c("2015", "2016", "2017", "2018", "2019") & 
                               DIA >= 5,
                             areaDomain = ECOSUBCD %in% c('M261Ep', 'M261Eq' ,'M261Eu' ,'M261Er' ,'M261Eo' ,'M262Bb' ,'M262Ba') &
                               DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)

#Dead basal area 
tpa.dia.second.mort.2015 <- tpa(ca, treeType = 'dead', byPlot = TRUE, bySpecies = TRUE, bySizeClass = TRUE, treeDomain = INVYR %in% c("2015", "2016", "2017", "2018", "2019") & 
                                DIA >= 5 & MORTYR %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                              areaDomain = ECOSUBCD %in% c('M261Ep', 'M261Eq' ,'M261Eu' ,'M261Er' ,'M261Eo' ,'M262Bb' ,'M262Ba') &
                                DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)


#Rename the dead BAA and TPA columns
tpa.dia.second.mort.2015 <- tpa.dia.second.mort.2015 %>% select(pltID, YEAR, COMMON_NAME, BAA, TPA) %>% rename(BAA.dead = BAA, TPA.dead = TPA)

join.dia.second.2015 <- left_join(tpa.dia.second.all.2015, tpa.dia.second.mort.2015, by = c('pltID', 'YEAR','COMMON_NAME'))

#Replace the NAs with 0s
join.dia.second.2015 <- join.dia.second.2015 %>% mutate(BAA.dead = replace(BAA.dead, is.na(BAA.dead), 0))


##Combine two different live queries together
join.dia.second.2002$time.period <- '1999-2002'
join.dia.second.2015$time.period <- '2012-2015'
join.dia.second <- rbind(join.dia.second.2002, join.dia.second.2015)
join.dia.second$sequence <- '2012-2015 Only'

##Combine two different dead queries together for tree mortality
# tpa.dia.second.mort.2002$time.period <- '1999-2002'
# tpa.dia.second.mort.2015$time.period <- '2012-2015'
# tpa.dia.second.mort <- rbind(tpa.dia.second.mort.2002, tpa.dia.second.mort.2015)
# tpa.dia.second.mort$sequence <- '2012-2015 Only'

#Combine the drought sequence for total basal area and mortality with species and DIA groups
#Total Basal Area
join.dia.all <- rbind(join.dia.both, join.dia.second)

#Mortality Basal Area
# tpa.dia.mort <- rbind(tpa.dia.second.mort, tpa.dia.second.mort)
join.dia.all$tree_type <- recode(.x=join.dia.all$COMMON_NAME, 'California black oak' = 'oak', 'California juniper' = 'juniper', 'California live oak' = 'oak', 'California sycamore' = 'deciduous', 'Coulter pine' = 'pine', 'chinkapin oak' = 'oak', 'Jeffrey pine' = 'pine',
                               'bigcone Douglas-fir' = 'fir', 'bigleaf maple' = 'deciduous', 'canyon live oak' = 'oak', 'curlleaf mountain-mahogany' = 'deciduous', 'incense-cedar' = 'cedar', 'interior live oak' = 'oak', 'limber pine' = 'pine',
                               'lodgepole pine' = 'pine', 'ponderosa pine' = 'pine', 'singleleaf pinyon' = 'pine', 'sugar pine' = 'pine', 'Utah juniper' = 'juniper', 'western juniper' = 'juniper', 'white alder' = 'deciduous', 'white fir' = 'fir', 'California laurel' = 'deciduous',
                               'California-laurel' = 'deciduous', 'Oregon ash' = 'deciduous', 'Douglas-fir' = 'fir', 'honey mesquite' = 'deciduous', 'desert ironwood' = 'deciduous', 'California red fir' = 'fir', 'California buckeye' = 'deciduous', 'Engelmann oak' = 'oak', 'grand fir' = 'fir', 'western white pine' = 'pine',
                               "western white pine" = 'pine', "whitebark pine" = 'pine', "mountain hemlock" = "other conifer", "gray or California foothill pine" = "pine", "foxtail pine" = 'pine', "blue oak" = 'oak', "California white oak" = 'oak', "quaking aspen" = 'deciduous',
                               "giant sequoia" = 'other conifer', "Unknown dead conifer" = 'unknown conifer', "ash spp." = 'deciduous', "black cottonwood" = 'deciduous', "California torreya (nutmeg)" = 'deciduous', "Oregon white oak" = 'oak', "Port-Orford-cedar" = 'cedar', "Pacific dogwood" = 'deciduous')

join.dia.all$BAA.dead <- join.dia.all$BAA.dead * (1/4.356)
join.dia.all$BAA <- join.dia.all$BAA * (1/4.356)
join.dia.all$BAA.all<- join.dia.all$BAA + join.dia.all$BAA.dead
#Make tree type a factor so that I can fill in missing combinations
join.dia.all$tree_type <- as.factor(join.dia.all$tree_type)
join.dia.all$sizeClass <- as.factor(join.dia.all$sizeClass)
join.dia.all.type <- join.dia.all %>% group_by(pltID, time.period, sequence, sizeClass, tree_type, .drop = FALSE) %>% 
  summarize(BAA.all.sum = sum(BAA.all), BAA.live.sum = sum(BAA), BAA.dead.sum = sum(BAA.dead))

#Convert tree_type and sizeClass back to characters
join.dia.all.type$tree_type <- as.character(join.dia.all.type$tree_type)
join.dia.all.type$sizeClass <- as.character(join.dia.all.type$sizeClass)

#Calculate the precentage mortality
join.dia.all.type$BAA.mort <- join.dia.all.type$BAA.dead.sum / (join.dia.all.type$BAA.all.sum) * 100
join.dia.all.type <- join.dia.all.type %>% mutate(BAA.mort = replace(BAA.mort, is.na(BAA.mort), 0))
summary(join.dia.all.type)
join.dia.all.summary <- join.dia.all.type %>% #mutate(BAA.mort = replace(BAA.mort, is.na(BAA.mort), 0)) %>%
  mutate(live = case_when(BAA.all.sum > 0 ~ 1, BAA.all.sum == 0 ~ 0), 
         dead = case_when(BAA.mort > 0 ~ 1, BAA.mort == 0 ~ 0)) %>%
  group_by(tree_type, time.period, sequence, sizeClass) %>% summarize(BAA.all.mean = mean(BAA.all.sum), BAA.all.sd = sd(BAA.all.sum),
                                                           BAA.live.mean = mean(BAA.live.sum), BAA.sd = sd(BAA.live.sum),
                                                           BAA.dead.mean = mean(BAA.dead.sum), BAA.dead.sd = sd(BAA.dead.sum),
                                                           BAA.mort.mean = mean(BAA.mort), BAA.mort.sd = sd(BAA.mort), count = n(),
                                                           live.count = sum(live), dead.count = sum(dead))

join.dia.all.type %>% filter(tree_type == 'pine') %>% summary()

#Do plots of the total basal area by sizeClass
p2 <- ggplot() + geom_line(data = join.dia.all %>% filter(tree_type != 'oak' & tree_type != 'cedar' & 
                                                               tree_type != 'juniper' & tree_type != 'other conifer' & 
                                                               tree_type != 'deciduous') %>% group_by(tree_type, time.period, sequence, sizeClass) %>%
                             summarize(BAA.mort = sum(BAA.dead)), 
                       mapping = aes(x = as.numeric(sizeClass) * 2.54, y = BAA.mort, color = time.period)) + 
                       xlab('Size Class (cm)') + ylab(expression('Basal Area (m'^2*' ha'^-1*')')) + theme_bw() +
                       # scale_color_manual(values = c("black", "black"),
                       # aesthetics = "color") +
                       facet_grid(tree_type ~ factor(sequence, levels = c('Both Droughts', '2012-2015 Only')))
p2
ggsave(filename = 'SFig11_conifer_basal_area_distribution.png', height=14, width=16, units = 'cm', dpi=900)

# print(sp.second.2002)
# print(sp.second.2015)

#Do plots of the total basal area by sizeClass
p2 <- ggplot() + geom_line(data = join.dia.all.type %>% filter(tree_type != 'oak' & tree_type != 'cedar' & 
                                                                 tree_type != 'juniper' & tree_type != 'other conifer' & 
                                                                 tree_type != 'deciduous') %>% group_by(tree_type, time.period, sequence, sizeClass) %>%
                             summarize(BAA.mean = mean(BAA.dead.sum)), 
                           mapping = aes(x = as.numeric(sizeClass) * 2.54, y = BAA.mean, color = time.period)) + 
  xlab('Size Class (cm)') + ylab(expression('Basal Area (m'^2*' ha'^-1*')')) + theme_bw() +
  # scale_color_manual(values = c("black", "black"),
  # aesthetics = "color") +
  facet_grid(tree_type ~ factor(sequence, levels = c('Both Droughts', '2012-2015 Only')))
p2

ggsave(filename = 'SFig12_conifer_mortality_distribution.png', height=14, width=16, units = 'cm', dpi=900)

#Try to do a mortality without tree species analysis
##Both Droughts regions estimates
#Doing a combined estimated of the Basal Area for Both Droughts during 1999-2002
#Total basal area and tpa estimates by species
all.forest$tree_type <- recode(.x=all.forest$COMMON_NAME, 'California black oak' = 'oak', 'California juniper' = 'juniper', 'California live oak' = 'oak', 'California sycamore' = 'deciduous', 'Coulter pine' = 'pine', 'chinkapin oak' = 'oak', 'Jeffrey pine' = 'pine',
                                 'bigcone Douglas-fir' = 'fir', 'bigleaf maple' = 'deciduous', 'canyon live oak' = 'oak', 'curlleaf mountain-mahogany' = 'deciduous', 'incense-cedar' = 'cedar', 'interior live oak' = 'oak', 'limber pine' = 'pine',
                                 'lodgepole pine' = 'pine', 'ponderosa pine' = 'pine', 'singleleaf pinyon' = 'pine', 'sugar pine' = 'pine', 'Utah juniper' = 'juniper', 'western juniper' = 'juniper', 'white alder' = 'deciduous', 'white fir' = 'fir', 'California laurel' = 'deciduous',
                                 'California-laurel' = 'deciduous', 'Oregon ash' = 'deciduous', 'Douglas-fir' = 'fir', 'honey mesquite' = 'deciduous', 'desert ironwood' = 'deciduous', 'California red fir' = 'fir', 'California buckeye' = 'deciduous', 'Engelmann oak' = 'oak', 'grand fir' = 'fir', 'western white pine' = 'pine',
                                 "western white pine" = 'pine', "whitebark pine" = 'pine', "mountain hemlock" = "other conifer", "gray or California foothill pine" = "pine", "foxtail pine" = 'pine', "blue oak" = 'oak', "California white oak" = 'oak', "quaking aspen" = 'deciduous',
                                 "giant sequoia" = 'other conifer', "Unknown dead conifer" = 'unknown conifer', "ash spp." = 'deciduous', "black cottonwood" = 'deciduous', "California torreya (nutmeg)" = 'deciduous', "Oregon white oak" = 'oak', "Port-Orford-cedar" = 'cedar', "Pacific dogwood" = 'deciduous')

# all.forest$tree_type <- recode(.x=all.forest$COMMON_NAME, 'California black oak' = 'other tree', 'California juniper' = 'other tree', 'California live oak' = 'other tree', 'California sycamore' = 'other tree', 'Coulter pine' = 'pine/fir', 'chinkapin oak' = 'other tree', 'Jeffrey pine' = 'pine/fir',
#                                  'bigcone Douglas-fir' = 'pine/fir', 'bigleaf maple' = 'other tree', 'canyon live oak' = 'other tree', 'curlleaf mountain-mahogany' = 'other tree', 'incense-cedar' = 'other tree', 'interior live oak' = 'other tree', 'limber pine' = 'pine/fir', 
#                                  'lodgepole pine' = 'pine/fir', 'ponderosa pine' = 'pine/fir', 'singleleaf pinyon' = 'pine/fir', 'sugar pine' = 'pine/fir', 'Utah juniper' = 'other tree', 'western juniper' = 'other tree', 'white alder' = 'other tree', 'white fir' = 'pine/fir', 'California laurel' = 'other tree',
#                                  'California-laurel' = 'other tree', 'Oregon ash' = 'other tree', 'Douglas-fir' = 'pine/fir', 'honey mesquite' = 'other tree', 'desert ironwood' = 'other tree', 'California red fir' = 'pine/fir', 'California buckeye' = 'other tree', 'Engelmann oak' = 'other tree', 'grand fir' = 'pine/fir', 'western white pine' = 'pine/fir',
#                                  "western white pine" = 'pine/fir', "whitebark pine" = 'pine/fir', "mountain hemlock" = "pine/fir", "gray or California foothill pine" = "pine/fir", "foxtail pine" = 'pine/fir', "blue oak" = 'other tree', "California white oak" = 'other tree', "quaking aspen" = 'other tree', 
#                                  "giant sequoia" = 'other tree', "Unknown dead conifer" = 'pine/fir', "ash spp." = 'other tree', "black cottonwood" = 'other tree', "California torreya (nutmeg)" = 'other tree', "Oregon white oak" = 'other tree', "Port-Orford-cedar" = 'other tree', "Pacific dogwood" = 'other tree')


all.forest
all.forest$BAA.dead <- all.forest$BAA.dead * (1/4.356)
all.forest$BAA <- all.forest$BAA * (1/4.356)
all.forest$BAA.all<- all.forest$BAA + all.forest$BAA.dead
#Make tree type a factor so that I can fill in missing combinations
all.forest$tree_type <- as.factor(all.forest$tree_type)
all.forest.type <- all.forest %>% group_by(pltID, time.period, sequence, tree_type, .drop = FALSE) %>% 
                                  summarize(BAA.all.sum = sum(BAA.all), BAA.live.sum = sum(BAA), BAA.dead.sum = sum(BAA.dead)) #%>%
                                  # ungroup() %>% #Ungroup the data
                                  # complete(pltID, time.period, tree_type, fill = list(BAA.all.sum = 0, BAA.dead.sum = 0, BAA.live.sum = 0)) #Add the missing combinations of data

all.forest.type$BAA.mort <- all.forest.type$BAA.dead.sum / (all.forest.type$BAA.all.sum) * 100
all.forest.type <- all.forest.type %>% mutate(BAA.mort = replace(BAA.mort, is.na(BAA.mort), 0))


all.forest.type.summary <- all.forest.type %>% #mutate(BAA.mort = replace(BAA.mort, is.na(BAA.mort), 0)) %>%
                                                      mutate(live = case_when(BAA.all.sum > 0 ~ 1, BAA.all.sum == 0 ~ 0), 
                                                      dead = case_when(BAA.mort > 0 ~ 1, BAA.mort == 0 ~ 0)) %>%
                                                group_by(tree_type, time.period, sequence) %>% summarize(BAA.all.mean = mean(BAA.all.sum), BAA.all.sd = sd(BAA.all.sum),
                                                                             BAA.live.mean = mean(BAA.live.sum), BAA.sd = sd(BAA.live.sum),
                                                                             BAA.dead.mean = mean(BAA.dead.sum), BAA.dead.sd = sd(BAA.dead.sum),
                                                                             BAA.mort.mean = mean(BAA.mort), BAA.mort.sd = sd(BAA.mort), count = n(),
                                                                             live.count = sum(live), dead.count = sum(dead))

# sp.both.2002[! sp.both.2002 %in% c('curlleaf mountain-mahogany', 'California live oak')]
#Die-off basal area
p4 <- ggbarplot(all.forest.type, #%>% filter(COMMON_NAME %in% sp.both.2002[! sp.both.2002 %in% c('curlleaf mountain-mahogany', 'California live oak')]), #, (sequence == 'Both Droughts' & drought == '1999-2002') | (sequence == '2012-2015 Only' & drought == '2012-2015')), 
                x = "time.period", y = "BAA.dead.sum", fill = 'tree_type', color = "sequence", 
                position = position_dodge(),
                add = "mean_se" , error.plot = "errorbar", alpha = 0.8) + 
  theme_bw() + guides(color = 'none', fill = guide_legend(title = "Tree \nType", ncol = 2)) +
  scale_color_manual(values = c("black", "black"),
                     aesthetics = "color") +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = c(0.78, 0.3), legend.text = element_text(size = 6), legend.title = element_text(size = 8),
        legend.direction = "vertical", axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10)) +
  # geom_text(data = p1_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
  # geom_text(data = p1_textb, mapping = aes(x = x, y = y, label = label), size = 3) +
  geom_text(data = data.frame(label = "Mean \n+/- SE", y = 1.7, x = 1.6, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2) + 
  facet_grid(~ factor(sequence, levels = c('Both Droughts', '2012-2015 Only')))
p4

ggsave(filename = 'SFig13_dead_basal_area_by_tree_type.png', height=7, width=16, units = 'cm', dpi=900)

#Total Basal Area
p5 <- ggbarplot(all.forest.type, #%>% filter(COMMON_NAME %in% sp.both.2002[! sp.both.2002 %in% c('curlleaf mountain-mahogany', 'California live oak')]), #, (sequence == 'Both Droughts' & drought == '1999-2002') | (sequence == '2012-2015 Only' & drought == '2012-2015')), 
                x = "time.period", y = "BAA.all.sum", position = position_dodge(), fill = 'tree_type', color = "sequence",
                add = "mean_se" , error.plot = "errorbar", alpha = 0.8) + 
  theme_bw() + guides(color = 'none', fill = guide_legend(title = "Tree \nType", ncol = 2)) +
  scale_color_manual(values = c("black", "black"),
                     aesthetics = "color") +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = c(0.5, 0.35), legend.text = element_text(size = 6), legend.title = element_text(size = 8),
        legend.direction = "vertical", axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10)) +
  # geom_text(data = p1_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
  # geom_text(data = p1_textb, mapping = aes(x = x, y = y, label = label), size = 3) +
  geom_text(data = data.frame(label = "Mean \n+/- SE", y = 7.9, x = 1.4, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2) + 
  facet_grid(~ factor(sequence, levels = c('Both Droughts', '2012-2015 Only')))
p5

ggsave(filename = 'SFig14_basal_area_by_tree_type.png', height=7, width=16, units = 'cm', dpi=900)

#Summary of Total Dead Basal Area
p6 <- ggbarplot(all.forest %>% group_by(time.period, sequence, pltID) %>% summarize(BAA.all = sum(BAA.all), BAA.live = sum(BAA), BAA.dead.sum = sum(BAA.dead), BAA.mort = sum(BAA.dead) / sum(BAA.all) * 100), #%>% filter(COMMON_NAME %in% sp.both.2002[! sp.both.2002 %in% c('curlleaf mountain-mahogany', 'California live oak')]), #, (sequence == 'Both Droughts' & drought == '1999-2002') | (sequence == '2012-2015 Only' & drought == '2012-2015')), 
                x = "time.period", y = "BAA.dead.sum", position = position_dodge(), color = "sequence",
                add = "mean_se" , error.plot = "errorbar", alpha = 0.8) + 
  theme_bw() + guides(color = 'none', fill = 'none') +
  scale_color_manual(values = c("black", "black"),
                     aesthetics = "color") +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = c(0.76, 0.1), legend.text = element_text(size = 6), legend.title = element_text(size = 8),
        legend.direction = "vertical", axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10)) +
  # geom_text(data = p1_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
  # geom_text(data = p1_textb, mapping = aes(x = x, y = y, label = label), size = 3) +
  geom_text(data = data.frame(label = "Mean \n+/- SE", y = 2.85, x = 1.45, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2) + 
  facet_grid(~ factor(sequence, levels = c('Both Droughts', '2012-2015 Only')))
p6

ggsave(filename = 'SFig15_dead_basal_area.png', height=7, width=16, units = 'cm', dpi=900)

#Summary of Total Dead Basal Area
p7 <- ggbarplot(all.forest %>% group_by(time.period, sequence, pltID) %>% summarize(BAA.all.sum = sum(BAA.all), BAA.live = sum(BAA), BAA.dead.sum = sum(BAA.dead), BAA.mort = sum(BAA.dead) / sum(BAA.all) * 100), #%>% filter(COMMON_NAME %in% sp.both.2002[! sp.both.2002 %in% c('curlleaf mountain-mahogany', 'California live oak')]), #, (sequence == 'Both Droughts' & drought == '1999-2002') | (sequence == '2012-2015 Only' & drought == '2012-2015')), 
                x = "time.period", y = "BAA.all.sum", position = position_dodge(), color = "sequence",
                add = "mean_se" , error.plot = "errorbar", alpha = 0.8) + 
  theme_bw() + guides(color = 'none', fill = 'none') +
  scale_color_manual(values = c("black", "black"),
                     aesthetics = "color") +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = c(0.76, 0.15), legend.text = element_text(size = 6), legend.title = element_text(size = 8),
        legend.direction = "vertical", axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10)) +
  # geom_text(data = p1_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
  # geom_text(data = p1_textb, mapping = aes(x = x, y = y, label = label), size = 3) +
  geom_text(data = data.frame(label = "Mean \n+/- SE", y = 19, x = 1.45, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2) + 
  facet_grid(~ factor(sequence, levels = c('Both Droughts', '2012-2015 Only')))
p7

ggsave(filename = 'SFig16_total_basal_area.png', height=7, width=16, units = 'cm', dpi=900)

# p5 <- ggplot() + geom_line(data = all.forest.type %>% 
#                              group_by (COMMON_NA, sizeClass, time.period, sequence) %>% summarize(TPA.mean = mean(TPA), BAA.mean = mean(BAA)), mapping = aes(x = sizeClass * 2.54, y = BAA.mean, color = time.period)) + 
#   xlab('Size Class (cm)') + ylab(expression('Basal Area (m'^2*' ha'^-1*')')) + theme_bw() +
#   facet_grid(COMMON_NAME ~ factor(sequence, levels = c('Both Droughts', '2012-2015 Only')))




#Proportion Mortality
p8 <- ggbarplot(all.forest.type %>% filter(tree_type != 'other conifer' & tree_type != "deciduous"), #%>% filter(COMMON_NAME %in% sp.both.2002[! sp.both.2002 %in% c('curlleaf mountain-mahogany', 'California live oak')]), #, (sequence == 'Both Droughts' & drought == '1999-2002') | (sequence == '2012-2015 Only' & drought == '2012-2015')), 
                x = "time.period", y = "BAA.mort", fill = 'tree_type', color = "sequence", 
                position = position_dodge(),
                add = "mean_se" , error.plot = "errorbar", alpha = 0.8) + 
  theme_bw() + guides(color = 'none', fill = guide_legend(title = "Tree \nType", ncol = 2)) +
  scale_color_manual(values = c("black", "black"),
                     aesthetics = "color") + xlab('Mortality (%)') +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = c(0.78, 0.3), legend.text = element_text(size = 6), legend.title = element_text(size = 8),
        legend.direction = "vertical", axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10)) +
  # geom_text(data = p1_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
  # geom_text(data = p1_textb, mapping = aes(x = x, y = y, label = label), size = 3) +
  geom_text(data = data.frame(label = "Mean \n+/- SE", y = 16.5, x = 1.6, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2) + 
  facet_grid(~ factor(sequence, levels = c('Both Droughts', '2012-2015 Only')))
p8

ggsave(filename = 'SFig17_percent_dead_by_tree_type.png', height=7, width=16, units = 'cm', dpi=900)

#Mortality percentage across the whole landscape
p9 <- ggbarplot(all.forest %>% group_by(time.period, sequence, pltID) %>% summarize(BAA.all = sum(BAA.all), BAA.live = sum(BAA), BAA.dead.sum = sum(BAA.dead), BAA.mort = sum(BAA.dead) / sum(BAA.all) * 100), #%>% filter(COMMON_NAME %in% sp.both.2002[! sp.both.2002 %in% c('curlleaf mountain-mahogany', 'California live oak')]), #, (sequence == 'Both Droughts' & drought == '1999-2002') | (sequence == '2012-2015 Only' & drought == '2012-2015')), 
                x = "time.period", y = "BAA.mort", position = position_dodge(), color = "sequence",
                add = "mean_se" , error.plot = "errorbar", alpha = 0.8) + 
  theme_bw() + guides(color = 'none', fill = 'none') +
  scale_color_manual(values = c("black", "black"),
                     aesthetics = "color") +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = c(0.76, 0.1), legend.text = element_text(size = 6), legend.title = element_text(size = 8),
        legend.direction = "vertical", axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10)) +
  # geom_text(data = p1_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
  # geom_text(data = p1_textb, mapping = aes(x = x, y = y, label = label), size = 3) +
  geom_text(data = data.frame(label = "Mean \n+/- SE", y = 14, x = 1.45, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2) + 
  facet_grid(~ factor(sequence, levels = c('Both Droughts', '2012-2015 Only')))
p9

ggsave(filename = 'SFig18_total_percent_dead.png', height=7, width=16, units = 'cm', dpi=900)


#Tukey Post Hoc analysis of dead basal area by tree species type
type.aov.dead <- aov(data = all.forest.type %>% filter(tree_type %in% c('pine', 'oak', 'fir')), BAA.mort ~ time.period * sequence * tree_type)
summary(type.aov.dead)

type.dead.tHSD <- TukeyHSD(type.aov.dead) 
type.dead.tHSD