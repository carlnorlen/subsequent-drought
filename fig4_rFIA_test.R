#Author: Carl Norlen
#Date Created: November 11, 2019
#Date Edited: April 14, 2022
#Purpose: Create bar graphs for manuscript FIA analysis, testing out a new way of calculating the bar charts

# Specify necessary packages
p <- c("RSQLite","dbplyr","ggplot2","dplyr","tidyr", "ggpubr", "RColorBrewer",  
	   'gt', 'gtsummary', 'webshot', 'kableExtra', 'broom', 'rFIA', 'sf', 'grid', 'gridExtra')

# install.packages("rFIA")

# Load packages
lapply(p,require,character.only=TRUE)

#Set working directory
setwd('C:/Users/can02/mystuff/subsequent-drought')



#Add Data Sets
fiaCA <- 'D:\\Large_Files\\FIA\\SQLite_FIADB_CA\\2019_CSV' #Downloaedd from FIA DataMart
dir_usfs <- "D:\\Large_Files\\USFS\\data\\subsections"
# fiaCA <- file.path(sql_dir, 'FIADB_CA.db')
# ref <- getFIA(states = 'ref', tables = c('FOREST_TYPE', 'FOREST_TYPE_GROUP'), dir = fiaCA)
ca <- readFIA(fiaCA)
# ca <- getFIA(states = 'CA', dir = 'D:\\Large_Files\\FIA\\SQLite_FIADB_CA\\2019_CSV')
# summary(ca)

#Creating a plot of mortality by tree species in two time periods and drought sequences.
#Create a notin operator
`%notin%` <- Negate(`%in%`)

#Possible way to deal with non-conifer forests
forest.names <- ca$REF_FOREST_TYPE
forest.names$FORTYPCD <- forest.names$VALUE
##Both Droughts regions estimates
#Doing a combined estimated of the Basal Area for Both Droughts during 1999-2002
#Total basal area and tpa estimates by species
tpa.both.all.2002 <- tpa(ca, byPlot = TRUE, treeType = 'live', bySpecies = TRUE, treeDomain = INVYR %in% c("2002", "2003", "2004", "2005", "2006") & DIA >= 5,
                         areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                           DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)
# forest_type = left_join(tpa.both.all.2002, forest.names, by = c('FORTYPCD'))
# forest_type
#Dead basal area 
tpa.both.mort.2002 <- tpa(ca, byPlot = TRUE, treeType = 'dead', bySpecies = TRUE, treeDomain = DIA >= 5 & INVYR %in% c("2002", "2003", "2004", "2005", "2006"),
                          areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                            DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)

#Combine multiple inventory years into one plot estimate
tpa.both.mort.2002 <- tpa.both.mort.2002 %>% select(pltID, YEAR, COMMON_NAME, BAA, TPA) %>% rename(BAA.dead = BAA, TPA.dead = TPA)

join.both.2002 <- left_join(tpa.both.all.2002, tpa.both.mort.2002, by = c('pltID', 'YEAR','COMMON_NAME'))
join.both.2002

#Replace the NAs with 0s
join.both.2002 <- join.both.2002 %>% mutate(BAA.dead = replace(BAA.dead, is.na(BAA.dead), 0))
# # join.both.2002 %>% select('pltID') %>% unique()

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

#Combine the 1999-2002 and 2012-2015 plots values
join.second.2002$time.period <- '1999-2002'
join.second.2015$time.period <- '2012-2015'
all.second <- rbind(join.second.2002, join.second.2015)
all.second
#Combine all the all the data together for drought regions
all.both$sequence <- 'Both Droughts'
all.second$sequence <- '2nd Drought Only'
all.forest <- rbind(all.both, all.second)
all.forest

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

#Calculate the percent mortality rate
all.forest.type$BAA.mort <- all.forest.type$BAA.dead.sum / (all.forest.type$BAA.all.sum) * 100

#Replace NA values of BAA mort with 0, I don't think I should do this because if there is nothing present there can't be a mortality rate
# all.forest.type <- all.forest.type %>% mutate(BAA.mort = replace(BAA.mort, is.na(BAA.mort), 0))


all.forest.type.summary <- all.forest.type %>% #mutate(BAA.mort = replace(BAA.mort, is.na(BAA.mort), 0)) %>%
                                                      mutate(live = case_when(BAA.all.sum > 0 ~ 1, BAA.all.sum == 0 ~ 0), 
                                                      dead = case_when(BAA.mort > 0 ~ 1, BAA.mort == 0 | is.na(BAA.mort) ~ 0)) %>%
                                                group_by(tree_type, time.period, sequence) %>% summarize(BAA.all.mean = mean(BAA.all.sum), BAA.all.sd = sd(BAA.all.sum),
                                                                             BAA.live.mean = mean(BAA.live.sum), BAA.sd = sd(BAA.live.sum),
                                                                             BAA.dead.mean = mean(BAA.dead.sum), BAA.dead.sd = sd(BAA.dead.sum),
                                                                             BAA.mort.mean = mean(BAA.mort), BAA.mort.sd = sd(BAA.mort), count = n(),
                                                                             live.count = sum(live), dead.count = sum(dead))

#Calculate the conifer fraction to eliminate forest with no conifers from the sample
all.summary <- all.forest %>% group_by(time.period, sequence, pltID) %>% summarize(BAA.all = sum(BAA.all), BAA.live = sum(BAA), BAA.dead.sum = sum(BAA.dead), BAA.mort = sum(BAA.dead) / sum(BAA.all) * 100)
conifer.summary <- all.forest %>% filter(tree_type %in% c('pine', 'fir', 'juniper', 'cedar', 'other conifer')) %>% 
                                  group_by(time.period, sequence, pltID) %>% summarize(BAA.all.conifer = sum(BAA.all), BAA.live.conifer = sum(BAA), BAA.dead.conifer = sum(BAA.dead))
join.summary <- left_join(all.summary, conifer.summary, by = c('pltID', 'time.period', 'sequence'))
join.summary$conifer.frac <- join.summary$BAA.all.conifer / join.summary$BAA.all * 100

plots <- join.summary %>% filter(conifer.frac >= 5) %>% ungroup() %>% pull(pltID) %>% unique()
plots
all.forest.type %>% filter(pltID %in% plots)
conifer.forest.type.summary <- all.forest.type %>% filter(pltID %in% plots) %>% #mutate(BAA.mort = replace(BAA.mort, is.na(BAA.mort), 0)) %>%
  mutate(live = case_when(BAA.all.sum > 0 ~ 1, BAA.all.sum == 0 ~ 0), 
         dead = case_when(BAA.mort > 0 ~ 1, BAA.mort == 0 | is.na(BAA.mort) ~ 0)) %>%
  group_by(tree_type, time.period, sequence) %>% summarize(BAA.all.mean = mean(BAA.all.sum), BAA.all.sd = sd(BAA.all.sum),
                                                           BAA.live.mean = mean(BAA.live.sum), BAA.sd = sd(BAA.live.sum),
                                                           BAA.dead.mean = mean(BAA.dead.sum), BAA.dead.sd = sd(BAA.dead.sum),
                                                           BAA.mort.mean = mean(BAA.mort), BAA.mort.sd = sd(BAA.mort), count = n(),
                                                           live.count = sum(live), dead.count = sum(dead))


#Replace the NAs with 0s
join.summary <- join.summary %>% mutate(BAA.all.conifer = replace(BAA.all.conifer, is.na(BAA.all.conifer), 0))
join.summary$conifer.frac <- join.summary$BAA.all.conifer / join.summary$BAA.all * 100
join.summary
#Create labels for the bar chart (a)
p1_texta <- data.frame(label = c("a", "b", "b", "a"),
                       sequence   = c('Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only'),
                       # tree_type = c('pine/fir', 'other tree', 'pine/fir', 'other tree', 
                       #               'pine/fir', 'other tree', 'pine/fir', 'other tree'),
                       y     = c(4.25, 1.0, 0.6, 3.15),
                       x     = c(1, 2, 1, 2)
)

#Letters to indicate sample sizes
p1_textb <- data.frame(label = c("n = 58", "n = 39", "n = 259", "n = 241"),
                       sequence   = c('Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only'),
                       y     = c(3.95, 0.7, 0.3, 2.85),
                       x     = c(1, 2, 1, 2)
)

#Summary of Total Dead Basal Area
p1 <- ggbarplot(all.forest %>% filter(pltID %in% plots) %>% group_by(time.period, sequence, pltID) %>% summarize(BAA.all = sum(BAA.all), BAA.live = sum(BAA), BAA.dead.sum = sum(BAA.dead), BAA.mort = sum(BAA.dead) / sum(BAA.all) * 100), #%>% filter(COMMON_NAME %in% sp.both.2002[! sp.both.2002 %in% c('curlleaf mountain-mahogany', 'California live oak')]), #, (sequence == 'Both Droughts' & drought == '1999-2002') | (sequence == '2012-2015 Only' & drought == '2012-2015')), 
                x = "time.period", y = "BAA.dead.sum", position = position_dodge(), color = "sequence", fill = 'gray',
                add = "mean_se" , error.plot = "errorbar", alpha = 0.8, 
                ylab = expression('Mortality (m'^2*' ha'^-1*')'), 
                xlab = "Drought Exposure", order = c("1999-2002", "2012-2015")) + 
  theme_bw() + guides(color = 'none') +
  scale_color_manual(values = c("black", "black"),
                     aesthetics = "color") + labs(tag = 'b)') +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = c(0.76, 0.1), legend.text = element_text(size = 6), legend.title = element_text(size = 8),
        legend.direction = "vertical", axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10), plot.margin = unit(c(0,0,2.5,5), "pt"),
        panel.spacing = unit(20, "pt"), plot.tag.position = c(0.54, 0.96), plot.tag = element_text(face = "bold")) +
  scale_x_discrete(labels = c("1st (1999-2002)", "2nd (2012-2015)")) +
  geom_text(data = p1_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
  geom_text(data = p1_textb, mapping = aes(x = x, y = y, label = label), size = 3) +
  geom_text(data = data.frame(label = "Mean \n+/- SE", y = 3.5, x = 1.2, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2) + 
  facet_grid(~ factor(sequence, levels = c('Both Droughts', '2nd Drought Only'))) 
p1

#Create a tree_type factor variable and sort it to make the plots
all.forest.type$tree_type.f <- all.forest.type$tree_type
all.forest.type$tree_type.f <- factor(all.forest.type$tree_type.f, levels= c('pine', 'fir', 'oak', 'juniper', 'cedar'))
# all.forest.type <- all.forest.type %>% mutate(tree_type.f = as.factor(tree.type.f(levels = c('pine', 'fir', 'oak', 'juniper', 'cedar'))))

#Add text to the plot
p2_texta <- data.frame(label = c("a", "bc", "cd", "d", "d", "cd", "cd", "cd", "d", "d",
                                 "cd", "d", "d", "d", "d","b", "b", "cd", "d", "cd"),
                       sequence   = c('Both Droughts', 'Both Droughts', 'Both Droughts', 'Both Droughts', 'Both Droughts', 'Both Droughts', 
                                      'Both Droughts', 'Both Droughts', 'Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only',
                                      '2nd Drought Only', '2nd Drought Only', '2nd Drought Only', '2nd Drought Only', '2nd Drought Only', '2nd Drought Only',
                                      '2nd Drought Only', '2nd Drought Only'),
                       # tree_type = c('pine/fir', 'other tree', 'pine/fir', 'other tree', 
                       #               'pine/fir', 'other tree', 'pine/fir', 'other tree'),
                       y     = c(2.5, 1.3, 0.55, 0.4, 0.28, 0.5, 0.28, 0.22, 0.4, 0.14, 
                                 0.24, 0.2, 0.14, 0.14, 0.16, 1.4, 1.43, 0.27, 0.14, 0.37),
                       x     = c(0.615, 0.81, 1.01, 1.2, 1.38, 1.615, 1.81, 2.01, 2.2, 2.38,
                                 0.615, 0.81, 1.01, 1.2, 1.38, 1.615, 1.81, 2.01, 2.2, 2.38)
)

#Die-off Mortality as a % of basal area
p2 <- ggbarplot(all.forest.type %>% filter(pltID %in% plots & tree_type != 'other conifer' & tree_type != 'deciduous'), #%>% filter(COMMON_NAME %in% sp.both.2002[! sp.both.2002 %in% c('curlleaf mountain-mahogany', 'California live oak')]), #, (sequence == 'Both Droughts' & drought == '1999-2002') | (sequence == '2012-2015 Only' & drought == '2012-2015')), 
                x = "time.period", y = "BAA.dead.sum", fill = 'tree_type.f', 
                color = "sequence", 
                position = position_dodge(), add = "mean_se" , error.plot = "errorbar", alpha = 0.8, 
                ylab = expression('Mortality (m'^2*' ha'^-1*')'), 
                xlab = "Drought Exposure", order = c("1999-2002", "2012-2015")) +
  theme_bw() + guides(color = 'none', fill = guide_legend(title = "Tree Type", label.position = "bottom", title.position="top", title.hjust = 0.5)) +
  scale_color_manual(values = c("black", "black"), aesthetics = "color") + labs(tag = 'd)') +
  scale_fill_discrete(labels = c("pine" = "Pine", "fir" = "Fir", "juniper" = "Juniper", "oak" = "Oak", "cedar" = "Cedar")) +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = c(0.99, 0.62), legend.text = element_text(size = 8), legend.title = element_text(size = 10),
        legend.direction = "horizontal", axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10), strip.background = element_blank(),
        strip.text.x = element_blank(), plot.margin = unit(c(2.5,0,0,5), "pt"), panel.spacing = unit(20, "pt"),
        plot.tag.position = c(0.54, 0.96), plot.tag = element_text(face = "bold")) +
  scale_x_discrete(labels = c("1st (1999-2002)", "2nd (2012-2015)")) +
  geom_text(data = p2_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
  # geom_text(data = p1_textb, mapping = aes(x = x, y = y, label = label), size = 3) +
  # geom_text(data = data.frame(label = "Mean \n+/- SE", y = 1.7, x = 1.5, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2) + 
  facet_grid(~ factor(sequence, levels = c('Both Droughts', '2nd Drought Only')))
p2

# ggbarplot(order)
#Combine the two figure panels into one	  
f1 <- ggarrange(p1, p2, ncol = 1, align = "v", labels = c("a)", "c)"), nrow = 2, heights = c(1, 0.95), common.legend = FALSE)
f1

ggsave(filename = 'Fig4_FIA_mortality_by_tree_type.png', height=14, width=16, units = 'cm', dpi=900)

#ANOVA and Tukey HSD for basal area die-off by sequence and time period
aov.dead <- aov(data = join.summary %>% filter(pltID %in% plots), #group_by(time.period, sequence, pltID) %>% 
                                      # summarize(BAA.all = sum(BAA.all), BAA.live = sum(BAA), 
                                      #           BAA.dead.sum = sum(BAA.dead), 
                                      #           BAA.mort = sum(BAA.dead) / sum(BAA.all) * 100), 
                       BAA.dead.sum ~ time.period * sequence)
summary(aov.dead)

#Tukey HSD
dead.tHSD <- TukeyHSD(aov.dead)
dead.tHSD

#ANOVA and Tukey HSD for basal are die-off by forest type, sequence, ane time period
type.aov.dead <- aov(data = all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine', 'oak', 'fir', 'juniper', 'cedar')), BAA.dead.sum ~ time.period * sequence * tree_type)
summary(type.aov.dead)

type.dead.tHSD <- TukeyHSD(type.aov.dead) 
type.dead.tHSD

#Create labels for the bar chart (a)
p3_texta <- data.frame(label = c("a", "a", "b", "b"),
                       sequence   = c('Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only'),
                       # tree_type = c('pine/fir', 'other tree', 'pine/fir', 'other tree', 
                       #               'pine/fir', 'other tree', 'pine/fir', 'other tree'),
                       y     = c(29, 21, 37.5, 37),
                       x     = c(1, 2, 1, 2)
)

#Letters to indicate sample sizes
p3_textb <- data.frame(label = c("n = 58", "n = 39", "n = 259", "n = 241"),
                       sequence   = c('Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only'),
                       y     = c(26, 18, 34.5, 34),
                       x     = c(1, 2, 1, 2)
)

#Summary of Total Basal Area
p3 <- ggbarplot(all.forest %>% filter(pltID %in% plots) %>% group_by(time.period, sequence, pltID) %>% summarize(BAA.all.sum = sum(BAA.all), BAA.live = sum(BAA), BAA.dead.sum = sum(BAA.dead), BAA.mort = sum(BAA.dead) / sum(BAA.all) * 100), #%>% filter(COMMON_NAME %in% sp.both.2002[! sp.both.2002 %in% c('curlleaf mountain-mahogany', 'California live oak')]), #, (sequence == 'Both Droughts' & drought == '1999-2002') | (sequence == '2012-2015 Only' & drought == '2012-2015')), 
                x = "time.period", y = "BAA.all.sum", position = position_dodge(), color = "sequence", fill = 'gray',
                add = "mean_se" , error.plot = "errorbar", alpha = 0.8, ylab = expression('Basal Area (m'^2*' ha'^-1*')')) + 
  theme_bw() + guides(color = 'none', fill = 'none') +
  scale_color_manual(values = c("black", "black"), aesthetics = "color") + labs(tag = 'b)') +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = c(0.76, 0.15), legend.text = element_text(size = 6), legend.title = element_text(size = 8),
        legend.direction = "vertical", axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10), plot.margin = unit(c(2.5,0,0,5), "pt"), 
        panel.spacing = unit(20, "pt"), plot.tag.position = c(0.54, 0.96), plot.tag = element_text(face = "bold")) +
  geom_text(data = p3_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
  geom_text(data = p3_textb, mapping = aes(x = x, y = y, label = label), size = 3) +
  geom_text(data = data.frame(label = "Mean \n+/- SE", y = 21.2, x = 1.2, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2) + 
  facet_grid(~ factor(sequence, levels = c('Both Droughts', '2nd Drought Only')))
p3

#Create a data frame for adding the panel 4 text.
p4_texta <- data.frame(label = c("ad", "ab", "ab", "b", "b", "ad", "ab", "ab", "ab", "b",
                                 "c", "d", "b", "b", "b", "c", "d", "b", "b", "b"),
                       sequence   = c('Both Droughts', 'Both Droughts', 'Both Droughts', 'Both Droughts', 'Both Droughts', 'Both Droughts', 
                                      'Both Droughts', 'Both Droughts', 'Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only',
                                      '2nd Drought Only', '2nd Drought Only', '2nd Drought Only', '2nd Drought Only', '2nd Drought Only', '2nd Drought Only',
                                      '2nd Drought Only', '2nd Drought Only'),
                       # tree_type = c('pine/fir', 'other tree', 'pine/fir', 'other tree', 
                       #               'pine/fir', 'other tree', 'pine/fir', 'other tree'),
                       y     = c(11.3, 6.3, 7.55, 3.75, 2.25, 8.7, 3.8, 5.3, 5.2, 1.5, 
                                 17, 12.1, 3.3, 2.8, 3.5, 19, 11.2, 3.7, 2.25, 3.8),
                       x     = c(0.615, 0.81, 1.01, 1.2, 1.38, 1.615, 1.81, 2.01, 2.2, 2.38,
                                 0.615, 0.81, 1.01, 1.2, 1.38, 1.615, 1.81, 2.01, 2.2, 2.38)
)

#Total Basal Area
p4 <- ggbarplot(all.forest.type %>% filter(pltID %in% plots & tree_type != 'other conifer' & tree_type != 'deciduous'),
                x = "time.period", y = "BAA.all.sum", position = position_dodge(), fill = 'tree_type.f', color = "sequence",
                add = "mean_se" , error.plot = "errorbar", alpha = 0.8, ylab = expression('Basal Area (m'^2*' ha'^-1*')'), 
                xlab = "Drought Exposure") +  
  theme_bw() + guides(color = 'none', fill = guide_legend(title = "Tree Type",  label.position = "bottom", title.position="top", title.hjust = 0.5)) +
  scale_color_manual(values = c("black", "black"), aesthetics = "color") + labs(tag =("d)")) +
  scale_fill_discrete(labels = c("pine" = "Pine", "fir" = "Fir", "juniper" = "Juniper", "oak" = "Oak", "cedar" = "Cedar")) +
  theme(legend.background = element_rect(colour = NA, fill = NA), 
        legend.position = c(0.3, 0.85), legend.text = element_text(size = 6), legend.title = element_text(size = 8),
        legend.direction = "horizontal",axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10), 
        strip.background = element_blank(), strip.text.x = element_blank(), plot.margin = unit(c(2.5,0,0,5), "pt"), 
        panel.spacing = unit(20, "pt"), plot.tag.position = c(0.54, 0.96), plot.tag = element_text(face = "bold")) +
  scale_x_discrete(labels = c("1st (1999-2002)", "2nd (2012-2015)")) +
  geom_text(data = p4_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
  facet_grid(~ factor(sequence, levels = c('Both Droughts', '2nd Drought Only')))
p4

f2 <- ggarrange(p3, p4, ncol = 1, labels = c('a)', 'c)'), align = "v", nrow = 2, heights = c(1, 0.95), common.legend = FALSE)
f2

ggsave(filename = 'SFig6_basal_area_boxplot.png', height=14, width=16, units = 'cm', dpi=900)

#ANOVA and Tukey HSD for total basal area sequence and time period
aov.all <- aov(data = all.forest %>% filter(pltID %in% plots) %>% group_by(time.period, sequence, pltID) %>% 
                  summarize(BAA.all.sum = sum(BAA.all), BAA.live = sum(BAA), 
                            BAA.dead.sum = sum(BAA.dead), 
                            BAA.mort = sum(BAA.dead) / sum(BAA.all) * 100), BAA.all.sum ~ time.period * sequence)
summary(aov.all)

#Tukey HSD
all.tHSD <- TukeyHSD(aov.all)
all.tHSD

#ANOVA and Tukey HSD for basal are die-off by forest type, sequence, ane time period
type.aov.all <- aov(data = all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine', 'oak', 'fir', 'juniper', 'cedar')), BAA.all.sum ~ time.period * sequence * tree_type)
summary(type.aov.all)

type.all.tHSD <- TukeyHSD(type.aov.all) 
type.all.tHSD





#Testing out a separation by Species and DIA group 
#Both Droughts for 1999-2002
#Live Basal Area
# makeClasses(ca, interval = as.numeric(10/2.54))
# ca.size <- makeClasses(ca$TREE$DIA, interval = 10/2.54)
#Create a list of size classes based on the tree table
# makeClasses(ca$TREE$DIA, interval = 10/2.54)
# ca$TREE$DIA
tpa.dia.both.all.2002 <- tpa(ca, treeType = 'live', byPlot = TRUE, bySpecies = TRUE, bySizeClass = TRUE, grpBy = DIA,# treeList = TRUE,
                             treeDomain = INVYR %in% c("2002", "2003", "2004", "2005", "2006") & DIA >= 5,
                             areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                               DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)

summary(tpa.dia.both.all.2002)

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