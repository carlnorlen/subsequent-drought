#Author: Carl Norlen
#Date Created: November 11, 2019
#Date Edited: June 3, 2022
#Purpose: Create bar graphs for manuscript FIA analysis, testing out a new way of calculating the bar charts

# Specify necessary packages
p <- c("RSQLite","dbplyr","ggplot2","dplyr","tidyr", "ggpubr", "RColorBrewer",  
	   'gt', 'gtsummary', 'webshot', 'kableExtra', 'broom', 'rFIA', 'sf', 'grid', 'gridExtra', 'purrr')

# install.packages("rFIA")

# Load packages
lapply(p,require,character.only=TRUE)

#Set working directory
setwd('C:/Users/can02/mystuff/subsequent-drought')

#Add Data Sets
fiaCA <- 'D:\\Large_Files\\FIA\\SQLite_FIADB_CA\\2019_CSV' #Downloaedd from FIA DataMart
dir_usfs <- "D:\\Large_Files\\USFS\\data\\subsections"

#Load teh FIA data
ca <- readFIA(fiaCA)

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

#Dead basal area 
tpa.both.mort.2002 <- tpa(ca, byPlot = TRUE, treeType = 'dead', bySpecies = TRUE, treeDomain = DIA >= 5 & INVYR %in% c("2002", "2003", "2004", "2005", "2006"),
                          areaDomain = ECOSUBCD %in% c('M262Bd','M262Be','M262Bg','M262Bh','M262Bf','M262Bo','M262Bi','M262Bm','M262Bl','M262Bc','M262Bp','M261Es') &
                            DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)

#Combine multiple inventory years into one plot estimate
tpa.both.mort.2002 <- tpa.both.mort.2002 %>% dplyr::select(pltID, YEAR, COMMON_NAME, BAA, TPA) %>% dplyr::rename(BAA.dead = BAA, TPA.dead = TPA)

join.both.2002 <- left_join(tpa.both.all.2002, tpa.both.mort.2002, by = c('pltID', 'YEAR','COMMON_NAME'))

#Replace the NAs with 0s
join.both.2002 <- join.both.2002 %>% dplyr::mutate(BAA.dead = replace(BAA.dead, is.na(BAA.dead), 0))

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
tpa.both.mort.2015 <- tpa.both.mort.2015 %>% dplyr::select(pltID, YEAR, COMMON_NAME, BAA, TPA) %>% dplyr::rename(BAA.dead = BAA, TPA.dead = TPA)

join.both.2015 <- left_join(tpa.both.all.2015, tpa.both.mort.2015, by = c('pltID', 'YEAR','COMMON_NAME'))

#Replace the NAs with 0s
join.both.2015 <- join.both.2015 %>% dplyr::mutate(BAA.dead = replace(BAA.dead, is.na(BAA.dead), 0))

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

#Dead basal area 
tpa.second.mort.2002 <- tpa(ca, byPlot = TRUE, treeType = 'dead', bySpecies = TRUE, treeDomain = DIA >= 5 & MORTYR != '' & INVYR %in% c("2002", "2003", "2004", "2005", "2006"),
                          areaDomain = ECOSUBCD %in% c('M261Ep', 'M261Eq' ,'M261Eu' ,'M261Er' ,'M261Eo' ,'M262Bb' ,'M262Ba') & 
                            DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)

#Combine multiple inventory years into one plot estimate
tpa.second.mort.2002 <- tpa.second.mort.2002 %>% dplyr::select(pltID, YEAR, COMMON_NAME, BAA, TPA) %>% dplyr::rename(BAA.dead = BAA, TPA.dead = TPA)

join.second.2002 <- left_join(tpa.second.all.2002, tpa.second.mort.2002, by = c('pltID', 'YEAR','COMMON_NAME'))

#Replace the NAs with 0s
join.second.2002 <- join.second.2002 %>% dplyr::mutate(BAA.dead = replace(BAA.dead, is.na(BAA.dead), 0))

#Doing a combined estimated of the Basal Area for Both Droughts during 2012-2015
#Total basal area and tpa estimates by species
tpa.second.all.2015 <- tpa(ca, byPlot = TRUE, treeType = 'live', bySpecies = TRUE, treeDomain = INVYR %in% c("2015", "2016", "2017", "2018", "2019") & 
                           DIA >= 5, #Trying to remove any earlier MORTYR values
                         areaDomain = ECOSUBCD %in% c('M261Ep', 'M261Eq' ,'M261Eu' ,'M261Er' ,'M261Eo' ,'M262Bb' ,'M262Ba') &
                           DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)

#Dead basal area 
tpa.second.mort.2015 <- tpa(ca, byPlot = TRUE, treeType = 'dead', bySpecies = TRUE, treeDomain = INVYR %in% c("2015", "2016", "2017", "2018", "2019") & 
                              DIA >= 5 & MORTYR %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                          areaDomain = ECOSUBCD %in% c('M261Ep', 'M261Eq' ,'M261Eu' ,'M261Er' ,'M261Eo' ,'M262Bb' ,'M262Ba') &
                            DSTRBCD1 %in% c(0, 10, 11, 12, 54, 70) & COND_STATUS_CD == 1)

#Combine multiple inventory years into one plot estimate
tpa.second.mort.2015 <- tpa.second.mort.2015 %>% dplyr::select(pltID, YEAR, COMMON_NAME, BAA, TPA) %>% dplyr::rename(BAA.dead = BAA, TPA.dead = TPA)

join.second.2015 <- left_join(tpa.second.all.2015, tpa.second.mort.2015, by = c('pltID', 'YEAR','COMMON_NAME'))

#Replace the NAs with 0s
join.second.2015 <- join.second.2015 %>% dplyr::mutate(BAA.dead = replace(BAA.dead, is.na(BAA.dead), 0))

#Combine the 1999-2002 and 2012-2015 plots values
join.second.2002$time.period <- '1999-2002'
join.second.2015$time.period <- '2012-2015'
all.second <- rbind(join.second.2002, join.second.2015)

#Combine all the all the data together for drought regions
all.both$sequence <- 'Both Droughts'
all.second$sequence <- '2nd Drought Only'
all.forest <- rbind(all.both, all.second)

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

#Convert units for Basal area
all.forest$BAA.dead <- all.forest$BAA.dead * (1/4.356)
all.forest$BAA <- all.forest$BAA * (1/4.356)

#Calculate total Basal Area
all.forest$BAA.all<- all.forest$BAA + all.forest$BAA.dead

#Make tree type a factor so that I can fill in missing combinations
all.forest$tree_type <- as.factor(all.forest$tree_type)
all.forest.type <- all.forest %>% group_by(pltID, time.period, sequence, tree_type, .drop = FALSE) %>% 
                                  summarize(BAA.all.sum = sum(BAA.all), BAA.live.sum = sum(BAA), BAA.dead.sum = sum(BAA.dead)) #%>%
                                  
#Calculate the percent mortality rate
all.forest.type$BAA.mort <- all.forest.type$BAA.dead.sum / (all.forest.type$BAA.all.sum) * 100

#Summarize the data by forest type
all.forest.type.summary <- all.forest.type %>% mutate(live = case_when(BAA.all.sum > 0 ~ 1, BAA.all.sum == 0 ~ 0), 
                                                      dead = case_when(BAA.mort > 0 ~ 1, BAA.mort == 0 | is.na(BAA.mort) ~ 0)) %>%
                                                group_by(tree_type, time.period, sequence) %>% summarize(BAA.all.mean = mean(BAA.all.sum), BAA.all.sd = sd(BAA.all.sum),
                                                                             BAA.live.mean = mean(BAA.live.sum), BAA.live.sd = sd(BAA.live.sum),
                                                                             BAA.dead.mean = mean(BAA.dead.sum), BAA.dead.sd = sd(BAA.dead.sum),
                                                                             BAA.mort.mean = mean(BAA.mort), BAA.mort.sd = sd(BAA.mort), count = n(),
                                                                             live.count = sum(live), dead.count = sum(dead))

#Calculate the conifer fraction to eliminate forest with no conifers from the sample
all.summary <- all.forest %>% group_by(time.period, sequence, pltID) %>% summarize(BAA.all = sum(BAA.all), BAA.live = sum(BAA), BAA.dead.sum = sum(BAA.dead), BAA.mort = sum(BAA.dead) / sum(BAA.all) * 100)

#Summarize the data on conifers per plot
conifer.summary <- all.forest %>% filter(tree_type %in% c('pine', 'fir', 'juniper', 'cedar', 'other conifer')) %>% 
                                  group_by(time.period, sequence, pltID) %>% summarize(BAA.all.conifer = sum(BAA.all), BAA.live.conifer = sum(BAA), BAA.dead.conifer = sum(BAA.dead))

#Join the overall and conifer summaries
join.summary <- left_join(all.summary, conifer.summary, by = c('pltID', 'time.period', 'sequence'))

#Calculate the conifer fraction (with Basal Area)
join.summary$conifer.frac <- join.summary$BAA.all.conifer / join.summary$BAA.all * 100

#Figure out which plots have 5% or greater conifer fraction
plots <- join.summary %>% filter(conifer.frac >= 5) %>% ungroup() %>% pull(pltID) %>% unique()

#Do a summary for plots with at least 5% conifer fraction
all.forest.type %>% filter(pltID %in% plots)
conifer.forest.type.summary <- all.forest.type %>% filter(pltID %in% plots) %>% 
  mutate(live = case_when(BAA.all.sum > 0 ~ 1, BAA.all.sum == 0 ~ 0), 
         dead = case_when(BAA.mort > 0 ~ 1, BAA.mort == 0 | is.na(BAA.mort) ~ 0)) %>%
  group_by(tree_type, time.period, sequence) %>% summarize(BAA.all.mean = mean(BAA.all.sum), BAA.all.sd = sd(BAA.all.sum),
                                                           BAA.live.mean = mean(BAA.live.sum), BAA.live.sd = sd(BAA.live.sum),
                                                           BAA.dead.mean = mean(BAA.dead.sum), BAA.dead.sd = sd(BAA.dead.sum),
                                                           BAA.mort.mean = mean(BAA.mort), BAA.mort.sd = sd(BAA.mort), count = n(),
                                                           live.count = sum(live), dead.count = sum(dead))

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
                xlab = NULL, order = c("1999-2002", "2012-2015")) + 
  theme_bw() + guides(color = 'none') +
  scale_color_manual(values = c("black", "black"),
                     aesthetics = "color") + labs(tag = 'b)') +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = c(0.76, 0.1), legend.text = element_text(size = 6, angle = 45), legend.title = element_text(size = 8),
        legend.direction = "vertical", axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10), plot.margin = unit(c(0,0,2.5,5), "pt"),
        panel.spacing = unit(20, "pt"), plot.tag.position = c(0.54, 0.96), plot.tag = element_text(face = "bold"),
        strip.text.x = element_text(size = 10, face = 'bold')) +
  # scale_x_discrete(labels = c("Response During\n1st Period", "Response During\n2nd Period")) +
  geom_text(data = p1_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
  geom_text(data = p1_textb, mapping = aes(x = x, y = y, label = label), size = 3) +
  geom_text(data = data.frame(label = "Mean \n+/- SE", y = 3.5, x = 1.2, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2) + 
  facet_grid(~ factor(sequence, levels = c('Both Droughts', '2nd Drought Only')), 
             labeller = as_labeller(c('Both Droughts' = "Exposed to Both Droughts", '2nd Drought Only' = "Exposed to 2nd Drought Only"))) 
p1

#Create a tree_type factor variable and sort it to make the plots
all.forest.type$tree_type.f <- all.forest.type$tree_type
all.forest.type$tree_type.f <- factor(all.forest.type$tree_type.f, levels= c('pine', 'fir', 'oak', 'juniper', 'cedar'))
# all.forest.type <- all.forest.type %>% mutate(tree_type.f = as.factor(tree.type.f(levels = c('pine', 'fir', 'oak', 'juniper', 'cedar'))))

#Add text to the plot
p2_texta <- data.frame(label = c("a", "bc", "cd", "cd", "cd", "cd", "cd", "cd", "cd", "cd",
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
                xlab = NULL, order = c("1999-2002", "2012-2015")) +
  theme_bw() + guides(color = 'none', fill = guide_legend(title = "Tree Type", label.position = "bottom", title.position="top", title.hjust = 0.5)) +
  scale_color_manual(values = c("black", "black"), aesthetics = "color") + labs(tag = 'd)') +
  scale_fill_discrete(labels = c("pine" = "Pine", "fir" = "Fir", "juniper" = "Juniper", "oak" = "Oak", "cedar" = "Cedar")) +
  theme(legend.background = element_rect(colour = NA, fill = NA), legend.justification = c(1, 0),
        legend.position = c(0.45, 0.55), legend.text = element_text(size = 8, angle = 45, vjust = 0.8), legend.title = element_text(size = 10),
        legend.direction = "horizontal", axis.text.x = element_text(size = 10, color = 'black'), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10), strip.background = element_blank(),
        strip.text.x = element_blank(), plot.margin = unit(c(2.5,0,0,5), "pt"), panel.spacing = unit(20, "pt"),
        plot.tag.position = c(0.54, 0.96), plot.tag = element_text(face = "bold")) +
  scale_x_discrete(labels = c("Response During\n1st Period", "Response During\n2nd Period")) +
  geom_text(data = p2_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
  # geom_text(data = p1_textb, mapping = aes(x = x, y = y, label = label), size = 3) +
  # geom_text(data = data.frame(label = "Mean \n+/- SE", y = 1.7, x = 1.5, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2) + 
  facet_grid(~ factor(sequence, levels = c('Both Droughts', '2nd Drought Only')), 
             labeller = as_labeller(c('Both Droughts' = "Exposed to Both Droughts", '2nd Drought Only' = "Exposed to 2nd Drought Only"))) 
p2

#Combine the two figure panels into one	  
f1 <- ggarrange(p1, p2, ncol = 1, align = "v", labels = c("a)", "c)"), nrow = 2, heights = c(1, 0.95), common.legend = FALSE)
f1

ggsave(filename = 'Fig4_FIA_mortality_by_tree_type.png', height=14, width=16, units = 'cm', dpi=900)

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
        legend.position = c(0.76, 0.15), legend.text = element_text(size = 6, angle = 45), legend.title = element_text(size = 8),
        legend.direction = "vertical", axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10), plot.margin = unit(c(2.5,0,0,5), "pt"), 
        panel.spacing = unit(20, "pt"), plot.tag.position = c(0.54, 0.96), plot.tag = element_text(face = "bold"),
        strip.text.x = element_text(size = 10, face = 'bold')) +
  geom_text(data = p3_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
  geom_text(data = p3_textb, mapping = aes(x = x, y = y, label = label), size = 3) +
  geom_text(data = data.frame(label = "Mean \n+/- SE", y = 21.2, x = 1.2, sequence = 'Both Droughts'), mapping = aes(x=x, y=y, label = label), size = 2) + 
  facet_grid(~ factor(sequence, levels = c('Both Droughts', '2nd Drought Only')),
             labeller = as_labeller(c('Both Droughts' = "Exposed to Both Droughts", '2nd Drought Only' = "Exposed to 2nd Drought Only")))
p3

#Create a data frame for adding the panel 4 text.
p4_texta <- data.frame(label = c("ad", "ab", "ab", "b", "b", "abd", "ab", "ab", "ab", "b",
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
                xlab = NULL, order = c('1999-2002', '2012-2015')) +  
  theme_bw() + guides(color = 'none', fill = guide_legend(title = "Tree Type",  label.position = "bottom", title.position="top", title.hjust = 0.5)) +
  scale_color_manual(values = c("black", "black"), aesthetics = "color") + labs(tag =("d)")) +
  scale_fill_discrete(labels = c("pine" = "Pine", "fir" = "Fir", "juniper" = "Juniper", "oak" = "Oak", "cedar" = "Cedar")) +
  theme(legend.background = element_rect(colour = NA, fill = NA), 
        legend.position = c(0.3, 0.75), legend.text = element_text(size = 8, angle = 45, vjust = 0.8), legend.title = element_text(size = 8),
        legend.direction = "horizontal",axis.text.x = element_text(size = 10, color = 'black'), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10), 
        strip.background = element_blank(), strip.text.x = element_blank(), plot.margin = unit(c(2.5,0,0,5), "pt"), 
        panel.spacing = unit(20, "pt"), plot.tag.position = c(0.54, 0.96), plot.tag = element_text(face = "bold")) +
  scale_x_discrete(labels = c("Response During\n1st Period", "Response During\n2nd Period")) +
  geom_text(data = p4_texta, mapping = aes(x = x, y = y, label = label), size = 5) +
  facet_grid(~ factor(sequence, levels = c('Both Droughts', '2nd Drought Only')))
p4

f2 <- ggarrange(p3, p4, ncol = 1, labels = c('a)', 'c)'), align = "v", nrow = 2, heights = c(1, 0.95), common.legend = FALSE)
f2

ggsave(filename = 'SFig6_basal_area_boxplot.png', height=14, width=16, units = 'cm', dpi=900)

#Tree Mortality Tables
#ANOVA and Tukey HSD for basal area die-off by sequence and time period
aov.dead <- aov(data = join.summary %>% filter(pltID %in% plots), 
                BAA.dead.sum ~ time.period * sequence)

#Tukey HSD
dead.tHSD <- TukeyHSD(aov.dead)

#Basal Area Tables
#ANOVA and Tukey HSD for total basal area sequence and time period
all.forest.plot <- all.forest %>% filter(pltID %in% plots) %>% group_by(time.period, sequence, pltID) %>% 
            summarize(BAA.all.sum = sum(BAA.all), BAA.live = sum(BAA), 
            BAA.dead.sum = sum(BAA.dead), 
            BAA.mort = sum(BAA.dead) / sum(BAA.all) * 100) 

#Summarize the overall data
conifer.forest.all.summary <- all.forest.plot %>% 
                              mutate(live = case_when(BAA.all.sum > 0 ~ 1, BAA.all.sum == 0 ~ 0), 
                              dead = case_when(BAA.mort > 0 ~ 1, BAA.mort == 0 | is.na(BAA.mort) ~ 0)) %>%
                              group_by(time.period, sequence) %>% 
                              summarize(BAA.all.mean = mean(BAA.all.sum), BAA.all.sd = sd(BAA.all.sum),
                                        BAA.live.mean = mean(BAA.live), BAA.live.sd = sd(BAA.live),
                                        BAA.dead.mean = mean(BAA.dead.sum), BAA.dead.sd = sd(BAA.dead.sum),
                                        BAA.mort.mean = mean(BAA.mort), BAA.mort.sd = sd(BAA.mort), count = n(),
                                        live.count = sum(live), dead.count = sum(dead))

aov.all <- aov(data = all.forest.plot, BAA.all.sum ~ time.period * sequence)
summary(aov.all)

#Tukey HSD
all.tHSD <- TukeyHSD(aov.all)

#Combine Tukey HSD values
all.tHSD.combine <- list(dead.tHSD, #mort.tHSD, 
                 all.tHSD)

#Create a data frame
df.all.tHSD <- as.data.frame(map_df(all.tHSD.combine, tidy))

#Add a column with variable labels.
df.all.tHSD$variable <- c('Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)',
                        # 'Mortality (%)', 'Mortality (%)', 'Mortality (%)', 'Mortality (%)', 'Mortality (%)', 'Mortality (%)', 'Mortality (%)', 'Mortality (%)',
                          'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)')

#Add Estimate 1 for Tukey HSD test
#Finish updating
df.all.tHSD$estimate.1 <- c(
  #Mortality
  mean((all.forest.plot %>% filter(time.period == '2012-2015'))$BAA.dead.sum), mean((all.forest.plot %>% filter( sequence == 'Both Droughts'))$BAA.dead.sum),
  mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.dead.sum),
  mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.dead.sum), mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.dead.sum),
  mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.dead.sum), mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.dead.sum),

  #Basal Area
  mean((all.forest.plot %>% filter(time.period == '2012-2015'))$BAA.all.sum), mean((all.forest.plot %>% filter(sequence == 'Both Droughts'))$BAA.all.sum),
  mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.all.sum),
  mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.all.sum), mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.all.sum),
  mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.all.sum), mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.all.sum)
)

#Add Estimate 2 for Tukey HSD test
df.all.tHSD$estimate.2 <- c(#Mortality
  mean((all.forest.plot %>% filter(time.period == '1999-2002'))$BAA.dead.sum), mean((all.forest.plot %>% filter(sequence == '2nd Drought Only'))$BAA.dead.sum),
  mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.dead.sum),
  mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.dead.sum),
  mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.dead.sum),
  
  #Basal Area
  mean((all.forest.plot %>% filter(time.period == '1999-2002'))$BAA.all.sum), mean((all.forest.plot %>% filter(sequence == '2nd Drought Only'))$BAA.all.sum),
  mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.all.sum),
  mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.all.sum),
  mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.all.sum)
)

#Select variables and put them in order
df.all.tHSD.label <- df.all.tHSD %>% dplyr::select(variable, contrast, estimate.1, estimate.2, estimate, conf.low, conf.high, adj.p.value) #, method, alternative)

#Change the column names
colnames(df.all.tHSD.label) <- c('Variable', 'Comparison', 'Estimate 1', 'Estimate 2', 'Difference', 'Low 95% CI', 'High 95% CI', 'p-value')

#Combined ANOVA and Tukey HSD table
tb1 <- kbl(df.all.tHSD.label, format = 'html', caption = "Table S3: FIA ANOVA and Tukey HSD Results", escape = F, digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb1, width = 10, file = "STable3_FIA_tHSD_test_results.png", zoom = 5.0)

#Calculate the precentage changes based on the Tukey HSD test
df.all.tHSD$diff.pct <- df.all.tHSD$estimate / df.all.tHSD$estimate.2 * 100

df.all.tHSD$low.pct <- df.all.tHSD$conf.low / df.all.tHSD$estimate.2 * 100

df.all.tHSD$high.pct <- df.all.tHSD$conf.high / df.all.tHSD$estimate.2 * 100

#Select variables and put them in order
df.all.tHSD.sup <- df.all.tHSD %>% dplyr::select(variable, contrast, estimate.1, estimate.2, estimate, conf.low, conf.high, diff.pct, low.pct, high.pct, adj.p.value) #, method, alternative)

#Change the column names
colnames(df.all.tHSD.sup) <- c('Variable', 'Comparison', 'Estimate 1', 'Estimate 2', 'Difference', 'Low 95% CI', 'High 95% CI', 'Difference (%)', 'Low (%)', 'High (%)', 'p-value')

#Combined ANOVA and Tukey HSD table. This is the same data table as Sup Table 3, but with percentage changes. It is not included with the manuscript.
tb2 <- kbl(df.all.tHSD.sup, format = 'html', caption = "Table S7: Field ANOVA and Tukey HSD Results", escape = F, digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb2, width = 10, file = "STable7_FIA_tHSD_test_results.png", zoom = 5.0)

#Summary statistics for all the data
tb3 <- conifer.forest.all.summary %>% dplyr::select("time.period", "sequence", "count", "live.count", "dead.count", "BAA.all.mean", "BAA.all.sd",        
                                                     "BAA.live.mean", "BAA.live.sd", "BAA.dead.mean", "BAA.dead.sd") %>% 
  kbl(caption = "Table S9: FIA Overall Summary Statistics") %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb3, width = 5, file = "STable8_Southern_California_mortality_summary_statistics_v1.png", zoom = 4.0) 



#Save summary statistics by tree species. This data table is not included with the manuscript
tb4 <- conifer.forest.type.summary %>% dplyr::select("tree_type", "time.period", "sequence", "count", "live.count", "dead.count", "BAA.all.mean", "BAA.all.sd",        
                                                 "BAA.live.mean", "BAA.live.sd", "BAA.dead.mean", "BAA.dead.sd") %>% 
  kbl(caption = "Table S10: FIA Tree Species Summary Statistics") %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb4, width = 5, file = "STable9_Southern_California_mortality_summary_statistics_v1.png", zoom = 4.0) 

#Analayis by Tree Speices
#ANOVA and Tukey HSD for basal are die-off by forest type, sequence, ane time period
type.aov.all <- aov(data = all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine', 'oak', 'fir', 'juniper', 'cedar')), BAA.all.sum ~ time.period * sequence * tree_type)
summary(type.aov.all)

type.all.tHSD <- TukeyHSD(type.aov.all) 
type.all.tHSD

#ANOVA and Tukey HSD for basal are die-off by forest type, sequence, ane time period
type.aov.dead <- aov(data = all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine', 'oak', 'fir', 'juniper', 'cedar')), BAA.dead.sum ~ time.period * sequence * tree_type)
summary(type.aov.dead)

type.dead.tHSD <- TukeyHSD(type.aov.dead) 
type.dead.tHSD

#For the broad species analysis
#Combine Tukey HSD values
type.tHSD.combine <- list(type.dead.tHSD, #type.basal.dead.tHSD, 
                         type.all.tHSD)

#Create a data frame
df.type.tHSD <- as.data.frame(map_df(type.tHSD.combine, tidy))
print(df.type.tHSD)
#Add a column with variable labels.
# df.type.tHSD$variable <- c('Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)',
#                          
#                           'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)')
#Add a variable column
df.type.tHSD.1 <- df.type.tHSD %>% slice(1:298) %>% mutate(variable = 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)')
df.type.tHSD.2 <- df.type.tHSD %>% slice(299:596) %>% mutate(variable = 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)')
df.type.tHSD.combine <- rbind(df.type.tHSD.1, df.type.tHSD.2)
#Add Estimate 1 for Tukey HSD test
#Finish updating
df.all.tHSD.combine$estimate.1 <- c(#Mortality
  mean((all.forest.plot %>% filter(time.period == '2012-2015'))$BAA.dead.sum), mean((all.forest.plot %>% filter( sequence == 'Both Droughts'))$BAA.dead.sum),
  mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.dead.sum),
  mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.dead.sum), mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.dead.sum),
  mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.dead.sum), mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.dead.sum),
  #Basal Area
  mean((all.forest.plot %>% filter(time.period == '2012-2015'))$BAA.all.sum), mean((all.forest.plot %>% filter(sequence == 'Both Droughts'))$BAA.all.sum),
  mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.all.sum),
  mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.all.sum), mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.all.sum),
  mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.all.sum), mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.all.sum)
)

#Add Estimate 2 for Tukey HSD test
df.all.tHSD.combine$estimate.2 <- c(#Mortality
  mean((all.forest.plot %>% filter(time.period == '1999-2002'))$BAA.dead.sum), mean((all.forest.plot %>% filter(sequence == '2nd Drought Only'))$BAA.dead.sum),
  mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.dead.sum),
  mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.dead.sum),
  mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.dead.sum),
  #Basal Area
  mean((all.forest.plot %>% filter(time.period == '1999-2002'))$BAA.all.sum), mean((all.forest.plot %>% filter(sequence == '2nd Drought Only'))$BAA.all.sum),
  mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.all.sum),
  mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.all.sum),
  mean((all.forest.plot %>% filter(time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.plot %>% filter(time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.all.sum)
)

#Select variables and put them in order
df.type.tHSD.label <- df.type.tHSD.combine %>% dplyr::select(variable, contrast, #estimate.1, estimate.2, 
                                                           estimate, conf.low, conf.high, adj.p.value) #, method, alternative)

#Change the column names
colnames(df.type.tHSD.label) <- c('Variable', 'Comparison', #'Estimate 1', 'Estimate 2', 
                                 'Difference', 'Low 95% CI', 'High 95% CI', 'p-value')

#Combined ANOVA and Tukey HSD table
tb5 <- kbl(df.type.tHSD.label, format = 'html', caption = "Table S10: FIA ANOVA and Tukey HSD Results by Species Group", escape = F, digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb5, width = 10, file = "STable10_FIA_tHSD_test_results_species.png", zoom = 5.0)

#Analayis by Just Pine trees
#ANOVA and Tukey HSD for basal are die-off by forest type, sequence, ane time period
pine.aov.all <- aov(data = all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine')), BAA.all.sum ~ time.period * sequence)

pine.all.tHSD <- TukeyHSD(pine.aov.all) 

#ANOVA and Tukey HSD for basal are die-off by forest type, sequence, ane time period
pine.aov.dead <- aov(data = all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine')), BAA.dead.sum ~ time.period * sequence)

pine.dead.tHSD <- TukeyHSD(pine.aov.dead) 

#For the broad species analysis
#Combine Tukey HSD values
pine.tHSD.combine <- list(pine.dead.tHSD, #type.basal.dead.tHSD, 
                          pine.all.tHSD)

#Create a data frame
df.pine.tHSD <- as.data.frame(map_df(pine.tHSD.combine, tidy))

#Add a column with variable labels.
df.pine.tHSD$variable <- c('Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)',

                          'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)')

#Finish updating
df.pine.tHSD$estimate.1 <- c(#Mortality
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & sequence == 'Both Droughts'))$BAA.dead.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.dead.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.dead.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.dead.sum),
  #Basal Area
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & sequence == 'Both Droughts'))$BAA.all.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.all.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.all.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.all.sum)
)

#Add Estimate 2 for Tukey HSD test
df.pine.tHSD$estimate.2 <- c(#Mortality
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & sequence == '2nd Drought Only'))$BAA.dead.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.dead.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.dead.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.dead.sum),
  #Basal Area
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & sequence == '2nd Drought Only'))$BAA.all.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.all.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.all.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('pine') & time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.all.sum)
)

#Select variables and put them in order
df.pine.tHSD.label <- df.pine.tHSD %>% dplyr::select(variable, contrast, estimate.1, estimate.2, 
                                                             estimate, conf.low, conf.high, adj.p.value) #, method, alternative)

#Change the column names
colnames(df.pine.tHSD.label) <- c('Variable', 'Comparison', 'Estimate 1', 'Estimate 2', 
                                  'Difference', 'Low 95% CI', 'High 95% CI', 'p-value')

#Combined ANOVA and Tukey HSD table
tb6 <- kbl(df.pine.tHSD.label, format = 'html', caption = "Table S5: Pine Tree FIA ANOVA and Tukey HSD Results", escape = F, digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb6, width = 10, file = "STable11_FIA_tHSD_test_results_pine.png", zoom = 5.0)


#Calculate the precentage changes based on the Tukey HSD test
df.pine.tHSD$diff.pct <- df.pine.tHSD$estimate / df.pine.tHSD$estimate.2 * 100

df.pine.tHSD$low.pct <- df.pine.tHSD$conf.low / df.pine.tHSD$estimate.2 * 100

df.pine.tHSD$high.pct <- df.pine.tHSD$conf.high / df.pine.tHSD$estimate.2 * 100

#Select variables and put them in order
df.pine.tHSD.sup <- df.pine.tHSD %>% dplyr::select(variable, contrast, estimate.1, estimate.2, estimate, conf.low, conf.high, diff.pct, low.pct, high.pct, adj.p.value) #, method, alternative)

#Change the column names
colnames(df.pine.tHSD.sup) <- c('Variable', 'Comparison', 'Estimate 1', 'Estimate 2', 'Difference', 'Low 95% CI', 'High 95% CI', 'Difference (%)', 'Low (%)', 'High (%)', 'p-value')

#Combined ANOVA and Tukey HSD table. This is the same data table as Sup Table 5, but with percentage changes. It is not included with the manuscript.
tb7 <- kbl(df.pine.tHSD.sup, format = 'html', caption = "Table S12: FIA ANOVA and Tukey HSD Results for Pines", escape = F, digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb7, width = 10, file = "STable12_FIA_tHSD_test_results_pine.png", zoom = 5.0)

#Analysis Just for Firs
fir.aov.all <- aov(data = all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir')), BAA.all.sum ~ time.period * sequence)

fir.all.tHSD <- TukeyHSD(fir.aov.all) 

#ANOVA and Tukey HSD for basal are die-off by forest type, sequence, ane time period
fir.aov.dead <- aov(data = all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir')), BAA.dead.sum ~ time.period * sequence)

fir.dead.tHSD <- TukeyHSD(fir.aov.dead) 

#For the broad species analysis
#Combine Tukey HSD values
fir.tHSD.combine <- list(fir.dead.tHSD, #type.basal.dead.tHSD, 
                          fir.all.tHSD)

#Create a data frame
df.fir.tHSD <- as.data.frame(map_df(fir.tHSD.combine, tidy))

#Add a column with variable labels.
df.fir.tHSD$variable <- c('Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)', 'Mortality (m<sup>2</sup> ha<sup>-1</sup>)',
                           
                           'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)', 'Basal Area (m<sup>2</sup> ha<sup>-1</sup>)')

#Add Estimate 1 for Tukey HSD test
#Finish updating
df.fir.tHSD$estimate.1 <- c(#Mortality
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & sequence == 'Both Droughts'))$BAA.dead.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.dead.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.dead.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.dead.sum),
  #Basal Area
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & sequence == 'Both Droughts'))$BAA.all.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.all.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.all.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015' & sequence == 'Both Droughts'))$BAA.all.sum)
)

#Add Estimate 2 for Tukey HSD test
df.fir.tHSD$estimate.2 <- c(#Mortality
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & sequence == '2nd Drought Only'))$BAA.dead.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.dead.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.dead.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.dead.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.dead.sum),
  #Basal Area
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & sequence == '2nd Drought Only'))$BAA.all.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.all.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.all.sum),
  mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '2012-2015' & sequence == '2nd Drought Only'))$BAA.all.sum), mean((all.forest.type %>% filter(pltID %in% plots & tree_type %in% c('fir') & time.period == '1999-2002' & sequence == 'Both Droughts'))$BAA.all.sum)
)

#Select variables and put them in order
df.fir.tHSD.label <- df.fir.tHSD %>% dplyr::select(variable, contrast, estimate.1, estimate.2, 
                                                     estimate, conf.low, conf.high, adj.p.value) #, method, alternative)

#Change the column names
colnames(df.fir.tHSD.label) <- c('Variable', 'Comparison', 'Estimate 1', 'Estimate 2', 
                                  'Difference', 'Low 95% CI', 'High 95% CI', 'p-value')

#Combined ANOVA and Tukey HSD table
tb8 <- kbl(df.fir.tHSD.label, format = 'html', caption = "Table S6: Fir Tree FIA ANOVA and Tukey HSD Results", escape = F, digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb8, width = 10, file = "STable13_FIA_tHSD_test_results_fir.png", zoom = 5.0)


#Calculate the precentage changes based on the Tukey HSD test
df.fir.tHSD$diff.pct <- df.fir.tHSD$estimate / df.fir.tHSD$estimate.2 * 100

df.fir.tHSD$low.pct <- df.fir.tHSD$conf.low / df.fir.tHSD$estimate.2 * 100

df.fir.tHSD$high.pct <- df.fir.tHSD$conf.high / df.fir.tHSD$estimate.2 * 100

#Select variables and put them in order
df.fir.tHSD.sup <- df.fir.tHSD %>% dplyr::select(variable, contrast, estimate.1, estimate.2, estimate, conf.low, conf.high, diff.pct, low.pct, high.pct, adj.p.value) #, method, alternative)

#Change the column names
colnames(df.fir.tHSD.sup) <- c('Variable', 'Comparison', 'Estimate 1', 'Estimate 2', 'Difference', 'Low 95% CI', 'High 95% CI', 'Difference (%)', 'Low (%)', 'High (%)', 'p-value')

#Combined ANOVA and Tukey HSD table. This is the same data table as Sup Table 3, but with percentage changes. It is not included with the manuscript.
tb9 <- kbl(df.fir.tHSD.sup, format = 'html', caption = "Table S14: FIA ANOVA and Tukey HSD Results for Firs", escape = F, digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb9, width = 10, file = "STable14_FIA_tHSD_test_results_fir.png", zoom = 5.0)
