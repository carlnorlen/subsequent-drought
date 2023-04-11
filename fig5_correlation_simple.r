#Author: Carl A. Norlen
#Date Created: November 11, 2019
#Date Edited: April 11, 2023
#Purpose: Create regression plots (Fig 5) and SPI48 grids (Sup Figures) for publication

#Packages to load
p <- c('dplyr','tidyr','ggplot2','ggpubr','segmented', 'patchwork','RColorBrewer','gt', 'gtsummary', 
       'webshot', 'kableExtra', 'broom', 'ggpmisc', 'relaimpo', 'mlr', 'caret', 'stats', 'purrr',
        'nlme')

#Load packages
lapply(p,require,character.only=TRUE)

#Set working directory
setwd('C:/Users/can02/mystuff/subsequent-drought')

#Increase the memory limit for R. Helps with spatially explicit analyses.
memory.limit(32000)

#Read in csv data for Regression Data Sets
dir_in <- "D:\\Large_Files\\Landsat"
# all.ca <- read.csv(file.path(dir_in, "Regression_all_socal_300m_v23.csv"))
all.ca <- read.csv(file.path(dir_in, "Regression_all_socal_300m_v23_v3.csv"))
# summary(all.ca)
# summary(all.ca.test)
#Calculate the difference between SPI48 2002 and SPI48 2015
all.ca$dSPI48 <- abs(all.ca$spi48_09_2015 - all.ca$spi48_09_2002)

#Adding a drought sequence column to the data set
all.ca <- all.ca %>% mutate(drought.sequence = case_when((spi48_09_2002 <= -1.5) & (spi48_09_2015 <= -1.5) & (dSPI48 <= 0.5) ~ 'Both Droughts', 
                     (spi48_09_2015 <= -1.5) & (spi48_09_2002 > spi48_09_2015) & (spi48_09_2002 > -1.5) & (dSPI48 > 0.5) ~ '2nd Drought Only',
                     (spi48_09_2002) <= -1.5 & (spi48_09_2002 < spi48_09_2015) & (spi48_09_2015 > -1.5) & (dSPI48 > 0.5) ~ '1st Drought Only')) 

#Check if PET_4yr_2009 is postive or negative
all.ca %>% filter(drought.sequence == 'Both Droughts') %>% dplyr::select(PET_4yr_2009, PET_4yr_2002, PET_4yr_2015) %>% 
           summarize(PET_2002.mean = mean(PET_4yr_2002), PET_2009.mean = mean(PET_4yr_2009), PET_2015.mean = mean(PET_4yr_2015))

#Select columns of data
all.ca.1stDrought <- dplyr::select(all.ca, c(system.index, dNDMI_2004, PET_4yr_2002, ppt_4yr_2002, tmax_4yr_2002, ET_4yr_2002, biomass_1999, ADS_2004, spi48_09_2002, elevation, latitude, longitude, USFS_zone, drought.sequence))

#Add the year of the 1999-2002 data
all.ca.1stDrought$drought <- '1999-2002'

#Rename the columns
colnames(all.ca.1stDrought) <- c('pixel.id', 'dNDMI', 'PET_4yr', 'ppt_4yr', 'tmax_4yr', 'ET_4yr', 'biomass', 'ADS', 'spi48', 'elevation', 'latitude', 'longitude', 'USFS', 'sequence', 'drought')

#Select columns of the 2012-2015 data
all.ca.2ndDrought <- dplyr::select(all.ca, c(system.index, dNDMI_2017, PET_4yr_2015, ppt_4yr_2015, tmax_4yr_2015, ET_4yr_2015, biomass_2012, ADS_2017, spi48_09_2015, elevation, latitude, longitude, USFS_zone, drought.sequence))

#Add the year of the 2012-2015 data
all.ca.2ndDrought$drought <- '2012-2015'

#Rename the columns
colnames(all.ca.2ndDrought) <- c('pixel.id', 'dNDMI', 'PET_4yr', 'ppt_4yr', 'tmax_4yr', 'ET_4yr', 'biomass', 'ADS', 'spi48', 'elevation', 'latitude', 'longitude', 'USFS', 'sequence', 'drought')

#Combine all the data in one data frame
all.ca.combined <- rbind(all.ca.1stDrought, all.ca.2ndDrought)

#Translate the region code to text
all.ca.combined$region[all.ca.combined$USFS == 261] <- "Sierra Nevada"
all.ca.combined$region[all.ca.combined$USFS == 262] <- "Southern California"

#Convert the ADS data to categorical mortality or no mortality
#Trying the ADS Categorical with a 3 threshold
all.ca.combined <- all.ca.combined %>% mutate(ADS.cat = case_when(
                                          (ADS) >= 5 ~ 1, #mortality
                                          (ADS) < 5 ~ 0)) #no mortality

#Make drought sequence into dummy categorical variables for statistical analysis
all.ca.sample <- all.ca.combined %>% mutate(sequence.c = case_when(
                                   sequence == 'Both Droughts' ~ 0, 
                                   sequence == '2nd Drought Only' ~ 1))

#Make years into dummy variables for statistical analysis
all.ca.sample <- all.ca.sample %>% mutate(drought.c = case_when(
                                    drought == '1999-2002' ~ 0, 
                                    drought == '2012-2015' ~ 1))

#Create data sets as factors
all.ca.sample$sequence.f <- as.factor(all.ca.sample$sequence)
all.ca.sample$drought.f <- as.factor(all.ca.sample$drought)

#Select the drought sequence samples and data columns for analysis
dataset <- all.ca.sample %>% dplyr::filter(sequence == 'Both Droughts' | sequence == '2nd Drought Only') %>%
                             dplyr::select('PET_4yr', 'dNDMI', 'drought.c', 'sequence.c', 'biomass', 'pixel.id', 'tmax_4yr', 'ADS.cat')

#Convert the dummy variables to a numeric format
dataset$sequence.n <- as.numeric(dataset$sequence.c)
dataset$drought.n <- as.numeric(dataset$drought.c)

#Calculate sample size for 1999-2002 and proportion impacted by drought
all.ca.combined %>% dplyr::filter(drought == '1999-2002' & spi48 <= -1.5) %>% count()
all.ca.combined %>% dplyr::filter(drought == '1999-2002' & spi48 <= -1.5) %>% count() / all.ca.combined %>% dplyr::filter(drought == '1999-2002') %>% count()
all.ca.combined %>% dplyr::filter(drought == '1999-2002' & spi48 <= -1.5) %>% count()

#Calculate sample sizes for 2012-2015 and proportion impacted by drought
all.ca.combined %>% dplyr::filter(drought == '2012-2015' & spi48 <= -1.5) %>% count()
all.ca.combined %>% dplyr::filter(drought == '2012-2015' & spi48 <= -1.5) %>% count() / all.ca.combined %>% dplyr::filter(drought == '2012-2015') %>% count()

#Set the random number seed
set.seed(090821)

#Convert dummy variables to factors
dataset$sequence.f <- as.factor(dataset$sequence.c)
dataset$drought.f <- as.factor(dataset$drought.c)

#Create a task to undersample the drought sequence data by a factor of 0.2 for 2012-2015 Only
task = makeClassifTask(data = dataset, target = "sequence.f")

task.under = undersample(task, rate = 0.2)

#The undersampled dataset
dataset.under <- getTaskData(task.under)

#Scale data sets to help with relative importance analysis.
dataset.under$dNDMI.scale <- scale(dataset.under$dNDMI)
dataset.under$PET_4yr.scale <- scale(dataset.under$PET_4yr)
dataset.under$biomass.scale <- scale(dataset.under$biomass)
dataset.under$tmax_4yr.scale <- scale(dataset.under$tmax_4yr)
dataset.under$sequence.scale <- scale(as.numeric(dataset.under$sequence.f))
dataset.under$drought.scale <- scale(as.numeric(dataset.under$drought.f))

#The full multiple regression linear model
dndmi.under.lm = lm(data = dataset.under, 
                formula = dNDMI ~ drought.f * sequence.f + PET_4yr + tmax_4yr + biomass)

#Get the model results as a dataframe
df.dndmi.lm <- dndmi.under.lm %>% tidy() %>% as.data.frame()

#Label the columns of the data frame
df.dndmi.lm$variable <- c('Intercept', 'Time Period', 'Drought Sequence', 'four-year Pr-ET', 'four-year Temperature', 'Biomass', 'Time Period:Drought Sequence')

#Calculate the relative importance of model variables with a relative weight analysis.
dndmi.relimp <- calc.relimp(dndmi.under.lm, rela = TRUE, type = "lmg") 
dndmi.relimp 
#Add the results of the relative weigth analysis to the data frame
# df.dndmi.lm$relimp <- c(0, 0.03706293, 0.01952194, 0.36576597, 0.09262433, 0.02743045, 0.45759438) #old result, without seed
df.dndmi.lm$relimp <- c(0, 0.03699812, 0.01722664, 0.36505762, 0.09369896, 0.02653059, 0.46048807)

#Covert the relative weight analysis outputs as percentages
df.dndmi.lm$relimp.pct <- df.dndmi.lm$relimp * 100

#Create a table of the relative weight analysis results
df.dndmi.tbl <- df.dndmi.lm %>% dplyr::select(variable, estimate, std.error, statistic, p.value, relimp.pct)

#Update the relative weight analysis column names
colnames(df.dndmi.tbl) <- c('Variable', 'Coefficient', 'Standard Error', 'T-Statistic', 'p-value', 'Relative Importance (%)')

#Create the data table with a title
tba <- kbl(df.dndmi.tbl, caption = "Table S6: Die-off (dNDMI) Multiple Linear Regression", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)

#Export the data table as a .PNG file
as_image(x = tba, width = 6, file = "STable6_multiple_regression_results.png", zoom = 5.0)

#Convert the dummy data back to a numeric format
dataset.under$sequence.f <- as.numeric(dataset.under$sequence.f)
dataset.under$drought.f <- as.numeric(dataset.under$drought.f)

#Filter the data into subsets for modeling
all.ca.both.1999 <- all.ca.sample %>% filter(sequence == 'Both Droughts' & drought == '1999-2002' & !is.na(sequence))
all.ca.both.2012 <- all.ca.sample %>% filter(sequence == 'Both Droughts' & drought == '2012-2015' & !is.na(sequence)) 
all.ca.second.1999 <- all.ca.sample %>% filter(sequence == '2nd Drought Only' & drought == '1999-2002' & !is.na(sequence))
all.ca.second.2012 <- all.ca.sample %>% filter(sequence == '2nd Drought Only' & drought == '2012-2015' & !is.na(sequence))

# #Linear Models for dNDMI ~ Pr-ET
#Models for Both Droughts
all.ca.both.1999.lm <- lm(data = all.ca.both.1999, dNDMI ~ PET_4yr) # 1999-2002 Model
all.ca.both.2012.lm <- lm(data = all.ca.both.2012, dNDMI ~ PET_4yr) # 2012-2015 Model

#Models for 2012-2015 Only
all.ca.second.1999.lm <- lm(data = all.ca.second.1999, dNDMI ~ PET_4yr) # 1999-2002 Model
all.ca.second.2012.lm <- lm(data = all.ca.second.2012, dNDMI ~ PET_4yr) # 2012-2015 Model

#Calculate the sgemented models
all.ca.both.1999.seg <- segmented(all.ca.both.1999.lm)
all.ca.both.2012.seg <- segmented(all.ca.both.2012.lm)
all.ca.second.1999.seg <- segmented(all.ca.second.1999.lm)
all.ca.second.2012.seg <- segmented(all.ca.second.2012.lm)

#Add predicted dNDMI values
all.ca.both.1999$dNDMI_predict = predict(all.ca.both.1999.seg)
all.ca.both.2012$dNDMI_predict = predict(all.ca.both.2012.seg )
all.ca.second.1999$dNDMI_predict = predict(all.ca.second.1999.seg)
all.ca.second.2012$dNDMI_predict = predict(all.ca.second.2012.seg)

#Add the segmented fits and Standard Errors
#Fits
all.ca.both.1999$dNDMI.fit = broken.line(all.ca.both.1999.seg)$fit
all.ca.both.2012$dNDMI.fit = broken.line(all.ca.both.2012.seg )$fit
all.ca.second.1999$dNDMI.fit = broken.line(all.ca.second.1999.seg)$fit
all.ca.second.2012$dNDMI.fit = broken.line(all.ca.second.2012.seg)$fit

#SE fit
all.ca.both.1999$dNDMI.se.fit = broken.line(all.ca.both.1999.seg)$se.fit
all.ca.both.2012$dNDMI.se.fit = broken.line(all.ca.both.2012.seg )$se.fit
all.ca.second.1999$dNDMI.se.fit = broken.line(all.ca.second.1999.seg)$se.fit
all.ca.second.2012$dNDMI.se.fit = broken.line(all.ca.second.2012.seg)$se.fit

#Recombine the data frames with the model fitted dNDMI as a column
all.ca.models <- rbind(all.ca.both.1999, all.ca.both.2012, all.ca.second.1999, all.ca.second.2012)

#R-Squared values for the four models
r2.a  <- format(summary(all.ca.both.1999.seg)$r.squared, digits = 2) #I could switch this back to segmented
r2.b <- format(summary(all.ca.both.2012.seg)$r.squared, digits = 2)
r2.c <- format(summary(all.ca.second.1999.seg)$r.squared, digits = 2)
r2.d <- format(summary(all.ca.second.2012.seg)$r.squared, digits = 2) #I could switch this back to segmented

#Create a data.frame of R.squared values
r2.text <- data.frame(
            label = c(as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 =r2.a)))), 
                    as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = r2.b)))),
                    as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = r2.c)))),
                    as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = r2.d))))
            ),
            sequence = c('Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only'),
            drought = c('1999-2002', '2012-2015', '1999-2002', '2012-2015'),
            x = c(2500, 2500, 2500, 2500),
            y = c(-0.25, -0.25, -0.25, -0.25)
)

letter.text <- data.frame(label = c("a)", "b)", "c)", "d)"),
                       sequence   = c('Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only'),
                       drought = c('1999-2002', '2012-2015', '1999-2002',  '2012-2015'),
                       y     = c(-0.3, -0.3, -0.3, -0.3),
                       x     = c(-2400, -2400, -2400, -2400)
)

ci.label <- data.frame(label = c("", "", "", "95%\nCI"),
                       sequence   = c('Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only'),
                       drought = c('1999-2002', '2012-2015', '1999-2002',  '2012-2015'),
                       y     = c(-0.02, -0.3, -0.3, -0.02),
                       x     = c(3000, -2400, -2400, 3450)
)

#Plot dNDMI versus Pr-ET by drought sequence and time period.
p3 <- ggscatter(all.ca.models, x = "PET_4yr", y = "dNDMI", point = FALSE) +
  geom_bin2d(binwidth = c(100, 0.0075), mapping = aes(group = ..count.., alpha = ..count..)) +
  #Piecewise linear regression fit line
  geom_line(data = all.ca.models, mapping = aes(x=PET_4yr, y=dNDMI.fit), size=2, color = 'black', linetype = 'dotdash') +
  #Piecewise fit uncertainty
  geom_ribbon(data = all.ca.models, mapping = aes(x = PET_4yr, y = dNDMI.fit, ymax = dNDMI.fit + 1.96*dNDMI.se.fit, ymin = dNDMI.fit - 1.96*dNDMI.se.fit), alpha = 0.4) +
  theme_bw() + guides(alpha = 'none') +
  ylab(label = "Die-off Severity (dNDMI)") +  xlab(label = expression('Cummulative Moisture Deficit (Pr-ET; mm 4 yr'^-1*')')) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_text(data = r2.text, mapping = aes(x = x, y = y, label = label), size = 3.5, parse = TRUE) +
  geom_text(data = letter.text, mapping = aes(x = x, y = y, label = label), size = 5, fontface = "bold") +
  geom_text(data = ci.label, mapping = aes(x = x, y = y, label = label), size = 2) +
  labs(fill = "Grid Cells") +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 10, hjust = 0.5), strip.text.x = element_text(size = 10, face = 'bold'), strip.text.y = element_text(size = 10, face = 'bold')) + #Presentation text sizes.
  scale_fill_gradient2(limits = c(0,370), breaks = c(5,100,200,300), midpoint = 185, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  scale_alpha(range = c(1, 1), limits = c(5, 370), na.value = 0.4) +
  #Annotations on the panels
  geom_text(data = data.frame(y = -0.25, x = -2250, sequence = 'Both Droughts', drought = '1999-2002', label = "More\nDie-off"), 
            mapping = aes(x=x, y=y, label = label), size = 2, fontface = 2) +
  geom_text(data = data.frame(y = 0.09, x = -2250, sequence = 'Both Droughts', drought = '1999-2002', label = "Less\nDie-off"),
            mapping = aes(x=x, y=y, label = label), size = 2, fontface = 2) +
  geom_text(data = data.frame(y = 0.09, x = -2250, sequence = '2nd Drought Only', drought = '1999-2002', label = "Drier"),
            mapping = aes(x=x, y=y, label = label), size = 2, fontface = 2) +
  geom_text(data = data.frame(y = 0.09, x = 3250, sequence = '2nd Drought Only', drought = '1999-2002', label = "Wetter"),
            mapping = aes(x=x, y=y, label = label), size = 2, fontface = 2) +
  ylim(0.1, -0.3) + xlim(-2500, 3500) +
  facet_grid(factor(sequence, levels = c('Both Droughts', '2nd Drought Only')) ~ drought,
                                                   labeller = as_labeller(c('1999-2002'='Response During 1st Period', '2012-2015'='Response During 2nd Period',
                                                                            'Both Droughts' = 'Exposed to Both Droughts', '2nd Drought Only' = 'Exposed to 2nd Drought Only')))
#Add a shared legend in a customized position on the figure
p4 <- p3 + theme(
  legend.background = element_rect(colour = NA, fill = NA), # This removes the white square behind the legend
  legend.justification = c(1, 0),
  legend.position = c(0.68, 0.8),
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 10),
  legend.direction = "vertical") +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 3,
                               title.position = "top", 
                               title.hjust = 0.5, 
                               ticks.colour = "black"))

p4

#Save the figure as a .png file
ggsave(filename = 'Fig5_regression_faceted_plot.png', device = 'png', height=16, width=16, units = 'cm', dpi=900)

# #Linear Models for dNDMI ~ Pr-ET
#Models for Both Droughts
biomass.both.1999.lm <- lm(data = all.ca.both.1999, dNDMI ~ biomass) # 1999-2002 Model
biomass.both.2012.lm <- lm(data = all.ca.both.2012, dNDMI ~ biomass) # 2012-2015 Model

#Models for 2012-2015 Only
biomass.second.1999.lm <- lm(data = all.ca.second.1999, dNDMI ~ biomass) # 1999-2002 Model
biomass.second.2012.lm <- lm(data = all.ca.second.2012, dNDMI ~ biomass) # 2012-2015 Model

#R-Squared values for the four models
bio.r2.a  <- format(summary(biomass.both.1999.lm)$r.squared, digits = 2) #I could switch this back to segmented
bio.r2.b <- format(summary(biomass.both.2012.lm)$r.squared, digits = 2)
bio.r2.c <- format(summary(biomass.second.1999.lm)$r.squared, digits = 2)
bio.r2.d <- format(summary(biomass.second.2012.lm)$r.squared, digits = 2)

#Add the letter labels for teh sub-plots.
bio.letter.text <- data.frame(label = c("a)", "b)", "c)", "d)"),
                          sequence   = c('Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only'),
                          drought = c('1999-2002', '2012-2015', '1999-2002',  '2012-2015'),
                          y     = c(-0.3, -0.3, -0.3, -0.3),
                          x     = c(25, 25, 25, 25)
)

bio.r2.text <- data.frame(
  label = c(as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 =bio.r2.a)))), 
            as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = bio.r2.b)))),
            as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = bio.r2.c)))),
            as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = bio.r2.d))))
  ),
  sequence = c('Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only'),
  drought = c('1999-2002', '2012-2015', '1999-2002', '2012-2015'),
  x = c(425, 425, 425, 425),
  y = c(-0.25, -0.25, -0.25, -0.25)
)

p5 <- ggscatter(all.ca.sample %>% filter(sequence == 'Both Droughts' | sequence == '2nd Drought Only'), x = "biomass", y = "dNDMI", point = FALSE) +
  geom_bin2d(binwidth = c(6.5, 0.0075), mapping = aes(group = ..count.., alpha = ..count..)) +
  geom_smooth(method = 'lm', formula = y ~ x, size = 2, se = TRUE, level = 0.95, linetype = 'dotdash', color = 'black') +
  theme_bw() + guides(alpha = FALSE) +
  ylab(label = "Die-off Severity (dNDMI)") +  xlab(label = expression('Live Aboveground Biomass (Mg ha'^-1*')')) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_text(data = bio.r2.text, mapping = aes(x = x, y = y, label = label), size = 3.5, parse = TRUE) +
  geom_text(data = bio.letter.text, mapping = aes(x = x, y = y, label = label), size = 5, fontface = "bold") +
  labs(fill = "Grid Cells") +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 10, hjust = 0.5), strip.text.x = element_text(size = 10, face = 'bold'), strip.text.y = element_text(size = 10, face = 'bold')) + #Presentation text sizes.
  scale_fill_gradient2(limits = c(0,380), breaks = c(5,100,200,300), midpoint = 190, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  scale_alpha(range = c(1, 1), limits = c(5, 370), na.value = 0.4) +
  ylim(0.1, -0.3) + xlim(0, 475) +
  facet_grid(factor(sequence, levels = c('Both Droughts', '2nd Drought Only')) ~ drought,
             labeller = as_labeller(c('1999-2002'='Response During 1st Period', '2012-2015'='Response During 2nd Period',
                                      'Both Droughts' = 'Exposed to Both Droughts', '2nd Drought Only' = 'Exposed to 2nd Drought Only')))

#Add a shared legend in a customized position on the figure
p6 <- p5 + theme(
  legend.background = element_rect(colour = NA, fill = NA), # This removes the white square behind the legend
  legend.justification = c(1, 0),
  legend.position = c(0.75, 0.8),
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 10),
  legend.direction = "vertical") +
  guides(
    fill = guide_colorbar(barwidth = 1, barheight = 3,
                               title.position = "top",
                               title.hjust = 0.5,
                               ticks.colour = "black"))

p6

ggsave(filename = 'FigS14_biomass_regression_faceted_plot.png', device = 'png', height=16, width=16, units = 'cm', dpi=900)

#Do a Tmax versus four-year Pr-ET figure
tmax.letter.text <- data.frame(label = c("a)", "b)", "c)", "d)"),
                              sequence   = c('Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only'),
                              drought = c('1999-2002', '2012-2015', '1999-2002',  '2012-2015'),
                              y     = c(-0.3, -0.3, -0.3, -0.3),
                              x     = c(1.5, 1.5, 1.5, 1.5)
)

p7 <- ggscatter(all.ca.sample %>% filter(sequence == 'Both Droughts' | sequence == '2nd Drought Only'), x = "tmax_4yr", y = "dNDMI", point = FALSE) +
  geom_bin2d(binwidth = c(0.5, 0.0075), mapping = aes(group = ..count.., alpha = ..count..)) +
  geom_smooth(method = 'lm', formula = y ~ x, size = 2, linetype = 'dotdash', se = TRUE, level = 0.95, color = 'black') +
  stat_cor(aes(label = paste(..rr.label..)), color = 'black', label.x.npc = 0.75, label.y.npc = 0.9, size = 3.5, digits = 2) +
  theme_bw() + guides(alpha = FALSE) +
  ylab(label = "Die-off Severity (dNDMI)") +  xlab(label = 'Maximum Temperature (C)') +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  # geom_text(data = bio.r2.text, mapping = aes(x = x, y = y, label = label), size = 3.5, parse = TRUE) +
  geom_text(data = tmax.letter.text, mapping = aes(x = x, y = y, label = label), size = 5, fontface = "bold") +
  labs(fill = "Grid Cells") +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 10, hjust = 0.5), strip.text.x = element_text(size = 10, face = 'bold'), strip.text.y = element_text(size = 10, face = 'bold')) + #Presentation text sizes.
  scale_fill_gradient2(limits = c(0,600), breaks = c(5,150,300,450), midpoint = 300, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  scale_alpha(range = c(1, 1), limits = c(5, 600), na.value = 0.4) +
  ylim(0.1, -0.3) + #xlim(0, 475) +
  facet_grid(factor(sequence, levels = c('Both Droughts', '2nd Drought Only')) ~ drought,
             labeller = as_labeller(c('1999-2002'='Response During 1st Period', '2012-2015'='Response During 2nd Period',
                                      'Both Droughts' = 'Exposed to Both Droughts', '2nd Drought Only' = 'Exposed to 2nd Drought Only')))

#Add a shared legend in a customized position on the figure
p8 <- p7 + theme(
  legend.background = element_rect(colour = NA, fill = NA), # This removes the white square behind the legend
  legend.justification = c(1, 0),
  legend.position = c(0.7, 0.8),
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 10),
  legend.direction = "vertical") +
  guides(
    fill = guide_colorbar(barwidth = 1, barheight = 3,
                          title.position = "top",
                          title.hjust = 0.5,
                          ticks.colour = "black"))

p8

ggsave(filename = 'FigS15_temperature_regression_faceted_plot.png', device = 'png', height=16, width=16, units = 'cm', dpi=900)

#Do the models separate by the Region
#Sierra Nevada
all.ca.both.1999.sierra <- all.ca.sample %>% filter(region == 'Sierra Nevada' & sequence == 'Both Droughts' & drought == '1999-2002' & !is.na(sequence))
all.ca.both.2012.sierra <- all.ca.sample %>% filter(region == 'Sierra Nevada' & sequence == 'Both Droughts' & drought == '2012-2015' & !is.na(sequence)) 
all.ca.second.1999.sierra <- all.ca.sample %>% filter(region == 'Sierra Nevada' & sequence == '2nd Drought Only' & drought == '1999-2002' & !is.na(sequence))
all.ca.second.2012.sierra <- all.ca.sample %>% filter(region == 'Sierra Nevada' & sequence == '2nd Drought Only' & drought == '2012-2015' & !is.na(sequence))
#Southern California
all.ca.both.1999.socal <- all.ca.sample %>% filter(region == 'Southern California' & sequence == 'Both Droughts' & drought == '1999-2002' & !is.na(sequence))
all.ca.both.2012.socal <- all.ca.sample %>% filter(region == 'Southern California' & sequence == 'Both Droughts' & drought == '2012-2015' & !is.na(sequence)) 
all.ca.second.1999.socal <- all.ca.sample %>% filter(region == 'Southern California' & sequence == '2nd Drought Only' & drought == '1999-2002' & !is.na(sequence))
all.ca.second.2012.socal <- all.ca.sample %>% filter(region == 'Southern California' & sequence == '2nd Drought Only' & drought == '2012-2015' & !is.na(sequence))

# #Linear Models for dNDMI ~ Pr-ET
#Models for Both Droughts
all.ca.both.1999.sierra.lm <- lm(data = all.ca.both.1999.sierra, dNDMI ~ PET_4yr) # 1999-2002 Model
all.ca.both.2012.sierra.lm <- lm(data = all.ca.both.2012.sierra, dNDMI ~ PET_4yr) # 2012-2015 Model
all.ca.both.1999.socal.lm <- lm(data = all.ca.both.1999.socal, dNDMI ~ PET_4yr) # 1999-2002 Model
all.ca.both.2012.socal.lm <- lm(data = all.ca.both.2012.socal, dNDMI ~ PET_4yr) # 2012-2015 Model

#Models for 2012-2015 Only
all.ca.second.1999.sierra.lm <- lm(data = all.ca.second.1999.sierra, dNDMI ~ PET_4yr) # 1999-2002 Model
all.ca.second.2012.sierra.lm <- lm(data = all.ca.second.2012.sierra, dNDMI ~ PET_4yr) # 2012-2015 Model
all.ca.second.1999.socal.lm <- lm(data = all.ca.second.1999.socal, dNDMI ~ PET_4yr) # 1999-2002 Model
all.ca.second.2012.socal.lm <- lm(data = all.ca.second.2012.socal, dNDMI ~ PET_4yr) # 2012-2015 Model

#Calculate the sgemented model
all.ca.both.1999.sierra.seg <- segmented(all.ca.both.1999.sierra.lm)
all.ca.both.2012.sierra.seg <- segmented(all.ca.both.2012.sierra.lm)
all.ca.second.1999.sierra.seg <- segmented(all.ca.second.1999.sierra.lm)
all.ca.second.2012.sierra.seg <- segmented(all.ca.second.2012.sierra.lm)
all.ca.both.1999.socal.seg <- segmented(all.ca.both.1999.socal.lm)
all.ca.both.2012.socal.seg <- segmented(all.ca.both.2012.socal.lm)
all.ca.second.1999.socal.seg <- segmented(all.ca.second.1999.socal.lm)
all.ca.second.2012.socal.seg <- segmented(all.ca.second.2012.socal.lm)

#Add predicted dNDMI values
all.ca.both.1999.sierra$dNDMI_predict = predict(all.ca.both.1999.sierra.seg)
all.ca.both.2012.sierra$dNDMI_predict = predict(all.ca.both.2012.sierra.seg)
all.ca.second.1999.sierra$dNDMI_predict = predict(all.ca.second.1999.sierra.seg)
all.ca.second.2012.sierra$dNDMI_predict = predict(all.ca.second.2012.sierra.seg)
all.ca.both.1999.socal$dNDMI_predict = predict(all.ca.both.1999.socal.seg)
all.ca.both.2012.socal$dNDMI_predict = predict(all.ca.both.2012.socal.seg)
all.ca.second.1999.socal$dNDMI_predict = predict(all.ca.second.1999.socal.seg)
all.ca.second.2012.socal$dNDMI_predict = predict(all.ca.second.2012.socal.seg)


#Recombine the data frames with the model fitted dNDMI as a column
all.ca.models.region <- rbind(all.ca.both.1999.sierra, all.ca.both.2012.sierra, all.ca.second.1999.sierra, all.ca.second.2012.sierra,
                       all.ca.both.1999.socal, all.ca.both.2012.socal, all.ca.second.1999.socal, all.ca.second.2012.socal)

#Create Letter labels
reg.letter.text <- data.frame(label = c("b)", "d)", "f)", "h)",
                                        "a)", "c)", "e)", "g)"),
                              sequence   = c('Both Droughts', 'Both Droughts', 
                                             '2nd Drought Only', '2nd Drought Only',
                                             'Both Droughts', 'Both Droughts', 
                                             '2nd Drought Only', '2nd Drought Only'),
                              drought = c('1999-2002', '2012-2015', 
                                          '1999-2002',  '2012-2015',
                                          '1999-2002', '2012-2015', 
                                          '1999-2002',  '2012-2015'),
                              region =   c('Sierra Nevada', 'Sierra Nevada','Sierra Nevada','Sierra Nevada',
                                                    'Southern California', 'Southern California','Southern California','Southern California'),
                              y     = c(-0.3, -0.3, -0.3, -0.3,
                                        -0.3, -0.3, -0.3, -0.3),
                              x     = c(-2400, -2400, -2400, -2400,
                                        -2400, -2400, -2400, -2400)
)


# summary(all.ca.models)
p9 <- ggscatter(all.ca.sample %>% filter(sequence %in% c('Both Droughts', '2nd Drought Only')), x = "PET_4yr", y = "dNDMI", point = FALSE) +
  geom_bin2d(binwidth = c(100, 0.0075), mapping = aes(group = ..count.., alpha = ..count..)) +
  theme_bw() + guides(alpha = FALSE) +
  ylab(label = "Die-off Severity (dNDMI)") +  xlab(label = expression('Cummulative Moisture Deficit (Pr-ET; mm 4 yr'^-1*')')) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  # geom_text(data = reg.r2.text, mapping = aes(x = x, y = y, label = label), size = 3.5, parse = TRUE) +
  geom_text(data = reg.letter.text, mapping = aes(x = x, y = y, label = label), size = 5, fontface = "bold") +
  labs(fill = "Grid Cells") +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 10, hjust = 0.5), strip.text.x = element_text(size = 10, face = 'bold'), strip.text.y = element_text(size = 10, face = 'bold')) + #Presentation text sizes.
  scale_fill_gradient2(limits = c(0,370), breaks = c(5,100,200,300), midpoint = 185, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  scale_alpha(range = c(1, 1), limits = c(5, 370), na.value = 0.4) +
  ylim(0.1, -0.3) + xlim(-2500, 3500) +
  facet_grid(factor(sequence, levels = c('Both Droughts', '2nd Drought Only')) ~ drought + factor(region, levels = c('Southern California', 'Sierra Nevada')),
             labeller = as_labeller(c('1999-2002'='Response During 1st Period', '2012-2015'='Response During 2nd Period',
                                      'Both Droughts' = 'Exposed to Both Droughts', '2nd Drought Only' = 'Exposed to 2nd Drought Only',
                                      'Sierra Nevada' = 'Sierra Nevada', 'Southern California' = 'Southern California')))

#Add a shared legend in a customized position on the figure
p10 <- p9 + theme(
  legend.background = element_rect(colour = NA, fill = NA), # This removes the white square behind the legend
  legend.justification = c(1, 0),
  legend.position = c(0.85, 0.78),
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 10),
  legend.direction = "vertical") +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 3,
                               title.position = "top", 
                               title.hjust = 0.5, 
                               ticks.colour = "black"))

p10

ggsave(filename = 'FigS18_PET_regression_with_region_faceted_plot.png', device = 'png', height=16, width=24, units = 'cm', dpi=900)

#Models for four-year ET versus dNDMI
ET_4yr.both.1999.lm <- lm(data = all.ca.both.1999, dNDMI ~ ET_4yr) # 1999-2002 Model
ET_4yr.both.2012.lm <- lm(data = all.ca.both.2012, dNDMI ~ ET_4yr) # 2012-2015 Model

#Models for 2012-2015 Only
ET_4yr.second.1999.lm <- lm(data = all.ca.second.1999, dNDMI ~ ET_4yr) # 1999-2002 Model
ET_4yr.second.2012.lm <- lm(data = all.ca.second.2012, dNDMI ~ ET_4yr) # 2012-2015 Model

#R-Squared values for the four models
ET.r2.a  <- format(summary(ET_4yr.both.1999.lm)$r.squared, digits = 2) #I could switch this back to segmented
ET.r2.b <- format(summary(ET_4yr.both.2012.lm)$r.squared, digits = 2)
ET.r2.c <- format(summary(ET_4yr.second.1999.lm)$r.squared, digits = 2)
ET.r2.d <- format(summary(ET_4yr.second.2012.lm)$r.squared, digits = 2)

#Add the letter labels for teh sub-plots.
ET.letter.text <- data.frame(label = c("a)", "b)", "c)", "d)"),
                              sequence   = c('Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only'),
                              drought = c('1999-2002', '2012-2015', '1999-2002',  '2012-2015'),
                              y     = c(-0.32, -0.32, -0.32, -0.32),
                              x     = c(350, 350, 350, 350)
)

ET.r2.text <- data.frame(
  label = c(as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 =bio.r2.a)))), 
            as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = bio.r2.b)))),
            as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = bio.r2.c)))),
            as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = bio.r2.d))))
  ),
  sequence = c('Both Droughts', 'Both Droughts', '2nd Drought Only', '2nd Drought Only'),
  drought = c('1999-2002', '2012-2015', '1999-2002', '2012-2015'),
  x = c(425, 425, 425, 425),
  y = c(-0.3, -0.3, -0.3, -0.3)
)


#four-year ET vs. dNDMI faceted plot regression
p11 <- ggscatter(all.ca.sample %>% filter(sequence == 'Both Droughts' | sequence == '2nd Drought Only'), x = "ET_4yr", y = "dNDMI", point = FALSE) +
  geom_bin2d(binwidth = c(100, 0.0075), mapping = aes(group = ..count.., alpha = ..count..)) +
  geom_smooth(method = 'lm', formula = y ~ x, size = 2, linetype = 'dotdash', se = TRUE, level = 0.95, color = 'black') +
  stat_cor(aes(label = paste(..rr.label..)), color = 'black', label.x.npc = 0.15, label.y.npc = 0.9, size = 3.5, digits = 2) +
  theme_bw() + guides(alpha = 'none') +
  ylab(label = "Die-off Severity (dNDMI)") +  xlab(label = expression('four-year ET (mm 4yr'^-1*')')) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  # geom_text(data = bio.r2.text, mapping = aes(x = x, y = y, label = label), size = 3.5, parse = TRUE) +
  geom_text(data = ET.letter.text, mapping = aes(x = x, y = y, label = label), size = 5, fontface = "bold") +
  labs(fill = "Grid Cells") +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 10, hjust = 0.5), strip.text.x = element_text(size = 10, face = 'bold'), strip.text.y = element_text(size = 10, face = 'bold')) + #Presentation text sizes.
  scale_fill_gradient2(limits = c(0,1500), breaks = c(5,500,1000), midpoint = 750, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  scale_alpha(range = c(1, 1), limits = c(5, 1500), na.value = 0.4) +
  ylim(0.1, -0.35) + #xlim(0, 475) +
  facet_grid(factor(sequence, levels = c('Both Droughts', '2nd Drought Only')) ~ drought,
             labeller = as_labeller(c('1999-2002'='Response During 1st Period', '2012-2015'='Response During 2nd Period',
                                      'Both Droughts' = 'Exposed to Both Droughts', '2nd Drought Only' = 'Exposed to 2nd Drought Only')))
p11
#Add a shared legend in a customized position on the figure
p12 <- p11 + theme(
  legend.background = element_rect(colour = NA, fill = NA), # This removes the white square behind the legend
  legend.justification = c(1, 0),
  legend.position = c(1.0, 0.8),
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 10),
  legend.direction = "vertical") +
  guides(
    fill = guide_colorbar(barwidth = 1, barheight = 3,
                          title.position = "top",
                          title.hjust = 0.5,
                          ticks.colour = "black"))

p12

ggsave(filename = 'SFig20_4yr_ET_regression_faceted_plot.png', device = 'png', height=16, width=16, units = 'cm', dpi=900)

#Count the samples for each panel
all.ca.sample %>% filter(drought == '1999-2002' & sequence == 'Both Droughts' & region == 'Southern California') %>% count()
all.ca.sample %>% filter(drought == '1999-2002' & sequence == 'Both Droughts' & region == 'Sierra Nevada') %>% count()
all.ca.sample %>% filter(drought == '1999-2002' & sequence == '2nd Drought Only' & region == 'Southern California') %>% count()
all.ca.sample %>% filter(drought == '1999-2002' & sequence == '2nd Drought Only' & region == 'Sierra Nevada') %>% count()

#Store filtered and sampled drought sequence data as its own vector
dataset.2 <- all.ca.sample %>% dplyr::filter(sequence == 'Both Droughts' | sequence == '2nd Drought Only') %>%
  dplyr::select('PET_4yr', 'ET_4yr', 'ppt_4yr', 'dNDMI', 'drought.f', 'sequence.f', 'biomass', 'pixel.id', 'tmax_4yr', 'ADS.cat')
dataset.2 %>% summary()
#Create a task to use with the caret package
task.2 <- makeClassifTask(data = dataset.2, target = "sequence.f")

#Under sample the data by 20%
task.2.under <- undersample(task.2, rate = 0.2)

#Get the actual data o]ut of the caret task
dataset.2.under <- getTaskData(task.2.under)

#Make variables into dummy categorical variables for statistical analysis
dataset.2.under <- dataset.2.under %>% mutate(sequence = case_when(
  sequence.f == 'Both Droughts' ~ 'Both Droughts', 
  sequence.f == '2nd Drought Only' ~ '2nd Drought Only'))

#Create column with the years for the data as a string
dataset.2.under <- dataset.2.under %>% mutate(drought = case_when(
  drought.f == '1999-2002' ~ '1999-2002', 
  drought.f == '2012-2015' ~ '2012-2015'))

#Sort the data by drought and pixel.id to prepare for Chi-squared test
dataset.3 <- dataset.2.under %>% arrange(drought.f, pixel.id, by_group = TRUE)

#Combined anova/t-test analysis for Biomass by year collected and drought sequence
biomass.aov <- aov(data = dataset.2.under, biomass ~ drought.f*sequence.f)

#Tukey Post Hoc analysis for biomass
biomass.tHSD <- TukeyHSD(biomass.aov) 

#ANOVA for Pr-ET
PET_4yr.aov <- aov(data = dataset.2.under, PET_4yr ~ drought.f*sequence.f)

#Tukey Post Hoc analysis for Pr-ET
PET_4yr.tHSD <- TukeyHSD(PET_4yr.aov)

#ANOVA for Precip
ppt_4yr.aov <- aov(data = dataset.2.under, ppt_4yr ~ drought.f*sequence.f)

#Tukey Post Hoc analysis for Pr-ET
ppt_4yr.tHSD <- TukeyHSD(ppt_4yr.aov)

#ANOVA for ET
ET_4yr.aov <- aov(data = dataset.2.under, ET_4yr ~ drought.f*sequence.f)

#Tukey Post Hoc analysis for ET
ET_4yr.tHSD <- TukeyHSD(ET_4yr.aov)

#ANOVA for Tmax
tmax_4yr.aov <- aov(data = dataset.2.under, tmax_4yr ~ drought.f*sequence.f)

#Tukey Post Hoc analysis for Tmax
tmax_4yr.tHSD <- TukeyHSD(tmax_4yr.aov)

#ANOVA for dNDMI
dNDMI.aov <- aov(data = dataset.2.under, dNDMI ~ drought.f*sequence.f)

#Tukey Post Hoc analysis for dNDMI
dNDMI.tHSD <- TukeyHSD(dNDMI.aov)

#Create a combined a list of Tukey HSD tests
tHSD <- list(biomass.tHSD, dNDMI.tHSD, 
             PET_4yr.tHSD, tmax_4yr.tHSD)

#Create a data frame of tukey HSD tests
df.tHSD <- as.data.frame(purrr::map_df(tHSD, tidy))

#Create labels for columns in HTML format
df.tHSD$variable <- c('Biomass (Mg ha<sup>-1</sup>)', 'Biomass (Mg ha<sup>-1</sup>)', 'Biomass (Mg ha<sup>-1</sup>)', 'Biomass (Mg ha<sup>-1</sup>)', 'Biomass (Mg ha<sup>-1</sup>)', 'Biomass (Mg ha<sup>-1</sup>)', 'Biomass (Mg ha<sup>-1</sup>)', 'Biomass (Mg ha<sup>-1</sup>)',
                   'dNDMI', 'dNDMI', 'dNDMI', 'dNDMI', 'dNDMI', 'dNDMI', 'dNDMI', 'dNDMI',
                   'Pr-ET (mm 4yr<sup>-1</sup>)', 'Pr-ET (mm 4yr<sup>-1</sup>)', 'Pr-ET (mm 4yr<sup>-1</sup>)', 'Pr-ET (mm 4yr<sup>-1</sup>)', 'Pr-ET (mm 4yr<sup>-1</sup>)', 'Pr-ET (mm 4yr<sup>-1</sup>)', 'Pr-ET (mm 4yr<sup>-1</sup>)', 'Pr-ET (mm 4yr<sup>-1</sup>)',
                   'Temperature (C)', 'Temperature (C)', 'Temperature (C)', 'Temperature (C)', 'Temperature (C)', 'Temperature (C)', 'Temperature (C)', 'Temperature (C)')
summary(dataset.2.under)
#Get the sample size for the two main drought sequences
dataset.2.under %>% filter(sequence == '2nd Drought Only' & drought == '1999-2002') %>% count()
dataset.2.under %>% filter(sequence == 'Both Droughts' & drought == '1999-2002') %>% count()

#Add mean values for Estimate 1
df.tHSD$estimate.1 <- c(#Biomass density
                        mean((dataset.2.under %>% filter(drought == '2012-2015'))$biomass), mean((dataset.2.under %>% filter(sequence == 'Both Droughts'))$biomass),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == '2nd Drought Only'))$biomass), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == 'Both Droughts'))$biomass),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == 'Both Droughts'))$biomass), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == 'Both Droughts'))$biomass),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == 'Both Droughts'))$biomass), mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == 'Both Droughts'))$biomass),
                        #dNDMI
                        mean((dataset.2.under %>% filter(drought == '2012-2015'))$dNDMI), mean((dataset.2.under %>% filter(sequence == 'Both Droughts'))$dNDMI),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == '2nd Drought Only'))$dNDMI), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == 'Both Droughts'))$dNDMI),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == 'Both Droughts'))$dNDMI), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == 'Both Droughts'))$dNDMI),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == 'Both Droughts'))$dNDMI), mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == 'Both Droughts'))$dNDMI),
                        #PET 4yr
                        mean((dataset.2.under %>% filter(drought == '2012-2015'))$PET_4yr), mean((dataset.2.under %>% filter(sequence == 'Both Droughts'))$PET_4yr),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == '2nd Drought Only'))$PET_4yr), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == 'Both Droughts'))$PET_4yr),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == 'Both Droughts'))$PET_4yr), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == 'Both Droughts'))$PET_4yr),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == 'Both Droughts'))$PET_4yr), mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == 'Both Droughts'))$PET_4yr),
                        #Temperature 4yr
                        mean((dataset.2.under %>% filter(drought == '2012-2015'))$tmax_4yr), mean((dataset.2.under %>% filter(sequence == 'Both Droughts'))$tmax_4yr),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == '2nd Drought Only'))$tmax_4yr), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == 'Both Droughts'))$tmax_4yr),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == 'Both Droughts'))$tmax_4yr), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == 'Both Droughts'))$tmax_4yr),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == 'Both Droughts'))$tmax_4yr), mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == 'Both Droughts'))$tmax_4yr))


#Add mean values for Estimate 2
df.tHSD$estimate.2 <- c(#Biomass
                        mean((dataset.2.under %>% filter(drought == '1999-2002'))$biomass), mean((dataset.2.under %>% filter(sequence == '2nd Drought Only'))$biomass),
                        mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == '2nd Drought Only'))$biomass), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == '2nd Drought Only'))$biomass),
                        mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == '2nd Drought Only'))$biomass), mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == '2nd Drought Only'))$biomass),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == '2nd Drought Only'))$biomass), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == 'Both Droughts'))$biomass),
                        #dNDMI
                        mean((dataset.2.under %>% filter(drought == '1999-2002'))$dNDMI), mean((dataset.2.under %>% filter(sequence == '2nd Drought Only'))$dNDMI),
                        mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == '2nd Drought Only'))$dNDMI), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == '2nd Drought Only'))$dNDMI),
                        mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == '2nd Drought Only'))$dNDMI), mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == '2nd Drought Only'))$dNDMI),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == '2nd Drought Only'))$dNDMI), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == 'Both Droughts'))$dNDMI),
                        #PrET 4yr
                        mean((dataset.2.under %>% filter(drought == '1999-2002'))$PET_4yr), mean((dataset.2.under %>% filter(sequence == '2nd Drought Only'))$PET_4yr),
                        mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == '2nd Drought Only'))$PET_4yr), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == '2nd Drought Only'))$PET_4yr),
                        mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == '2nd Drought Only'))$PET_4yr), mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == '2nd Drought Only'))$PET_4yr),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == '2nd Drought Only'))$PET_4yr), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == 'Both Droughts'))$PET_4yr),
                        #Tmax 4yr
                        mean((dataset.2.under %>% filter(drought == '1999-2002'))$tmax_4yr), mean((dataset.2.under %>% filter(sequence == '2nd Drought Only'))$tmax_4yr),
                        mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == '2nd Drought Only'))$tmax_4yr), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == '2nd Drought Only'))$tmax_4yr),
                        mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == '2nd Drought Only'))$tmax_4yr), mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == '2nd Drought Only'))$tmax_4yr),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == '2nd Drought Only'))$tmax_4yr), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == 'Both Droughts'))$tmax_4yr))

#Select and sort the tukey HSD columns and 
df.tHSD.pub <- df.tHSD %>% dplyr::select(variable, contrast, estimate.1, estimate.2, estimate, conf.low, conf.high, adj.p.value)

#Name the columns of the data frame
colnames(df.tHSD.pub) <- c('Variable', 'Comparison', 'Estimate 1', 'Estimate 2','Difference', 'Low 95% CI', 'High 95% CI', 'p-value')

#ANOVA and Tukey HSD comparing by time period and drought sequence
tb1 <- kbl(df.tHSD.pub, format = 'html', caption = "Table S2: ANOVA and Tukey HSD Results", digits = 3, escape = F) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb1, width = 10, file = "STable2_tHSD_test_results.png", zoom = 5.0)  

#Create same table as Table 2, but add a percent change column, not included with manuscript
#Calculate proportion differences from Tukey HSD tests
df.tHSD$diff.pct <- df.tHSD$estimate / df.tHSD$estimate.1 * 100

df.tHSD$low.pct <- df.tHSD$conf.low / df.tHSD$estimate.1 * 100

df.tHSD$high.pct <- df.tHSD$conf.high / df.tHSD$estimate.1 * 100

#Select and sort the tukey HSD columns and 
df.tHSD.sup <- df.tHSD %>% dplyr::select(variable, contrast, estimate.1, estimate.2, estimate, conf.low, conf.high, diff.pct, high.pct, low.pct, adj.p.value)

#Name the columns of the data frame
colnames(df.tHSD.sup) <- c('Variable', 'Comparison', 'Estimate 1', 'Estimate 2','Difference', 'Low 95% CI', 'High 95% CI', 'Difference (%)', 'Low (%)', 'High (%)', 'p-value')

#ANOVA and Tukey HSD comparing by time period and drought sequence, same as Table S2 plus % changes
tb3 <- kbl(df.tHSD.sup, format = 'html', caption = "Table S13: ANOVA and Tukey HSD Results with Percentages", digits = 3, escape = F) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb3, width = 10, file = "STable13_tHSD_test_results_with_pct.png", zoom = 5.0) 

#Filtering by drought sequence
summary(all.ca)
all.ca.both <- all.ca %>% dplyr::filter(drought.sequence == 'Both Droughts')
all.ca.2015 <- all.ca %>% dplyr::filter(drought.sequence == '2nd Drought Only')

#Paired t-tests for Geo-spatial data sets
#Biomass
biomass.both.t <- t.test(data = all.ca.both, x = all.ca.both$biomass_1999, y = all.ca.both$biomass_2012, paired = TRUE)
biomass.2015.t <- t.test(data = all.ca.2015, x = all.ca.2015$biomass_1999, y = all.ca.2015$biomass_2012, paired = TRUE)

#four-year Pr-ET
PET_4yr.both.t <- t.test(data = all.ca.both, x = all.ca.both$PET_4yr_2002, y = all.ca.both$PET_4yr_2015, paired = TRUE)
PET_4yr.2015.t <- t.test(data = all.ca.2015, x = all.ca.2015$PET_4yr_2002, y = all.ca.2015$PET_4yr_2015, paired = TRUE)

#four-year Precip
ppt_4yr.both.t <- t.test(data = all.ca.both, x = all.ca.both$ppt_4yr_2002, y = all.ca.both$ppt_4yr_2015, paired = TRUE)
ppt_4yr.2015.t <- t.test(data = all.ca.2015, x = all.ca.2015$ppt_4yr_2002, y = all.ca.2015$ppt_4yr_2015, paired = TRUE)

#four-year ET
ET_4yr.both.t <- t.test(data = all.ca.both, x = all.ca.both$ET_4yr_2002, y = all.ca.both$ET_4yr_2015, paired = TRUE)
ET_4yr.2015.t <- t.test(data = all.ca.2015, x = all.ca.2015$ET_4yr_2002, y = all.ca.2015$ET_4yr_2015, paired = TRUE)

#four-year Tmax
tmax_4yr.both.t <- t.test(data = all.ca.both, x = all.ca.both$tmax_4yr_2002, y = all.ca.both$tmax_4yr_2015, paired = TRUE)
tmax_4yr.2015.t <- t.test(data = all.ca.2015, x = all.ca.2015$tmax_4yr_2002, y = all.ca.2015$tmax_4yr_2015, paired = TRUE)

#dNDMI
dNDMI.both.t <- t.test(data = all.ca.both, x = all.ca.both$dNDMI_2004, y = all.ca.both$dNDMI_2017, paired = TRUE)
dNDMI.2015.t <- t.test(data = all.ca.2015, x = all.ca.2015$dNDMI_2004, y = all.ca.2015$dNDMI_2017, paired = TRUE)

#Combine all the t-test results in a list
t <- list(biomass.both.t, biomass.2015.t, dNDMI.both.t, dNDMI.2015.t,  
          PET_4yr.both.t, PET_4yr.2015.t, tmax_4yr.both.t, tmax_4yr.2015.t)

#Combine the t-test results in a data frame
df.t <- as.data.frame(purrr::map_df(t, tidy))

#Add a variable label column
df.t$variable <- c('Biomass (Mg ha<sup>-1</sup>)','Biomass (Mg ha<sup>-1</sup>)','dNDMI','dNDMI',
                   'Pr-ET (mm 4yr<sup>-1</sup>)','Pr-ET (mm 4yr<sup>-1</sup>)','Temperature (C)','Temperature (C)')

#Add a drought sequence column
df.t$sequence <- c('Both Droughts', '2nd Drougth Only', 'Both Droughts', '2nd Drougth Only',
                   'Both Droughts', '2nd Drougth Only','Both Droughts', '2nd Drougth Only')

#Add mean values for 1999-2002
df.t$value.1999 <- c(mean(all.ca.both$biomass_1999), mean(all.ca.2015$biomass_1999), mean(all.ca.both$dNDMI_2004), mean(all.ca.2015$dNDMI_2004),
                     mean(all.ca.both$PET_4yr_2002), mean(all.ca.2015$PET_4yr_2002), mean(all.ca.both$tmax_4yr_2002), mean(all.ca.2015$tmax_4yr_2002))

#Add mean values for 2012-2015
df.t$value.2012 <- c(mean(all.ca.both$biomass_2012), mean(all.ca.2015$biomass_2012), mean(all.ca.both$dNDMI_2017), mean(all.ca.2015$dNDMI_2017),
                     mean(all.ca.both$PET_4yr_2015), mean(all.ca.2015$PET_4yr_2015), mean(all.ca.both$tmax_4yr_2015), mean(all.ca.2015$tmax_4yr_2015))

#Select columns and put them in order
df.t.label <- df.t %>% dplyr::select(variable, sequence, value.1999, value.2012, estimate, conf.low, conf.high, statistic, p.value) 

#Give the different columns names
colnames(df.t.label) <- c('Variable','Drought Sequence', 'Estimate 1', 'Estimate 2', 'Difference', 'Low 95% CI', 'High 95% CI', 't', 'p-value')
 
#Paired T-test table
tb2 <- kbl(df.t.label, format = 'html', caption = "Table S1: Paired two-tailed T-Test Results", escape = F, digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb2, width = 10, file = "STable1_paired_t_test_results.png", zoom = 5.0)

#Create table that is the same as Table 2, but has percent change column. Not included with manuscript
#Calculate percent differences for paired t-tests
df.t$diff.pct <- df.t$estimate / df.t$value.1999 * 100

df.t$low.pct <- df.t$conf.low / df.t$value.1999 * 100

df.t$high.pct <- df.t$conf.high / df.t$value.1999 * 100

#Select columns
df.t.sup <- df.t %>% dplyr::select(variable, sequence, value.1999, value.2012, estimate, conf.low, conf.high, diff.pct, low.pct, high.pct, statistic, p.value, parameter) 

#Give the different columns names
colnames(df.t.sup) <- c('Variable','Drought Sequence', 'Estimate 1', 'Estimate 2', 'Difference', 'Low 95% CI', 'High 95% CI', 'Difference (%)', 'Low (%)', 'High (%)', 't', 'p-value', 'df (n-1)')

#Paired T-test table
tb4 <- kbl(df.t.sup, format = 'html', caption = "Table S8: Paired two-tailed T-Test Results with Percentages", escape = F, digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb4, width = 10, file = "STable8_t_test_results_with_pct.png", zoom = 5.0)

#Select out ADS die-off data for chi-square test
ADS.1999.both.alive <- all.ca.sample %>% dplyr::filter(sequence == 'Both Droughts' & drought == '1999-2002' & ADS.cat == 0) %>% count()
ADS.1999.both.alive
ADS.1999.both.dead <- all.ca.sample %>% dplyr::filter(sequence == 'Both Droughts' & drought == '1999-2002' & ADS.cat == 1) %>% count()
ADS.1999.both.dead
ADS.2015.both.alive <- all.ca.sample %>% dplyr::filter(sequence == 'Both Droughts' & drought == '2012-2015' & ADS.cat == 0) %>% count()
ADS.2015.both.alive
ADS.2015.both.dead <- all.ca.sample %>% dplyr::filter(sequence == 'Both Droughts' & drought == '2012-2015' & ADS.cat == 1) %>% count()
ADS.2015.both.dead
ADS.1999.second.alive <- all.ca.sample %>% dplyr::filter(sequence == '2nd Drought Only' & drought == '1999-2002' & ADS.cat == 0) %>% count()
ADS.1999.second.alive
ADS.1999.second.dead <- all.ca.sample %>% dplyr::filter(sequence == '2nd Drought Only' & drought == '1999-2002' & ADS.cat == 1) %>% count()
ADS.1999.second.dead
ADS.2015.second.alive <- all.ca.sample %>% dplyr::filter(sequence == '2nd Drought Only' & drought == '2012-2015' & ADS.cat == 0) %>% count()
ADS.2015.second.alive
ADS.2015.second.dead <- all.ca.sample %>% dplyr::filter(sequence == '2nd Drought Only' & drought == '2012-2015' & ADS.cat == 1) %>% count()
ADS.2015.second.dead

#Create a frequency matrix for a chi-square test
ADS <- matrix(c(as.numeric(ADS.1999.both.dead), as.numeric(ADS.2015.both.dead), as.numeric(ADS.1999.second.dead), as.numeric(ADS.2015.second.dead),
                  as.numeric(ADS.1999.both.alive), as.numeric(ADS.2015.both.alive), as.numeric(ADS.1999.second.alive), as.numeric(ADS.2015.second.alive)), ncol=2)

#Give the matrix column and row names
colnames(ADS) <- c("Mortality", "No Mortality")
rownames(ADS) <- c('Both Droughts: 1999-2002', 'Both Droughts: 2012-2015', '2nd Drought Only: 1999-2002','2nd Drought Only: 2012-2015')

#Convert the matrix to a data frame
ADS <- as.data.frame(ADS)
ADS
#Do the chi-squared test
mychi <- chisq.test(ADS)

#Print the results of the chi-square
mychi

#Print the expected result if there is no association
mychi$expected

#Calculate proportion of ADS mortality and no mortality by drought sequence and time period
#2012-2015 Only sample in 1999-2002
second.2002 <- all.ca.sample %>% filter(sequence == '2nd Drought Only' & drought == '1999-2002') %>% dplyr::select(ADS.cat) %>% sum()
second.2002.total <- all.ca.sample %>% filter(sequence == '2nd Drought Only' & drought == '1999-2002') %>% dplyr::select(ADS.cat) %>% count()
second.2002 / second.2002.total

#2nd Drought Only sample in 2012-2015
second.2015 <- all.ca.sample %>% filter(sequence == '2nd Drought Only' & drought == '2012-2015') %>% dplyr::select(ADS.cat) %>% sum()
second.2015.total <- all.ca.sample %>% filter(sequence == '2nd Drought Only' & drought == '2012-2015') %>% dplyr::select(ADS.cat) %>% count()
second.2015 / second.2015.total

#Both Droughts sample in 1999-2002
both.2002 <- all.ca.sample %>% filter(sequence == 'Both Droughts' & drought == '1999-2002') %>% dplyr::select(ADS.cat) %>% sum()
both.2002.total <- all.ca.sample %>% filter(sequence == 'Both Droughts' & drought == '1999-2002') %>% dplyr::select(ADS.cat) %>% count()
both.2002 / both.2002.total

#Both Droughts sample in 2012-2015
both.2015 <- all.ca.sample %>% filter(sequence == 'Both Droughts' & drought == '2012-2015') %>% dplyr::select(ADS.cat) %>% sum()
both.2015.total <- all.ca.sample %>% filter(sequence == 'Both Droughts' & drought == '2012-2015') %>% dplyr::select(ADS.cat) %>% count()
both.2015 / both.2015.total