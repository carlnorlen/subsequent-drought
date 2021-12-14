#Author: Carl A. Norlen
#Date Created: November 11, 2019
#Date Edited: November 17, 2021
#Purpose: Create density plot figures for supplement of various landsat derived variables and group by drought and region

#Packages to load
p <- c('dplyr','tidyr','ggplot2','ggpubr','segmented', 'patchwork','RColorBrewer','gt', 'gtsummary', 
       'webshot', 'kableExtra', 'broom', 'ggpmisc', 'relaimpo', 'mlr', 'caret', 'stats', 'purrr')
# Load packages
lapply(p,require,character.only=TRUE)
# If necessary: install/update packages
# install.packages('purrr',repo='https://cloud.r-project.org/')
# library(purrr)
#Set working directory
#setwd("/C/Users/Carl/mystuff/Goulden_Lab/Forest_Dieback/dieback/figure_set/zonal_stats/type_conifer")
#cd /C/Users/Carl/mystuff/Goulden_Lab/Forest_Dieback/dieback/figure_set/sup_figures_redo
#cd /C/Users/can02/mystuff/Goulden_Lab/Forest_Dieback/dieback/final_figure_set
#Command for calling the script in the command line: R < sfig3_correlation.r --vanilla
setwd('C:/Users/can02/mystuff/Goulden_Lab/Forest_Dieback/dieback/final_figure_set_redo')

#Read in csv data for Regression Data Sets
dir_in <- "D:\\Large_Files\\Landsat"
all.ca <- read.csv(file.path(dir_in, "Regression_all_socal_300m_v23.csv"))

all.ca$dSPI48 <- abs(all.ca$spi48_09_2015 - all.ca$spi48_09_2002)

#Convert trees/acre to trees/hectare
all.ca$ADS_2004 <- all.ca$ADS_2004 * 2.41705
all.ca$ADS_2017 <- all.ca$ADS_2017 * 2.41705

#Calculate difference in Pr-ET 4yr
all.ca$dPET_4yr <- all.ca$PET_4yr_2015 - all.ca$PET_4yr_2002

#Adding a drought sequence column to the data set
all.ca <- all.ca %>% mutate(drought.sequence = case_when((spi48_09_2002 <= -1.5) & (spi48_09_2015 <= -1.5) & (dSPI48 <= 0.5) ~ 'Both Droughts', 
                     (spi48_09_2015 <= -1.5) & (spi48_09_2002 > spi48_09_2015) & (spi48_09_2002 > -1.5) & (dSPI48 > 0.5) ~ '2012-2015 Only',
                     (spi48_09_2002) <= -1.5 & (spi48_09_2002 < spi48_09_2015) & (spi48_09_2015 > -1.5) & (dSPI48 > 0.5) ~ '1999-2002 Only')) #,
                     #spi48_09_2002 > -1.5 & spi48_09_2015 > -1.5 & dSPI48 <= 0.5 ~ 'No Droughts'))

# all.ca %>% group_by(drought.sequence) %>% count()

#California drought sequences California region
all.ca.1stDrought <- dplyr::select(all.ca, c(system.index, NDMI_1999, dNDMI_2004, dET_2004, dBiomass_2004, PET_4yr_2002, ppt_4yr_2002, tmax_4yr_2002, ET_4yr_2002, ET_1999, biomass_1999, ADS_2004, spi48_09_2002, elevation, latitude, longitude, USFS_zone, drought.sequence))
all.ca.1stDrought$drought <- '1999-2002'
colnames(all.ca.1stDrought) <- c('pixel.id','NDMI', 'dNDMI', 'dET', 'dBiomass', 'PET_4yr', 'ppt_4yr', 'tmax_4yr', 'ET_4yr', 'ET', 'biomass', 'ADS', 'spi48', 'elevation', 'latitude', 'longitude', 'USFS', 'sequence', 'drought')
# names(all.ca.1stDrought)
all.ca.2ndDrought <- dplyr::select(all.ca, c(system.index, NDMI_2012, dNDMI_2017, dET_2017, dBiomass_2017, PET_4yr_2015, ppt_4yr_2015, tmax_4yr_2015, ET_4yr_2015, ET_2012, biomass_2012, ADS_2017, spi48_09_2015, elevation, latitude, longitude, USFS_zone, drought.sequence))
all.ca.2ndDrought$drought <- '2012-2015'
colnames(all.ca.2ndDrought) <- c('pixel.id', 'NDMI', 'dNDMI', 'dET', 'dBiomass', 'PET_4yr', 'ppt_4yr', 'tmax_4yr', 'ET_4yr', 'ET', 'biomass', 'ADS', 'spi48', 'elevation', 'latitude', 'longitude', 'USFS', 'sequence','drought')
# names(all.ca.2ndDrought)
# names(all.ca.1stDrought)
all.ca.combined <- rbind(all.ca.1stDrought, all.ca.2ndDrought)

#Translate the region code to text
all.ca.combined$region[all.ca.combined$USFS == 261] <- "Sierra Nevada"
all.ca.combined$region[all.ca.combined$USFS == 262] <- "Southern California"

#Pulling out a random sample
# pixel.sample <- all.ca.combined %>% dplyr::filter(sequence != '1999-2002 Only' & !is.na(sequence)) %>% group_by(sequence) %>% dplyr::select(pixel.id) %>% unique() %>% sample_frac(0.1)

#Filter the full data set for the sample I selected
# all.ca.sample <- all.ca.combined %>% filter(pixel.id %in% pixel.sample$pixel.id)
# all.ca.sample <- all.ca.combined

all.ca.combined <- all.ca.combined %>% mutate(ADS.cat = case_when(
                                          (ADS / 2.41705) >= 5 ~ 1, 
                                          (ADS / 2.41705) < 5 ~ 0))
# summary(all.ca.sample)

#Make variables into dummy categorical variables for statistical analysis
all.ca.sample <- all.ca.combined %>% mutate(sequence.f = case_when(
                                   sequence == 'Both Droughts' ~ 0, 
                                   sequence == '2012-2015 Only' ~ 1))

all.ca.sample <- all.ca.sample %>% mutate(drought.f = case_when(
                                    drought == '1999-2002' ~ 0, 
                                    drought == '2012-2015' ~ 1))

dataset <- all.ca.sample %>% dplyr::filter(sequence == 'Both Droughts' | sequence == '2012-2015 Only') %>%
                             dplyr::select('PET_4yr', 'NDMI', 'dNDMI', 'drought.f', 'sequence.f', 'biomass', 'pixel.id', 'tmax_4yr', 'ADS.cat')

dataset$sequence.f <- as.numeric(dataset$sequence.f)
dataset$drought.f <- as.numeric(dataset$drought.f)
# summary(dataset)
# cov(dataset)
all.ca
all.ca.combined %>% dplyr::filter(drought == '1999-2002' & spi48 <= -1.5) %>% count()
all.ca.combined %>% dplyr::filter(drought == '1999-2002' & spi48 <= -1.5) %>% count() / all.ca.combined %>% dplyr::filter(drought == '1999-2002') %>% count()

summary(all.ca.combined %>% dplyr::filter(drought == '1999-2002'))

all.ca.combined %>% dplyr::filter(drought == '2012-2015' & spi48 <= -1.5) %>% count()
all.ca.combined %>% dplyr::filter(drought == '2012-2015' & spi48 <= -1.5) %>% count() / all.ca.combined %>% dplyr::filter(drought == '2012-2015') %>% count()


all.ca.sample$sequence.f <- factor(all.ca.sample$sequence.f)
all.ca.sample$drought.f <- factor(all.ca.sample$drought.f)

# summary(dataset)
# cov(dataset)

dataset$sequence.f <- as.factor(dataset$sequence.f)
dataset$drought.f <- as.factor(dataset$drought.f)

task = makeClassifTask(data = dataset, target = "sequence.f")
task.over <- undersample(task, rate = 0.2)

dataset.over <- getTaskData(task.over)

#Scale data sets to help with relative importance analysis.
dataset.over$dNDMI.scale <- scale(dataset.over$dNDMI)
dataset.over$PET_4yr.scale <- scale(dataset.over$PET_4yr)
dataset.over$biomass.scale <- scale(dataset.over$biomass)
dataset.over$tmax_4yr.scale <- scale(dataset.over$tmax_4yr)
dataset.over$sequence.scale <- scale(as.numeric(dataset.over$sequence.f))
dataset.over$drought.scale <- scale(as.numeric(dataset.over$drought.f))

#The full model
# dndmi.lm = lm(data = filter(all.ca.sample, sequence == 'Both Droughts' | sequence == '2012-2015 Only'), 
#                 formula = dNDMI ~ drought.f*sequence.f + PET_4yr + biomass + tmax_4yr)
# dndmi.both.lm = lm(data = filter(all.ca.sample, sequence == 'Both Droughts'), 
#               formula = dNDMI ~ drought.f + PET_4yr + biomass + tmax_4yr)
# dndmi.2015.lm = lm(data = filter(all.ca.sample, sequence == '2012-2015 Only'), 
#                    formula = dNDMI ~ drought.f + PET_4yr + biomass + tmax_4yr)
dndmi.over.lm = lm(data = dataset.over, 
                formula = dNDMI ~ drought.f * sequence.f + PET_4yr + tmax_4yr + biomass) # + NDMI)

summary(dndmi.over.lm)
df.dndmi.lm <- dndmi.over.lm %>% tidy() %>% as.data.frame()
df.dndmi.lm$variable <- c('Intercept', 'Time Period', 'Drought Sequence', 'four-year Pr-ET', 'four-year Temperature', 'Biomass', 'Time Period:Drought Sequence')
# calc.relimp(dndmi.over.lm, rela = TRUE, type = "lmg")
dndmi.relimp <- calc.relimp(dndmi.over.lm, rela = TRUE, type = "lmg") 
# dndmi.relimp
# dndmi.relimp$lmg
# as.data.frame(dndmi.relimp$lmg)
# rbind(data.frame(lmg = 0), dndmi.relimp$lmg)
df.dndmi.lm$relimp <- c(0, 0.03706293, 0.01952194, 0.36576597, 0.09262433, 0.02743045, 0.45759438)
# When NDMI (second to last) is included c(0, 0.03608272, 0.01951721, 0.33348437, 0.07745288, 0.02456035, 0.06889390, 0.44000856)

df.dndmi.lm$relimp.pct <- df.dndmi.lm$relimp * 100

df.dndmi.tbl <- df.dndmi.lm %>% dplyr::select(variable, estimate, std.error, statistic, p.value, relimp.pct)
# df.tHSD
colnames(df.dndmi.tbl) <- c('Variable', 'Coefficient', 'Standard Error', 'T-Statistic', 'p-value', 'Relative Importance (%)')
tba <- kbl(df.dndmi.tbl, caption = "Table S4: Die-off (dNDMI) Multiple Linear Regression", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tba, width = 6, file = "STable4_multiple_regression_results.png", zoom = 5.0)

# ADS.over.glm = glm(data = dataset.over, 
#                    formula = ADS.cat ~ drought.f * sequence.f + PET_4yr + tmax_4yr + biomass, family = 'binomial')
# summary(ADS.over.glm)
# df.ADS.glm <- ADS.over.glm %>% tidy() %>% as.data.frame()
# df.ADS.glm
# df.ADS.glm$variable <- c('Intercept', 'Time Period', 'Drought Sequence', 'four-year Pr-ET', 'four-year Temperature', 'Biomass', 'Time Period:Drought Sequence')
# 
# df.ADS.tbl <- df.ADS.glm %>% dplyr::select(variable, estimate, std.error, statistic, p.value,)
# # df.tHSD
# colnames(df.ADS.tbl) <- c('Variable', 'Coefficient', 'Standard Error', 'Z-value', 'p-value')
# tbb <- kbl(df.ADS.tbl, caption = "Table S5: Die-off (ADS) Multiple Logistic Regression", digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
# as_image(x = tbb, width = 6, file = "STable5_multiple_logistic_regression_results.png", zoom = 5.0)
# 
# nullmod <- glm(data = dataset.over, ADS.cat~1, family="binomial")
# nullmod
# logLik(nullmod)
# logLik(ADS.over.glm)
# pseudo.rsq <- 1-(logLik(ADS.over.glm)/logLik(nullmod))
# pseudo.rsq
# summary(dndmi.scale.lm)
# plot(resid(dndmi.over.lm))
dataset.over$sequence.f <- as.numeric(dataset.over$sequence.f)
dataset.over$drought.f <- as.numeric(dataset.over$drought.f)

#Do a analysis with segmented linear regression and scaled predictors
# cor(dataset.over)
# dndmi.over.seg <- segmented(obj = dndmi.over.lm, seg.Z = ~PET_4yr)
dndmi.scale.lm = lm(data = dataset.over, 
                    formula = dNDMI.scale ~ drought.scale * sequence.scale + PET_4yr.scale + tmax_4yr.scale + biomass.scale)
dndmi.scale.seg <- segmented(obj = dndmi.scale.lm, seg.Z = ~PET_4yr.scale)
summary(dndmi.scale.seg)
dndmi.scale.imp <- caret::varImp(dndmi.scale.seg, useModel = TRUE, nonpara = TRUE, scale = FALSE)
# colnames(dndmi.scale.imp)
dndmi.scale.imp$Rela.Imp <- dndmi.scale.imp$'Overall' / sum(dndmi.scale.imp$'Overall')
dndmi.scale.imp
names(dndmi.scale.seg)
dndmi.seg.tbl <- dndmi.scale.seg %>% dplyr::select('terms', 'coefficients')
data.frame(dndmi.scale.seg$coefficients)
df.dndmi.seg <- summary(dndmi.scale.seg) %>% tidy() %>% as.data.frame()
df.dndmi.lm$variable <- c('Intercept', 'Time Period', 'Drought Sequence', 'four-year Pr-ET', 'four-year Temperature', 'Biomass', 'Time Period:Drought Sequence')
# calc.relimp(dndmi.over.lm, rela = TRUE, type = "lmg")
dndmi.relimp <- calc.relimp(dndmi.over.lm, rela = TRUE, type = "lmg") 


#Filter the data into subsets for modeling
all.ca.both.1999 <- all.ca.sample %>% filter(sequence == 'Both Droughts' & drought == '1999-2002' & !is.na(sequence))
all.ca.both.2012 <- all.ca.sample %>% filter(sequence == 'Both Droughts' & drought == '2012-2015' & !is.na(sequence)) 
all.ca.second.1999 <- all.ca.sample %>% filter(sequence == '2012-2015 Only' & drought == '1999-2002' & !is.na(sequence))
all.ca.second.2012 <- all.ca.sample %>% filter(sequence == '2012-2015 Only' & drought == '2012-2015' & !is.na(sequence))

# #Linear Models for dNDMI ~ Pr-ET
all.ca.both.1999.lm <- lm(data = all.ca.both.1999, dNDMI ~ PET_4yr)
summary(all.ca.both.1999.lm)
all.ca.both.2012.lm <- lm(data = all.ca.both.2012, dNDMI ~ PET_4yr)
all.ca.second.1999.lm <- lm(data = all.ca.second.1999, dNDMI ~ PET_4yr)
all.ca.second.2012.lm <- lm(data = all.ca.second.2012, dNDMI ~ PET_4yr)

all.ca.both.1999.seg <- segmented(all.ca.both.1999.lm)
summary(all.ca.both.1999.seg)
all.ca.second.2012.seg <- segmented(all.ca.second.2012.lm)

#Add predicted dNDMI values
all.ca.both.1999$dNDMI_predict = predict(all.ca.both.1999.seg)
all.ca.both.2012$dNDMI_predict = predict(all.ca.both.2012.lm )
all.ca.second.1999$dNDMI_predict = predict(all.ca.second.1999.lm)
all.ca.second.2012$dNDMI_predict = predict(all.ca.second.2012.seg)

#Recombine the data frames with the model fitted dNDMI as a column
all.ca.models <- rbind(all.ca.both.1999, all.ca.both.2012, all.ca.second.1999, all.ca.second.2012)

#R-Squared values for the four models

r2.a  <- format(summary(all.ca.both.1999.seg)$r.squared, digits = 3)
r2.b <- format(summary(all.ca.both.2012.lm)$r.squared, digits = 2)
r2.c <- format(summary(all.ca.second.1999.lm)$r.squared, digits = 1)
r2.d <- format(summary(all.ca.second.2012.seg)$r.squared, digits = 3)

#Create a data.frame of R.squared values
r2.text <- data.frame(
            label = c(as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 =r2.a)))), 
                    as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = r2.b)))),
                    as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = r2.c)))),
                    as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = r2.d))))
            ),
            sequence = c('Both Droughts', 'Both Droughts', '2012-2015 Only', '2012-2015 Only'),
            drought = c('1999-2002', '2012-2015', '1999-2002', '2012-2015'),
            x = c(2500, 2500, 2500, 2500),
            y = c(-0.25, -0.25, -0.25, -0.25)
)

# as.character(as.expression(substitute(italic(R)^2~"="~r2, list(r2 = r2.a))))
# r2.text
# paste(R^2,' = ',format(r2.a, digits = 3), sep = "")
# expression(paste('R^2 =',format(r2.b, digits = 2), sep = ""))

#Consider adding a figure for ADS similar to the Pr-ET figure

#Plot dNDMI versus Pr-ET by drought sequence and time period.
p3 <- ggscatter(all.ca.models, x = "PET_4yr", y = "dNDMI", point = FALSE) +
  geom_bin2d(binwidth = c(100, 0.0075)) +
  geom_line(data = all.ca.models, mapping = aes(x=PET_4yr, y=dNDMI_predict), size=2, color = 'black') +
  theme_bw() +
  ylab(label = "Die-off (dNDMI)") +  xlab(label = expression('Pr-ET (mm 4 yr'^-1*')')) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_text(data = r2.text, mapping = aes(x = x, y = y, label = label), size = 3.5, parse = TRUE) +
  # stat_cor(aes(label = paste(..rr.label..)), size = 3.5, color = 'black', label.y.npc="top", label.x.npc = 0.7, r.accuracy = 0.001, p.accuracy = 0.001) +
  labs(fill = "Grid Cells") +
  # guides(color=guide_legend(title.position = "top", title.hjust = 0.5, ncol = 2, nrow=5, byrow=TRUE), shape=guide_legend(title.position = "top", title.hjust = 0.5, ncol = 2, nrow=5, byrow=TRUE)) +
  # ggtitle("1999-2002 Drought") +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 10, hjust = 0.5), strip.text.x = element_text(size = 10), strip.text.y = element_text(size = 10)) + #Presentation text sizes.
  scale_fill_gradient2(limits = c(10,370), breaks = c(10,100,200,300), midpoint = 185, low = "cornflowerblue", mid = "yellow", high = "red", na.value = 'transparent') +
  # scale_alpha(range = c(0.1, 1)) +
  ylim(0.1, -0.3) + xlim(-2500, 3500) + facet_grid(factor(sequence, levels = c('Both Droughts', '2012-2015 Only')) ~ drought)

p3 + theme(
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

ggsave(filename = 'Fig5_regression_faceted_plot.png', device = 'png', height=16, width=16, units = 'cm', dpi=900)

#Store filtered and sampled drought sequence data as its own vector
dataset.2 <- all.ca.sample %>% dplyr::filter(sequence == 'Both Droughts' | sequence == '2012-2015 Only') %>%
  dplyr::select('PET_4yr', 'ET_4yr', 'ppt_4yr', 'dNDMI', 'drought.f', 'sequence.f', 'biomass', 'pixel.id', 'tmax_4yr', 'ADS.cat')
summary(dataset.2)
task.2 <- makeClassifTask(data = dataset.2, target = "sequence.f")
task.2.under <- undersample(task.2, rate = 0.2)

dataset.2.under <- getTaskData(task.2.under)

#Make variables into dummy categorical variables for statistical analysis
dataset.2.under <- dataset.2.under %>% mutate(sequence = case_when(
  sequence.f == 0 ~ 'Both Droughts', 
  sequence.f == 1 ~ '2012-2015 Only'))

dataset.2.under <- dataset.2.under %>% mutate(drought = case_when(
  drought.f == 0 ~ '1999-2002', 
  drought.f == 1 ~ '2012-2015'))
dataset.2.under
dataset.3 <- dataset.2.under %>% arrange(drought.f, pixel.id, by_group = TRUE)
dataset.3

# all.ca.filter <- filter(all.ca.sample, !is.na(sequence) & sequence != '1999-2002 Only')
summary(dataset.2.under)
#Trying out a combined anova/t-test analysis for Biomass
biomass.aov <- aov(data = dataset.2.under, biomass ~ drought*sequence)

#Tukey Post Hoc analysis for biomass
biomass.tHSD <- TukeyHSD(biomass.aov) 

#ANOVA for Pr-ET
PET_4yr.aov <- aov(data = dataset.2.under, PET_4yr ~ drought*sequence)

#Tukey Post Hoc analysis for Pr-ET
PET_4yr.tHSD <- TukeyHSD(PET_4yr.aov)

#ANOVA for Precip
ppt_4yr.aov <- aov(data = dataset.2.under, ppt_4yr ~ drought*sequence)

#Tukey Post Hoc analysis for Pr-ET
ppt_4yr.tHSD <- TukeyHSD(ppt_4yr.aov)

#ANOVA for ET
ET_4yr.aov <- aov(data = dataset.2.under, ET_4yr ~ drought*sequence)

#Tukey Post Hoc analysis for ET
ET_4yr.tHSD <- TukeyHSD(ET_4yr.aov)

#ANOVA for Tmax
tmax_4yr.aov <- aov(data = dataset.2.under, tmax_4yr ~ drought*sequence)

#Tukey Post Hoc analysis for Tmax
tmax_4yr.tHSD <- TukeyHSD(tmax_4yr.aov)

#ANOVA for dNDMI
dNDMI.aov <- aov(data = dataset.2.under, dNDMI ~ drought*sequence)

#Tukey Post Hoc analysis for dNDMI
dNDMI.tHSD <- TukeyHSD(dNDMI.aov)
# summary(dNDMI.tHSD)
#Combine a list of tukey HSD tests
tHSD <- list(biomass.tHSD, dNDMI.tHSD, 
             PET_4yr.tHSD, tmax_4yr.tHSD)

#Create a data frame of tukey HSD tests
df.tHSD <- as.data.frame(purrr::map_df(tHSD, tidy))

#Labels for columns
df.tHSD$variable <- c('Biomass (Mg ha<sup>-1</sup>)', 'Biomass (Mg ha<sup>-1</sup>)', 'Biomass (Mg ha<sup>-1</sup>)', 'Biomass (Mg ha<sup>-1</sup>)', 'Biomass (Mg ha<sup>-1</sup>)', 'Biomass (Mg ha<sup>-1</sup>)', 'Biomass (Mg ha<sup>-1</sup>)', 'Biomass (Mg ha<sup>-1</sup>)',
                   'dNDMI', 'dNDMI', 'dNDMI', 'dNDMI', 'dNDMI', 'dNDMI', 'dNDMI', 'dNDMI',
                   'Pr-ET (mm 4yr<sup>-1</sup>)', 'Pr-ET (mm 4yr<sup>-1</sup>)', 'Pr-ET (mm 4yr<sup>-1</sup>)', 'Pr-ET (mm 4yr<sup>-1</sup>)', 'Pr-ET (mm 4yr<sup>-1</sup>)', 'Pr-ET (mm 4yr<sup>-1</sup>)', 'Pr-ET (mm 4yr<sup>-1</sup>)', 'Pr-ET (mm 4yr<sup>-1</sup>)',
                   'Temperature (C)', 'Temperature (C)', 'Temperature (C)', 'Temperature (C)', 'Temperature (C)', 'Temperature (C)', 'Temperature (C)', 'Temperature (C)')

dataset.2.under %>% filter(sequence == '2012-2015 Only' & drought == '1999-2002') %>% count()
dataset.2.under %>% filter(sequence == 'Both Droughts' & drought == '1999-2002') %>% count()
#Add mean values for 1999-2002
df.tHSD$estimate.1 <- c(#Biomass density
                        mean((dataset.2.under %>% filter(drought == '2012-2015'))$biomass), mean((dataset.2.under %>% filter(sequence == 'Both Droughts'))$biomass),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == '2012-2015 Only'))$biomass), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == 'Both Droughts'))$biomass),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == 'Both Droughts'))$biomass), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == 'Both Droughts'))$biomass),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == 'Both Droughts'))$biomass), mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == 'Both Droughts'))$biomass),
                        #dNDMI
                        mean((dataset.2.under %>% filter(drought == '2012-2015'))$dNDMI), mean((dataset.2.under %>% filter(sequence == 'Both Droughts'))$dNDMI),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == '2012-2015 Only'))$dNDMI), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == 'Both Droughts'))$dNDMI),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == 'Both Droughts'))$dNDMI), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == 'Both Droughts'))$dNDMI),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == 'Both Droughts'))$dNDMI), mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == 'Both Droughts'))$dNDMI),
                        #PET 4yr
                        mean((dataset.2.under %>% filter(drought == '2012-2015'))$PET_4yr), mean((dataset.2.under %>% filter(sequence == 'Both Droughts'))$PET_4yr),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == '2012-2015 Only'))$PET_4yr), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == 'Both Droughts'))$PET_4yr),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == 'Both Droughts'))$PET_4yr), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == 'Both Droughts'))$PET_4yr),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == 'Both Droughts'))$PET_4yr), mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == 'Both Droughts'))$PET_4yr),
                        #Temperature 4yr
                        mean((dataset.2.under %>% filter(drought == '2012-2015'))$tmax_4yr), mean((dataset.2.under %>% filter(sequence == 'Both Droughts'))$tmax_4yr),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == '2012-2015 Only'))$tmax_4yr), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == 'Both Droughts'))$tmax_4yr),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == 'Both Droughts'))$tmax_4yr), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == 'Both Droughts'))$tmax_4yr),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == 'Both Droughts'))$tmax_4yr), mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == 'Both Droughts'))$tmax_4yr))

df.tHSD$estimate.1
#Add mean values for 2012-2015
df.tHSD$estimate.2 <- c(#Biomass
                        mean((dataset.2.under %>% filter(drought == '1999-2002'))$biomass), mean((dataset.2.under %>% filter(sequence == '2012-2015 Only'))$biomass),
                        mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == '2012-2015 Only'))$biomass), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == '2012-2015 Only'))$biomass),
                        mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == '2012-2015 Only'))$biomass), mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == '2012-2015 Only'))$biomass),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == '2012-2015 Only'))$biomass), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == 'Both Droughts'))$biomass),
                        #dNDMI
                        mean((dataset.2.under %>% filter(drought == '1999-2002'))$dNDMI), mean((dataset.2.under %>% filter(sequence == '2012-2015 Only'))$dNDMI),
                        mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == '2012-2015 Only'))$dNDMI), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == '2012-2015 Only'))$dNDMI),
                        mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == '2012-2015 Only'))$dNDMI), mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == '2012-2015 Only'))$dNDMI),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == '2012-2015 Only'))$dNDMI), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == 'Both Droughts'))$dNDMI),
                        #PrET 4yr
                        mean((dataset.2.under %>% filter(drought == '1999-2002'))$PET_4yr), mean((dataset.2.under %>% filter(sequence == '2012-2015 Only'))$PET_4yr),
                        mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == '2012-2015 Only'))$PET_4yr), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == '2012-2015 Only'))$PET_4yr),
                        mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == '2012-2015 Only'))$PET_4yr), mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == '2012-2015 Only'))$PET_4yr),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == '2012-2015 Only'))$PET_4yr), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == 'Both Droughts'))$PET_4yr),
                        #Tmax 4yr
                        mean((dataset.2.under %>% filter(drought == '1999-2002'))$tmax_4yr), mean((dataset.2.under %>% filter(sequence == '2012-2015 Only'))$tmax_4yr),
                        mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == '2012-2015 Only'))$tmax_4yr), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == '2012-2015 Only'))$tmax_4yr),
                        mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == '2012-2015 Only'))$tmax_4yr), mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == '2012-2015 Only'))$tmax_4yr),
                        mean((dataset.2.under %>% filter(drought == '2012-2015' & sequence == '2012-2015 Only'))$tmax_4yr), mean((dataset.2.under %>% filter(drought == '1999-2002' & sequence == 'Both Droughts'))$tmax_4yr))

df.tHSD$diff.pct <- df.tHSD$estimate / df.tHSD$estimate.1 * 100

df.tHSD$low.pct <- df.tHSD$conf.low / df.tHSD$estimate.1 * 100

df.tHSD$high.pct <- df.tHSD$conf.high / df.tHSD$estimate.1 * 100

#Select and sort the tukey HSD columns and 
df.tHSD.pub <- df.tHSD %>% dplyr::select(variable, contrast, estimate.1, estimate.2, estimate, conf.low, conf.high, adj.p.value)

#Name the columns of the data frame
colnames(df.tHSD.pub) <- c('Variable', 'Comparison', 'Estimate 1', 'Estimate 2','Difference', 'Low 95% CI', 'High 95% CI', 'p-value')

#ANOVA and Tukey HSD comparing by time period and drought sequence
tb1 <- kbl(df.tHSD.pub, format = 'html', caption = "Table S1: ANOVA and Tukey HSD Results", digits = 3, escape = F) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb1, width = 10, file = "STable1_tHSD_test_results.png", zoom = 5.0)  

#Select and sort the tukey HSD columns and 
df.tHSD.sup <- df.tHSD %>% dplyr::select(variable, contrast, estimate.1, estimate.2, estimate, conf.low, conf.high, diff.pct, high.pct, low.pct, adj.p.value)

#Name the columns of the data frame
colnames(df.tHSD.sup) <- c('Variable', 'Comparison', 'Estimate 1', 'Estimate 2','Difference', 'Low 95% CI', 'High 95% CI', 'Difference (%)', 'Low (%)', 'High (%)', 'p-value')

#ANOVA and Tukey HSD comparing by time period and drought sequence
tb3 <- kbl(df.tHSD.sup, format = 'html', caption = "Table S5: ANOVA and Tukey HSD Results", digits = 3, escape = F) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb3, width = 10, file = "STable5_tHSD_test_results.png", zoom = 5.0) 

#Filtering by drought sequence
all.ca.both <- all.ca %>% dplyr::filter(drought.sequence == 'Both Droughts')
all.ca.2015 <- all.ca %>% dplyr::filter(drought.sequence == '2012-2015 Only')

#Paired t-tests for geospatial data sets
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
df.t$sequence <- c('Both Droughts', '2012-2015 Only', 'Both Droughts', '2012-2015 Only',
                   'Both Droughts', '2012-2015 Only','Both Droughts', '2012-2015 Only')

#Add mean values for 1999-2002
df.t$value.1999 <- c(mean(all.ca.both$biomass_1999), mean(all.ca.2015$biomass_1999), mean(all.ca.both$dNDMI_2004), mean(all.ca.2015$dNDMI_2004),
                     mean(all.ca.both$PET_4yr_2002), mean(all.ca.2015$PET_4yr_2002), mean(all.ca.both$tmax_4yr_2002), mean(all.ca.2015$tmax_4yr_2002))

#Add mean values for 2012-2015
df.t$value.2012 <- c(mean(all.ca.both$biomass_2012), mean(all.ca.2015$biomass_2012), mean(all.ca.both$dNDMI_2017), mean(all.ca.2015$dNDMI_2017),
                     mean(all.ca.both$PET_4yr_2015), mean(all.ca.2015$PET_4yr_2015), mean(all.ca.both$tmax_4yr_2015), mean(all.ca.2015$tmax_4yr_2015))

df.t$diff.pct <- df.t$estimate / df.t$value.1999 * 100

df.t$low.pct <- df.t$conf.low / df.t$value.1999 * 100

df.t$high.pct <- df.t$conf.high / df.t$value.1999 * 100

#Select columns and put them in order
df.t.label <- df.t %>% dplyr::select(variable, sequence, value.1999, value.2012, estimate, conf.low, conf.high, statistic, p.value) 

#Give the different columns names
colnames(df.t.label) <- c('Variable','Drought Sequence', 'Estimate 1', 'Estimate 2', 'Difference', 'Low 95% CI', 'High 95% CI', 't', 'p-value')
 
#Paired T-test table
tb2 <- kbl(df.t.label, format = 'html', caption = "Table S2: Paired two-tailed T-Test Results", escape = F, digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb2, width = 10, file = "STable2_t_test_results.png", zoom = 5.0)

df.t.sup <- df.t %>% dplyr::select(variable, sequence, value.1999, value.2012, estimate, conf.low, conf.high, diff.pct, low.pct, high.pct, statistic, p.value, parameter) 

#Give the different columns names
colnames(df.t.sup) <- c('Variable','Drought Sequence', 'Estimate 1', 'Estimate 2', 'Difference', 'Low 95% CI', 'High 95% CI', 'Difference (%)', 'Low (%)', 'High (%)', 't', 'p-value', 'df (n-1)')

#Paired T-test table
tb4 <- kbl(df.t.sup, format = 'html', caption = "Table S6: Paired two-tailed T-Test Results", escape = F, digits = 3) %>% kable_classic_2(font_size = 14, full_width = F)
as_image(x = tb4, width = 10, file = "STable6_t_test_results.png", zoom = 5.0)

#Select out ADS die-off data for chi-square test
ADS.1999.both.alive <- all.ca.sample %>% dplyr::filter(sequence == 'Both Droughts' & drought == '1999-2002' & ADS.cat == 0) %>% count()
ADS.1999.both.alive
ADS.1999.both.dead <- all.ca.sample %>% dplyr::filter(sequence == 'Both Droughts' & drought == '1999-2002' & ADS.cat == 1) %>% count()
ADS.1999.both.dead
ADS.2015.both.alive <- all.ca.sample %>% dplyr::filter(sequence == 'Both Droughts' & drought == '2012-2015' & ADS.cat == 0) %>% count()
ADS.2015.both.alive
ADS.2015.both.dead <- all.ca.sample %>% dplyr::filter(sequence == 'Both Droughts' & drought == '2012-2015' & ADS.cat == 1) %>% count()
ADS.2015.both.dead
ADS.1999.second.alive <- all.ca.sample %>% dplyr::filter(sequence == '2012-2015 Only' & drought == '1999-2002' & ADS.cat == 0) %>% count()
ADS.1999.second.alive
ADS.1999.second.dead <- all.ca.sample %>% dplyr::filter(sequence == '2012-2015 Only' & drought == '1999-2002' & ADS.cat == 1) %>% count()
ADS.1999.second.dead
ADS.2015.second.alive <- all.ca.sample %>% dplyr::filter(sequence == '2012-2015 Only' & drought == '2012-2015' & ADS.cat == 0) %>% count()
ADS.2015.second.alive
ADS.2015.second.dead <- all.ca.sample %>% dplyr::filter(sequence == '2012-2015 Only' & drought == '2012-2015' & ADS.cat == 1) %>% count()
ADS.2015.second.dead

#Create a frequency matrix for a chi-square test
ADS <- matrix(c(as.numeric(ADS.1999.both.dead), as.numeric(ADS.2015.both.dead), as.numeric(ADS.1999.second.dead), as.numeric(ADS.2015.second.dead),
                  as.numeric(ADS.1999.both.alive), as.numeric(ADS.2015.both.alive), as.numeric(ADS.1999.second.alive), as.numeric(ADS.2015.second.alive)), ncol=2)

#Give the matrix column and row names
colnames(ADS) <- c("Mortality", "No Mortality")
rownames(ADS) <- c('Both Droughts: 1999-2002', 'Both Droughts: 2012-2015', '2012-2015 Only: 1999-2002','2012-2015 Only: 2012-2015')

#Convert the matrix to a data frame
ADS <- as.data.frame(ADS)

#Do the chi-squared test
mychi <- chisq.test(ADS)
mychi
mychi$expected

#Do a chi-squared just for the both drought locations
ADS.both <- matrix(c(as.numeric(ADS.1999.both.dead), as.numeric(ADS.2015.both.dead),
                as.numeric(ADS.1999.both.alive), as.numeric(ADS.2015.both.alive)), ncol=2)

colnames(ADS.both) <- c("Mortality", "No Mortality")
rownames(ADS.both) <- c('Both Droughts: 1999-2002', 'Both Droughts: 2012-2015')
ADS.both <- as.data.frame(ADS.both)

mychi.both <- chisq.test(ADS.both)
mychi.both
mychi.both$expected

#Do a chi-squared just for the 2012-2015 Only locations
ADS.second <- matrix(c(as.numeric(ADS.1999.second.dead), as.numeric(ADS.2015.second.dead),
                       as.numeric(ADS.1999.second.alive), as.numeric(ADS.2015.second.alive)), ncol=2)

colnames(ADS.second) <- c("Mortality", "No Mortality")
rownames(ADS.second) <- c('2012-2015 Only: 1999-2002','2012-2015 Only: 2012-2015')
ADS.second <- as.data.frame(ADS.second)

mychi.second <- chisq.test(ADS.second)
mychi.second
mychi.second$expected

#Calculate proportion of ADS mortality and no mortality by drought sequence and time period
second.2002 <- all.ca.sample %>% filter(sequence == '2012-2015 Only' & drought == '1999-2002') %>% dplyr::select(ADS.cat) %>% sum()
second.2002.total <- all.ca.sample %>% filter(sequence == '2012-2015 Only' & drought == '1999-2002') %>% dplyr::select(ADS.cat) %>% count()
second.2002 / second.2002.total

second.2015 <- all.ca.sample %>% filter(sequence == '2012-2015 Only' & drought == '2012-2015') %>% dplyr::select(ADS.cat) %>% sum()
second.2015.total <- all.ca.sample %>% filter(sequence == '2012-2015 Only' & drought == '2012-2015') %>% dplyr::select(ADS.cat) %>% count()
second.2015 / second.2015.total

both.2002 <- all.ca.sample %>% filter(sequence == 'Both Droughts' & drought == '1999-2002') %>% dplyr::select(ADS.cat) %>% sum()
both.2002.total <- all.ca.sample %>% filter(sequence == 'Both Droughts' & drought == '1999-2002') %>% dplyr::select(ADS.cat) %>% count()
both.2002 / both.2002.total

both.2015 <- all.ca.sample %>% filter(sequence == 'Both Droughts' & drought == '2012-2015') %>% dplyr::select(ADS.cat) %>% sum()
both.2015.total <- all.ca.sample %>% filter(sequence == 'Both Droughts' & drought == '2012-2015') %>% dplyr::select(ADS.cat) %>% count()
both.2015 / both.2015.total
