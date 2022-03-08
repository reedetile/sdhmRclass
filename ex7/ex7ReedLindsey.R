#sdhmR Exercise 07
#03/07/2022
#RCS & LAP

#----------Global options---------#
#Setting Paths
path.root <- "D:/OneDrive - University of Vermont/Classes/Spring2022/sdhmR/sdhmR-V2022.1" #Reed Laptop Path
#path.root <- "C:/Users/14842/Documents/SDHM/sdhmR-V2022.1" #Lindsey Path
path.ex <- paste(path.root, "/data/exercise/traindat", sep = "") #Path to mod 2
path.preds <- paste(path.root, '/data/exercise/preds', sep = '')
path.figs <- paste(path.root, "/powerpoints/figures", sep = "") #path to save figs
path.gis <- paste(path.root, "/data/gis_layers", sep = "") #path to gis files

# some libraries
library(raster)	# FXNS: extent, raster, res, rasterize, values, writeRaster, 
#       cellFromXY, extract
library(sf)    	# FXNS: st_as_sf, st_as_sfc, st_bbox, st_convex_hull, st_union
#       st_buffer, st_read, st_write, st_intersection, st_geometry,
#       st_drop_geometry, st_coordinates, st_sf
library(terra)	# FXNS: as.data.frame, rast, vect
library(sp)
library(dplyr)
library(tidyverse)
library(data.table)
library(modeest)
library(DAAG)
# source course CRSs
setwd(path.root)
getwd()
source("r_code/prjs4_sdhmR.r")
ls(pattern = "prj.") # should be 5 prj.* objects

############################
#Question 1: Import and explore the data
load('tr_PERS.Rdata')
class(tr_PERS)
head(tr_PERS)

#attempt to use for() loop
descSTAT <- {}
descSTAT <- as.data.frame(descSTAT)
for(i in 27:length(tr_PERS)){
  mean <- mean(tr_PERS[,i], na.rm = T) #mean returned NA
  NAS <- sum(is.na(tr_PERS[,i])) #checked for number of NAs
  SD <- sd(tr_PERS[,i], na.rm = T) #standard deviation
  N <- nrow(tr_PERS) - sum(is.na(tr_PERS[,i])) #check total N
  rname <- names(tr_PERS[,i])
  stats <- c(rname, mean, NAS, SD, N)
  descSTAT <- rbind(descSTAT, stats)
}

#explore etpt5
mean(tr_PERS$etpt_5.img, na.rm = T) #mean returned NA
sum(is.na(tr_PERS$etpt_5.img)) #checked for number of NAs
sd(tr_PERS$etpt_5.img, na.rm = T) #standard deviation
nrow(tr_PERS) - sum(is.na(tr_PERS$etpt_5.img)) #check total N

#explore etpt6
mean(tr_PERS$etpt_6.img, na.rm = T) #mean returned NA
sum(is.na(tr_PERS$etpt_6.img)) #checked for number of NAs
sd(tr_PERS$etpt_6.img, na.rm = T) #standard deviation
nrow(tr_PERS) - sum(is.na(tr_PERS$etpt_6.img)) #check total N
#explore etpt_sprin
mean(tr_PERS$etpt_sprin.img, na.rm = T) #mean returned NA
sum(is.na(tr_PERS$etpt_sprin.img)) #checked for number of NAs
sd(tr_PERS$etpt_sprin.img, na.rm = T) #standard deviation
nrow(tr_PERS) - sum(is.na(tr_PERS$etpt_sprin.img)) #check total N
#explore mind_yr_ave
mean(tr_PERS$mind_yr_av.img, na.rm = T) #mean returned NA
sum(is.na(tr_PERS$mind_yr_av.img)) #checked for number of NAs
sd(tr_PERS$mind_yr_av.img, na.rm = T) #standard deviation
nrow(tr_PERS) - sum(is.na(tr_PERS$mind_yr_av.img)) #check total N
#explore prec_winte
mean(tr_PERS$prec_winte.img, na.rm = T) #mean returned NA
sum(is.na(tr_PERS$prec_winte.img)) #checked for number of NAs
sd(tr_PERS$prec_winte.img, na.rm = T) #standard deviation
nrow(tr_PERS) - sum(is.na(tr_PERS$prec_winte.img)) #check total N
#explore tave_s_hal
mean(tr_PERS$tave_s_hal.img, na.rm = T) #mean returned NA
sum(is.na(tr_PERS$tave_s_hal.img)) #checked for number of NAs
sd(tr_PERS$tave_s_hal.img, na.rm = T) #standard deviation
nrow(tr_PERS) - sum(is.na(tr_PERS$tave_s_hal.img)) #check total N
#explore tave_sprin
mean(tr_PERS$tave_sprin.img, na.rm = T) #mean returned NA
sum(is.na(tr_PERS$tave_sprin.img)) #checked for number of NAs
sd(tr_PERS$tave_sprin.img, na.rm = T) #standard deviation
nrow(tr_PERS) - sum(is.na(tr_PERS$tave_sprin.img)) #check total N
#explore tmax_summe
mean(tr_PERS$tmax_summe.img, na.rm = T) #mean returned NA
sum(is.na(tr_PERS$tmax_summe.img)) #checked for number of NAs
sd(tr_PERS$tmax_summe.img, na.rm = T) #standard deviation
nrow(tr_PERS) - sum(is.na(tr_PERS$tmax_summe.img)) #check total N


#Boxplots
par(mfrow = c(3, 3))
boxplot(tr_PERS$etpt_5.img ~ tr_PERS$PERS106, xlab = "Presence:Absence", 
        ylab = "etpt5", na.rm = T)
boxplot(tr_PERS$etpt_6.img ~ tr_PERS$PERS106, xlab = "Presence:Absence", 
        ylab = "etpt6", na.rm = T)
boxplot(tr_PERS$etpt_sprin.img ~ tr_PERS$PERS106, xlab = "Presence:Absence", 
        ylab = "etpt_sprin", na.rm = T)
boxplot(tr_PERS$mind_yr_av.img ~ tr_PERS$PERS106, xlab = "Presence:Absence", 
        ylab = "mind_yr_ave", na.rm = T)
boxplot(tr_PERS$prec_winte.img ~ tr_PERS$PERS106, xlab = "Presence:Absence", 
        ylab = "prec_winte", na.rm = T)
boxplot(tr_PERS$tave_s_hal.img ~ tr_PERS$PERS106, xlab = "Presence:Absence", 
        ylab = "tave_s_hal", na.rm = T)
boxplot(tr_PERS$tave_sprin.img ~ tr_PERS$PERS106, xlab = "Presence:Absence", 
        ylab = "tave_sprin", na.rm = T)
boxplot(tr_PERS$tmax_s_hal.img ~ tr_PERS$PERS106, xlab = "Presence:Absence", 
        ylab = "tmax_s_hal", na.rm = T)
boxplot(tr_PERS$tmax_summe.img ~ tr_PERS$PERS106, xlab = "Presence:Absence", 
        ylab = "tmax_summe", na.rm = T)

############################################################################
#Question 2: Construct full- and reduced-variable logistic GLM’s
#Determine an estimate of model fit, based on deviance.
#-------------------------------------------------------------------------#

mod1.LR <- glm(as.factor(PERS106) ~
                 etpt_5.img+etpt_6.img+etpt_sprin.img+mind_yr_av.img+prec_winte.img+tave_s_hal.img+tave_sprin.img+
                 tmax_s_hal.img+tmax_summe.img, data = tr_PERS, family = binomial)
summary(mod1.LR) #everything is sig. or marginal except etpt6

# model 1 fit
mod1.fit <- 100 * (1 - mod1.LR$deviance/mod1.LR$null.deviance) # model fit
mod1.fit  # examine fit
mod1.pred <- predict(mod1.LR, type = "response") # model prediction
head(mod1.pred)
#not a great predictor

#   variable reduction: backwards
mod2.LR <- step(mod1.LR, trace = F) # backwards stepwise variable reduction
mod2.fit <- 100 * (1 - mod2.LR$deviance/mod2.LR$null.deviance)
mod2.fit # model fit

# model 1 v. model 2 fit
100 * (1 - mod1.LR$deviance/mod1.LR$null.deviance)  # fit model 1
100 * (1 - mod2.LR$deviance/mod2.LR$null.deviance)  # fit model 2

# model 2 prediction
mod2.pred <- predict(mod2.LR, type = "response") # model prediction
head(mod2.pred)

# model 1 v. model 2 prediction
head(mod1.pred) # mod 1 prediction
head(mod2.pred) # mod 2 prediction

# model 2 summary
summary(mod2.LR) # reduced model summary 

###########################################################################
#Question 3: calculate accuracy matrix using
#* Resubstitution approaches, and
#* A 10-fold cross--validation approach
#--------------------------------------------------------------------------
pers <- 'PERS106'
#Resubstitution
mod1.LR.pred <- predict(mod1.LR, type = 'response')
head(mod1.LR.pred)

#10x cross-val
set.seed(1234) # set and save seed if desire replicability of samples
glm.cv10 <- CVbinary(mod1.LR, nfolds = 10, print.details = F) # cross validate model w/10 folds
ls(glm.cv10) # examine crossval object
head(glm.cv10$cvhat) # examine 



# examine resubstitution and cross-validation estimates
head(mod1.LR.pred) # examine resub prediction
head(glm.cv10$cvhat) # examine xval prediction
glm.cvpred <- glm.cv10$cvhat # assign new name to crossval estimates
dat2 <- cbind(pers, tr_PERS[2], mod1.LR.pred, glm.cvpred) # build dataframe
head(dat2, 2) # examine; NOTE will differ each run unless seed is saved 


