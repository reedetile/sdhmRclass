#Description-----------------------------------------
#description of script
#  30 Mar 2022
#RCS

#Initialize -----------------------------------------
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
library(PresenceAbsence)
options("rgdal_show_exportToProj4_warnings"="none") # run this before library(rgdal)
library(rgdal)
library(dismo)
library(maptools)
library(randomForest)
library(gbm)
# source course CRSs
setwd(path.root)
getwd()
source("r_code/prjs4_sdhmR.r")
ls(pattern = "prj.") # should be 5 prj.* objects

# Load functions--------------------------------------

# build model formula as alternative hard code
mod.form <- function(dat ,r.col, p.col) {
  # generic formula construction function; inputs as:
  #  resp =>col 1 in dataframe such that r.col=1, 
  #  preds=>col 2 thru ncol in dataframe such that p.col=2
  #  NOTE: predictor vars as factors; coerce PRIOR to formula construction
  # example call: mod.form(tr_PERS,1,2)
  n.col <- ncol(dat) # No. columns in dataframe
  resp <- colnames(dat[r.col]) # assign resp column name
  resp <- paste("as.factor(", colnames(dat[r.col]), ")", sep = "") # assign resp column name
  pred <- colnames(dat[c(p.col:n.col)]) # assign preds column names
  mod.formula <- as.formula(paste(resp, "~", paste(pred, collapse = "+"))) # build formula 
}

# Global Variables-------------------------------------
setwd(path.ex)
load('tr_PERS.RData')
class(tr_PERS)
head(tr_PERS)
drops <- c('wgs_xF','wgs_yF','cell.wgs_x','cell.wgs_y','exp5nrm','exp3nrm','rough_1k','prad_sw_di',
           'etpt_5','mind_yr_av','prec_winte','tave_sprin','UNIQUEID.x'
           ,'UNIQUEID.y')
tr_PERS <- tr_PERS[ ,!(names(tr_PERS) %in% drops)]
tr_PERS <- na.omit(tr_PERS)
dim(tr_PERS)

# Program Body------------------------------------------

###Question 1: explore data:###
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
par(mfrow=c(1,1))

###Question 2: Creat BRT model###
# BRT model formulation !! WARNING !! resp for datasets in col=4

resp <- paste("as.factor(", colnames(tr_PERS[2]), ")", sep = "") # assign resp to col number
n.col <- ncol(tr_PERS) # number of columns
pred <- 5:n.col # assign predictors to column numbers 

################################################################################
######## START BRT MODELS
date() # start time stamp
pers.BRT <- gbm.step(data = tr_PERS, gbm.x = pred, gbm.y = 2, family = "bernoulli",
                     tree.complexity = 3, learning.rate = 1e-04, bag.fraction = 0.75, n.folds = 10, 
                     n.trees = 50, plot.main = TRUE, keep.fold.fit = TRUE) 
date()

#run again#
pers.BRT2 <- gbm.step(data = tr_PERS, gbm.x = pred, gbm.y = 2, family = "bernoulli",
                     tree.complexity = 3, learning.rate = 0.01, bag.fraction = 0.75, n.folds = 10, 
                     n.trees = 50, plot.main = TRUE, keep.fold.fit = TRUE) 
date()

### Question 3: Calculate accuracy metric###
################################################################################
#Resubstitution
# build testing dataframe using model predictions
modl <- "pers.BRT2" # add var to keep track of model
tr_PERS2 <- cbind(modl, tr_PERS[2], pers.BRT2$fitted, pers.BRT2$fold.fit) # build dataframe
names(tr_PERS2)[3:4] <- c("pred", "cvpred") # rename vars
head(tr_PERS2, 2) # just to see logit scale
tr_PERS2$cvpred <- exp(tr_PERS2$cvpred)/(1 + exp(tr_PERS2$cvpred)) # convert from logit
head(tr_PERS2, 2) # examine prediction dataframe 

# determine best threshold using PresenceAbsence package
#   see help(optimal.thresholds) for more info
#library(PresenceAbsence)  # PresenceAbsence for accuracy metrics
mod.cut <- optimal.thresholds(tr_PERS2, opt.methods = c("ObsPrev")) # threshold=PREVALENCE
mod.cut # examine
#mod.cut <- optimal.thresholds(tr_PERS2, opt.methods = c("MaxKappa")) # MaxKappa option
#mod.cut # MaxKappa threshold

# generate confusion matrix
mod2.cfmatR <- table(tr_PERS2[[2]], factor(as.numeric(tr_PERS2$pred >= mod.cut$pred)))
mod2.cfmatX <- table(tr_PERS2[[2]], factor(as.numeric(tr_PERS2$cvpred >= mod.cut$cvpred)))
mod2.cfmatR # examine
mod2.cfmatX # examine 

# calculate model accuracies with standard deviation=F
mod2.acc <- presence.absence.accuracy(tr_PERS2, threshold = mod.cut$pred, st.dev = F)
tss <- mod2.acc$sensitivity + mod2.acc$specificity - 1 # code TSS metric
mod2.acc <- cbind(mod2.acc[1:7], tss) # bind all metrics
mod2.acc[c(1, 4:5, 7:8)] # examine accuracies

# plotting AUC
auc.roc.plot(tr_PERS2, color = T) # basic AUC plot; pkg PresenceAbsence 

#########################################################################################
# #10x Cross-val

set.seed(1234) # set.seed for repeatability
xfold <- 10 # set No. xfolds
pers.BRF.xf <- sample(rep(c(1:xfold), length = nrow(tr_PERS))) # vector of random xfolds
pers.BRF.predXF <- rep(0, length = nrow(tr_PERS)) # empty predict vector
pers.BRF.sampleXF <- rep(0, length = nrow(tr_PERS)) # empty xfold vector

# simple loop for xfold
for (i in 1:xfold) {
  tr <- tr_PERS[pers.BRF.xf != i, ] # training not eq. i
  te <- tr_PERS[pers.BRF.xf == i, ] # test eq. i
  # mod1 <- mod.form(tr_PERS, 2, 5)
  BRF <- gbm.step(data = tr_PERS, gbm.x = pred, gbm.y = 2, family = "bernoulli",
                  tree.complexity = 3, learning.rate = 0.01, bag.fraction = 0.75, n.folds = 10,
                  n.trees = 50, plot.main = TRUE, keep.fold.fit = TRUE)
  pers.BRF.predXF[pers.BRF.xf == i]<-  predict(te, BRF, n.trees = BRF$gbm.call$best.trees,
                                               type = "response")
                                               # predict to test data
  pers.BRF.sampleXF[pers.BRF.xf == i] <- i #assign xfold sample to vector
} #end xfold loop
head(pers.BRF.predXF) # examine vector xfold prediction

modl <- "pers.BRF"
pers.BRF.crossval <- cbind(modl,tr_PERS[2],pers.BRF.predXF)
head(pers.BRF.crossval)

mod.cut.crossval <- optimal.thresholds(pers.BRF.crossval, opt.methods = c("ReqSens"), req.sens = 0.95)
mod.cut.crossval # sensitivity set at 0.95

# generate confusion matrix
pers.BRF.crossval.cfmat <- table(pers.BRF.crossval[[2]],
                                factor(as.numeric(pers.BRF.crossval$pers.BRF.predXF >= mod.cut.crossval$pers.BRF.predXF)))
pers.BRF.crossval.cfmat # examine

# calculate model accuracies with standard deviation=F
pers.BRF.XF.acc <- presence.absence.accuracy(pers.BRF.crossval,
                                            threshold = mod.cut.crossval$pers.BRF.predXF, st.dev = F)
tss <- pers.BRF.XF.acc$sensitivity + pers.BRF.XF.acc$specificity - 1 # code TSS metric
pers.BRF.XF.acc <- cbind(pers.BRF.XF.acc[1:7], tss) # bind all metrics
pers.BRF.XF.acc[c(1, 4:5, 7:8)] # examine accuracies

# plotting AUC
auc.roc.plot(pers.BRF.crossval, color = T) # basic AUC plot; pkg PresenceAbsence

##################################################################################3
#Question 4: Project

setwd(path.gis)
states <- st_read(dsn = ".", layer = "na_states_wgs") # import shapefile

#create a probability model
setwd(path.preds)
load('pers.topoDOM.RData')
pers.dom <- pers.topoDOM
pers.prob.BRT <- predict(pers.dom, pers.BRT2, 
                        type = "response", n.trees = pers.BRT2$gbm.call$best.trees) # prediction raster
pers.prob.BRT # examine 

# next reclassify based on threshold mod.cut per above
pers.class.BRT <- reclassify(pers.prob.BRT, c(0,mod.cut[[2]],0, 
                                            mod.cut[[2]],1,1),overwrite=TRUE)
pers.class.BRT

#restrict to domain#
setwd(path.ex)
load("pers.bufptR.img")
# # crop(x = pers.topoDOM, y = )
#load('pers.PPsA.RData',verbose = TRUE)
# plot(pers.topoDOM)
# pers.bufR <-raster(pers.bufR)
# plot(pers.bufR)

new.pers.bufptR <- raster::projectRaster(pers.bufptR, pers.class.BRT)

pers.class.BRT <- pers.class.BRT*new.pers.bufptR
pers.prob.BRT <- pers.prob.BRT*new.pers.bufptR

par(mfrow=c(1,1))
#Plot probability map#
plot(pers.prob.BRT, axes = T, main = 'Probability Map')
plot(st_geometry(states), add = T)

#Classification Map#
plot(pers.class.BRT, legend = F, axes = T, main = "Classification Map") # plot classification map
plot(st_geometry(states), add = T, lwd = 1.5) # add state boundaries

#######################################################################
#Question 5: save everything
setwd(path.ex)
save(mod.cut, pers.BRT,pers.BRT2, file = 'ex11.RData')
save(pers.class.BRT, file = 'persClassBRT.img')
save(pers.prob.BRT, file = 'persProbBRT.img')
