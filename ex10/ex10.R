#Description-----------------------------------------
#sdhmR exercise 08
#  28 Mar 2022
#RCS *LAP

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
  # example call: mod.form(dat1,1,2)
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
###Question 2: 
# * Construct a Random Forest (RF) model using the presence:absence data
# * Build plots of variable importance values

pers.RF <- randomForest(mod.form(tr_PERS, 2, 5), importance = T, keep.forest=T,data = tr_PERS)
varImpPlot(pers.RF, main = "Variable Importance Plots")


persRF.pred <- predict(pers.RF, type = 'prob')[,2]
head(persRF.pred)

###Question 3: Accuracy Metrics###

##################################Resubstitution#############################
# build testing dataframe using model predictions
modl <- "pers.RF.RF" # add var to keep track of model
dat2 <- cbind(modl, tr_PERS[2], persRF.pred) # build dataframe w/pers.RF predictions
head(dat2, 2) # examine prediction dataframe 

# determine best threshold using PresenceAbsence package Sec7.1
#   see help(optimal.thresholds) for more info
#library(PresenceAbsence) # PresenceAbsence for accuracy metrics
#help(optimal.thresholds) # options for optimizing threshold
mod.cut <- optimal.thresholds(dat2, opt.methods = c("ReqSens"), req.sens = 0.95)
mod.cut # sensitivity set at 0.95
#mod.cutK=optimal.thresholds(dat2,opt.methods=c('MaxKappa')); mod.cutK # MaxKappa option

# generate confusion matrix
pers.RF.cfmat <- table(dat2[[2]], factor(as.numeric(dat2$persRF.pred >= mod.cut$persRF.pred)))
pers.RF.cfmat # examine

# calculate model accuracies with standard deviation=F
pers.RF.acc <- presence.absence.accuracy(dat2, threshold = mod.cut$persRF.pred, st.dev = F)
tss <- pers.RF.acc$sensitivity + pers.RF.acc$specificity - 1 # code TSS metric
pers.RF.acc <- cbind(pers.RF.acc[1:7], tss) # bind all metrics
pers.RF.acc[c(1, 4:5, 7:8)] # examine accuracies

# plotting AUC
auc.roc.plot(dat2, color = T) # basic AUC plot; pkg PresenceAbsence 
#############################################################################

#############################10x Cross Validation############################
set.seed(1234) # set.seed for repeatability
xfold <- 10 # set No. xfolds
pers.xf <- sample(rep(c(1:xfold), length = nrow(tr_PERS))) # vector of random xfolds
pers.RF.predXF <- rep(0, length = nrow(tr_PERS)) # empty predict vector
pers.RF.sampleXF <- rep(0, length = nrow(tr_PERS)) # empty xfold vector

# simple loop for xfold
for (i in 1:xfold) {
  tr <- tr_PERS[pers.xf != i, ] # training not eq. i
  te <- tr_PERS[pers.xf == i, ] # test eq. i
  # mod1 <- mod.form(tr_PERS, 2, 5)
  RF <- randomForest(mod.form(tr_PERS, 2, 5), importance = T, keep.forest=T,data = tr) 
  pers.RF.predXF[pers.xf == i]<-  predict(RF,te,type='prob')[,2]# predict to test data
  pers.RF.sampleXF[pers.xf == i] <- i #assign xfold sample to vector
} #end xfold loop
head(pers.RF.predXF) # examine vector xfold prediction 

modl <- "pers.RF.RF"
pers.crossval <- cbind(modl,tr_PERS[2],pers.RF.predXF)
head(pers.crossval)

mod.cut.crossval <- optimal.thresholds(pers.crossval, opt.methods = c("ReqSens"), req.sens = 0.95)
mod.cut.crossval # sensitivity set at 0.95

# generate confusion matrix
pers.RF.crossval.cfmat <- table(pers.crossval[[2]], 
                                factor(as.numeric(pers.crossval$pers.RF.predXF >= mod.cut.crossval$pers.RF.predXF)))
pers.RF.crossval.cfmat # examine

# calculate model accuracies with standard deviation=F
pers.RF.XF.acc <- presence.absence.accuracy(pers.crossval, 
                                            threshold = mod.cut.crossval$pers.RF.predXF, st.dev = F)
tss <- pers.RF.XF.acc$sensitivity + pers.RF.XF.acc$specificity - 1 # code TSS metric
pers.RF.XF.acc <- cbind(pers.RF.XF.acc[1:7], tss) # bind all metrics
pers.RF.XF.acc[c(1, 4:5, 7:8)] # examine accuracies

# plotting AUC
auc.roc.plot(pers.crossval, color = T) # basic AUC plot; pkg PresenceAbsence 
#############################################################################

##################Out-of-Bag#################################
# confusion from ReqSens=.95 option & class.error
cbind(pers.RF.cfmat, rbind(pers.RF.cfmat[3]/sum(pers.RF.cfmat[c(1, 3)]), 
                        pers.RF.cfmat[2]/sum(pers.RF.cfmat[c(2, 4)])))

# OOB confusion matrix & class.error
pers.RF$confusion # OOB confusion
pers.RF.cfmat # specified confusion

# OOB accuracy estimates with standard deviation=F
oob.acc <- presence.absence.accuracy(dat2, st.dev = F) # oob accuracies
tss <- oob.acc$sensitivity + oob.acc$specificity - 1 # code TSS metric
oob.acc <- cbind(oob.acc[1:7], tss) # bind all metrics
oob.acc[c(1, 4:5, 7:8)] # examine accuracies

# comparison of resub, cross val, & OOB accuracies
pers.RF.acc[c(1, 4:5, 7:8)] # examine ReqSens accuracies
pers.RF.XF.acc[c(1, 4:5, 7:8)] # examine accuracies
oob.acc[c(1, 4:5, 7:8)] # examine OOB accuracies 

########################################Build 2 prediction Maps######################################################

setwd(path.gis)
states <- st_read(dsn = ".", layer = "na_states_wgs") # import shapefile

#create a probability model
setwd(path.preds)
load('pers.topoDOM.RData')
pers.dom <- pers.topoDOM
pers.prob.RF <- predict(pers.dom, pers.RF, 
                         type = "prob", fun = predict, index = 2, overwrite = T) # prediction raster
pers.prob.RF # examine 

# next reclassify based on threshold mod.cut per above
pers.class.RF <- reclassify(pers.prob.RF, c(0,mod.cut[[2]],0, 
                                              mod.cut[[2]],1,1),overwrite=TRUE)
pers.class.RF

#restrict class and prob models to domain
setwd(path.ex)
load("pers.bufptR.img")
# # crop(x = pers.topoDOM, y = )
#load('pers.PPsA.RData',verbose = TRUE)
# plot(pers.topoDOM)
# pers.bufR <-raster(pers.bufR)
# plot(pers.bufR)

new.pers.bufptR <- raster::projectRaster(pers.bufptR, pers.class.RF)

pers.class.RF <- pers.class.RF*new.pers.bufptR
pers.prob.RF <- pers.prob.RF*new.pers.bufptR

par(mfrow=c(1,1))
#Plot probability map#
plot(pers.prob.RF, axes = T, main = 'Probability Map')
plot(st_geometry(states), add = T)

#Classification Map#
plot(pers.class.RF, legend = F, axes = T, main = "Classification Map") # plot classification map
plot(st_geometry(states), add = T, lwd = 1.5) # add state boundaries

#########################Question 5: save the data###############################
save(pers.RF,pers.class.RF,pers.prob.RF,dat2,oob.acc,pers.RF.acc, file = 'ex10.RData')
save(pers.class.RF, file = 'persClassRF.img')
save(pers.prob.RF, file = 'persProbRF.img')
