#sdhm ex9
#RCS & LAP
#3/14/2022

#########################################################
#Global options
#Create paths
#load libraries
#load map projections

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

# source course CRSs
setwd(path.root)
getwd()
source("r_code/prjs4_sdhmR.r")
ls(pattern = "prj.") # should be 5 prj.* objects

###########################################################
#Import and explore data
#NOTE:  intent is not to reduce number of variables; that was completed in exercise #5.  Rather, calculate simple descriptive statistics (mean, sd, n) and boxplots

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

#################################################################################
#Question 2
# * Construct a MAXENT model using the presence:absence data
# * Build plots of variable importance values

head(tr_PERS,2)
PERS.xy <- tr_PERS[,3:4]
#Building raster stack + domain
setwd(path.preds)
pers.list <- list.files(pattern = ".img$") # list of .img files; $ strips extra
pers.list # examine
pers.dom <- stack(pers.list) # build raster stack
pers.dom # examine stack
names(pers.dom) # examine stack names

# save 20% presence for testing, 80% training; 5-fold x-val
set.seed(1234) # set.seed if desire repeatability
#library(dismo) # dismo needed for kfold and maxent
fold <- kfold(PERS.xy, k = 5) # data k-folds == 5
pers.tst <- PERS.xy[fold == 1, ] # build test data; the 20%
pers.tr <- PERS.xy[fold != 1, ] # build training data; the 80%

# run maxent model; requires dismo
n.col <- ncol(tr_PERS)
mod1.MAX <- maxent(x = tr_PERS[5:n.col],p = tr_PERS[2]) # call for true pers/abs 
mod1.MAX
plot(mod1.MAX)

#################################################################################
#Question 3
# * Calculate accuracy metrics (as in Module 3.2 and 3.3, Analytical Intermission) using:
# * Resubstitution approaches, and
# * A 10-fold cross--validation approach




pers <- 'PERS106'
#Resubstitution
mod1.MAX.pred <- predict(mod1.MAX, tr_PERS)
head(mod1.MAX.pred)
modl <- 'mod1.MAX'
dat2 <- cbind(modl,tr_PERS[2],mod1.MAX.pred)
head(dat2)
names(pers.dom)
names(pers.dom) <- paste(names(pers.dom),'img')
mod1.val <- evaluate(model = mod1.MAX, p = tr_PERS[tr_PERS$PERS106 == 1, c(3:4)], 
                     a = tr_PERS[tr_PERS$PERS106 == 0, c(3:4)], 
                     x = pers.dom)
mod1.val
threshold(mod1.val)
mod.cut <- threshold(mod1.val)
mod.cut
mod1.acc <- presence.absence.accuracy(dat2, threshold = mod.cut[[2]],st.dev=F)
tss <- mod1.acc$sensitivity + mod1.acc$specificity


# Simple 10 fold code
set.seed(1234) # set.seed for repeatability
pers.xf <- sample(rep(c(1:10), length = nrow(tr_PERS))) # vector of random xfolds
mod1.predXF <- rep(0, length = nrow(tr_PERS)) # empty vector of 0
xfold <- 10 # set No. xfolds
# simple loop for xfold
for (i in 1:xfold) {
        tr <- tr_PERS[pers.xf != i, ] # training not eq. i
        te <- tr_PERS[pers.xf == i, ] # test eq. i
        mx <- maxent(tr_PERS[5:ncol(tr_PERS)], tr_PERS[2], data = tr) 
        # maxent model on training. first value = predictors, second = pres/abs
        mod1.predXF[pers.xf == i] <- predict(mx, te) # predict to test
}
head(mod1.predXF) # examine vector xfold prediction 


# examine resubstitution and cross-validation estimates
head(mod1.MAX.pred) # examine resub prediction
head(mod1.predXF) # examine xval prediction
cross.val <- cbind(modl, tr_PERS[2], mod1.predXF) # build dataframe
head(cross.val, 2) # examine; NOTE will differ each run unless seed is saved 

#calculate model accuracies#
mod1.accXF <- presence.absence.accuracy(cross.val, threshold = mod.cut[[2]],st.dev=F)
tss <- mod1.accXF$sensitivity + mod1.acc$specificity - 1
mod1.accXF <- cbind(mod1.acc[1:7],tss)
mod1.accXF

############################################################################################
## Question #4

## Question #4
# * Build 2 prediction maps:
#         * A raw probability estimate for each cell in the modelling domain; and
# * A classified map based on the selected threshold from Question #4
#setwd(path.ex)
#load('ex9.RData')

setwd(path.gis)
states <- st_read(dsn = ".", layer = "na_states_wgs") # import shapefile

#create a probability model
pers.prob.MAX <- predict(pers.dom, mod1.MAX, 
                     type = "response", fun = predict, index = 2, overwrite = T) # prediction raster
pers.prob.MAX # examine 

# next reclassify based on threshold mod.cut per above
pers.class.MAX <- reclassify(pers.prob.MAX, c(0,mod.cut[[2]],0, 
                                      mod.cut[[2]],1,1),overwrite=TRUE)
pers.class.MAX

#restrict class and prob models to domain
setwd(path.ex)
load('pers.PPsA.RData')
pres.bufpt <-raster(pers.bufR)
plot(pers.bufR)
new.pers.bufR <- projectRaster(pers.bufR, pers.class.MAX)
pers.class.MAX <- pers.class.MAX*new.pers.bufR
pers.prob.MAX <- pers.prob.MAX*new.pers.bufR

par(mfrow=c(1,1))
#Plot probability map#
plot(pers.prob.MAX, axes = T, main = 'Probability Map')
plot(st_geometry(states), add = T)

#Classification Map#
plot(pers.class.MAX, legend = F, axes = T, main = "Classification Map") # plot classification map
plot(st_geometry(states), add = T, lwd = 1.5) # add state boundaries

#########################################################################3
#Question 5: Save your data
setwd(path.ex)
save.image(pers.class.MAX ,pers.prob.MAX,  file= 'ex9.RData')
save(cross.val,dat2,mod.cut,mod1.acc,mod1.accXF,mod1.MAX,mod1.val,mx,pers.class.MAX,pers.prob.MAX,PERS.xy, file = 'ex9.RData')
save.image()
