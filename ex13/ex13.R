#Description-----------------------------------------
#Exercise 13: Field Campaigns and SDHMs
#  06 Apr 2022
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

set.seed(1234)


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
#load in ensemble model
setwd(path.ex)
load('ensemble.dom.RData', verbose = T)

#Read in Frames
setwd(path.gis)
states <- st_read(dsn = ".", layer = "na_states_wgs") # import shapefile

setwd(path.preds)
load('pers.topoDOM.RData')
pers.dom <- pers.topoDOM

setwd(path.ex)
load('pres.fnet.RData', verbose = TRUE)

##################################
pres.fnetSP <- as_Spatial(pres.fnetSF)
fnet1 <- crop(rasterize(pres.fnetSP,pers.dom[[1]]),pers.bufptR)
fnet2 <- as.data.frame(fnet1)
head(fnet2)

#remove duplicate fnetids from p/a data
pers.PA <- subset(pers.PPsA, !duplicated(pers.PPsA[,1]))
head(pers.PA,2)

#remove spp P/A from fishet, fnet3 will be sample frame for field test
fnet3 <- as.data.frame(fnet2[!fnet2$FNETID %in% pers.PA,])
head(fnet3)
dim(fnet3)


#create final df for sample selection: FNETID and cell x&y
fnet.merge <- merge(fnet2,fnet3, by = c('FNETID','cell.wgs_x','cell.wgs_y'))
head(fnet.merge,2)
dim(fnet.merge)

pers.sample.dom <- stack(prob


load('pers.PPsA.RData', verbose = TRUE)
fnet1 = pres.fnetDF[!pres.fnetDF$FNETID %in% pers.PPsA$FNETID,]
names(fnet1)
fnet2 = fnet1[sample(nrow(fnet1),250),]
dim(fnet2)
#giggle plot#
load('pres.bufSF.RDS')
load("pers.bufptR.img")
fnet2SF <- st_as_sf(fnet2, coords = c('cell.wgs_x', 'cell.wgs_y'), crs = prj.wgs84)
plot(fnet2SF$geometry)
plot(pers.bufptR, add =T)
plot(pres.bufSF, add = T)
#it works! mostly#

#####################################################################################
#Build sample frames for field sample extractions
sample.dom <- stack(prob.dom,probSTD.dom,clas.dom)
names(sample.dom) <- c('prob.dom','probSTD.dom','clas.dom')
sample.dom

# extract data from rasters & bind to sample dataframe
ext.1 <- raster::extract(x = sample.dom, y = fnet2[, 2:3], method = 'simple') # basic extract
ext.1 <- as.data.frame((ext.1)
fr.2sample <- cbind(fnet.s1, ext.1) # bind extracted values to sample DF
head(fr.2sample, 2) # examine
