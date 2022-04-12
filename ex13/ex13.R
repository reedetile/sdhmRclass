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
load()
pres.fnetR <- raster(pres.fnetR)
setwd(path.gis)
load('pers.bufptR.RData')
fnet1 <- crop(pres.fnetR, pers.bufptR) # crop fishnet to spp domain

plot(pres.fnetR,)
plot(st_geometry(states), add = T) # spp polygon
plot(fnet1, add = T, col = "red", legend = F) # spp bbox
plot(st_geometry(pers.bufptR), lwd = 2, add = T) # spp polygon
points(tr_PERS$tr.wgs_x, tr_PERS$tr.wgs_y, pch = 20, col = "white") # add spp P/A points

