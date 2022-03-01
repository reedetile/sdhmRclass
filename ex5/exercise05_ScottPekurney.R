#sdhmR Exercise 05
#02/23/2022
#RCS

#----------Global options---------#
#Setting Paths
path.root <- "D:/OneDrive - University of Vermont/Classes/Spring2022/sdhmR/sdhmR-V2022.1" #Reed Path
# path.root <- "C:/Users/14842/Documents/SDHM/sdhmR-V2022.1" #Lindsey Path
path.ex <- paste(path.root, "/data/exercise/traindat", sep = "") #Path to mod 2
path.preds <- paste(path.root, '/data/exercise/preds', sep ='')
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

# source course CRSs
setwd(path.root)
source("r_code/prjs4_sdhmR.r") # source course CRSs
ls(pattern = "prj.") # should be 5 prj.* objects

######Question 1######
setwd(path.preds)
etpt_5<- raster('etpt_5.img')
etpt_6 <- raster('etpt_6.img')
etpt_sprin <- raster('etpt_sprin.img')
exp1nrm <- raster('exp1nrm.img')
exp3nrm <- raster('exp3nrm.img')
exp5nrm <- raster('exp5nrm.img')
mind_yr_av <- raster('mind_yr_av.img')
prad_sw_di <- raster('prad_sw_di.img')
prec_w_hal <- raster('prec_w_hal.img')
prec_winte <- raster('prec_winte.img')
rough_1k <- raster('rough_1k.img')
tave_s_hal <- raster('tave_s_hal.img')
tave_sprin <- raster('tave_sprin.img')
tmax_summe <- raster('tmax_summe.img')
topos <- raster('topos.img')

#Check projections
projection(etpt_5)
projection(etpt_6)
projection(etpt_sprin)
projection(exp3nrm)
projection(exp5nrm)
projection(mind_yr_av)
projection(prad_sw_di)
projection(prad_sw_di)
projection(prec_w_hal)
projection(prec_winte)
projection(rough_1k)
projection(tave_s_hal)
projection(tave_sprin)
projection(tmax_summe)
projection(topos)

#compare raster resolution
res(etpt_5)
res(etpt_6)
res(etpt_sprin)
res(exp3nrm)
res(exp5nrm)
res(mind_yr_av)
res(prad_sw_di)
res(prad_sw_di)
res(prec_w_hal)
res(prec_winte)
res(rough_1k)
res(tave_s_hal)
res(tave_sprin)
res(tmax_summe)
res(topos)
#all resolutions look the same, so no need to convert

####Question 2###
setwd(path.ex)
pres.abs.pers <- get(load("pers.PPsA.RData"))
#load('pers.bufR.RData')

setwd(path.preds)
preds.list <- list.files(pattern = ".img$") # list of .img files; $ strips extra
preds.list # examine

# loop for extracting topo data w/raster stack
layers <- {} # initialize (empty) list of raster layers
for (i in 1:length(preds.list)) {
  r1 <- crop(raster(preds.list[i]), pers.bufR) # crop pred var raster to buffer
  names(r1) <- strsplit(preds.list[i], "_wgs.img") # assign name to raster
  layers <- c(layers, r1) # add raster to layer of rasters
}
layers # examine raster stack; return is a list of raster layers

# build the raster stack
pers.topoDOM <- stack(layers) # create a raster stack
pers.topoDOM # examine stack

# extract from stack 
t1.pers <- extract(pers.topoDOM, pres.abs.pers[, c("tr.wgs_x", "tr.wgs_y")]) # extract values from raster stack
# terra implementation; identical as raster extract
#t1 <- terra::extract(pied.topoDOM, pres.abs[, c("tr.wgs_x", "tr.wgs_y")]) # basic extract
head(t1.pers, 2) # examine extracted matrix
pers.trTOPO <- cbind(pres.abs.pers, t1.pers) # bind to train dataframe
head(pers.trTOPO, 2) # examine training data frame

# write out data files if desired
setwd(path.ex)
write.csv(pers.trTOPO, file = "pers_trTOPO.csv", row.names = F) # save .csv
save(pers.trTOPO, file = "pers.trTOPO.RData") # save .RData
save(pers.topoDOM, file = "pers.topoDOM.RData") # save .RData