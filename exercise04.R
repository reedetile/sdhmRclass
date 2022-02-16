#sdhmR assignment 4
#Feb 18 2022
#RCS

#----------Global options---------#
#Setting Paths
path.root <- "D:/OneDrive - University of Vermont/Classes/Spring2022/sdhmR/sdhmR-V2022.1"
path.ex <- paste(path.root, "/data/exercise/traindat", sep = "") #Path to mod 2
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

#--------question 1------------#
setwd(path.ex)
pres <- read.csv('spp106pr_PERS.csv')
pres
presSF <- st_as_sf(pres, coords = c('wgs_xF', 'wgs_yF'), crs = prj.wgs84)
plot(presSF['PERS106'], col = "darkgreen", pch = 20, axes = T)

#Build a buffered presence bbox
extb <- extent(min(pres$wgs_xF) - 0.5, max(pres$wgs_xF) + 0.5, 
               min(pres$wgs_yF) - 0.5, max(pres$wgs_yF) + 0.5)
# create .5 decideg buffered poly
pres.bufSF <- st_as_sfc(st_bbox(extb, crs = prj.wgs84))
pres.bufSF # examine
plot(st_geometry(pres.bufSF), axes = T) # NOT RUN
plot(presSF['PERS106'], col = "darkgreen", pch = 20, axes = T, add = T)

#----------Question 2---------------#
setwd(path.gis)
t1 <- raster("templateR_wgs.img") # alternative; import wgs84 template
pres.bufSP <- as_Spatial(pres.bufSF)
pres.bufR <- crop(rasterize(pres.bufSP, t1), pres.bufSP)

# extract coords, cell id of coords, & build dataframe
f1 <- sp::coordinates(pres.bufR) # get spatial coords from pied.bufR
f2 <- cellFromXY(pres.bufR, f1) # extract cell number from buffered extent
f3 <- as.data.frame(cbind(f1, f2)) # build datframe of x,y & cell number
head(f3, 2) # examine
tail(f3, 1) # f2 value should = ncell in pres.bufR; below
ncell(pres.bufR) # should = f2 value above

names(f3)[1:3] <- c("cell.wgs_x", "cell.wgs_y", "FNETID") # assign names
f3 <- f3[c("FNETID", "cell.wgs_x", "cell.wgs_y")] # reorder

#Examine
dim(f3) # dimension; rows is maximum number of cells in fishnet 
names(f3) # names in fishnet
head(f3) #examine
tail(f3)

#convert to spatial object
pres.fnetSF <- st_as_sf(f3, coords = c("cell.wgs_x", "cell.wgs_y"), 
                        crs = prj.wgs84, remove = F) # remove=F retains input x,y)
head(pres.fnetSF, 2) # examine

# build stand-alone dataframe
class(pres.fnetSF) # note dataframe class; will extract data.frame
pres.fnetDF <- st_drop_geometry(pres.fnetSF) # build dataframe
head(pres.fnetDF, 2) # examine
class(pres.fnetDF) # check if class=data.frame

# export as esri shapefile & dataframe
setwd(path.ex) # output path
st_write(pres.fnetSF, dsn = ".", layer = "pied_fnetSF", driver = "ESRI Shapefile",
         delete_layer = T, delete_dsn = T) # output shapefile

# save objects
setwd(path.ex) # output path
save("pres.fnetSF", "pres.fnetDF", file = "pres.fnet.RData")
