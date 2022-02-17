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

#-----------Question 3------------#
pres.xy <- pres[c("wgs_xF", "wgs_yF")]  # get x,y of species presences
p1 <- sp::coordinates(pres.xy) # set spatial coords from pied.bufptR

# extract FNETID of presence from  modelling frame
pres.fnetid <- cellFromXY(pres.bufR, p1)
head(pres.fnetid)

#checks
length(pres.fnetid) # should match number of presences ??
length(pres$PERS106) # it does ... life is good !!

# create species dataframe with FNETIDs
tru.presFNET <- cbind(pres.fnetid, pres) # add fishnet ID to tru.pres
names(tru.presFNET)[1] <- "FNETID" # name change 
head(tru.presFNET, 2) # examine

# giggle plots
plot(pres.bufR, col = "gray70", legend = F, main = "Points in modelling frame")  # main plot
points(pres$wgs_xF, pres$wgs_yF, pch = 20, col = "darkgreen") # add spp locations

#extract fnetid
buf.fnetid <- extract(pres.bufR, pres.fnetDF[c("cell.wgs_x", "cell.wgs_y")])

# create modelling dataframe with FNETIDs
tru.bufFNET <- cbind(pres.fnetDF, buf.fnetid) # bind modelling frame w/FISHNET
head(tru.bufFNET, 2) # examine

# some internal checking
length(tru.bufFNET$buf.fnetid) # number FNETIDs in tru.bufFNET
ncell(pres.bufR) # should equal above
table(tru.bufFNET$buf.fnetid)[[1]] # number of FNETIDs in pied.bufptR
length(which(is.na(tru.bufFNET$bufpt.fnetid))) # No. NAs

# giggle plots: modelling frames nested in fishnet
plot(pres.bufSP, col = "gray90", legend = F, main = "Modelling frame on FISHNET")  # main plot
plot(pres.bufR, col = "gray50", legend = F, add = T)  # main plot

# giggle plots: spp location nested in modelling frames nested in fishnet
plot(pres.bufSP, col = "gray90",
     main = "SP locations nested in modelling frame in FISHNET")  # main plot
points(pres$wgs_xF, pres$wgs_yF, pch = 20, col = "darkgreen") # spp locations if desired
plot(pres.bufR, col = "gray50", legend = F, add = T)  # main plot

######## START MERGE SPP LOCATION, MODELLING FRAME, ALL BY FISHNET ID'S 
# examine dataframes
head(pres.fnetDF, 2) # fishnet dataframe
dim(pres.fnetDF)[1] # size of fishnet dataframe
head(tru.bufFNET, 2) # modelling dataframe
table(tru.bufFNET$buf.fnetid)[[1]] # number of FNETIDs in pied.bufptR
head(tru.bufFNET, 2) # spp locations dataframe
dim(tru.bufFNET)[1] # number spp locations

# begin merge: NOTE merge by=c("sort vars") & all.y=T options
m1 <- merge(pres.fnetDF, tru.bufFNET, by = c("FNETID", "cell.wgs_x", "cell.wgs_y"), all.y = T)
head(m1, 2) # examine: FNETID no longer ranked but not to worry ...
pres.indexFNET <- merge(m1, tru.presFNET, by = c("FNETID"), all = T) # final merge:assign DF name
head(pres.indexFNET, 2) # final merge: FNETID now ranked
names(pres.indexFNET)[4] <- "in.modFR" # change some names
names(pres.indexFNET) # examine names

# internal checking
length(pres.indexFNET$FNETID) # does it match w/above ?? yes !
table(pres.indexFNET$in.modFR)[[1]] # does it match w/above ?? yes !
table(pres.indexFNET$PERS106)[[1]] # does it match w/above ?? yes !
names(pres.indexFNET) # names make sense ?? yes !

# outfile final fnet index dataframe
setwd(path.mod2)
save(pres.indexFNET, file = "pres.indexFNET.RData")

# examine the fishnet
head(pres.indexFNET)