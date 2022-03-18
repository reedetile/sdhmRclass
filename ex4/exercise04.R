#sdhmR assignment 4
#Feb 18 2022
#RCS

#----------Global options---------#
#Setting Paths
path.root <- "D:/OneDrive - University of Vermont/Classes/Spring2022/sdhmR/sdhmR-V2022.1"
#path.root <- "C:/Users/14842/Documents/SDHM/sdhmR-V2022.1"
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
pers <- read.csv('spp106pr_PERS.csv')
pers
persSF <- st_as_sf(pers, coords = c('wgs_xF', 'wgs_yF'), crs = prj.wgs84)
plot(persSF['PERS106'], col = "darkgreen", pch = 20, axes = T)

#Build a buffered presence bbox
extb <- extent(min(pers$wgs_xF) - 0.5, max(pers$wgs_xF) + 0.5, 
               min(pers$wgs_yF) - 0.5, max(pers$wgs_yF) + 0.5)
# create .5 decideg buffered poly
pers.bufSF <- st_as_sfc(st_bbox(extb, crs = prj.wgs84))
pers.bufSF # examine
plot(st_geometry(pers.bufSF), axes = T) # NOT RUN
plot(persSF['PERS106'], col = "darkgreen", pch = 20, axes = T, add = T)

#----------Question 2---------------#
setwd(path.gis)
t1 <- raster("templateR_wgs.img") # alternative; import wgs84 template
pers.bufSP <- as_Spatial(pers.bufSF)
pers.bufR <- crop(rasterize(pers.bufSP, t1), pers.bufSP)

# extract coords, cell id of coords, & build dataframe
f1 <- sp::coordinates(pers.bufR) # get spatial coords from pied.bufR
f2 <- cellFromXY(pers.bufR, f1) # extract cell number from buffered extent
f3 <- as.data.frame(cbind(f1, f2)) # build datframe of x,y & cell number
head(f3, 2) # examine
tail(f3, 1) # f2 value should = ncell in pers.bufR; below
ncell(pers.bufR) # should = f2 value above

names(f3)[1:3] <- c("cell.wgs_x", "cell.wgs_y", "FNETID") # assign names
f3 <- f3[c("FNETID", "cell.wgs_x", "cell.wgs_y")] # reorder

#Examine
dim(f3) # dimension; rows is maximum number of cells in fishnet 
names(f3) # names in fishnet
head(f3) #examine
tail(f3)

#convert to spatial object
pers.fnetSF <- st_as_sf(f3, coords = c("cell.wgs_x", "cell.wgs_y"), 
                        crs = prj.wgs84, remove = F) # remove=F retains input x,y)
head(pers.fnetSF, 2) # examine

# build stand-alone dataframe
class(pers.fnetSF) # note dataframe class; will extract data.frame
pers.fnetDF <- st_drop_geometry(pers.fnetSF) # build dataframe
head(pers.fnetDF, 2) # examine
class(pers.fnetDF) # check if class=data.frame

# export as esri shapefile & dataframe
setwd(path.ex) # output path
st_write(pers.fnetSF, dsn = ".", layer = "pied_fnetSF", driver = "ESRI Shapefile",
         delete_layer = T, delete_dsn = T) # output shapefile

# save objects
setwd(path.ex) # output path
save("pers.bufR", file = "pers.bufR.RData")
save("pers.fnetSF", "pers.fnetDF", file = "pers.fnet.RData")

#-----------Question 3------------#
pers.xy <- pers[c("wgs_xF", "wgs_yF")]  # get x,y of species persences
p1 <- sp::coordinates(pers.xy) # set spatial coords from pied.bufptR

# extract FNETID of presence from  modelling frame
pers.fnetid <- cellFromXY(pers.bufR, p1)
head(pers.fnetid)

#checks
length(pers.fnetid) # should match number of persistance ??
length(pers$PERS106) # it does ... life is good !!

# create species dataframe with FNETIDs
tru.persFNET <- cbind(pers.fnetid, pers) # add fishnet ID to tru.pers
names(tru.persFNET)[1] <- "FNETID" # name change 
head(tru.persFNET, 2) # examine

# giggle plots
plot(pers.bufR, col = "gray70", legend = F, main = "Points in modelling frame")  # main plot
points(pers$wgs_xF, pers$wgs_yF, pch = 20, col = "darkgreen") # add spp locations

#extract fnetid
buf.fnetid <- raster::extract(pers.bufR, pers.fnetDF[c("cell.wgs_x", "cell.wgs_y")], na.rm = T)

# create modelling dataframe with FNETIDs
tru.bufFNET <- cbind(pers.fnetDF, buf.fnetid) # bind modelling frame w/FISHNET
head(tru.bufFNET, 2) # examine

# some internal checking
length(tru.bufFNET$buf.fnetid) # number FNETIDs in tru.bufFNET
ncell(pers.bufR) # should equal above
table(tru.bufFNET$buf.fnetid)[[1]] # number of FNETIDs in pied.bufptR
length(which(is.na(tru.bufFNET$bufpt.fnetid))) # No. NAs

# giggle plots: modelling frames nested in fishnet
plot(pers.bufSP, col = "gray90", legend = F, main = "Modelling frame on FISHNET")  # main plot
plot(pers.bufR, col = "gray50", legend = F, add = T)  # main plot

# giggle plots: spp location nested in modelling frames nested in fishnet
plot(pers.bufSP, col = "gray90",
     main = "SP locations nested in modelling frame in FISHNET")  # main plot
points(pers$wgs_xF, pers$wgs_yF, pch = 20, col = "darkgreen") # spp locations if desired
plot(pers.bufR, col = "gray50", legend = F, add = T)  # main plot

######## START MERGE SPP LOCATION, MODELLING FRAME, ALL BY FISHNET ID'S 
# examine dataframes
head(pers.fnetDF, 2) # fishnet dataframe
dim(pers.fnetDF)[1] # size of fishnet dataframe
head(tru.bufFNET, 2) # modelling dataframe
table(tru.bufFNET$buf.fnetid)[[1]] # number of FNETIDs in pied.bufptR
head(tru.bufFNET, 2) # spp locations dataframe
dim(tru.bufFNET)[1] # number spp locations

# begin merge: NOTE merge by=c("sort vars") & all.y=T options
m1 <- merge(pers.fnetDF, tru.bufFNET, by = c("FNETID", "cell.wgs_x", "cell.wgs_y"), all.y = T)
head(m1, 2) # examine: FNETID no longer ranked but not to worry ...
pers.indexFNET <- merge(m1, tru.persFNET, by = c("FNETID"), all = T) # final merge:assign DF name
head(pers.indexFNET, 2) # final merge: FNETID now ranked
names(pers.indexFNET)[4] <- "in.modFR" # change some names
names(pers.indexFNET) # examine names

# internal checking
length(pers.indexFNET$FNETID) # does it match w/above ?? yes !
table(pers.indexFNET$in.modFR)[[1]] # does it match w/above ?? yes !
table(pers.indexFNET$PERS106)[[1]] # does it match w/above ?? yes !
names(pers.indexFNET) # names make sense ?? yes !

# outfile final fnet index dataframe
setwd(path.ex)
save(pers.indexFNET, file = "pers.indexFNET.RData")

# examine the fishnet
head(pers.indexFNET)

##################################################3
#Extracting pseduo absence
# create conditional vectors: FNETIDs by spp locations & in modelling frame
p2.spp <- subset(pers.indexFNET$FNETID, pers.indexFNET$PERS106 == 1) # FNETIDs of spp locations
p2.modFR <- subset(pers.indexFNET$FNETID, pers.indexFNET$in.modFR == 1) # FNETIDs in modelling frame
length(p2.spp) # should equal N of spp locations
length(p2.modFR) # should equal N modelling frame

# drop presence cell FNETIDs; remaining are possible psuedo-abs cell FNETIDs
p3 <- pers.indexFNET[!pers.indexFNET$FNETID %in% p2.spp, ] # background from fishnet
p4 <- pers.indexFNET[!pers.indexFNET$FNETID %in% p2.spp & pers.indexFNET$FNETID %in% p2.modFR, ] # background from modelling frame
pers.bufpt <- p4 # new name to dataframe: this dataframe used from now on

# internal checking
length(pers.indexFNET$FNETID) # N of FISHNET
dim(p3)[1] # N of FISHNET - N spp locations
table(pers.indexFNET$PERS106)[[1]] # N spp locations
dim(p3)[1] + table(pers.indexFNET$PERS106)[[1]] # should equal N of FISHNET
length(p2.modFR) # N of modelling frame
dim(p4)[1] # N modelling frame - N spp locations
dim(p4)[1] + table(pers.indexFNET$PERS106)[[1]] # should equal N modelling frame

# start the draws; multiple options shown below
#   NOTE: set seed if desire repeatability of srs
set.seed(1234) # set seed to ensure repeatability: make sure you remember what it is !!
pers.srs1 <- pers.bufpt[sample(1:nrow(pers.bufpt), 
                               table(pers.indexFNET$PERS106)[[1]], replace = F), ]
pers.srs1$PERS106 <- 0 # assign 0 to pers.abs
pers.srs1$in.modFR <- 1 # assign 1 to in.modFR
dim(pers.srs1) # dim[1] should equal N spp locations, dim[2] the No. variables
head(pers.srs1, 2) # examine

# start the draws; multiple options shown below
#   N=2*No. pers
set.seed(1234) # set seed to ensure repeatability
pers.srs2 <- pers.bufpt[sample(1:nrow(pers.bufpt), 2 * table(pers.indexFNET$PERS106)[[1]], replace = F), ]
pers.srs2$PERS106 <- 0  # assign 0 to PERS.abs
pers.srs2$in.modFR <- 1  # assign 1 to in.modFR


######## START MERGE PSEUDO-ABS WITH TRUE PRESENCE DATAFRAME  
# merge with true presences for predictor extraction
head(tru.persFNET, 2)
head(pers.srs1, 2) # examine; both MUST have FNETID

pers.PPsA <- merge(tru.persFNET, pers.srs1, by = c("FNETID", "PERS106", "wgs_xF", "wgs_yF",
                                                   'exp5nrm','exp3nrm','rough_1k','prad_sw_di',
                                                   'etpt_5','mind_yr_av','prec_winte','tave_sprin'), 
                   all = T) # merge
pers.PPsA$in.modFR <- NULL # drop in.modFR index no longer needed
dim(pers.PPsA) # examine
head(pers.PPsA, 2) # dim => No. pers + No. pers.abs

# create common x,y from tru pers and pseudo-abs x,y's
pers.PPsA[352:355, ] # note diff [X,Y] coords for pers=0 & =1

# create new vars wgs_x & wgs_y; used later in raster stack extraction
pers.PPsA$tr.wgs_x <- ifelse(pers.PPsA$PERS106 == 0, pers.PPsA$cell.wgs_x, pers.PPsA$wgs_xF)
pers.PPsA$tr.wgs_y <- ifelse(pers.PPsA$PERS106 == 0, pers.PPsA$cell.wgs_y, pers.PPsA$wgs_yF) 
pers.PPsA[352:355, ] # examine a subset

# resulting data frame is model training data set; outfile if desired
setwd(path.ex)
write.csv(pers.PPsA, file = "pers_PPsA.csv", row.names = F) # save .csv
save(pers.PPsA, pers.bufpt, pers.bufR, pers.bufSF, pers.bufSP,
     file = "pers.PPsA.RData") # save .RData

