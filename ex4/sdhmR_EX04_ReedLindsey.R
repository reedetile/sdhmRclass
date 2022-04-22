
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

## Question #1

setwd(path.ex)
pres <- read.csv('spp106pr_PERS.csv')
head(pres, 2)
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

#####Building Point buffer###
pers.bufptSF <- st_buffer(presSF,
                          units::set_units(0.5,degree))
pers.bufptSF <- st_union(pers.bufptSF)
plot(st_geometry(pers.bufptSF))
plot(st_geometry(pres.bufSF), add = T)
setwd(path.ex)
save(pres.bufSF, file = 'pres.bufSF.RDS')  

  ## Question #2 Build a fishnet for the bounding box
  
setwd(path.gis)
t1 <- raster("templateR_wgs.img") # alternative; import wgs84 template
pers.bufptSP <- as_Spatial(pers.bufptSF)
pers.bufptR <- raster::crop(rasterize(pers.bufptSP, t1), pers.bufptSP)
plot(pers.bufptR)
save(pers.bufptR, file = 'pers.bufptR.RData')
# extract coords, cell id of coords, & build dataframe
f1 <- sp::coordinates(pers.bufptR) # get spatial coords from pied.bufR
f2 <- cellFromXY(pers.bufptR, f1) # extract cell number from buffered extent
f3 <- as.data.frame(cbind(f1, f2)) # build datframe of x,y & cell number
head(f3, 2) # examine
tail(f3, 1) # f2 value should = ncell in pers.bufptR; below
ncell(pers.bufptR) # should = f2 value above

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

---
  
  ## Question #3
  
  # * Use the fishnet to create a pseudo--absence data frame of [X,Y]’s
  # * For simplicity’s sake, set $n$ for pseudo--absence at twice (2$\times$) the number of presences in your respective RANG, PERS, MORT, and SEED data set
  # * Bind these data to the presence dataframe you imported in Question #1
  
  pres.xy <- pres[c("wgs_xF", "wgs_yF")]  # get x,y of species presences
p1 <- sp::coordinates(pres.xy) # set spatial coords from pied.bufptR

# extract FNETID of presence from  modelling frame
pres.fnetid <- cellFromXY(pers.bufptR, p1)
head(pres.fnetid)

#checks
length(pres.fnetid) # should match number of presences ??
length(pres$PERS106) # it does ... life is good !!

# create species dataframe with FNETIDs
tru.presFNET <- cbind(pres.fnetid, pres) # add fishnet ID to tru.pres
names(tru.presFNET)[1] <- "FNETID" # name change 
head(tru.presFNET, 2) # examine

# giggle plots
plot(pers.bufptR, col = "gray70",
     legend = F,
     main = "Points in modelling frame")  # main plot
points(pres$wgs_xF,
       pres$wgs_yF,
       pch = 20,
       col = "darkgreen") # add spp locations

#extract fnetid
buf.fnetid <- raster::extract(pers.bufptR, pres.fnetDF[c("cell.wgs_x", "cell.wgs_y")])

# create modelling dataframe with FNETIDs
tru.bufFNET <- cbind(pres.fnetDF, buf.fnetid) # bind modelling frame w/FISHNET
head(tru.bufFNET, 2) # examine

# some internal checking
length(tru.bufFNET$buf.fnetid) # number FNETIDs in tru.bufFNET
ncell(pers.bufptR) # should equal above
table(tru.bufFNET$buf.fnetid)[[1]] # number of FNETIDs in pied.bufptR
length(which(is.na(tru.bufFNET$bufpt.fnetid))) # No. NAs

# giggle plots: modelling frames nested in fishnet
plot(pers.bufptSP, col = "gray90", main = "Modelling frame on FISHNET")  # main plot
plot(pers.bufptR, col = "gray50", add = T)  # main plot

# giggle plots: spp location nested in modelling frames nested in fishnet
plot(pers.bufptSP, col = "gray90",
     main = "SP locations nested in modelling frame in FISHNET")  # main plot
points(pres$wgs_xF, pres$wgs_yF, pch = 20, col = "darkgreen") # spp locations if desired
plot(pers.bufptR, col = "gray50", legend = F, add = T)  # main plot

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
setwd(path.ex)
save(pres.indexFNET, file = "pres.indexFNET.RData")

# examine the fishnet
head(pres.indexFNET)

##################################################3
#Extracting pseduo absence
# create conditional vectors: FNETIDs by spp locations & in modelling frame
p2.spp <- subset(pres.indexFNET$FNETID, pres.indexFNET$PERS106 == 1) # FNETIDs of spp locations
p2.modFR <- subset(pres.indexFNET$FNETID, pres.indexFNET$in.modFR == 1) # FNETIDs in modelling frame
length(p2.spp) # should equal N of spp locations
length(p2.modFR) # should equal N modelling frame

# drop presence cell FNETIDs; remaining are possible psuedo-abs cell FNETIDs
p3 <- pres.indexFNET[!pres.indexFNET$FNETID %in% p2.spp, ] # background from fishnet
p4 <- pres.indexFNET[!pres.indexFNET$FNETID %in% p2.spp & pres.indexFNET$FNETID %in% p2.modFR, ] # background from modelling frame
pers.bufpt <- p4 # new name to dataframe: this dataframe used from now on

# internal checking
length(pres.indexFNET$FNETID) # N of FISHNET
dim(p3)[1] # N of FISHNET - N spp locations
table(pres.indexFNET$PERS106)[[1]] # N spp locations
dim(p3)[1] + table(pres.indexFNET$PERS106)[[1]] # should equal N of FISHNET
length(p2.modFR) # N of modelling frame
dim(p4)[1] # N modelling frame - N spp locations
dim(p4)[1] + table(pres.indexFNET$PERS106)[[1]] # should equal N modelling frame

# start the draws; multiple options shown below
#   NOTE: set seed if desire repeatability of srs
set.seed(1234) # set seed to ensure repeatability: make sure you remember what it is !!
pers.srs1 <- pers.bufpt[sample(1:nrow(pers.bufpt), 
                               table(pres.indexFNET$PERS106)[[1]], replace = F), ]
pers.srs1$PERS106 <- 0 # assign 0 to pers.abs
pers.srs1$in.modFR <- 1 # assign 1 to in.modFR
dim(pers.srs1) # dim[1] should equal N spp locations, dim[2] the No. variables
head(pers.srs1, 2) # examine

# start the draws; multiple options shown below
#   N=2*No. pres
set.seed(1234) # set seed to ensure repeatability
pers.srs2 <- pers.bufpt[sample(1:nrow(pers.bufpt), 2 * table(pres.indexFNET$PERS106)[[1]], replace = F), ]
pers.srs2$PERS106 <- 0  # assign 0 to PERS.abs
pers.srs2$in.modFR <- 1  # assign 1 to in.modFR


######## START MERGE PSEUDO-ABS WITH TRUE PRESENCE DATAFRAME  
# merge with true presences for predictor extraction
head(tru.presFNET, 2)
head(pers.srs1, 2) # examine; both MUST have FNETID
pers.PPsA <- merge(tru.presFNET,
                   pers.srs1,
                   by = c("FNETID", "PERS106", "wgs_xF", "wgs_yF"), 
                   all = T) # merge
pers.PPsA$in.modFR <- NULL # drop in.modFR index no longer needed
dim(pers.PPsA) # examine
head(pers.PPsA, 2) # dim => No. pres + No. pers.abs

# create common x,y from tru pres and pseudo-abs x,y's
pers.PPsA[352:355, ] # note diff [X,Y] coords for pres=0 & =1

# create new vars wgs_x & wgs_y; used later in raster stack extraction
pers.PPsA$tr.wgs_x <- ifelse(pers.PPsA$PERS106 == 0, pers.PPsA$cell.wgs_x, pers.PPsA$wgs_xF)
pers.PPsA$tr.wgs_y <- ifelse(pers.PPsA$PERS106 == 0, pers.PPsA$cell.wgs_y, pers.PPsA$wgs_yF) 
pers.PPsA[352:355, ] # examine a subset


---
#   
#   ## Question #4
#   
#   * Save your data as R objects, including both the true presence and pseudo-absences:
#   * Dataframe;
# * Point shapefile with geometry in R
# * Export as a point shapefile in ESRI format
# * Save these R objects in a **`.RData`** file
# * Save the bounding box as a raster--based **`.img`** file


# resulting data frame is model training data set; outfile if desired
setwd(path.ex)
write.csv(pers.PPsA, file = "pers_PPsA.csv", row.names = F) # save .csv
save(pers.PPsA, file = "pers.PPsA.RData") # save .RData

writeRaster(pers.bufptR,
            filename = paste(path.figs,"/pers.bufptR.img", sep =''),
            options = c('TFW = YES'),
            overwrite = TRUE)


---
  
  ## The End
  
  ---
  