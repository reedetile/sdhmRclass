## Question #1

#* Build ensemble map products of:
#  * SDHM mean and sd probabilities
#  * SDHM concordance maps of 5, 3, and union of all SDHM overlaps
#  * "Clip" all these maps by the bounding boxes created earlier (see Module 2.3.3 #for refresher, if needed)
#  * Output these map products as **`.img`** files

#path.root <- "D:/OneDrive - University of Vermont/Classes/Spring2022/sdhmR/sdhmR-V2022.1" #Reed Laptop Path
path.root <- "C:/Users/14842/Documents/SDHM/sdhmR-V2022.1" #Lindsey Path
# some pathnames; yours will be specific to your CPU !!

# below is the recommended class root dir
  #path.root <- "~/sdhmR-V2020.1"  # typical class root dir
path.mod <- paste(path.root, "/data/exercise/traindat", sep = "")
  #path.figs <- paste(path.root, "/powerpoints/figures", sep = "")
  #path.gis <- paste(path.root, "/data/gis_layers", sep = "")

# load libraries
  library(raster)
 options("rgdal_show_exportToProj4_warnings" = "none") # run this before library(rgdal)
  library(rgdal)
  library(gam)
  library(randomForest)
  library(dismo)
  library(gbm)
  library(sf)

################################################################################
######## START INITIALIZATION OF DATA STRUCTURES
# load SDM models; assumes have saved models w/consistent file naming process
  setwd(paste(path.mod, sep = ""))
 load('ex7.RData')
 GLM.cut <- mod.cut
 GLM <- mod1.acc
 load('ex8.RData')
GAM.cut <- cut
GAM <- mods.acc
 load('ex9.RData')
 MAX <- mod1.MAX
MAX.cut <- mod.cut
 load('ex10.RData')
 RF <- pers.RF
 load('ex11.RData')
 BRT <- pers.BRT
  mod.list <- c("GLM",
                "GAM",
                "MAX",
                "RF",
                "BRT"
                ) # build list of SDHM models
  mod.list # examine
  
  cut.list <- c("GLM.cut",
                "GAM.cut",
                "MAX.cut"
  )
  cut.list # threshold cuts?

# list of thresholds for classification; assume have been saved
  load("modF.cut.RData") # load threshold cuts as modF.cut object
  modF.cut # examine

  
  
  
## Question #2
# Calculate the frequencies of "presence" in each of the 5 SDHMs


## Question #3
#Tally the frequencies of concordance after "clipping" and compare with above


## Question #4

#* Save your data as R objects:
#  * All ensemble prediction maps as **`.img`** format
#* Save these R objects in a **`.RData`** file as well


## The End
