


## Question #1

#* Build ensemble map products of:
#  * SDHM mean and sd probabilities
#  * SDHM concordance maps of 5, 3, and union of all SDHM overlaps
#  * "Clip" all these maps by the bounding boxes created earlier (see Module 2.3.3 #for refresher, if needed)
#  * Output these map products as **`.img`** files

#path.root <- "D:/OneDrive - University of Vermont/Classes/Spring2022/sdhmR/sdhmR-V2022.1" #Reed Laptop Path
path.root <- "C:/Users/14842/Documents/SDHM_Reed" #Lindsey Path
# some pathnames; yours will be specific to your CPU !!

# below is the recommended class root dir
  #path.root <- "~/sdhmR-V2020.1"  # typical class root dir
path.mod06 <- paste(path.root, "/ex6", sep = "")
  #path.figs <- paste(path.root, "/powerpoints/figures", sep = "")
  #path.gis <- paste(path.root, "/data/gis_layers", sep = "")

# load libraries
  library(raster)
 options("rgdal_show_exportToProj4_warnings"="none") # run this before library(rgdal)
  library(rgdal)
  library(gam)
  library(randomForest)
  library(dismo)
  library(gbm)
  library(sf)

################################################################################
######## START INITIALIZATION OF DATA STRUCTURES
# load SDM models; assumes have saved models w/consistent file naming process
  setwd(paste(path.mod06, sep = ""))
 load('tr_PERS.RData')
  mod.list <- c("mod.BRT.RData",
                "mod.GAM.RData",
                "mod.LR.RData",
    "mod.MAX.RData",
    "mod.RF.RData") # build list of SDHM models
  mod.list # examine
```

```{r}
# loop for loading models and assigning consistent names
  for (i in 1:length(mod.list)) {
    m1 <- unlist(strsplit(mod.list[i],
                          ".RData")) # char manipulation
    m2 <- substr(m1,
                 5,
                 nchar(m1)) # char manipulation
    assign(paste("modF.",
                 m2,
                 sep = ""),
           get(load(mod.list[i])))
  }
  ls(pattern = "modF.") # new r objects for use in prediction 

```
```{r}
# list of thresholds for classification; assume have been saved
  load("modF.cut.RData") # load threshold cuts as modF.cut object
  modF.cut # examine
```

**YET ANOTHER NOTE RELATED TO CODE:**  
If King's English interpretation is required, it will go here, after each code chunk.

---

## Question #2

* Calculate the frequencies of "presence" in each of the 5 SDHMs

```{r}



```

---

## Question #3

* Tally the frequencies of concordance after "clipping" and compare with above

---

## Question #4

* Save your data as R objects:
  * All ensemble prediction maps as **`.img`** format
* Save these R objects in a **`.RData`** file as well

---

## The End

---
