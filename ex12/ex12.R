## Question #1

#* Build ensemble map products of:
#  * SDHM mean and sd probabilities
#  * SDHM concordance maps of 5, 3, and union of all SDHM overlaps
#  * "Clip" all these maps by the bounding boxes created earlier (see Module 2.3.3 #for refresher, if needed)
#  * Output these map products as **`.img`** files

#path.root <- "D:/OneDrive - University of Vermont/Classes/Spring2022/sdhmR/sdhmR-V2022.1" #Reed Laptop Path
path.root <- "C:/Users/14842/Documents/SDHM/sdhmR-V2022.1" #Lindsey Path
path.mod <- paste(path.root, "/data/exercise/traindat", sep = "")
path.preds <- paste(path.root, '/data/exercise/preds', sep = '')
path.maps <- paste(path.root, '/data/exercise/maps', sep = '')
path.gis <- paste(path.root, "/data/gis_layers", sep = "")
path.figs <- paste(path.root, "/powerpoints/figures", sep = "")


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
 
# load SDM models. I renamed the models and the threshold cuts to make it more clear
 
setwd(paste(path.mod, sep = ""))
load('ex7.RData')
  LR.cut <- mod.cut$glm.cvpred ### Assuming this is the cut point? In his example it's just one value
  LR <- mod2.LR
load('ex8.RData')
  GAM.cut <- cut$pred
  GAM <- mod1.GAM
load('ex9.RData')
  MAX <- mod1.MAX
  MAX.cut <- mod.cut$spec_sens
load('ex10.RData')
  RF <- pers.RF
  RF.cut <- mod.cut$persRF.pred
load('ex11.RData')
  BRT <- pers.BRT
  BRT.cut <- mod.cut$pred
  

### Building a list of cut points. Missing the RF and BRT threshold cuts.
  cut.list <- c(LR.cut,
                GAM.cut,
                MAX.cut,
                RF.cut,
                BRT.cut
  )
  cut.list # threshold cuts?

# raster stack of predictors assumes rasters all have same projection, extent & resolution
  setwd(path.preds)
  pers.list <- list.files(pattern = ".img$") # list of .img files; $ strips extra
  pers.list # examine
  pers.dom <- stack(pers.list) # build raster stack
  pers.dom # examine stack
  names(pers.dom) 

# import some shapefiles for pretty plots
  setwd(path.gis)
  pegr6.mahog <- st_read(dsn = ".", layer = "rareplant_mahog")
  pegr6.frame <- st_read(dsn = ".", layer = "rareplant_frame")
  
# LR prediction and classification
  setwd(paste(path.maps,
              sep = ""))
  names(pers.dom) <- paste(names(pers.dom),'img')
  modFprob.LR <- predict(pers.dom,
                         LR,
                         filename = "modFprob.GLM.img", 
                         type = "response",
                         fun = predict,
                         index = 2,
                         overwrite = T)
  modFclass.LR <- reclassify(pers.prob,
                             filename = "modFclas.LR.img",
                           c(0,
                             mod.cut[[2]],
                             0,
                                        mod.cut[[2]],
                             1,
                             1),
                           overwrite=TRUE)
  
# giggle plots
  par(mfrow = c(1, 2))
  plot(modFprob.LR,
       axes = T,
       main = "LR probability map") # plot probability map
  plot(st_geometry(pegr6.mahog),
       add = T)
  plot(st_geometry(pegr6.frame),
       add = T)  # make pretty
  plot(modFclass.LR,
       axes = T,
       main = "LR classfied map") # plot classified map
  plot(st_geometry(pegr6.mahog),
       add = T)
  plot(st_geometry(pegr6.frame),
       add = T) # make pretty 
  par(mfrow = c(1, 1))
  
  # save LR plots if desired 
setwd(paste(path.figs,
             sep = ""))
savePlot(filename = "mod06fig01.pdf", type = "pdf")

# GAM prediction and classification
  library(gam)
  modFprob.GAM <- predict(pers.dom,
                          GAM,
                          filename = "modFprob.GAM.img", 
                          type = "response",
                          fun = predict,
                          index = 2,
                          overwrite = T)
  modFclas.GAM <- reclassify(modFprob.GAM,
                             filename = "modFclas.GAM.img", 
                             (c(0,
                                GAM.cut,
                                0,
                                GAM.cut,
                                1,
                                1)),
                             overwrite = T) 
  
  # giggle plots
  par(mfrow = c(1, 2))
  plot(modFprob.GAM,
       axes = T,
       main = "GAM probability map") # plot probability map
  plot(st_geometry(pegr6.mahog),
       add = T)
  plot(st_geometry(pegr6.frame),
       add = T) # make pretty
  plot(modFclas.GAM,
       axes = T,
       main = "GAM classfied map") # plot classified map
  plot(st_geometry(pegr6.mahog),
       add = T)
  plot(st_geometry(pegr6.frame),
       add = T) # make pretty 
  par(mfrow = c(1, 1)) 
  
  
  
# RF prediction and classification
  library(randomForest)  # load library
  modFprob.RF <- predict(pred.dom,
                         mod.RF,
                         filename = "modFprob.RF.img", 
                         type = "prob",
                         fun = predict,
                         index = 2,
                         overwrite = T) # prob map
  modFclas.RF <- reclassify(modFprob.RF,
                            filename = "modFclas.RF.img", 
                            (c(0,
                               modF.cut$RF.cut,
                               0,
                               modF.cut$RF.cut,
                               1,
                               1)),
                            overwrite = T) # class map
  
  # giggle maps
  par(mfrow = c(1, 2))
  plot(modFprob.RF,
       axes = T,
       main = "RF probability map") # plot probability map
  plot(st_geometry(pegr6.mahog),
       add = T)
  plot(st_geometry(pegr6.frame),
       add = T) # make pretty
  plot(modFclas.RF,
       axes = T,
       main = "RF classfied map") # plot classified map
  plot(st_geometry(pegr6.mahog),
       add = T)
  plot(st_geometry(pegr6.frame),
       add = T) # make pretty 
  par(mfrow = c(1, 1))  
  
 ########################################################################### 
## Question #2
# Calculate the frequencies of "presence" in each of the 5 SDHMs

#############################################################################
## Question #3
#Tally the frequencies of concordance after "clipping" and compare with above

#############################################################################
## Question #4

#* Save your data as R objects:
#  * All ensemble prediction maps as **`.img`** format
#* Save these R objects in a **`.RData`** file as well


## The End
