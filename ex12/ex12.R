## Question #1

#* Build ensemble map products of:
#  * SDHM mean and sd probabilities
#  * SDHM concordance maps of 5, 3, and union of all SDHM overlaps
#  * "Clip" all these maps by the bounding boxes created earlier (see Module 2.3.3 #for refresher, if needed)
#  * Output these map products as **`.img`** files

path.root <- "D:/OneDrive - University of Vermont/Classes/Spring2022/sdhmR/sdhmR-V2022.1" #Reed Laptop Path
#path.root <- "C:/Users/14842/Documents/SDHM/sdhmR-V2022.1" #Lindsey Path
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
                             filename = "modFclass.LR.img",
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
  modFprob.RF <- predict(pers.dom,
                         RF,
                         #filename = "modFprob.RF.img", 
                         type = "prob",
                         fun = predict,
                         index = 2,
                         overwrite = T) # prob map
  modFclas.RF <- reclassify(modFprob.RF,
                            filename = "modFclas.RF.img", 
                            (c(0,
                               RF.cut,
                               0,
                               RF.cut,
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
  

library(dismo)
library(gbm)
modFprob.BRT <- predict(pers.dom,
                        BRT,
                        n.trees = BRT$gbm.call$best.trees, 
                          type = "response",
                        filename = "modFprob.BRT.img",
                        overwrite = T) # prob map
  modFclas.BRT <- reclassify(modFprob.BRT,
                             c(0, BRT.cut,
                               0, BRT.cut,
                               1,
                               1), 
                             filename = "modFclas.BRT.img",
                             overwrite = T) # clas map
  
  # giggle plots
  par(mfrow = c(1, 2))
  plot(modFprob.BRT,
       axes = T,
       main = "BRT probability map") # plot probability map
  plot(st_geometry(pegr6.mahog),
       add = T)
  plot(st_geometry(pegr6.frame),
       add = T) # make pretty
  plot(modFclas.BRT, axes = T,
       main = "BRT classfied map") # plot classified map
  plot(st_geometry(pegr6.mahog),
       add = T)
  plot(st_geometry(pegr6.frame),
       add = T) # make pretty 
  par(mfrow = c(1, 2))
  
  # save BRT plots if desired 
  setwd(path.figs)
  savePlot(filename = "mod06fig04.pdf",
           type = "pdf")
  
  ## MAXENT prediction and classification
  setwd(path.maps)
  library(dismo) # load library
  modFprob.MAX <- predict(MAX,
                          pers.dom,
                          filename = "modFprob.MAX.img",
                          overwrite = T)  
  modFclas.MAX <- reclassify(modFprob.MAX,
                             filename = "modFclas.MAX.img", 
                             c(0,
                               MAX.cut,
                               0,
                               MAX.cut,
                               1,
                               1),
                             overwrite = T) # clas map
  
  # giggle plots
  par(mfrow = c(1, 2))
  plot(modFprob.MAX, axes = T, main = "MAX probability map") # plot probability map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T) # make pretty
  plot(modFclas.MAX, axes = T, main = "MAX classfied map") # plot classified map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T) # make pretty 
  par(mfrow = c(1, 1))
  
  # save MAX plots if desired 
  setwd(path.figs)
  savePlot(filename = "mod06fig05.pdf", type = "pdf")
  ####
  ######## END PREDICTION PROBABILITY AND CLASSIFED MAPS
  ################################################################################
  
  ################################################################################
  ######## START ENSEMBLE PROCESS
  # load probability & classified maps; create stacks
  setwd(paste(path.maps, sep = ""))
  pr.list <- unlist(unique(strsplit(list.files(pattern = "Fprob"),
                                    ".aux.xml")))
  pr.list # examine
  prob.dom <- stack(pr.list) # raster stack prob maps
  prob.dom # examine
  
  # cl.list <- list.files(pattern = "Fclas")
  cl.list <- unlist(unique(strsplit(list.files(pattern = "Fclas"),
                                    ".aux.xml")))
  cl.list # examine
  clas.dom <- stack(cl.list) # raster stack classified maps
  clas.dom # examine
  
  # standardize all prediction maps 0-1.0
  maxValue(prob.dom) # extract max value for each prob map
  layers <- {} # initialize (empty) list of raster layers
  for (i in 1:length(names(prob.dom))) {
    m1 <- prob.dom[[i]] # get a prob map
    m2 <- 1/maxValue(prob.dom[[i]]) * prob.dom[[i]] # standardize all probs to max=1
    m3 <- unlist(strsplit(names(prob.dom[[i]]), "[.]")) # split prob layer name apart
    names(m2) <- paste(m3[1], "STD.", m3[2], sep = "")  # assign name to raster value
    assign(paste(m3[1], "STD.", m3[2], sep = ""), m2) # assign new name to standardized layer
    layers <- c(layers, get(paste(m3[1], "STD.", m3[2], sep = "")))
  }
  probSTD.dom <- stack(layers)
  maxValue(probSTD.dom) # extract max value for each prob map; standardized ?
  
  # save stacks
  setwd(path.mod)
  save(prob.dom,
       probSTD.dom,
       clas.dom,
       file = "ensemble.dom.RData")
  
  ######################################################
  # descriptive stats on raster maps: mean & sum
  prob.mean <- mean(prob.dom) # mean prob map
  # access min max values of descriptive stat raster
  maxValue(prob.mean) # max pr.mean
  minValue(prob.mean) # min pr.mean
  probSTD.mean <- mean(probSTD.dom) # mean standardized prob map
  clas.sum <- sum(clas.dom) # sum of models by cell
  save(prob.mean,probSTD.mean,clas.sum, file = 'ensembleSTATS.RData')
  # giggle plots: mean & sum
  par(mfrow = c(1, 2))
  plot(prob.mean,
       axes = T,
       main = "MEAN probability map") # plot probability map
  plot(st_geometry(pegr6.mahog),
       add = T)
  plot(st_geometry(pegr6.frame),
       add = T) # make pretty
  plot(clas.sum,
       axes = T,
       main = "Concordance CLASS map: Ramp") # plot concordance map
  plot(st_geometry(pegr6.mahog),
       add = T)
  plot(st_geometry(pegr6.frame),
       add = T) # make pretty 
  par(mfrow = c(1, 1))
  
  # save plots if desired 
  setwd(path.figs)
  savePlot(filename = "mod06fig06.pdf", type = "pdf")
  
  ####
  # arithmetic on raster maps: multiplication & subsetting
  prob.mean1 <- (clas.sum > 0) * prob.mean  # mean where class=1
  clas.sum1 <- clas.sum > 0 # sum where No. models >=1
  
  # giggle plots: means X classifications
  par(mfrow = c(1, 2))
  plot(prob.mean1,
       axes = T,
       main = "MEAN probability map: Presence=1") # plot probability map
  plot(st_geometry(pegr6.mahog),
       add = T)
  plot(st_geometry(pegr6.frame),
       add = T) # make pretty
  plot(clas.sum1, axes = T,
       main = "Concordance CLASS map: Union") # plot concordance map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T) # make pretty 
  par(mfrow = c(1, 1))
  
  
  ####
  # calculate measures of variation
  prob.sd <- calc(prob.dom, sd) # sd prob map
  prob.var <- calc(prob.dom, var) # var prob map
  prob.cv <- (prob.sd/prob.mean)*100 # coefficient of variation
  
  # giggle plots: sd and var
  par(mfrow = c(1, 2))
  plot(prob.sd,
       axes = T,
       main = "SD probability map") # plot classified map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T) # make pretty
  plot(prob.var, axes = T, main = "VAR probability map") # plot classified map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T) # make pretty 

  
  # make pretty 
  
  # save plots if desired
  #setwd(path.figs)
  #savePlot(filename = "mod06fig13.pdf", type = "pdf")
  ######## END ENSEMBLE INTERPRETATION
  ################################################################################
 ########################################################################### 
## Question #2
# Calculate the frequencies of "presence" in each of the 5 SDHMs

  ####
  # some summary statistics: frequencies
  clas.freqM <- freq(clas.dom) # [0,1] freqs by clas map; rtns list
  clas.freqM[c(1,5)] # examine freqs for 1st 2 models
  clas.freqS <- data.frame(freq(clas.sum)) # freqs of concordance of models
  clas.freqS # examine; value is concordance freq
  clas.freqS$count[6]/sum(clas.freqS$count[2:6]) # prop. models concordance=5 
  
  
  ######## START ENSEMBLE INTERPRETATION
  ####
  # 6 prob map plots
  par(mfrow = c(2, 3))
  plot(modFprob.LR,
       axes= T,
       main = "LR probability map") # LR prob map
  plot(st_geometry(pegr6.mahog),
       add = T)
  plot(st_geometry(pegr6.frame),
       add = T) # make pretty
  
  plot(modFprob.GAM,
       axes = T,
       main = "GAM probability map") # GAM prob map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T) # make pretty
  
  plot(modFprob.MAX,
       axes = T,
       main = "MAX probability map") # MAX prob map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T) # make pretty
  
  plot(modFprob.RF,
       axes = T,
       main = "RF probability map") # RF prob map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T) # make pretty
  
  plot(modFprob.BRT,
       axes = T,
       main = "BRT probability map") # BRT prob map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T) # make pretty 
  
  plot(prob.mean1,
       axes = T,
       main = "MEAN probability map: \nPresence=1") # MEAN prob map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T) # make pretty 
  
  
  ####
  # 6 classified maps
  par(mfrow = c(2, 3))
  plot(modFclass.LR,
       axes = T,
       main = "LR classfied map") # LR classified map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T) # make pretty
  
  plot(modFclas.GAM, axes = T, main = "GAM classfied map") # GAM classified map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T) # make pretty
  
  plot(modFclas.MAX, axes = T, main = "MAX classfied map") # MAX classified map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T) # make pretty
  
  plot(modFclas.RF, axes = T, main = "RF classfied map") # RF classified map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T) # make pretty
  
  plot(modFclas.BRT, axes = T, main = "BRT classfied map") # BRT classified map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T) # make pretty 
  
  plot(clas.sum, axes = T, main = "Concordance CLASS \nmap: Ramp") # plot concordance map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T) # make pretty
  
  # save plots if desired 
  #setwd(path.figs)
  #savePlot(filename = "mod06fig11.pdf", type = "pdf")
  
  ####
  # 6 classification union maps
  clas.sumU <- clas.sum > 0 # sum where No. models =union
  clas.sum1 <- clas.sum > 1 # sum where No. models =2+
  clas.sum2 <- clas.sum > 2 # sum where No. models =3+
  clas.sum3 <- clas.sum > 3 # sum where No. models =4+
  clas.sum4 <- clas.sum > 4 # sum where No. models =5 
  
  # giggle plots
  par(mfrow = c(2, 3))
  plot(clas.sum, axes = T, main = "Concordance CLASS \nmap: Ramp")  # RAMP concordance map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T)  # make pretty
  
  plot(clas.sumU, axes = T, main = "Concordance CLASS \nmap: Union")  # UNION concordance map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T)  # make pretty
  
  plot(clas.sum1, axes = T, main = "Concordance CLASS \nmap: 2+")  # 2+ concordance map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T)  # make pretty
  
  plot(clas.sum2, axes = T, main = "Concordance CLASS \nmap: 3+")  # 3+ concordance map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T)  # make pretty
  
  plot(clas.sum3, axes = T, main = "Concordance CLASS \nmap: 4+")  # 4+ concordance map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T)  # make pretty 
  
  plot(clas.sum4, axes = T, main = "Concordance CLASS \nmap: 5") # 5 concordance map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T) # make pretty
  
  # save plots if desired
  #setwd(path.figs)
  #savePlot(filename = "mod06fig12.pdf", type = "pdf")
  
  ####
  # some variance analyses
  q1 <- summary(prob.sd)[2] # 1st quartile
  q2 <- summary(prob.sd)[3] # 2nd quartile
  q3 <- summary(prob.sd)[4] # 3rd quartile
  prob.sd1q <- (prob.sd >= q1) * clas.sum1 # where 2+ models agree
  prob.sd2q <- (prob.sd >= q2) * clas.sum1 # where 2+ models agree
  prob.sd3q <- (prob.sd >= q3) * clas.sum1 # where 2+ models agree
  
  # giggle plots
  par(mfrow = c(2, 2))
  plot(prob.sd, axes = T, main = "SD probability map") # SD prob map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T) # make pretty
  
  plot(prob.sd1q, axes = Tmain = "1st quartile SD map") # 1stQ prob map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T) # make pretty
  
  plot(prob.sd2q, axes = T, main = "2nd quartile SD map") # 2ndQ prob map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T) # make pretty
  
  plot(prob.sd3q, axes = T, main = "3rd quartile SD map") # 3rdQ prob map
  plot(st_geometry(pegr6.mahog), add = T)
  plot(st_geometry(pegr6.frame), add = T)
  
  
#############################################################################
## Question #3
#Tally the frequencies of concordance after "clipping" and compare with above

#### clipping...
  # Need to clip pers.DOM 

  pers.dom # examine stack
  names(pers.dom) 
  plot(pers.dom)
############################################# CLIPP
  
  setwd(path.mod)
  load("pers.bufR.RData")
  load("pers.bufptR.img")
  pers.dom.clip <- raster::crop(x = pers.dom, y = pers.bufptR)
  plot(pers.dom.clip)
  plot(pers.bufR)
  plot(pers.dom)
  
#############################################################################
## Question #4

#* Save your data as R objects:
#  * All ensemble prediction maps as **`.img`** format
#* Save these R objects in a **`.RData`** file as well


## The End
