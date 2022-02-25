#sdhmR Exercise 06
#02/23/2022
#RCS & LAP

### GIT TEST

#HI LINDSEY

#----------Global options---------#
#Setting Paths
#path.root <- "D:/OneDrive - University of Vermont/Classes/Spring2022/sdhmR/sdhmR-V2022.1"
path.root <- "C:/Users/14842/Documents/SDHM/sdhmR-V2022.1"
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

# source course CRSs
setwd(path.root)
getwd()
source("r_code/prjs4_sdhmR.r")
ls(pattern = "prj.") # should be 5 prj.* objects

######Question 1######
######################
# Break the predictor variables into logical groups of **topography**, **temperature**, and **precipitation**.  Do this by creating a new column called **`pred_type`** and add it the dataframe.
setwd(path.ex)
load('pers.trTOPO.RDATA')
table(pers.trTOPO['PERS106'])
  
unique(pers.trTOPO$PERS106)
unique(pers.trTOPO$etpt_5.x)
head(pers.trTOPO)

##### IDEAS I HAD THAT ARN'T WORKING #################
### Also, are we sure pers.trTOPO is built correctly? Why are there so many versions of the predictor within it?
preds.list <- list.files(pattern = ".img$") # list of .img files; $ strips extra
preds.list # examine

pred_type <- ifelse(pres.abs$SPPRES106 != NA, 
             extract(elev, pres.abs[, c("cell.wgs_x", "cell.wgs_y")]), 
             extract(elev, pres.abs[, c("wgs_xF", "wgs_yF")]))

###########################################

## Question #2
#* Assess correlations:
#  * Among all variables; and
#* Within each of the logical groupings created above

# numeric correlations
cut.point <- 0.7 # set cutpoint for correlation
c1 <- cor(pers.trTOPO[, c(27:42)],
          use = "pairwise.complete.obs", 
          method = "spearman") # est. correlation
c1 # examine
c2 <- subset(c1 > cut.point | c1 < -cut.point) # matrix of cor>cutpoint
c2 # examine; FALSE indicates cor<cutpoint

panel.cor <- function(x, y,
                      digits=2,
                      prefix="",
                      cex.cor) 
{ usr <- par("usr"); on.exit(par(usr)) 
par(usr = c(0, 1, 0, 1)) 
r <- abs(cor(x,
             y,
             use = "pairwise.complete.obs")) 
txt <- format(c(r,
                0.123456789),
              digits = digits)[1] 
txt <- paste(prefix,
             txt,
             sep = "") 
if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 

test <- cor.test(x,y) 
# borrowed from printCoefmat
Signif <- symnum(test$p.value,
                 corr = FALSE,
                 na = FALSE, 
                 cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                 symbols = c("***", "**", "*", ".", " ")) 
text(0.5,
     0.5,
     txt,
     cex = cex * r)
text(.8,
     .8,
     Signif,
     cex = cex,
     col = 2) 
}

pairs(pers.trTOPO[,
              c(27:42)],
      lower.panel = panel.smooth, 
      upper.panel = panel.cor,
      main = "Topo Variables") 

## Question #3

#* Examine variable importance of each predictor variable as related to presence:absence using the process described in the Module.     

#### START function variable importance
varimp.glm <- function(tr.spp,
                       tr.var,
                       pres,
                       pf,
                       pl) {
  tmp.mat <- matrix(ncol = 2,
                    nrow = (pl - pf + 1))
  for (i in pf:pl) {
    # option: linear+quadratic; linear only
    tmp <- glm(tr.spp[, pres] ~ tr.var[, i] + 
                 I((tr.var[, i])^2),
               na.action = na.omit, 
               family = binomial)
    # linear only glm
    #tmp <- glm(tr.spp[, pres] ~ tr.var[, i], na.action = na.omit, family = binomial)
    tmp.mat[(i - pf + 1), 1] <- tmp$aic
    tmp.mat[(i - pf + 1), 2] <- (1 - (tmp$deviance/tmp$null.deviance))
  }
  return(tmp.mat)
} 
#### END function variable importance

# estimate VIP values => AIC & Adj deviance
tr.vip <- pers.trTOPO[, c(2, 27:42)] # keep only P/A & predictors
pres <- 1 # column for presence:absence
v.start <- 2 # column start predictor variables
v.stop <- ncol(tr.vip) # last column predictor variables
v.num <- v.stop - 1 # number predictor variables
dev.fit <- varimp.glm(tr.vip,
                      tr.vip,
                      pres,
                      v.start,
                      v.stop) # call VIP function
dev.fit # output matrix; col=1 AIC, col=2 Adj deviance 

# built basic barplot if desired
d.max <- ceiling(signif(max(dev.fit[, 2]), 2) * 10)/10 # max of y-axis
ylim.r <- range(0, d.max) # range y-axis
x.labs <- names(tr.vip[2:v.stop]) # x-axis labels
barplot(dev.fit[, 2],
        col = "darkgreen",
        ylim = ylim.r,
        main = "topo VIPs", 
        ylab = "adj.D2",
        names = x.labs,
        las = 2) # barplot
abline(h = 0) # add horizontal line
abline(mean(dev.fit[, 2]),
       0,
       lt = 3) # ref lines; dash=mean adj.dev 

# save plots
#setwd(path.figs)
#savePlot(filename = "mod2.7fig03.pdf", type = "pdf")
######## END VARIABLE IMPORTANCE AMONG TOPOGARPHIC PREDICTORS
################################################################################

## Question #4

#* Eliminate redundant variables with a goal of retaining 7-10 of the 16 available
#* Justify your decision(s) to keep / remove variables using bullets

### Redundant variables don't cross mean
### Variables that cross mean have most explanatory value

#---
  
  ## Question #5
  
#  * Include the term **tr_MORT**, **tr_PERS**, **tr_RANG**, and **tr_SEED** somewhere in the final training data objects, depending on your data group.  Use of **tr_ByGroup** here means the data are now ready for model creation.
#* Save your data as R objects:
#  * Dataframe;
#* Point shapefile with geometry in R
#* Export as a point shapefile in ESRI format
#* Save these R objects in a **`.RData`** file

#These data will be used as we next begin the SDHM model constructions.