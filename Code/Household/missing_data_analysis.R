#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  script to analyse missing data in sample
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("mice", "VIM")


### SET WORKING DIRECTORY
wdPath<-"~/Global-to-local-GLOBIOM"
setwd(wdPath)

### SET DATAPATH
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM\\Data\\Raw\\MWI\\Household_survey\\2010\\IHS3"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### REVEALING MISSING DATA
# http://web.maths.unsw.edu.au/~dwarton/missingDataLab.html
md.pattern(MWI2010)
aggr(MWI2010, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(MWI2010), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
#missing.pattern.plot(MWI2010, mis.col=mdc(2), obs.col=mdc(1), main="Misisng Pattern")     
marginplot(MWI2010[, c("plot_area_qty_gps", "plot_area_qty_f")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)
# Note that, if data are MCAR, we expect the blue and red box plots to be identical

### IMPUTE MISSING VALUES
imp = mice(MWI2010, m=5, printFlag=FALSE, maxit = 40, seed=2525)
