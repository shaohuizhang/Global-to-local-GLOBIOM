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

### Read data
MWI2010_raw <- readRDS("Cache/MWI2010_raw.rds")

### REVEALING MISSING DATA
# http://web.maths.unsw.edu.au/~dwarton/missingDataLab.html
md.pattern(MWI2010_raw)
aggr(MWI2010_raw, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(MWI2010), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
#missing.pattern.plot(MWI2010, mis.col=mdc(2), obs.col=mdc(1), main="Misisng Pattern")     
marginplot(MWI2010_raw[, c("plot_area_qty_gps", "plot_area_qty_f")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)
# Note that, if data are MCAR, we expect the blue and red box plots to be identical

### convert NAN to NA
# Missing values in .dta files are coded as NAN in R.
# Probably needed for imputation as it imputes NA (and probably not NAN)
# Note that NAN is counted as NA in summary!

nan_clean_f<-function(x){
  x[do.call(cbind, lapply(x, is.nan))]<-NA
  return(x)
}

MWI2010_raw <- nan_clean_f(MWI2010_raw)

# Remove observations where cropcode is missing as we cannot impute those
MWI2010_raw <- filter(MWI2010_raw, !is.na(crop))

### Create test data set
#MWI2010 <- MWI2010_raw[c(1:1500),]

### IMPUTE MISSING VALUES
# Impute
MWI2010_imp = mice(MWI2010_raw, m=5, printFlag=FALSE, maxit = 40, seed=2525)

# Check plausibility of imputed values for instance by comparing yield per crop and region. 
# Imputed values should be in the same range.
# Compare distributions with and without imputation

# Obtain all x imputed values and take average
MWI2010_imp <- complete(MWI2010_imp, action = "long") %>%
  select(-.imp) %>%
  group_by(.id, case_id, ea_id, plotnum, crop) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  select(-.id)

summary(MWI2010_imp)

# Save file
saveRDS(MWI2010_imp, "Cache/MWI2010_imp.rds")

  
