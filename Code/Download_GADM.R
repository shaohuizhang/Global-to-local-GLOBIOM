#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Download GADM maps
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
#p_load("WDI", "countrycode")


### SET WORKING DIRECTORY
wdPath<-"~/Global-to-local-GLOBIOM"
setwd(wdPath)

### SET DATAPATH

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### FUNCTIONS
# Obtain country coordinates for target country
GADM_f <- function(iso3c, lev=0, proj = "+proj=longlat +datum=WGS84", dataPath = getwd()){
  #if(!dir.exists(dataPath)) dir.create(dataPath)
  gadm = getData('GADM', country=iso3c, level=lev, path = dataPath)
  # change projection 
  projection <- proj
  country.sp <- spTransform(gadm, CRS(projection))
  return(country.sp)
}

# List of countries
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM\\Data\\Processed\\MWI\\GADM_maps"
MWI_adm0 <- GADM_f("MWI", lev = 0, dataPath = dataPath)
MWI_adm1 <- GADM_f("MWI", lev = 1, dataPath = dataPath)
MWI_adm2 <- GADM_f("MWI", lev = 2, dataPath = dataPath)
MWI_adm3 <- GADM_f("MWI", lev = 3, dataPath = dataPath)


TZA_adm1 <- GADM_f("TZA", lev = 1, dataPath = file.path(getwd(), "Data/GADM_maps"))
EHT_adm1 <- GADM_f("ETH", lev = 1, dataPath = file.path(getwd(), "Data/GADM_maps"))
