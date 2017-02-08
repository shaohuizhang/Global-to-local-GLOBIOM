#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Combine data from different parts of the hh survey.
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
p_load("haven")


### SET WORKING DIRECTORY
wdPath<-"~/Global-to-local-GLOBIOM"
setwd(wdPath)

### SET DATAPATH
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM\\Data\\Raw\\MWI\\Household_survey\\2010\\IHS3"

### LOCATION
suppressMessages(source("Code/Household/location_MWI_2010_11.R"))

### OUTPUT
suppressMessages(source("Code/Household/output_MWI_2010_11.R"))

### PLOT AREA
suppressMessages(source("Code/Household/plot_area_MWI_2010_11.R"))

### JOIN DATA
MWI2010 <- left_join(location2010, output2010)
MWI2010 <- left_join(cs2010, plot_area2010)


