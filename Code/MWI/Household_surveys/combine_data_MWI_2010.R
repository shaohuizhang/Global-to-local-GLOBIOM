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


### DETERMINE ROOT PATH AND SET WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)

### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))

### LOAD DATA
# Survey
suppressMessages(source("Code/MWI/Household_surveys/survey_MWI_2010.R"))
summary(survey2010)

# Location
suppressMessages(source("Code/MWI/Household_surveys/location_MWI_2010.R"))
summary(location2010)

# Crops
suppressMessages(source("Code/MWI/Household_surveys/crops_MWI_2010.R")) 
summary(crops2010)

# Permanent crops
suppressMessages(source("Code/MWI/Household_surveys/pcrops_MWI_2010.R")) 
summary(pcrops2010)

# Plot area
suppressMessages(source("Code/MWI/Household_surveys/plot_area_MWI_2010.R"))
summary(plot_area2010)

### JOIN DATA
# Bind crops and permanent crops
crops <- bind_rows(crops2010, pcrops2010) %>%
  dplyr::select(case_id, ea_id, plotnum, crop_code, crop_qty_harv)

# link other variables
MWI2010 <- left_join(crops, survey2010)
MWI2010 <- left_join(MWI2010, plot_area2010)
MWI2010 <- left_join(MWI2010, location2010)

### CLEAN UP
rm(location2010, crops2010, pcrops2010, crops, plot_area2010, survey2010)

