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
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM\\Data\\"

### LOAD DATA
# Survey
suppressMessages(source("Code/Household/survey_MWI_2010.R"))
summary(survey2010)

# Location
suppressMessages(source("Code/Household/location_MWI_2010.R"))
summary(location2010)

# Output
suppressMessages(source("Code/Household/output_MWI_2010.R")) # includes duplicates! CHECK
summary(output2010)

# Plot area
suppressMessages(source("Code/Household/plot_area_MWI_2010.R"))
summary(plot_area2010)

### JOIN DATA
MWI2010 <- left_join(survey2010, output2010)
MWI2010 <- left_join(MWI2010, output2010)

# Save file
saveRDS(MWI2010, "cache/MWI2010_raw.rds")

MWI2010 <- left_join(MWI2010, output2010)
MWI2010 <- left_join(MWI2010, plot_area2010)

output2010X <- unique(output2010)
