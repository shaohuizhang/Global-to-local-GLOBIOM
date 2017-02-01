#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Test
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
#p_load("WDI", "countrycode")


### SET WORKING DIRECTORY
# wdPath<-"~/Global-to-local-GLOBIOM"
# setwd(wdPath)

### SET DATAPATH
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM\\Data\\Raw\\MWI\\Household_survey\\2010\\IHS3"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### CROPS
# -------------------------------------
# Output MWI 2010_11 (wave 1)
# two seasons rainy and dry
# crops and permanent crops
# seed = seed planted in the rainy season for crop (factor)
# -------------------------------------

# load crop codes
permanent_crop_list <- read_csv(file.path(dataPath, "Conversion/MWI_permanent_crop_list_2010.csv"))

# crop output
pcrops2010 <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_P.dta")) %>%
  select(case_id, ea_id, plotnum=ag_p0b, crop_code=ag_p0d,
         area_qty=ag_p02a, area_unit=ag_p02b, 
         crop_qty_harv=ag_p09a, crop_qty_unit=ag_p09b)

# change one_crop to crop stand
output2010$crop_stand <- ifelse(output2010$crop_stand %in% 1, 1,
                                ifelse(output2010$crop_stand %in% 2, 0, NA))
output2010$crop_share <- as_factor(output2010$crop_share)
output2010$harv_start <- as_factor(output2010$harv_start)
output2010$harv_end <- as_factor(output2010$harv_end)
output2010$crop_code <- as.integer(output2010$crop_code)
output2010$unit <- as.integer(output2010$unit)
output2010$condition <- as.integer(output2010$condition)
output2010$crop_code <- as.integer(output2010$crop_code)

# Link cropcodes
output2010 <- left_join(output2010, crop_list)
