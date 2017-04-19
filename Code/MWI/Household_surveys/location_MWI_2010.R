#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Location of MWI households
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

### SET DATAPATH
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM"

### CREATE LOCATION DF
# Load region and district information
location <- read_dta(file.path(dataPath, "Data\\MWI\\Raw\\Household_surveys\\2010\\IHS3\\Household/HH_MOD_A_FILT.dta")) %>%
  transmute(case_id, ea_id, region=NA, district = as.character(as_factor(hh_a01)), district_code = hh_a01, rural = as_factor(reside)) %>%
  mutate(rural) 

# there are three regions in Malawi:
# 2: Central, 1: Northern and 3: Southern. The first 
# number of the district code tels us which
# is which

location <- location %>%
  mutate(region = ifelse(district_code < 200, "NORTH",
                    ifelse(district_code >=200 & district_code < 300, "CENTRAL",
                       "SOUTH"))) %>%
  dplyr::select(-district_code)

# Load geo-location
geo <- read_dta(file.path(dataPath, "Data\\MWI\\Raw\\Household_surveys\\2010\\IHS3\\HouseholdGeovariables_DTA/HouseholdGeovariables.dta")) %>%
  rename(lon = lon_modified, lat = lat_modified)


# Rename districts so they match up with GADM map. 
# Allocate the four urban regions (cities) to the regions they are located in. Mzuzu City is located in the Mzimba District
location <- location %>%
  mutate(district = dplyr::recode(district, "Blantyre City" = "Blantyre",
                                     "Lilongwe City" = "Lilongwe",
                                     "Mzuzu City" = "Mzimba",
                                     "Zomba City" = "Zomba",
                                     "Nkhatabay" = "Nkhata Bay",
                                     "Nkhota kota" = "Nkhotakota",
                                     "Blanytyre" = "Blantyre"))

# Join location and geo 
location <- left_join(location, geo)
