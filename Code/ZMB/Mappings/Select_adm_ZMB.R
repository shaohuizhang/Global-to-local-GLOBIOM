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
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM\\Data"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### ZMB
ZMB_adm2 <- readRDS(file.path(dataPath, "Processed\\ZMB\\GADM_maps\\GADM_2.8_ZMB_adm2.rds"))

# Save list of adm level 1 districts that will be used as regional identifiers
adm_list_ZMB <- ZMB_adm2@data %>%
  transmute(iso3c = ISO, adm1 = toupper(NAME_1), adm2 = toupper(NAME_2))
write_csv(adm_list_ZMB, file.path(dataPath, "Processed/ZMB/Mappings/adm_list_ZMB.csv"))

# Load adm regions as in various national statistics databases
adm_nat_stat1_raw <- read_excel(file.path(dataPath, "Raw\\ZMB\\National_statistics/ag_statistics_1987_2014.xlsx"), sheet = "processed")
adm_nat_stat1 <- adm_nat_stat1_raw %>%
  dplyr::select(REGNAME = `Regions Name`, REGCODE = `Regions TERRID`) %>%
  unique

adm_nat_stat2_raw <- read_excel(file.path(dataPath, "Raw\\ZMB\\National_statistics/crop_forecast_2009_2015.xlsx"), sheet = "processed")
adm_nat_stat2 <- adm_nat_stat2_raw %>%
  dplyr::select(REGNAME = `Location Name`, REGCODE = `Location RegionId`) %>%
  unique
