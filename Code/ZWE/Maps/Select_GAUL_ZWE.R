#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to select GAUL maps per year and country
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
p_load("WDI", "countrycode", "plotKML")


### SET WORKING DIRECTORY
wdPath <- "~/Global-to-local-GLOBIOM"
setwd(wdPath)

### SET DATAPATH
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM"


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### SOURCE

### LOAD GAUL
# GAUL adm1
ogrListLayers(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_1\\g2015_2000_1.shp"))
GAUL_adm1_2000 <- readOGR(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_1\\g2015_2000_1.shp"), layer = "g2015_2000_1")

# GAUL adm2
ogrListLayers(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_2\\g2015_2000_2.shp"))
GAUL_adm2_2000 <- readOGR(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_2\\g2015_2000_2.shp"), layer = "g2015_2000_2")


### SELECT TARGET COUNTRIES
#http://www.fao.org/countryprofiles/iso3list/en/
GAUL_adm2_2000_df <- GAUL_adm2_2000@data

# Gaul adm1
GADM_ZWE_adm1 <- GAUL_adm1_2000[GAUL_adm1_2000$ADM0_NAME == "Zimbabwe",]
plot(GADM_ZWE_adm1)
saveRDS(GADM_ZWE_adm1, file.path(dataPath, "Data\\ZWE\\Processed\\Maps\\GAUL_ZWE_adm1_2000.rds"))

# Gaul adm2
GADM_ZWE_adm2 <- GAUL_adm2_2000[GAUL_adm2_2000$ADM0_NAME == "Zimbabwe",]
plot(GADM_ZWE_adm2)
saveRDS(GADM_ZWE_adm2, file.path(dataPath, "Data\\ZWE\\Processed\\Maps\\GAUL_ZWE_adm2_2000.rds"))


