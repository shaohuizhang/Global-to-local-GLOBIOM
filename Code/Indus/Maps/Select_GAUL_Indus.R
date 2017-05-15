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


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### LOAD GAUL
# GAUL adm1
ogrListLayers(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_1\\g2015_2000_1.shp"))
GAUL_adm1_2000_raw <- readOGR(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_1\\g2015_2000_1.shp"), layer = "g2015_2000_1")

# GAUL adm2
ogrListLayers(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_2\\g2015_2000_2.shp"))
GAUL_adm2_2000_raw <- readOGR(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_2\\g2015_2000_2.shp"), layer = "g2015_2000_2")


### SELECT TARGET COUNTRIES
#http://www.fao.org/countryprofiles/iso3list/en/
GAUL_adm2_2000_df <- GAUL_adm2_2000_raw@data

# indus countries
# Added more countries because it is nicer for mapping
indus_c <- c("Pakistan", "India", "China", "Afghanistan", "Nepal", "Tajikistan", "Uzbekistan", "Turkmenistan")

# Gaul adm1
GAUL_adm1_2000 <- GAUL_adm1_2000_raw[GAUL_adm1_2000_raw$ADM0_NAME %in% indus_c,]
plot(GAUL_adm1_2000)

# Gaul adm2
GAUL_adm2_2000 <- GAUL_adm2_2000_raw[GAUL_adm2_2000_raw$ADM0_NAME %in% indus_c,]
plot(GAUL_adm2_2000)


### SAVE MAPS
saveRDS(GAUL_adm1_2000, file.path(dataPath, "Data\\indus\\Processed\\Maps\\GAUL_indus_adm1_2000.rds"))
saveRDS(GAUL_adm2_2000, file.path(dataPath, "Data\\indus\\Processed\\Maps\\GAUL_indus_adm2_2000.rds"))


