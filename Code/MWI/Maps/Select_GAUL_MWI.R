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
GAUL_adm1_2000 <- readOGR(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_1\\g2015_2000_1.shp"), layer = "g2015_2000_1")

# GAUL adm2
ogrListLayers(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_2\\g2015_2000_2.shp"))
GAUL_adm2_2000 <- readOGR(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_2\\g2015_2000_2.shp"), layer = "g2015_2000_2")


### SELECT TARGET COUNTRIES
#http://www.fao.org/countryprofiles/iso3list/en/
GAUL_adm2_2000_df <- GAUL_adm2_2000@data

# Gaul adm1
GAUL_MWI_adm1_2000 <- GAUL_adm1_2000[GAUL_adm1_2000$ADM0_NAME == "Malawi",]
plot(GAUL_MWI_adm1_2000)

# Gaul adm2
GAUL_MWI_adm2_2000 <- GAUL_adm2_2000[GAUL_adm2_2000$ADM0_NAME == "Malawi",]
plot(GAUL_MWI_adm2_2000)


### ANALYSE MAPS, COMPARE WITH SECONDARY ADM INFORMATION AND CORRECT WHERE NECESSARY 
# In some cases the map contains areas that are not relevant and may results in distortions when they are combined with simu maps
# For example in case of Malawi, simus are located in so-called 'Area under National Administration', which is Lake Malawi. These need to be removed.

# Analyse areas that potentially need to be removed
GAUL_MWI_adm1_2000_df <- GAUL_MWI_adm1_2000@data
GAUL_MWI_adm2_2000_df <- GAUL_MWI_adm2_2000@data
area_remove <- c("Area under National Administration")

# Gaul adm1
plot(GAUL_MWI_adm1_2000)
plot(GAUL_MWI_adm1_2000[GAUL_MWI_adm1_2000$ADM1_NAME == area_remove,], add = T, border = "red")
GAUL_MWI_adm1_2000_adj <- GAUL_MWI_adm1_2000[GAUL_MWI_adm1_2000$ADM1_NAME != area_remove,]
plot(GAUL_MWI_adm1_2000_adj)

# Gaul adm2
plot(GAUL_MWI_adm2_2000)
plot(GAUL_MWI_adm2_2000[GAUL_MWI_adm2_2000$ADM1_NAME == area_remove,], add = T, border = "red")
GAUL_MWI_adm2_2000_adj <- GAUL_MWI_adm2_2000[GAUL_MWI_adm2_2000$ADM2_NAME != area_remove,]
plot(GAUL_MWI_adm2_2000_adj)

### SAVE MAPS
saveRDS(GAUL_MWI_adm1_2000_adj, file.path(dataPath, "Data\\MWI\\Processed\\Maps\\GAUL_MWI_adm1_2000_adj.rds"))
saveRDS(GAUL_MWI_adm2_2000_adj, file.path(dataPath, "Data\\MWI\\Processed\\Maps\\GAUL_MWI_adm2_2000_adj.rds"))
saveRDS(GAUL_MWI_adm1_2000, file.path(dataPath, "Data\\MWI\\Processed\\Maps\\GAUL_MWI_adm1_2000.rds"))
saveRDS(GAUL_MWI_adm2_2000, file.path(dataPath, "Data\\MWI\\Processed\\Maps\\GAUL_MWI_adm2_2000.rds"))


