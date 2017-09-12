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
# GAUL adm0
ogrListLayers(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_0\\g2015_2000_0.shp"))
GAUL_adm0_2000 <- readOGR(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_0\\g2015_2000_0.shp"), layer = "g2015_2000_0")

# GAUL adm1
ogrListLayers(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_1\\g2015_2000_1.shp"))
GAUL_adm1_2000 <- readOGR(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_1\\g2015_2000_1.shp"), layer = "g2015_2000_1")

# GAUL adm2
ogrListLayers(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_2\\g2015_2000_2.shp"))
GAUL_adm2_2000 <- readOGR(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_2\\g2015_2000_2.shp"), layer = "g2015_2000_2")


### SELECT TARGET COUNTRIES
#http://www.fao.org/countryprofiles/iso3list/en/
GAUL_adm2_2000_df <- GAUL_adm2_2000@data
country <- "Zambia"

# Gaul adm0
GAUL_ZMB_adm0_2000 <- GAUL_adm0_2000[GAUL_adm0_2000$ADM0_NAME == country,]
plot(GAUL_ZMB_adm0_2000)

# Gaul adm1
GAUL_ZMB_adm1_2000 <- GAUL_adm1_2000[GAUL_adm1_2000$ADM0_NAME == country,]
plot(GAUL_ZMB_adm1_2000)

# Gaul adm2
GAUL_ZMB_adm2_2000 <- GAUL_adm2_2000[GAUL_adm2_2000$ADM0_NAME == country,]
plot(GAUL_ZMB_adm2_2000)


### ANALYSE MAPS, SAVE ADM INFO, COMPARE WITH SECONDARY ADM INFORMATION AND CORRECT WHERE NECESSARY 
# In some cases the map contains areas that are not relevant and may results in distortions when they are combined with simu maps
# For example in case of Malawi, simus are located in so-called 'Area under National Administration', which is Lake Malawi. These need to be removed.

# Analyse areas that potentially need to be removed
# Not needed for Zambia
GAUL_ZMB_adm0_2000_df <- GAUL_ZMB_adm0_2000@data
GAUL_ZMB_adm1_2000_df <- GAUL_ZMB_adm1_2000@data
GAUL_ZMB_adm2_2000_df <- GAUL_ZMB_adm2_2000@data

# Save adm info: only adm1 is relevant
GAUL_ZMB_adm_2000_list <- GAUL_ZMB_adm2_2000_df %>%
  transmute(adm1_GAUL = toupper(ADM1_NAME)) %>%
  arrange(adm1_GAUL) %>%
  unique()
write_csv(GAUL_ZMB_adm_2000_list, file.path(dataPath, "Data/ZMB/Processed/Mappings/GAUL_ZMB_adm_2000_list.csv"))

# Gaul adm0
plot(GAUL_ZMB_adm0_2000)

# Gaul adm1
plot(GAUL_ZMB_adm1_2000)

# Gaul adm2
plot(GAUL_ZMB_adm2_2000)


### SAVE
# Adm names
adm1_list <- GAUL_ZMB_adm1_2000_df %>%
  transmute(adm1_GAUL = toupper(ADM1_NAME))
write_csv(adm1_list, file.path(dataPath, "Data/ZMB/Processed/Mappings/gaul_ZMB_adm1_list.csv"))
adm2_list <- GAUL_ZMB_adm2_2000_df %>%
  transmute(adm2_GAUL = toupper(ADM2_NAME))
write_csv(adm2_list, file.path(dataPath, "Data/ZMB/Processed/Mappings/gaul_ZMB_adm2_list.csv"))

# Maps
saveRDS(GAUL_ZMB_adm0_2000, file.path(dataPath, "Data\\ZMB\\Processed\\Maps\\GAUL_ZMB_adm0_2000.rds"))
saveRDS(GAUL_ZMB_adm1_2000, file.path(dataPath, "Data\\ZMB\\Processed\\Maps\\GAUL_ZMB_adm1_2000.rds"))
saveRDS(GAUL_ZMB_adm2_2000, file.path(dataPath, "Data\\ZMB\\Processed\\Maps\\GAUL_ZMB_adm2_2000.rds"))


