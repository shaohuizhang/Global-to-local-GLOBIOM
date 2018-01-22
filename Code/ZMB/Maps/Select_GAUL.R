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



### SET COUNTRY
source("Code/ZMB/Set_country.R")

### LOAD GAUL
# GAUL adm0
ogrListLayers(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_0\\g2015_2000_0.shp"))
gaul_adm0_2000 <- readOGR(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_0\\g2015_2000_0.shp"), layer = "g2015_2000_0")

# GAUL adm1
ogrListLayers(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_1\\g2015_2000_1.shp"))
gaul_adm1_2000 <- readOGR(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_1\\g2015_2000_1.shp"), layer = "g2015_2000_1")

# GAUL adm2
ogrListLayers(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_2\\g2015_2000_2.shp"))
gaul_adm2_2000 <- readOGR(file.path(dataPath, "Data\\Global\\GAUL\\g2015_2000_2\\g2015_2000_2.shp"), layer = "g2015_2000_2")


### SELECT TARGET COUNTRIES
#http://www.fao.org/countryprofiles/iso3list/en/
gaul_adm2_2000_df <- gaul_adm2_2000@data

# Gaul adm0
gaul_adm0_2000 <- gaul_adm0_2000[gaul_adm0_2000$ADM0_NAME == country_sel,]
plot(gaul_adm0_2000)

# Gaul adm1
gaul_adm1_2000 <- gaul_adm1_2000[gaul_adm1_2000$ADM0_NAME == country_sel,]
plot(gaul_adm1_2000)

# Gaul adm2
gaul_adm2_2000 <- gaul_adm2_2000[gaul_adm2_2000$ADM0_NAME == country_sel,]
plot(gaul_adm2_2000)


### ANALYSE MAPS, SAVE ADM INFO, COMPARE WITH SECONDARY ADM INFORMATION AND CORRECT WHERE NECESSARY 
# In some cases the map contains areas that are not relevant and may results in distortions when they are combined with simu maps
# For example in case of Malawi, simus are located in so-called 'Area under National Administration', which is Lake Malawi. These need to be removed.

# Analyse areas that potentially need to be removed
gaul_adm0_2000_df <- gaul_adm0_2000@data
gaul_adm1_2000_df <- gaul_adm1_2000@data
gaul_adm2_2000_df <- gaul_adm2_2000@data

# Save adm info
gaul_adm_2000_list <- gaul_adm2_2000_df %>%
  transmute(adm2_gaul = toupper(ADM2_NAME), adm1_gaul = toupper(ADM1_NAME)) %>%
  arrange(adm2_gaul)
write_csv(gaul_adm_2000_list, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Mappings/gaul_adm_2000_list_", iso3c_sel, ".csv")))

### SAVE FINAL MAPS
# Select final map
adm <- gaul_adm1_2000

# Read names
adm_map <- read_excel(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Mappings/Mappings_",iso3c_sel, ".xlsx")), sheet = paste0(iso3c_sel, "2adm")) %>%
  filter(year == 2000) %>%
  dplyr::select(adm, adm_gaul) %>%
  na.omit %>%
  unique()

# Combine with gaul and replace
adm_df <- adm@data %>%
  mutate(adm_gaul = toupper(ADM1_NAME)) %>%
  left_join(adm_map)

adm@data <- adm_df

# save
gaulPath <- file.path(dataPath, paste0("Data\\", iso3c_sel, "\\Processed\\Maps\\gaul"))
dir.create(gaulPath)

saveRDS(adm, file.path(dataPath, paste0("Data\\", iso3c_sel, "\\Processed\\Maps\\gaul\\adm_2000_", iso3c_sel, ".rds")))
writeOGR(adm, dsn = file.path(dataPath, paste0("Data\\", iso3c_sel, "\\Processed\\Maps\\gaul")), paste0("adm_2000_", iso3c_sel), driver="ESRI Shapefile")

