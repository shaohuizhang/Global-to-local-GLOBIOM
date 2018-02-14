#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to compare land use and land cover data
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "sf", "leaflet", "mapview")
# Additional packages
p_load("countrycode")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)
options(max.print=1000000) # more is printed on screen


### SET COUNTRY
source("Code/ZMB/Set_country.R")


# LOAD DATA
# Crop cover data
lc_raw <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/lc_ESA_2000_", iso3c_sel, ".rds"))) %>%
  mutate(type = "lc")

# Agricultural statistics
lu_adm_raw <- read_csv(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/ag_stat_2000_", iso3c_sel, ".csv"))) %>%
  mutate(type = "lu")


### COMPARE LC and LU AT ADM2
# Combine
adm0_comp <- bind_rows(
  lu_adm_raw %>%
    filter(adm_level == 0) %>%
    group_by(type) %>%
    summarise(value = sum(area, na.rm = T)),
  lc_raw %>%
    #filter(lc_class == "Cropland") %>%
    filter(lc_class %in% c("Mosaic cropland", "Cropland irrigated / post-flooding", "Cropland, rainfed")) %>%
    group_by(type) %>%
    summarise(value = sum(lc_area, na.rm = T)))

ggplot(data = adm0_comp, aes(x = type, y = value, fill = type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Crop cover and land use comparison",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma, expand = c(0, 0)) +
  theme_bw()


### COMPARE ADM2
# Combine
adm2_comp <- bind_rows(
  lu_adm_raw %>%
    filter(adm_level == 1) %>%
    group_by(type, adm) %>%
    summarise(value = sum(area, na.rm = T)),
  lc_raw %>%
#    filter(lc_class == "Cropland") %>%
  filter(lc_class %in% c("Mosaic cropland", "Cropland irrigated / post-flooding", "Cropland, rainfed")) %>%
    rename(adm = adm1) %>%
    group_by(type, adm) %>%
    summarise(value = sum(lc_area, na.rm = T)))

ggplot(data = adm2_comp, aes(x = adm, y = value, fill = type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Crop cover and land use comparison",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma, expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

# Identify adms with not enough crop cover
check_adm2 <- adm2_comp %>%
  spread(type, value) %>%
  mutate(check_lu = lc - lu) %>%
  filter(check_lu <0)


### CORRECT DISCREPANCY LAND COVER AND LAND USE AND PREPARE GRID FILES
# lc
lc <- lc_raw %>%
  #filter(lc_class == "Cropland")
  filter(lc_class %in% c("Mosaic cropland", "Cropland irrigated / post-flooding", "Cropland, rainfed")) 
  

# lu_adm
lu_adm <- lu_adm_raw %>%
  dplyr::select(adm, adm_level, short_name, value = area)


### SAVE MODEL INPUT DATA
# lc
saveRDS(lc, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/GAMS/lc_2000_", iso3c_sel, ".rds"))) 

# lu_adm
saveRDS(lu_adm, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/GAMS/lu_adm_2000_", iso3c_sel, ".rds"))) 
