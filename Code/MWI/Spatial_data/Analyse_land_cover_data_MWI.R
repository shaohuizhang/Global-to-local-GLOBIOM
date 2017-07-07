#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to analyse FAO land cover data
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


### CHECK IF THEIR ARE TEMPORARY FILES (CREATED BY RASTER PACKAGE) AND REMOVE
showTmpFiles()
removeTmpFiles()


### LOAD DATA
# adm
adm1 <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GAUL_MWI_adm1_2000_adj.rds"))
adm2 <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GAUL_MWI_adm2_2000_adj.rds"))

# Grid
grid <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/grid_MWI.rds"))

# Land cover
land_cover <- readRDS(file.path(dataPath, "Data/MWI/Processed/Spatial_data/land_cover_FAO_2000_MWI.rds"))

# Agricultural statistics
ag_stat <- read_csv(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/ag_stat_2000_MWI.csv"))

# Irrigation
GMIA <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GMIA_MWI.rds"))
irrigation <- read_csv(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/Irrigation_MWI.csv"))


### CREATE MAP FORMATS
# land cover
# Transform to sf for easy plotting with ggplot
grid_sf <- st_as_sf(grid)

check1 <- land_cover %>%
  group_by(gridID, agg2) %>%
  summarize(area = sum(area, na.rm = T)) %>%
  left_join(grid_sf,.) 
  

ggplot(grid_sf) + 
  geom_sf() + 
  geom_sf(data = filter(grid_sf, gridID %in% grid_check$gridID), fill = "red") +
  geom_sf(data = urban_mask_sf, fill = "blue", alpha = 0.5) +
  geom_point(data = cities, aes(x = long, y = lat), col = "green") +
  geom_text(data = cities, aes(x = long, y = lat, label = name))

ggplot(check1) + geom_sf(aes(fill = area), colour = NA) + 
  facet_wrap(~ agg2) +
  scale_fill_gradient(low="grey",high="red", na.value = "blue")

# http://pierreroudier.github.io/teaching/20170626-Pedometrics/20170626-web-vis.html

mapview(GMIA, na.color = "transparent", legend = TRUE) + mapview(adm1)
viewExtent(grid)
slideView(adm1, GMIA)
plainView(GMIA)
mapshot(m, file = "test.png")
