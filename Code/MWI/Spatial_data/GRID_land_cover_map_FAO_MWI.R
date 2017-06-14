#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to grid FAO land cover data
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
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


### LOAD FAO MAP
ogrListLayers(file.path(dataPath, "Data\\MWI\\Raw\\Spatial_data\\FAO_Land_Cover_Data\\DATA\\NATIONAL_LC/Malawi_lc.shp"))
land_cover_map_FAO_raw <- readOGR(file.path(dataPath, "Data\\MWI\\Raw\\Spatial_data\\FAO_Land_Cover_Data\\DATA\\NATIONAL_LC/Malawi_lc.shp"))

land_cover_map_FAO <- land_cover_map_FAO_raw
land_cover_map_df <- land_cover_map_FAO@data %>% 
  mutate(LCCSUSLB = factor(LCCSUSLB),
         E2000USLB = factor(E2000USLB),
         E1990USLB = factor(E1990USLB))
land_cover_map_FAO@data <- land_cover_map_df 


### RASTERIZE FAO MAP
# Create raster frame
r_ext <- raster(extent(land_cover_map_FAO), resolution = 30)
crs_fao <- crs(land_cover_map_FAO)
projection(r_ext) <- crs_fao

# Rasterize
land_cover_map <- rasterize(land_cover_map_FAO, r_ext, field = land_cover_map_FAO@data[,"LCCSUSLB"])
writeRaster(land_cover_map, file.path(dataPath, "Data/MWI/Raw/Spatial_data/FAO_Land_Cover_Data/DATA/NATIONAL_LC/land_cover_map_FAO.grd"))
land_cover_map <- raster(file.path(dataPath, "Data/MWI/Raw/Spatial_data/FAO_Land_Cover_Data/DATA/NATIONAL_LC/land_cover_map_FAO.grd"))


### ADD ATTRIBUTES
# prepare map
# http://stackoverflow.com/questions/19586945/how-to-legend-a-raster-using-directly-the-raster-attribute-table-and-displaying
land_cover_map <- ratify(land_cover_map)
rat <- levels(land_cover_map)[[1]] #get the values of the unique cell frot the attribute table

# Create attribute table
# NEED TO RECODE COMBINATIONS OF CLASSES TO USEFULE ONES AND THEN ADD TABLE
rat <- left_join(rat, ESA_legend)

# Create colours for legend and sort in right order
ESA_colour <- rat
ESA_colour <- ESA_colour %>%
  filter(ID %in% seq(0, 220, 10)) %>%
  mutate(colour= rgb(R, G, B, max = 255)) %>%
  unique()
ESA_colour <- ESA_colour[order(ESA_colour$land_cover_short, decreasing = F),]

# Links levels
levels(land_cover_map) <- rat
levels(land_cover_map)
rm(rat)

# Visualise
