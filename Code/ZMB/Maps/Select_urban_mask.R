#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Code to select urban mask
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "sf")
# Additional packages
p_load("WDI", "countrycode", "R.utils", "plotKML", "ggthemes")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)

### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### LOAD GAUL MAPS
adm2 <- readRDS(file.path(dataPath, "Data/ZMB/Processed/Maps/GAUL_ZMB_adm2_2000.rds"))
plot(adm2)


### URBAN MASK
# Load global mask
urban_mask_raw <- readOGR(file.path(dataPath, "Data/Global/GRUMPv1/global_urban_extent_polygons_v1.01.shp"))

# Select country information
urban_mask <- urban_mask_raw[urban_mask_raw$ISO3 == iso3c_sel,]                 

# City information
data(world.cities)
cities <- world.cities %>%
  mutate(iso3c = countrycode(country.etc, "country.name", "iso3c")) %>%
  filter(iso3c == iso3c_sel)

# Plot
ggplot() + 
  geom_polygon(data = adm2, aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
  geom_polygon(data = urban_mask, aes(x = long, y = lat, group = group), fill = "red") +
  geom_point(data = cities, aes(x = long, y = lat), colour = "green") 

# Save data
urban_maskPath <- file.path(dataPath, paste0("Data\\", iso3c_sel, "\\Processed\\Maps\\urban_mask"))
dir.create(urban_maskPath)

saveRDS(urban_mask, file.path(dataPath, "Data/ZMB/Processed/Maps/urban_mask/urban_mask_ZMB.rds"))
