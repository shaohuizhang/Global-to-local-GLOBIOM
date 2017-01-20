#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to process land cover map and aggregate to SIMU level
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
p_load("WDI", "countrycode")


### SET WORKING DIRECTORY
wdPath<-"~/Global-to-local-GLOBIOM"
setwd(wdPath)

### SET DATAPATH
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM\\Data"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### LOAD SIMU MAP
SIMU_LU <- read_csv(file.path(dataPath, "GLOBIOM/simu_lu/SimUIDLUID.csv"))
SIMU_5min <- raster(file.path(dataPath, "GLOBIOM/simu_raster_5min/rasti_simu_gr.tif"))

### LOAD GADM MAPS
# Load previously saved map (Download_GADM.r)
country_map_raw <- readRDS("Data/GADM_2.8_MWI_adm1.rds")
spplot(country_map_raw, "OBJECTID")

### LOAD LAND COVER MAP AND PROCESS
# Load map
#FAO_lum <- readOGR(file.path(dataPath, "Raw\\MWI\\Land_use_maps\\FAO_Land_Cover_Data\\DATA\\NATIONAL_LC\\malawi_lc.shp"))
land_cover_map_raw <- raster(file.path(dataPath, "Raw\\MWI\\Land_use_maps\\RCMRD\\Final Corrected Land Cover\\Final Corrected Land Cover\\Scheme 1\\malawi 2000 classification scheme1.img"))
land_cover_map_raw

# Reveal the land cover classes if the are stored as factors
levels(land_cover_map_raw)

### REPROJECT SIMU AND COUNTRY MAP
#' It is essential to use the same projection for all maps. If there is a difference we reproject the SIMU and country_map
#' Another route would be to reproject the country land cover map to the global SIMU projection. 
#' This is however not recomendable as land cover maps tend to have a very high resolution. Reprojection will therefore costs 
#' a lot of time and potentially introduces distortions. 
#' It is important to set the method to "ngb" as the map presents categorical values, otherwise SIMU IDs will be somehow averaged.
#' It is important to save files to file using filename = or remove temporary files otherwise the HD might be full quickly!

# Compare CRS of SIMU and target country land cover map
crs(land_cover_map_raw)
crs(country_map_raw)
crs(SIMU_5min)

standard_crs <- crs(SIMU_5min)

# Reproject country land use cover map to standard CRS
land_cover_map <- projectRaster(land_cover_map_raw , crs = standard_crs, method = "ngb", filename = "Data/land_cover_map.grd", overwrite = TRUE)
levelplot(land_cover_map)

SIMU2country <- projectRaster(SIMU2country , crs = country_crs, method = "ngb", filename = "Data/SIMU2country.grd", overwrite = TRUE)

# Create mask for each land cover class and calculate value


mask1 <- setValues(raster(land_cover_map), NA)
## Assign 1 to formask to all cells corresponding to the forest class
mask1 <- setValues(raster(land_cover_map), NA)

# Create SIMU stack
base
nclasses <- 1
## Assign a 1 to rNA wherever an NA is enountered in covs
for(i in 1:nclasses){
  classlayer <- setValues(raster(land_cover_map), NA)
  classlayer[land_cover_map==i] <- i
  base <- addLayer(base, classlayer)
}


plot(formask, col="dark green", legend = FALSE)
proj4string(country_map)
CRS()







# Crop SIMU map to country map
SIMU2country <- crop(SIMU_5min, country_map_raw, updateNA=TRUE)
SIMU2country

# Reproject SIMU to CRS of target country
SIMU2country <- projectRaster(SIMU2country , crs = country_crs, method = "ngb", filename = "Data/SIMU2country.grd", overwrite = TRUE)

# Reproject country map
country_map <- spTransform(country_map_raw, crs(country_crs))
country_map

# Plot land cover map with country map overlay
levelplot(SIMU2country) + layer(sp.polygons(country_map, col='black'))

projInfo(type = "proj")

### 

levels(land_cover_map_raw)
area_brick <- area(land_cover_brick, na.rm = TRUE)
x <- land_cover_brick[[2]]
x <- area(land_cover_map_raw)

check <- area(land_cover_brick, na.rm = TRUE)


### AGGREGATE LAND COVER MAP TO SIMU
# 1. 


# 2. 


# 3. 

# 4. 

# 5. 


# 6. 



land_cover_map <- projectRaster(land_cover_map_raw, crs = standard_crs, method = "ngb", filename = "Data/MWI_land_cover_map.grd")
land_cover_map
showTmpFiles()
?removeTmpFiles(h=24)
land_cover_brick <- layerize(land_cover_map_raw, filename = "Data/MWI_land_cover_brick.grd", overwrite = T)


##############
JICA_lum <- readOGR(file.path(dataPath, "Raw\\MWI\\Land_use_maps\\JICA Malawi\\LU_data_2000\\LU00-MW-00000.shp"))
spplot(JICA_lum)
