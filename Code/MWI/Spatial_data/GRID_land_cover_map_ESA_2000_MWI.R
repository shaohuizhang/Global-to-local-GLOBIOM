#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to grid ESA land cover map
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
p_load("WDI", "countrycode", "plotKML", "sf")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### SOURCE
source("Code/process_large_raster_f.R")


### CHECK IF THEIR ARE TEMPORARY FILES (CREATED BY RASTER PACKAGE) AND REMOVE
showTmpFiles()
removeTmpFiles()


### LOAD GAUL MAPS
adm1 <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GAUL_MWI_adm1_2000_adj.rds"))
adm2 <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/GAUL_MWI_adm2_2000_adj.rds"))
plot(adm1)
plot(adm2)

### LOAD COUNTRY GRID
grid <- readRDS(file.path(dataPath, "Data/MWI/Processed/Maps/grid_MWI.rds"))
plot(grid)

### LOAD ADM GRID INFO
adm_grid <- read_csv(file.path(dataPath, "Data/MWI/Processed/Spatial_data/adm_grid_2000_MWI.csv"))


### LOAD LAND COVER MAP 
# Load global ESA map
land_cover_map_ESA <- raster(file.path(dataPath, "Data\\Global\\ESA\\Annual_maps\\ESACCI-LC-L4-LCCS-Map-300m-P1Y-2000-v2.0.7.tif"))
land_cover_map_ESA

# check projection, should be same as r
# REPROJECT, IF NEEDED
crs(grid)
crs(land_cover_map_ESA)


### PREPARE COUNTRY LAND COVER MAP
# Mask takes a long time and is not needed
land_cover_map <- crop(land_cover_map_ESA, grid)

# Add attributes
# Load ESA legend
ESA_legend <- read_csv(file.path(dataPath, "Data\\Global\\ESA\\ESACCI-LC-Legend.csv")) %>%
  mutate(ID = land_cover_code)


# Add attributes
# http://stackoverflow.com/questions/19586945/how-to-legend-a-raster-using-directly-the-raster-attribute-table-and-displaying
land_cover_map <- ratify(land_cover_map)
rat <- levels(land_cover_map)[[1]] #get the values of the unique cell frot the attribute table
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
levelplot(land_cover_map, att='land_cover_short', col.regions = ESA_colour$colour, margin = F) +
  layer(sp.polygons(adm2, col = "black", lwd = 2)) +
  layer(sp.polygons(grid, col = "black", lwd = 2))


### GRID CROP LAND DATA
# Add area information to land cover map
area <- area(land_cover_map)
land_cover_map <- stack(land_cover_map, area)

# Create SimuID list
# Add unique as several SimuS consist of multiple polygons!
grid_list <- unique(grid@data$gridID)

# Overlay land cover map and grid polygon
# function to extract values by gridID
extract_grid_f <- function(polyID, cover){
  print(polyID)
  poly_grid <- grid[grid$gridID==polyID,]
  df_poly <- raster::extract(land_cover_map, poly_grid, df = T) %>%
    setNames(c("ID", "class", "area")) %>%
    na.omit() %>%
    filter(class %in% cover) %>%
    #group_by(class) %>%
    summarize(area = sum(area)) %>%
    mutate(gridID = polyID)
  return(df_poly)
}

# Run function and combine
# Select rainfed and irrigated cropland and aggregate (later we will try to split)
crop_cover <- bind_rows(lapply(grid_list, extract_grid_f, c(10, 20))) 
# saveRDS(crop_cover, file.path(dataPath, "Data/MWI/Processed\\Spatial_data/crop_cover_ESA_2000_MWI.rds"))
crop_cover <- readRDS(file.path(dataPath, "Data/MWI/Processed\\Spatial_data/crop_cover_ESA_2000_MWI.rds"))

### COMBINE WITH ADM DATA
# Load gridded adm data
adm_grid <- read_csv(file.path(dataPath, "Data/MWI/processed/Spatial_data/adm_grid_2000_MWI.csv"))

# Combine data and express area in ha
crop_cover <- crop_cover %>%
  left_join(., adm_grid) %>%
  mutate(area = area * 100,
         grid_size = grid_size * 100)

# save
saveRDS(crop_cover, file.path(dataPath, "Data/MWI/Processed\\Spatial_data/crop_cover_ESA_2000_MWI.rds"))


# VALIDATE RESULTS

# Compare results with land cover map
cropland <- crop_cover$gridID[crop_cover$area > 50]

# Create mask for specific class: 10 annual cropland
# Should also include class = 20.
check_r <- keep_value_r_f(land_cover_map, c(10), filename= "Cache/check.grd")
plot(check_r, col = "black")
plot(adm2, add = T, border = "blue")
plot(grid[grid$gridID %in% cropland,], add = T, border = "red")
rm(check_r)
file.remove("Cache/check.grd")
file.remove("Cache/check.gri")
