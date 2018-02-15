#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to process geo-spatial irrigation data
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "leaflet", "mapview", "sf")
# Additional packages
p_load("XLConnect", "measurements", "nominatim")


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


### LOAD MAPS AND DATA
# adm
# Adm
adm <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaul/adm_2000_", iso3c_sel, ".rds")))

# Grid
grid <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/grid/grid_30sec_r_", iso3c_sel, ".tif")))
names(grid) <- "gridID"

# gia
gia_raw <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/maps/gia/gia_", iso3c_sel, ".tif"))) 
names(gia_raw) <- "value"

# land_cover
lc <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/GAMS/lc_2000_", iso3c_sel, ".rds"))) 
lc <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/maps/lc/lc_syn_30_sec_2000_", iso3c_sel, ".tif")))



### DETECT IRRIGATION SCHEMES
# Find irrigation schemes by plotting gia on Google Earth
# In Google Earth, choose year and draw polygon around area.
# Make sure to select ha and meters for units, make opacity for area 0% 
# Save polygon


# We use the following procedure to add irrigated area
#' 1. Irrigation scheme is identified and polygon is created
#' 2. polygon is transferred to a raster with gridID
#' 3. We assume the actual area irrigated, taken from other sources, is evenly distributed over all grid cells
#' 4. Irrigated area is added to grid cells of gia map



plotKML::plotKML(gia_raw)

mapview(adm, col.regions = NA, alpha.regions = 0, color = "black", lwd = 1) +
  mapview(sugar1, col.regions = NA, alpha.regions = 0, color = "red", lwd = 2) +
  mapview(gia_raw)  

sugc1_raw <- readOGR(file.path(dataPath, paste0("Data/", iso3c_sel, "/Raw/Spatial_data/Irrigation/Zambia_sugar_2000.kmz")))
unzip(file.path(dataPath, paste0("Data/", iso3c_sel, "/Raw/Spatial_data/Irrigation/Zambia_sugc_2000.kmz")), 
      exdir = file.path(dataPath, paste0("Data/", iso3c_sel, "/Raw/Spatial_data/Irrigation/")))



### OVERLAY GIA AND IRRIGATION LOCATION
# Calculate area of gia
grid_area <- area(grid)
names(grid_area) <- "area"
comb <- stack(grid_area, grid)


# Function to prepare input for model
proc_ir_f <- function(scheme, ir_area){
  
  # Load spatial data
  ir_poly <- readOGR(file.path(dataPath, paste0("Data/", iso3c_sel, "/Raw/Spatial_data/Irrigation/", scheme, ".kml")))
  plot(ir_poly)

  # Prepare naming
  ir_data <- unlist(strsplit(scheme, "_"))
  
  scheme_name <- ir_data[1]
  crop <- ir_data[2]
  year <- ir_data[3]
  
  # Extract
  ir <- crop(comb, ir_poly)
  ir <- mask(ir, ir_poly)
  plot(ir)
  
  # Process
  ir_df <- as.data.frame(rasterToPoints(ir)) %>%
    na.omit
  ncells <- NROW(ir_df)
  
  print(paste0("Polygon area of ", crop, " is ", round(sum(ir_df$area*100)), " ha compared to ", ir_area, " ha target area"))
  
  ir_df <- ir_df %>%
    mutate(value = ir_area/ncells,
           scheme = scheme_name,
           year = year,
           short_name = crop,
           system = "I",
           sy = paste0(crop, "_I")) %>% # to ha
    dplyr::select(-x, -y)
  return(ir_df)
}

# Create irrigation df with detailed info
ir_scheme <- bind_rows(
  proc_ir_f("Munkumpu_whea_2000", 2000),
  proc_ir_f("Zambia_sugc_2000", 9000)
)

