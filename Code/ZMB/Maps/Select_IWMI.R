#'========================================================================================================================================
#' Project:  Global-to-local-GLOBIOM
#' Subject:  Code to select IMWI irrigation maps
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
#p_load("")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### SOURCE FUNCTIONS
source(file.path(root, "Code/Support/functions.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### SET COUNTRY
source("Code/ZMB/Set_country.R")


### LOAD IWMI MAP
IWMI_r_raw <- raster(file.path(dataPath, "Data/Global/IWMI/IRRA_Africa_2010_v1_1c/irra_africa_2010_v1_1.img")) 
irra_africa_class <- levels(IWMI_r_raw)[[1]] %>%
  filter(COUNT != 0, ID != 0)

crs(IWMI_r_raw) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

### LLOAD ADM
adm0 <- readRDS(file.path(dataPath, paste0("Data\\", iso3c_sel, "\\Processed\\Maps/GAUL_", iso3c_sel, "_adm0_2000.rds")))

### SELECT COUNTRY GMIA RASTER
IWWI <- crop(IWMI_r_raw, adm0)
IWMI <- mask(IWMI, adm0)
plot(IWMI)
freq(IWMI)

# Save map
writeRaster(IWMI, file.path(dataPath, paste0("Data/", iso3c_sel, "/Raw/Spatial_data/Irrigation/IWMI_2010", iso3c_sel, ".tif")), overwrite = T)
rm(IWMI)

### RESAMPLE MAP TO 30 ARC-SEC GRID
# Specify input and output files
ir_raw_file <- file.path(dataPath, paste0("Data/", iso3c_sel, "/Raw/Spatial_data/Irrigation/IWMI_2010", iso3c_sel, ".tif"))
ir_file <- file.path(dataPath, paste0("Data\\", iso3c_sel, "/Processed/Maps/ir_IWMI_30sec_2010_", iso3c_sel, ".tif"))
grid_file <- file.path(dataPath, paste0("Data\\", iso3c_sel, "/Processed/Maps/30sec_grid_r_", iso3c_sel, ".tif"))

# Resample
ir <- align_raster_f(ir_raw_file, grid_file, ir_file, nThreads = "ALL_CPUS", verbose = T, 
                     output_Raster = T, overwrite = TRUE, r = "near", border = adm0)
names(ir) <- "ir"
levelplot(ir)
plot(adm0, add = T)
freq(ir)

