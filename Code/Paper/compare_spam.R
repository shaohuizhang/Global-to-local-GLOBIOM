#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to compare lu map with spam
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "leaflet", "mapview")
# Additional packages
p_load("countrycode", "plotKML", "gdxrrw")

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
options(max.print=1000000) # more is printed on screen


### CHECK IF THEIR ARE TEMPORARY FILES (CREATED BY RASTER PACKAGE) AND REMOVE
showTmpFiles()
removeTmpFiles()

### LINK GAMS LIBRARIES
GAMSPath <- "C:\\GAMS\\win64\\24.4"
igdx(GAMSPath)


### SET COUNTRY
source("Code/MWI/Set_country.R")

### DATA
# Adm
adm0 <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaul/GAUL_", iso3c_sel, "_adm0_2000_adj.rds")))

# Load results from land use allocation model
file <- file.path(dataPath, "Model/Results/entropy.gdx")
land_use_raw <- rgdx.param(file, "Palloc", names = c("gridID", "system", "value"),  compress = T) %>%
  mutate(system = as.character(system),
         gridID = as.numeric(as.character(gridID)))

# SPAM
maize_sp_raw <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/maps/spam/spam_maize_", iso3c_sel, "_2005.tif")))

# Load country grid
grid_r <- readRDS(file.path(dataPath, "Data/MWI  - old/Processed/Maps/grid_r_MWI.rds"))


### GRID LU DATA
maize_sp <- stack(grid_r, maize_sp_raw)
maize_sp <- as.data.frame(rasterToPoints(maize_sp))

### PREPARE LU DATA
land_use <- land_use_raw %>%
  spread(system, value, fill = NA) 

# Add grid cell coordinates
grid_df <- as.data.frame(rasterToPoints(grid_r))

land_use <- land_use %>%
  left_join(grid_df,.) %>%
  gather(system, value, -gridID, -x, -y)

# Add short_name
land_use <- land_use %>%
  mutate(short_name = substr(system,0,4),
         short_name = ifelse(system %in% c("teas_coff_H", "teas_coff_I"), "teas_coff", short_name))

maize <- filter(land_use, short_name == "maiz", system == "maiz_S")

### COMPARE WITH LU MODEL
maize_comp <- left_join(maize, maize_sp) %>%
  na.omit

plot(maize_comp$value, maize_comp$spam_maize_MWI_2005)

cor(maize_comp$value, maize_comp$spam_maize_MWI_2005)
