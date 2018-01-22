#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to grid IWMI 2000 land cover data to 30 arc-sec and calculate shares per land cover class
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
p_load("countrycode", "plotKML")

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


### SET COUNTRY
source("Code/ZMB/Set_country.R")

### DATA
# Adm
adm1 <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/gaul/GAUL_", iso3c_sel, "_adm1_2000.rds")))

# Adm_map
adm1_map <- read_excel(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Mappings/Mappings_",iso3c_sel, ".xlsx")), sheet = paste0(iso3c_sel, "2adm")) %>%
  filter(year == 2000) %>%
  dplyr::select(adm1, adm1_GAUL) %>%
  na.omit %>%
  unique()

# Land cover
lc_raw <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Raw/Spatial_data/Irrigation/IWMI_2010", iso3c_sel, ".tif")))

# Load land cover classes
lc_class <- read_csv(file.path(dataPath, "Data/Global/ESA/ESACCI-LC-Legend.csv")) %>%
   dplyr::select(lc_code, lc)

# Grid
grid_30sec <- raster(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Maps/grid/grid_30sec_r_", iso3c_sel, ".tif")))
names(grid_30sec) <- "gridID"


### RESAMPLE 30 SEC GRID TO LC RESOLUTION
# Specify input and output files
lc_raw_file <- file.path(dataPath, paste0("Data/", iso3c_sel, "/Raw/Spatial_data/Land_cover/ESA/ESA_", iso3c_sel, "_raw_2000.tif"))
grid_lc_res_file <- file.path(dataPath, paste0("Data\\", iso3c_sel, "/Processed/Maps/grid/grid_ESA_", iso3c_sel, "_raw_2000.tif"))
grid_30sec_file <- file.path(dataPath, paste0("Data\\", iso3c_sel, "/Processed/Maps/grid/grid_30sec_r_", iso3c_sel, ".tif"))

# Resample
# No need to mask grid (slow) as the grid_30sec is already masked => use align_raster2_f
grid_lc_res <- align_raster2_f(grid_30sec_file, lc_raw_file, grid_lc_res_file, nThreads = "ALL_CPUS", verbose = T, 
                         output_Raster = T, overwrite = TRUE, r = "near", border = adm1)
names(grid_lc_res) <- "gridID"
grid_lc_res
levelplot(grid_lc_res)

### COMBINE LC AND GRID AND CALCULATE SHARES
# Combine and calculate shares
lc_stack <- stack(grid_lc_res, lc_raw)
lc <- as.data.frame(rasterToPoints(lc_stack)) %>%
  set_names(c("x", "y", "gridID", "lc_code")) %>%
  na.omit() %>%
  unique() %>%
  group_by(gridID, lc_code) %>%
  summarize(n = n())  %>%
  mutate(share = n / sum(n, na.rm = T)) %>%
  dplyr::select(-n) 
summary(lc)

# Calculate area
grid_size <- area(grid_30sec)
names(grid_size) <- "grid_size"
grid_size <- stack(grid_30sec, grid_size)
grid_size <- as.data.frame(rasterToPoints(grid_size)) %>%
  na.omit

lc <- left_join(lc, grid_size) %>%
  mutate(value = grid_size*share*100) # in ha

# Filter out crop grid cells
crop_class <- lc_class %>%
  filter(lc == "crops")

lc <- lc %>%
  filter(lc_code %in% crop_class$lc_code)

table(lc$lc_code)

# Save
saveRDS(lc, file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/Agricultural_statistics/lc_ESA_sh_2000_", iso3c_sel, ".rds")))



adm_sel <- adm1[adm1$ADM1_NAME== "Central",]
plot(adm_sel)
adm_sel_p <- crop(grid_5min_r, adm_sel)
adm_sel_p <- mask(adm_sel_p, adm_sel)
adm_sel_p <- rasterToPolygons(adm_sel_p)
plot(adm_sel_p[c(1:500),], col = "pink", add = T)
plot(adm_sel_p[c(500:1142),], col = "green", add = T)
plot(adm_sel_p, add = T)
plot(adm_sel_p)
polist <- list(adm_sel_p[c(1:500),], adm_sel_p[c(501:1142),])
polist <- list(adm_sel_p[c(1:10),], adm_sel_p[c(11:20),])
