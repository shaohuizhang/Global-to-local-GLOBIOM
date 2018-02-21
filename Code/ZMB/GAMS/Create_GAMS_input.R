#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to create GAMS data files
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("countrycode", "gdxrrw")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET PATH
source(file.path(root, "Code/get_dataPath.r"))


### SOURCE FUNCTIONS
source(file.path(root, "Code/Support/functions.r"))
source(file.path(root, "Code/Support/R2GDX.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)
options(max.print=1000000) # more is printed on screen


### LINK GAMS LIBRARIES
igdx(GAMSPath)


### SET COUNTRY
source("Code/ZMB/Set_country.R")


### LOAD DATA
# Sy
lu_sy <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/GAMS/lu_sy_2000_", iso3c_sel, ".rds")))

# Lu adm
lu_adm <- readRDS(file.path(paste0(dataPath, "/Data/", iso3c_sel, "/Processed/GAMS/lu_adm_2000_", iso3c_sel, ".rds"))) 

# Lc  
lc <- readRDS(file.path(paste0(dataPath, "/Data/", iso3c_sel, "/Processed/GAMS/lc_2000_", iso3c_sel, ".rds"))) 

# Ir
lc_ir <- readRDS(file.path(paste0(dataPath, "/Data/", iso3c_sel, "/Processed/GAMS/lc_ir_2000_", iso3c_sel, ".rds"))) 

# lc_det
lc_det <- readRDS(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/GAMS/lu_detail_2000_", iso3c_sel, ".rds")))

# Priors
priors <- readRDS(file.path(paste0(dataPath, "/Data/", iso3c_sel, "/Processed/GAMS/priors_2000_", iso3c_sel, ".rds"))) 


### CREATE GAMS PARAMETERS 
# deptots(k,s)
# Land use for (selected) crops and all subnational regions (adm1 or adm2). 
adm_area <- lu_adm %>%
  filter(adm_level == 1) %>%
  dplyr::select(adm, short_name, value)

adm_area_gdx <- para_gdx(adm_area, c("adm", "short_name"), "adm_area", "Crop area per adm")


# lc(i)
lc_m <- lc %>%
  dplyr::select(gridID, value)

lc_gdx <- para_gdx(lc_m, c("gridID"), "lc", "Crop cover per grid cell")


# produ(j)
crop_area <- lu_sy %>%
  dplyr::select(sy, value)

crop_area_gdx <- para_gdx(crop_area, c("sy"), "crop_area", "Total area per crop")


# irrarea(i)
ir_area <-lc_ir %>%
  dplyr::select(gridID, value)

ir_area_gdx <- para_gdx(ir_area, c("gridID"), "ir_area", "Irrigated area per grid cell")


# ir_crop(j)
ir_crop <- lu_sy %>%
  filter(system == "I") %>%
  dplyr::select(sy, value)

ir_crop_gdx <- para_gdx(ir_crop, c("sy"), "ir_crop", "Total irrigated area per crop")

# det_area(i,j)
det_area <- lc_det %>%
  dplyr::select(gridID, sy, value = alloc)

det_area_gdx <- para_gdx(det_area, c("gridID", "sy"), "det_area", "Detailed land use information")


### CREATE GAMS SETS
# i: grid cells
i_set <- lc %>%
  dplyr::select(gridID) %>%
  unique()

i_set_gdx <- set_gdx(i_set, c("gridID"), "i", "Pixels")

# j: Crops with technology identifier
j_set <- lu_sy %>%
  dplyr::select(sy) %>%
  unique() 

j_set_gdx <- set_gdx(j_set, c("sy"), "j", "Crops with technology identifier")


# s: Main crops
s_set <- lu_adm %>%
  dplyr::select(short_name) %>%
  unique() 

s_set_gdx <- set_gdx(s_set, c("short_name"), "s", "Main crops")


# k: Subnat names wich have statistics 
k_set <- lu_adm %>%
  filter(adm_level == 1) %>%
  dplyr::select(adm) %>%
  unique()

k_set_gdx <- set_gdx(k_set, c("adm"), "k", "Subnat names wich have statistics")


# n(s,j)  Main crops with corresponding sub-crops 
n_set <- lu_sy %>%
  dplyr::select(short_name, sy) %>%
  unique() %>%
  setNames(c("s", "j"))

n_set_gdx <- set_gdx(n_set, c("s","j"), "n", ts="Main crops with corresponding sub-crops")


# l(k,i)  Pixels in subnat with statistics   
l_set <- lc %>%
  dplyr::select(adm, gridID) %>%
  unique() %>%
  setNames(c("k", "i"))

l_set_gdx <- set_gdx(l_set, c("k","i"), "l", ts="Pixels in Subnat with statistics")


# m(k, s) Main crop names in subnat with stat
m_set <- lu_adm %>%
  filter(adm_level == 1) %>%
  dplyr::select(adm, short_name) %>%
  unique() %>%
  setNames(c("k", "s"))

m_set_gdx <- set_gdx(m_set, c("k", "s"), "m", ts="Main crop names in Subnat with stat")

# scalelp: number of grid cells to scale optimization so numbers do not get too small
# Equal to number of i_set
scalef <- nrow(i_set)
scalef_gdx <- scalar_gdx(scalef, "scalef", "Scaling factor")

# Priors
priors_gdx <- para_gdx(priors, c("gridID", "sy"), "priors", "Prior area allocation shares")


### WRITE
wgdx(file.path(dataPath, paste0("Model/", iso3c_sel, "/Data/input_data_", iso3c_sel, "_2000.gdx")),
     lc_gdx, adm_area_gdx, ir_crop_gdx, ir_area_gdx, crop_area_gdx, det_area_gdx, priors_gdx,
     i_set_gdx, j_set_gdx, k_set_gdx, n_set_gdx, l_set_gdx, m_set_gdx, s_set_gdx, 
     scalef_gdx)
