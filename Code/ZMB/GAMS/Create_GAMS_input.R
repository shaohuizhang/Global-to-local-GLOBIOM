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
ir <- readRDS(file.path(paste0(dataPath, "/Data/", iso3c_sel, "/Processed/GAMS/ir_2000_", iso3c_sel, ".rds"))) 

# Priors
priors_raw <- readRDS(file.path(paste0(dataPath, "/Data/", iso3c_sel, "/Processed/GAMS/priors_2000_", iso3c_sel, ".rds"))) 


### CREATE GAMS PARAMETERS 
# deptots(k,s)
# Land use for (selected) crops and all subnational regions (adm1 or adm2). 
deptots <- lu_adm %>%
  filter(adm_level == 1) %>%
  dplyr::select(adm, short_name, value)

deptots_gdx <- para_gdx(deptots, c("adm", "short_name"), "deptots", "Ratio per main crops")


# avail(i,c)
avail <- lc %>%
  dplyr::select(gridID, value = lc_area)

avail_gdx <- para_gdx(avail, c("gridID"), "avail", "Available area per grid cell")


# produ(j)
produ <- lu_sy %>%
  dplyr::select(sy, value)

produ_gdx <- para_gdx(produ, c("sy"), "produ", "Production of crops")


# irrarea(i)
irrarea <- ir %>%
  dplyr::select(gridID, value = gmia)

irrarea_gdx <- para_gdx(irrarea, c("gridID"), "irrarea", "Irrigrated area per grid cell")


# irrcrops(j)
irrcrops <- lu_sy %>%
  filter(system == "I") %>%
  dplyr::select(sy, value)

irrcrops_gdx <- para_gdx(irrcrops, c("sy"), "irrcrops", "Area of irrigrated crops")


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
# NB RENAME adm1 into adm
l_set <- lc %>%
  dplyr::select(adm1, gridID) %>%
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
scalelp <- nrow(i_set)
scalelp_gdx <- scalar_gdx(scalelp, "scalelp", "Scalar for lp")

# Priors
priors <- priors_raw %>%
  mutate(value = priors_norm * scalelp) %>%
  dplyr::select(gridID, sy, value) %>%
  mutate(value = ifelse(value == 0, value+0.000001, value))
priors_gdx <- para_gdx(priors, c("gridID", "sy"), "rev", "Priors for cross-entropy")


### WRITE
wgdx(file.path(dataPath, paste0("Model/", iso3c_sel, "/Data/input_data_", iso3c_sel, "_2000.gdx")),
     avail_gdx, deptots_gdx, irrcrops_gdx, irrarea_gdx, produ_gdx, priors_gdx,
     i_set_gdx, j_set_gdx, k_set_gdx, n_set_gdx, l_set_gdx, m_set_gdx, s_set_gdx, 
     scalelp_gdx)
