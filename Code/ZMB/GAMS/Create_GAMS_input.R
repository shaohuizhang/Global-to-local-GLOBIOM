#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to create GAMS data fils
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

# lu_grid
# add grid specific lu values
# Kafue flats on wikipedia for location of sugar plantations

# Pop per grid cell
pop <- readRDS(file.path(paste0(dataPath, "/Data/", iso3c_sel, "/Processed/GAMS/pop_2000_", iso3c_sel, ".rds"))) 


### CREATE GAMS PARAMETERS 
# deptots(k,s)
deptots <- lu_adm %>%
  filter(adm_level == 1) %>%
  dplyr::select(adm, short_name, value)

deptots_gdx <- para_gdx(deptots, c("adm", "short_name"), "deptots", "Ratio per main crops")


# avail(i,c)
avail <- lc %>%
  dplyr::select(gridID, value)

avail_gdx <- para_gdx(avail, c("gridID"), "avail", "Available area per grid cell")


# produ(j)
produ <- lu_sy %>%
  dplyr::select(system, value)

produ_gdx <- para_gdx(produ, c("system"), "produ", "Production of crops")


### CREATE GAMS SETS
# i: grid cells
i_set <- lc %>%
  dplyr::select(gridID) %>%
  unique()

i_set_gdx <- set_gdx(i_set, c("gridID"), "i", "Pixels")

# j: Crops with technology identifier
j_set <- lu_sy %>%
  dplyr::select(system) %>%
  unique() 

j_set_gdx <- set_gdx(j_set, c("system"), "j", "Crops with technology identifier")


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
  dplyr::select(short_name, system) %>%
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
scalelp <- nrow(i_set)
scalelp_gdx <- scalar_gdx(scalelp, "scalelp", "Scalar for lp")


### CREATE GAMS PRIORS
# Calculate prior for crop area by using share of rural population. 
# If we have information on crop area at adm2 level, we calculate the prior at adm2 level.

# Create gridID and system combinations
priors_base <- expand.grid(gridID = i_set$gridID, system = j_set$system)

# Merge pop and crop cover data
priors <- priors_base %>%
  left_join(.,tot_pop_grid) %>%
  mutate(prior = value/sum(value)) %>%
  dplyr::select(gridID, system, prior)

# 
# # Calculate crop area prior for crops at adm2 level
# crop_area_prior_adm2 <- grid_sel %>%
#   group_by(adm2) %>%
#   mutate(pop_share = value/sum(value)) %>%
#   dplyr::select(gridID, adm = adm2, pop_share) %>%
#   left_join(.,lu_adm) %>%
#   mutate(crop_area_prior = pop_share * value) %>%
#   dplyr::select(gridID, adm, short_name, crop_area_prior)
# 
# # Calculate crop area prior for crops at adm0 level
# # Need to filter out crops for which adm2 data is available
# crops_adm2 <- unique(crop_area_prior_adm2$short_name)
# lu_adm0 <- filter(lu_adm, !(short_name %in% crops_adm2))
# 
# crop_area_prior_adm0 <- grid_sel %>%
#   group_by(adm0) %>%
#   mutate(pop_share = value/sum(value)) %>%
#   dplyr::select(gridID, adm = adm0, pop_share) %>%
#   left_join(., lu_adm0) %>%
#   mutate(crop_area_prior = pop_share * value) %>%
#   dplyr::select(gridID, adm, short_name, crop_area_prior)
# 
# # Calculate prior as share of total
# # Apply crop_cover
# prior <- bind_rows(crop_area_prior_adm0, crop_area_prior_adm2) %>%
#   ungroup() %>%
#   group_by(short_name) %>%
#   mutate(prior = crop_area_prior/sum(crop_area_prior)) %>%
#   dplyr::select(-crop_area_prior, -adm) %>%
#   rename(i = gridID, j = short_name, rev = prior)


# Check if total area adds to sum as in ag_stat
#sum(prior$crop_area_prior)
#sum(ag_stat_2000$value)

# save
# NB: data is only read correctly by GAMS if GRID_ID numbers are manually formated to number, zero digits
# Perhaps save as txt?
priors_gdx <- para_gdx(priors, c("gridID", "system"), "rev", "Priors for cross-entropy")


scalelp_gdx <- scalar_gdx(scalelp, "scalelp", "Scalar for lp")
# Write gdx file
wgdx(file.path(dataPath, "Model/Data/spam_data.gdx"), 
     avail_gdx, deptots_gdx, icrops_gdx, produ_gdx, priors_gdx,
     c_set_gdx, i_set_gdx, j_set_gdx, k_set_gdx, n_set_gdx, l_set_gdx, m_set_gdx, cg_set_gdx, s_set_gdx, 
     scalelp_gdx)
