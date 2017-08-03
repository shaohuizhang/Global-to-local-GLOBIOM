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
p_load("countrycode")


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


### LOAD DATA
model_data <- readRDS(file.path(dataPath, "Data/MWI/processed/GAMS/model_data.rds"))
lu_system <- model_data[["lu_system"]]
lu_adm <- model_data[["lu_adm"]]
lc_ir <- model_data[["lc_ir"]]
lc_av <- model_data[["lc_av"]]

# Pop per grid cell
tot_pop_grid <- read_csv(file.path(dataPath, "Data/MWI/Processed/Spatial_data/tot_pop_grid_2000_MWI.csv"))


### CREATE GAMS INPUT DATA FILES
# CHECK: WILL BE REPLACED BY SCRIPT THAT WRITES GDX

# deptots(k,s)
deptots <- lu_adm %>%
  filter(adm_level == 2) %>%
  dplyr::select(adm, short_name, value)

write_csv(deptots, file.path(dataPath, "Model/Data/deptots.csv"), col_names = F)

# avail(i,c)
avail <- lc_av %>%
  dplyr::select(gridID, lc, value)

write_csv(avail, file.path(dataPath, "Model/Data/avail.csv"), col_names = F)

# produ(j)
produ <- lu_system %>%
  dplyr::select(system, value)
write_csv(produ, file.path(dataPath, "Model/Data/produ.csv"), col_names = F)

# icrops(i,j)
icrops <- lc_ir %>%
  dplyr::select(gridID, system, value)
write_csv(icrops, file.path(dataPath, "Model/Data/icrops.csv"), col_names = F)


### CREATE GAMS SETS

# # Function to write set as table with x columns
# # Need to be improved to add, between each element but not last.
# table_f <- function(vec, col){
#   fname <- deparse(substitute(vec))
#   vec <- paste(vec, collapse = ", ")
#   suppressWarnings(length(vec) <- prod(dim(matrix(vec, ncol = col))))
#   df <- matrix(vec, ncol = col,byrow = T)
#   df[is.na(df)] <- ""
#   write.table(df, file = file.path(dataPath, paste0("Data/MWI/Processed/sets/", fname, ".txt")), 
#               row.names = FALSE, col.names = FALSE, quote = FALSE)
# }

# c: crop groups
c_set <- lc_av %>%
  dplyr::select(lc) %>%
  unique()
write_set_f(file.path(dataPath, "Model/Data"), c_set)

# i: grid cells
i_set <- lc_av %>%
  dplyr::select(gridID) %>%
  unique()
write_set_f(file.path(dataPath, "Model/Data"), i_set)

# j: Crops with technology identifier
j_set <- lu_system %>%
  dplyr::select(system) %>%
  unique() 
write_set_f(file.path(dataPath, "Model/Data"), j_set)

# s: Main crops
s_set <- lu_adm %>%
  dplyr::select(short_name) %>%
  unique() 
write_set_f(file.path(dataPath, "Model/Data"), s_set)

# k: Subnat names wich have statistics 
k_set <- lu_adm %>%
  filter(adm_level == 2) %>%
  dplyr::select(adm) %>%
  unique()
write_set_f(file.path(dataPath, "Model/Data"), k_set)

# n(s,j)  Main crops with corresponding sub-crops 
n_set <- lu_system %>%
  dplyr::select(short_name, system) %>%
  unique() %>%
  setNames(c("s", "j")) %>%
  mutate(n = paste(s, j, sep = "    .    ")) %>%
  dplyr::select(n)
write_set_f(file.path(dataPath, "Model/Data"), n_set)

# l(k,i)  Pixels in subnat with statistics   
l_set <- lc_av %>%
  dplyr::select(adm2, gridID) %>%
  unique() %>%
  setNames(c("k", "i")) %>%
  mutate(l = paste(k, i, sep = "    .    ")) %>%
  dplyr::select(l)
write_set_f(file.path(dataPath, "Model/Data"), l_set)

# m(k, s) Main crop names in subnat with stat
m_set <- lu_adm %>%
  filter(adm_level == 2) %>%
  dplyr::select(adm, short_name) %>%
  unique() %>%
  setNames(c("k", "s")) %>%
  mutate(m = paste(k, s, sep = "    .    ")) %>%
  dplyr::select(m)
write_set_f(file.path(dataPath, "Model/Data"), m_set)

# cg(c, s) Main crops in crop groups 
cg_set <- lu_adm %>%
  dplyr::select(lc, short_name) %>%
  unique() %>%
  setNames(c("c", "s")) %>%
  mutate(cg = paste(c, s, sep = "    .    ")) %>%
  dplyr::select(cg)
write_set_f(file.path(dataPath, "Model/Data"), cg_set)


### CREATE PRIORS
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
write_csv(priors, file.path(dataPath, "Model/Data/priors.csv"), col_names = F)

