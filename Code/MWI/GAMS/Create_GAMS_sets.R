#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to create GAMS sets
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

# CHECK NEED TO BE CHANGED TO FAO
# Crop cover data
crop_cover <- readRDS(file.path(dataPath, "Data/MWI/Processed\\Spatial_data/crop_cover_2000_MWI.rds"))

# Agricultural statistics
ag_stat_2000 <- read_csv(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/ag_stat_2000_MWI.csv"))

# Irrigaton statistics
ir_stat <- readRDS(file.path(dataPath, "Data/MWI/Processed/Agricultural_statistics/irrigation_stat_MWI_2000.rds")) 

# Gridded adm
adm_grid_2000 <- read_csv(file.path(dataPath, "Data/MWI/processed/Spatial_data/adm_grid_2000_MWI.csv"))


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

modelPath <- file.path(dataPath, "Model/Sets")

# i: grid cells
i_set <- crop_cover %>%
  dplyr::select(gridID) %>%
  rename(i = gridID)
write_set_f(modelPath, i_set)

# j: Crops with technology identifier
j_set <- ag_stat_2000 %>%
  dplyr::select(short_name) %>%
  unique()
write_set_f(modelPath, j_set)

# s: Main crops
s_set <- ag_stat_2000 %>%
  dplyr::select(short_name) %>%
  unique()
write_set_f(modelPath, s_set)

# k: SubNat names wich have statistics 
k_set <- ag_stat_2000 %>%
  filter(adm_level == 2) %>%
  dplyr::select(adm) %>%
  unique()
write_set_f(modelPath, k_set)

# n(s,j)  Main Crops with corresponding sub-crops 
n_set <- bind_cols(s_set, j_set) %>%
  setNames(c("s", "j")) %>%
  mutate(n = paste(s, j, sep = "    .    ")) %>%
  dplyr::select(n)
n_set <- paste(n_set$n, collapse = ", ")
write_set_f(modelPath, n_set)

# l(k,i)  Pixels in SubNat with statistics   
l_set <- crop_cover %>%
  dplyr::select(adm2, gridID) %>%
  setNames(c("k", "i")) %>%
  mutate(l = paste(k, i, sep = "    .    ")) %>%
  dplyr::select(l)
write_set_f(modelPath, l_set)

# m(k, s) Main Crop Names in SubNat with stat
m_set <- ag_stat_2000 %>%
  filter(adm_level == 2) %>%
  dplyr::select(adm, short_name) %>%
  unique() %>%
  setNames(c("k", "s")) %>%
  mutate(m = paste(k, s, sep = "    .    ")) %>%
  dplyr::select(m)
write_set_f(modelPath, m_set)

