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


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)
options(max.print=1000000) # more is printed on screen

### LOAD DATA
# Crop cover data
crop_cover <- readRDS(file.path(dataPath, "Data/MWI/Processed\\Spatial_data/crop_cover_ESA_2000_MWI.rds"))

# Agricultural statistics

# Gridded adm
adm_grid_2000 <- read_csv(file.path(dataPath, "Data/MWI/processed/Spatial_data/adm_grid_2000_MWI.csv"))

### CREATE GAMS SETS
# Function to write set as vector
write_set_f <- function(vec){
  fname <- deparse(substitute(vec))
  write.table(vec, file = file.path(dataPath, paste0("Data/MWI/Processed/sets/", fname, ".txt")), 
              row.names = FALSE, col.names = FALSE, quote = FALSE)
}


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

# i: grid cells
i_set <- crop_cover %>%
  filter(area >0) %>%
  dplyr::select(gridID) %>%
  rename(i = gridID)
write_set_f(i_set)

# j: Crops with technology identified (equals main crops for now)
j_set <- FAOSTAT_2000 %>%
  select(short_name) %>%
  unique()
write_set_f(j_set)

# s: Main crops
s_set <- FAOSTAT_2000 %>%
  select(short_name) %>%
  unique()
write_set_f(s_set)

# k: SubNat names wich have statistics 
k_set <- adm_2000 %>%
  select(adm) %>%
  unique()
write_set_f(k_set)

# n(s,j)  Main Crops with corresponding sub-crops 
n_set <- bind_cols(s_set, j_set) %>%
  setNames(c("s", "j")) %>%
  mutate(n = paste(s, j, sep = ". ")) %>%
  select(n)
n_set <- paste(n_df$n, collapse = ", ")
write_set_f(n_set)

# l(k,i)  Pixels in SubNat with statistics   
l_set <- read_csv(file.path(dataPath, "Data/MWI/processed/Spatial_data/adm_grid_2000_MWI.csv")) %>%
  filter(adm2_GAUL %in% k_set$adm) %>%
  select(adm2_GAUL, gridID) %>%
  setNames(c("k", "i")) %>%
  mutate(l = paste(k, i, sep = "    .    ")) %>%
  select(l)
write_set_f(l_set)
