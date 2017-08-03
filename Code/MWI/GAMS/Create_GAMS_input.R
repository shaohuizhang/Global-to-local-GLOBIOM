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
produ <- lu_system 

# icrops(i,j)
icrops <- lc_ir %>%
  dplyr::select(gridID, short_name, value)

write_csv(produ, file.path(dataPath, "Model/Data/iprodu.csv"), col_names = F)


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

# i: grid cells
i_set <- lc_av %>%
  dplyr::select(gridID) %>%
  unique() %>%
  rename(i = gridID)
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
n_set <- paste(n_set$n, collapse = ", ")
write_set_f(file.path(dataPath, "Model/Data"), n_set)

# l(k,i)  Pixels in subnat with statistics   
l_set <- lc_av %>%
  dplyr::select(adm2, gridID) %>%
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
