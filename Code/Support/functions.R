#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script conduct simple imputation using values from other years
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


### AGRICULTURAL STATISTICS PROCESSING
# Add imputed value to x years by copying
copy_val_f <- function(df, y){
  copy_x <- length(y)
  df2 <- df[rep(1, copy_x),]
  df2$year <- y
  return(df2)
}


### MODEL PREPARATION  
# Function to write set as vector
write_set_f <- function(p, vec){
  fname <- deparse(substitute(vec))
  write.table(vec, file = file.path(p, paste0(fname, ".txt")), 
              row.names = FALSE, col.names = FALSE, quote = FALSE)
}
