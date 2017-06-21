#'========================================================================================================================================
#' Project:  TOOLS
#' Subject:  Functions to write a dataframe as GDX files
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
#if(!require(pacman)) install.packages("pacman")
# Key packages
#p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
#p_load("WDI", "countrycode")

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### NOTES
#' This script contains functions to convert dataframes into gdx files.
#' To do this the function wdgx of the package gdxrrw needs to be supplied with a list for: (1) the data values and (2) each variable
#' This is tedious and the following should be taken into account
#' 1. Data must be supplied as matrix
#' 2. The list must contain information on the set (see function below), most importantly the list of unique labels.
#' 
#' I use the following procedure to create the file
#' 1. Create a dataframe that first lists the variables (as factors) and in the final column the values.
#' 2. Obtain the factor levels from the factor variables using extractLevels_f
#' 3. Convert the data.frame to a matrix using factor2Number_f.
#' 4. Create the parameter file using GDXPara_f. NOTE, use 'unname' in front of the level argument as it is not allowed to have a name.
#' 5. Create the sets files (one for each set) using GDXSet_f. NOTE, use 'unname' in front of the level argument as it is not allowed to have a name.
#' 6. Create the name of the GDX file including the full path
#' 7. Use wdgx to create the file with as arguments: First the parameter file and second the sets files (in the same order as the parameter file).
#' 
#' Example
#' df: dataframe with first sets (as factors) and then value
#' level_file <- extractLevels_f(df)
#' matrix_file <- factor2Number_f(df)
#' set1_file <- GDXSet_f("set1", "set 1 description", unname(levelsfile["set1"]))
#' set2_file <- GDXSet_f("set2", "set 2 description", unname(levelsfile["set2"]))
#' parameter_file GDXPara_f(matrix_file, "valueName", "value description", unname(level_file))
#' filename <- file.path(dataPath, "filename.gdx"))
#' 
#' wdgx(filename, parameter_file, set1_file, set2_file)

### Function to extract levels of data frame
extractLevels_f <- function(df){
  fac <- which(sapply(df, class) == "factor")
  df2 <- lapply(fac, function(x) (levels(df[[x]])))
  return((df2))
}

### Function to convert data.frame to matrix, converting all factors to numeric
factor2Number_f<- function(df){
  for(i in which(sapply(df, class) == "factor")) df[[i]] = as.numeric(df[[i]])
  df<-as.matrix(df, rownames.force = NA) # rownames.force does not work so followed by dimnames
  dimnames(df)<-NULL
  return(df)
}


### Function to create list with information for a set
GDXSet_f<-function(name, ts, levels){
  Set <- list()
  Set[["name"]] <-name
  Set[["ts"]] <- ts
  Set[["type"]] <- "set"
  Set[["dim"]] <- 1
  Set[["form"]] <- "full"
  Set[["uels"]] <- levels
  Set[["val"]] <- array(rep(1, length(levels[[1]])))
  return(Set)
}

### Function to create list with information for a parameter
GDXPara_f<-function(val, name, ts, levels){
  Para <- list()
  Para[["name"]] <- name
  Para[["ts"]] <- ts
  Para[["type"]] <- "parameter"
  Para[["dim"]] <- length(levels)
  Para[["form"]] <- "sparse"
  Para[["uels"]] <- levels
  Para[["val"]] <- val
  return(Para)
}
