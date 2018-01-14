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

# Add imputed value to x years by copying
fill_val_f <- function(df, ty = 2000, direction = "past"){
  if(ty %in% c(df$year)){
    cat("year already available, no imputation\n")
    df2 <- NULL
    return(df[FALSE,])
  } else {
    if(direction == "future"){
      cat("impute into the future\n")
      sy <- max(df$year)
      try(if(ty < sy) stop("use past option"))
      copy_x <- length(c(ty:sy))-1
      index <- which(df$year == sy)
      df2 <- df[rep(index, copy_x),]
      df2$year <- c((sy+1):ty)
      df2$source <- "impute"
      return(df2)      
    } else {
      if(direction == "past"){
        cat("impute into the past\n")
        sy <- min(df$year)
        try(if(ty > sy) stop("use future option"))
        copy_x <- length(c(sy:ty))-1
        index <- which(df$year == sy)
        df2 <- df[rep(index, copy_x),]
        df2$year <- c(ty:(sy-1))
        df2$source <- "impute"
        return(df2)}
      }
  }
}


### MODEL PREPARATION  
# Function to write set as vector
write_set_f <- function(p, vec){
  fname <- deparse(substitute(vec))
  write.table(vec, file = file.path(p, paste0(fname, ".txt")), 
              row.names = FALSE, col.names = FALSE, quote = FALSE)
}



# Function to align rasters without mask at end
# Based on align_rasters from gdalUtils
align_raster_f <- function (unaligned, reference, dstfile, output_Raster = FALSE, 
                            nThreads = 1, verbose = FALSE, r, border, ...) 
{
  reference_info <- gdalinfo(reference, proj4 = TRUE, raw_output = FALSE, 
                             verbose = verbose)
  proj4_string <- reference_info$proj4
  bbox <- reference_info$bbox
  te <- c(reference_info$bbox[1, 1], reference_info$bbox[2, 
                                                         1], reference_info$bbox[1, 2], reference_info$bbox[2, 
                                                                                                            2])
  ts <- c(reference_info$columns, reference_info$rows)
  if (missing(dstfile)) 
    dstfile <- tempfile()
  if (is.character(nThreads)) {
    if (nThreads == "ALL_CPUS") {
      multi = TRUE
      wo = "NUM_THREADS=ALL_CPUS"
    }
  }
  else {
    if (nThreads == 1) {
      multi = FALSE
      wo = NULL
    }
    else {
      multi = TRUE
      wo = paste("NUM_THREADS=", nThreads, sep = "")
    }
  }
  r = r
  synced <- gdalwarp(srcfile = unaligned, dstfile = dstfile, 
                     te = te, t_srs = proj4_string, ts = ts, output_Raster = output_Raster, 
                     multi = multi, wo = wo, verbose = verbose, r = r, ...)
  
  synced <- mask(synced, border)
  writeRaster(synced, dstfile, overwrite = T)
  return(synced)
}

align_raster2_f <- function (unaligned, reference, dstfile, output_Raster = FALSE, 
                            nThreads = 1, verbose = FALSE, r, overwrite = F, ...) 
{
  reference_info <- gdalinfo(reference, proj4 = TRUE, raw_output = FALSE, 
                             verbose = verbose)
  proj4_string <- reference_info$proj4
  bbox <- reference_info$bbox
  te <- c(reference_info$bbox[1, 1], reference_info$bbox[2, 
                                                         1], reference_info$bbox[1, 2], reference_info$bbox[2, 
                                                                                                            2])
  ts <- c(reference_info$columns, reference_info$rows)
  if (missing(dstfile)) 
    dstfile <- tempfile()
  if (is.character(nThreads)) {
    if (nThreads == "ALL_CPUS") {
      multi = TRUE
      wo = "NUM_THREADS=ALL_CPUS"
    }
  }
  else {
    if (nThreads == 1) {
      multi = FALSE
      wo = NULL
    }
    else {
      multi = TRUE
      wo = paste("NUM_THREADS=", nThreads, sep = "")
    }
  }
  r = r
  synced <- gdalwarp(srcfile = unaligned, dstfile = dstfile, 
                     te = te, t_srs = proj4_string, ts = ts, output_Raster = output_Raster, 
                     multi = multi, wo = wo, verbose = verbose, r = r, overwrite = overwrite)
}