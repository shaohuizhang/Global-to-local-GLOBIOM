#'========================================================================================================================================
#' Project:  TOOLS
#' Subject:  Functions to write r dataframes as GDX files
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


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


# CHECK: writing dim might not even necessary. See writeTansport.R in gdxrrw package folder for examples.

# Function to create val for parameter prep file
val_gdx <- function(val, variables){
  
  # Create factors of variables
  val[,variables] <- lapply(val[,variables, drop = F] , factor) # Drop added otherwise val becomes a vector
  
  # Convert factor variables to numeric
  for(i in which(sapply(val, class) == "factor")) val[[i]] = as.numeric(val[[i]])  
  val <- as.matrix(val)
  val <- unname(val)
  return(val)  
}


# Function to create uels for parameter prep file
uels_gdx <- function(uels, variables){
  uels <- uels[names(uels) %in% variables]
  uels <- lapply(uels, factor)
  uels <- lapply(uels,levels)
  return(uels)
}

# Function prepare parameter gdx file
para_gdx <- function(df, variables, name, ts = NULL, type = "parameter",  form = "sparse"){
  
  # Prepare input
  val <- val_gdx(df, variables)
  uels <- uels_gdx(df, variables)
  dim <- length(uels)
  ts <- ifelse(is.null(ts), name, ts)
  
  # Create parameter list
  para <- list()
  para[["val"]] <- val    # Array containing the symbol data
  para[["name"]] <- name  # Symbol name (data item)
  para[["dim"]] <- dim    # Dimension of symbol = levels
  para[["ts"]] <- ts      # Explanatory text for the symbol
  para[["uels"]] <- uels  # Unique Element Labels (UELS) (levels)
  para[["type"]] <- type  # Type of the symbol
  para[["form"]] <- form  # Representation, sparse or full
  return(para)
}


# Function prepare sets gdx file
set_gdx <- function(df, variables, name = NULL, ts = NULL, type = "set"){
  
  # Prepare input
  uels <- uels_gdx(df, variables)
  
  if(length(variables) > 1) {
    val <- val_gdx(df, variables)
    form <- "sparse"
  } else {
    val <- array(rep(1, length(uels[[1]])))
    form <- "full"
  }
  
  dim <- length(uels)
  name <- ifelse(is.null(name), variables, name)
  ts <- ifelse(is.null(ts), variables, ts)
  
  # Create set list
  set <- list()
  set[["val"]] <- val
  set[["name"]] <- name
  set[["ts"]] <- ts
  set[["type"]] <- type
  set[["dim"]] <- dim
  set[["form"]] <- form
  set[["uels"]] <- uels
  return(set)
}

# Function to prepare scalar gdx file
scalar_gdx <- function(val, name = NULL, ts = NULL, type = "parameter", form = "full"){
  
  # Create scalar list
  scalar <- list()
  scalar[["val"]] <- val
  scalar[["name"]] <- name
  scalar[["ts"]] <- ts
  scalar[["type"]] <- type
  scalar[["form"]] <- form
  return(scalar)
}

  