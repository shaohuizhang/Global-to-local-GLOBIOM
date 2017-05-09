#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  get data path 
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

# Use this file to set your path to the data
# check your computer username using
# Sys.info()["user"] and use this in the if
# statement. Then add your dataPath within 
# the {} brackets

# Michiel IIASA
if(Sys.info()["user"] == "vandijkm") {
  dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM"}

if(Sys.info()["user"] == "vandijkm") {
  FAOSTATPath <- "C:\\Users\\vandijkm\\DATA\\FAOSTAT_20170117"}

if(Sys.info()["user"] == "vandijkm") {
  ISWELPath <- "P:/is-wel"}

# Anybody else:
if(Sys.info()["user"] == "") {
  dataPath <- ""}


