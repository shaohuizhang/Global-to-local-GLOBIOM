#'=================================================================================================
#' Project:  CIMMYT
#' Subject:  get data path 
#' Author:   Tom Morley
#' Contact:  tomas.morley@wur.nl
#' Output:   correct datapath for user
#'=================================================================================================

# Use this file to set your path to the data
# check your computer username using
# Sys.info()["user"] and use this in the if
# statement. Then add your dataPath within 
# the {} brackets

# Michiel WEcR
if(Sys.info()["user"] == "dijk158") {
  dataPath <- "C:\\Users\\dijk158\\OneDrive - IIASA\\SurveyData\\MWI/"}

# Michiel IIASA
if(Sys.info()["user"] == "vandijkm") {
  dataPath <- dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM\\Data\\"}

# Anybody else:
if(Sys.info()["user"] == "") {
  dataPath <- ""}