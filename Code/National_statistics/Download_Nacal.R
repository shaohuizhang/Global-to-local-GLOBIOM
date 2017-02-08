#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to create maps
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

# ### INSTALL TABULIZER
# https://ropensci.org/tutorials/tabulizer_tutorial.html
# ghit::install_github(c("ropenscilabs/tabulizerjars", "ropenscilabs/tabulizer"), INSTALL_opts = "--no-multiarch")


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("ghit", "tabulizer")

wdPath<-"~/Global-to-local-GLOBIOM"
setwd(wdPath)

### SET DATAPATH
dataPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM\\Data"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### EXTRACT TABLES FROM NACAL REPORT
# Select pdf
doc <- file.path(dataPath, "Raw\\MWI\\National_statistics\\MWI\\Nacal_Report.pdf")

# Extract tables
pdf_raw <- extract_tables(doc, pages = c(102:110, 131, 132), method = "data.frame")

districts <- c("Karonga", "Rumphi", "Nkhata Bay", "Mzimba", "Mzuzu city", "Kasungu", "Ntchisi", "Dowa", "Nkhotakota", 
"Salima", "Dedza", "Ntcheu", "Lilongwe rural", "Lilongwe City", "Mchinji", "Balaka", "Mangochi", "Machinga", "Zomba rural",
"Zomba City", "Chiradzulu", "Blantyre rural", "Blantyre City", "Thyolo", "Mulanje", "Phalombe", "Mwanza", "Chikwawa", "Nsanje")

# Function to clean raw table from pdf
clean_tab_f <- function(df, name_df, unit){
  cut <- which(grepl("District", df$X))
  mwi <- which(grepl("Malawi", df$X.1))
  nrow <- nrow(df)
  sel <- df[c(mwi,cut:nrow),c(-1)]
  sel <- sel[colSums(!is.na(sel)) > 0] # Remove columns that are all NA
  names(sel) <- c("District", name_df)
  for(i in name_df) {
    sel[[i]] = trimws(sel[[i]])
    sel[[i]] = gsub(",", "", sel[[i]])
    sel[[i]] = as.numeric(sel[[i]])
  }
  sel$unit <- unit
  return(sel)
}

# Table 3.6
Table3_6_name <- c("Local", "Composite", "Recycled", "Hybrid", "production", "Total")
Table3_6 <- clean_tab_f(pdf_raw[[1]], Table3_6_name, "tons")

# Table 3.7
Table3_7_name <- c("Millit", "Sorghum", "Rice")
Table3_7 <- clean_tab_f(pdf_raw[[2]], Table3_7_name, "tons")

# Table 3.8
Table3_8_name <- c("Cassava", "Sweet_potato", "Irish_potato")
Table3_8 <- clean_tab_f(pdf_raw[[3]], Table3_8_name, "tons")

# Table 3.9
Table3_9_name <- c("Groundnuts", "Ordinary_beans", "Ground_beans", "Soya_beans", "Pigeon_peas", "Cow_peas")
Table3_9 <- clean_tab_f(pdf_raw[[4]], Table3_9_name, "tons")

# Table 3.10
Table3_10_name <- c("Local", "Composite", "Recycled", "Hybrid", "Total")
Table3_10 <- clean_tab_f(pdf_raw[[5]], Table3_10_name, "ha")

# Table 3.11
Table3_11_name <- c("Rice", "Millet", "Sorghum")
Table3_11 <- clean_tab_f(pdf_raw[[6]], Table3_11_name, "ha")

# Table 3.12
Table3_12_name <- c("Sweet_potatoes", "Irish_potatoes", "Cassava")
Table3_12 <- clean_tab_f(pdf_raw[[7]], Table3_12_name, "ha")

# Table 3.13
Table3_13_name <- c("Groundnuts", "Ground_beans", "Soya_beans", "Pigeon_peas", "Cow_peas", "Ordinary_beans")
Table3_13 <- clean_tab_f(pdf_raw[[8]], Table3_13_name, "ha")

# Table 3.14
Table3_14_name <- c("Cotton", "Tobacco", "Sunflower")
Table3_14 <- clean_tab_f(pdf_raw[[9]], Table3_14_name)

# Table 5.2
Table5_2_name <- c("Cattle", "Goats", "Sheep", "Pigs", "Chicken")
Table5_2 <- clean_tab_f(pdf_raw[[10]], Table5_2_name, "number")

# Table 5.2
Table5_3_name <- c("Donkeys", "Rabbits", "Guinea_pigs", "Ducks", "Guinea fowls", "Doves", "Turkeys")
Table5_3 <- clean_tab_f(pdf_raw[[11]], Table5_3_name, "number")

check <- pdf_raw[[11]]
