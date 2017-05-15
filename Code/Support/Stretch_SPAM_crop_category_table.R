#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script stretch SPAM-FAO crop categorie table
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


### STRETCH SPAM CROP CATEGORY TABLE
# Read SPAM TABLE
SPAM <- read_excel(file.path(dataPath, "Data/GLOBAL/SPAM/SPAM_Mappings.xlsx"), sheet = 1)

# Function to stretch crop mappings
stretch_f <- function(id){
  row <- SPAM %>% filter(ID == id)
  code <- as.numeric(trimws(unlist(strsplit(row$Code, ","))))
  length_code <- length(code)
  df <- row[rep(1, length_code),]
  df$Code <- code
  return(df)
}

# Crop IDS that need to be stretchec
target_ID <- c(8, 13, 19, 25, 27, 31, 39, 40, 41, 42)
stretch_SPAM <- bind_rows(lapply(target_ID, stretch_f))

# Replace rows in original table
SPAM_upd <- SPAM %>%
  filter(!ID %in% target_ID) %>%
  mutate(Code = as.numeric(Code)) %>%
  bind_rows(.,stretch_SPAM) %>%
  arrange(ID)

# Remove SPAM split for millets and green coffee
SPAM_upd <-SPAM_upd %>%
  filter(!ID %in% c(6, 33)) %>%
  mutate(`Long Name` = dplyr::recode(`Long Name`, "Pearl Millet" = "Millet",
                                                  "Arabica Coffee" = "Coffee, Green"),
         `Short Name` = dplyr::recode(`Short Name`, "pmil" = "mill",
                                                    "acof" = "coff"))


# Save file
write_csv(SPAM_upd, file.path(dataPath, "Data/Global/SPAM/SPAM_crop_cat_stretched.csv"))
