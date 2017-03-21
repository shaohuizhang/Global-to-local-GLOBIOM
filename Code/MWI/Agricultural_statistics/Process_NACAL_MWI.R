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


### LOAD DISTRICT MAPPING
adm_list_MWI <- read_csv(file.path(dataPath, "Processed/MWI/Mappings/adm_list_MWI.csv"))


### EXTRACT TABLES FROM NACAL REPORT
# Select pdf
doc <- file.path(dataPath, "Raw\\MWI\\National_statistics\\MWI\\Nacal_Report.pdf")

# Extract tables
pdf_raw <- extract_tables(doc, pages = c(102:110, 131, 132), method = "data.frame")

districts <- c("Chitipa", "Karonga", "Rumphi", "Nkhata Bay", "Mzimba", "Mzuzu city", "Kasungu", "Ntchisi", "Dowa", "Nkhotakota", 
"Salima", "Dedza", "Ntcheu", "Lilongwe rural", "Lilongwe City", "Mchinji", "Balaka", "Mangochi", "Machinga", "Zomba rural",
"Zomba City", "Chiradzulu", "Blantyre rural", "Blantyre City", "Thyolo", "Mulanje", "Phalombe", "Mwanza", "Chikwawa", "Nsanje")

# Function to clean raw table from pdf
clean_tab_f <- function(df, name_df, unit){
  cut <- which(grepl("District", df$X))
  nrow <- nrow(df)
  sel <- df[c(cut:nrow),c(-1)]
  sel <- sel[colSums(!is.na(sel)) > 0] # Remove columns that are all NA
  names(sel) <- c("District", name_df)
  for(i in name_df) {
    sel[[i]] = trimws(sel[[i]])
    sel[[i]] = gsub(",", "", sel[[i]])
    sel[[i]] = as.numeric(sel[[i]])
  }
  sel <- sel %>% 
    mutate(unit = unit,
           adm1_as = trimws(District)) %>%
    dplyr::select(-District) %>%
    gather(crop_as, value, -unit, -adm1_as) %>%
    mutate(adm1_as = toupper(adm1_as),
           crop_as = toupper(crop_as))
  return(sel)
}

NACAL <- list()

# Table 3.6
Table3_6_name <- c("Local", "Composite", "Recycled", "Hybrid", "Winter_production", "Total_maize")
NACAL[["Table3_6"]] <- clean_tab_f(pdf_raw[[1]], Table3_6_name, "tons")
rm(Table3_6_name)

# Table 3.7
Table3_7_name <- c("Millet", "Sorghum", "Rice")
NACAL[["Table3_7"]] <- clean_tab_f(pdf_raw[[2]], Table3_7_name, "tons")
rm(Table3_7_name)

# Table 3.8
Table3_8_name <- c("Cassava", "Sweet_potatoes", "Irish_potatoes")
NACAL[["Table3_8"]] <- clean_tab_f(pdf_raw[[3]], Table3_8_name, "tons")
rm(Table3_8_name)

# Table 3.9
Table3_9_name <- c("Groundnuts", "Ordinary_beans", "Ground_beans", "Soya_beans", "Pigeon_peas", "Cow_peas")
NACAL[["Table3_9"]] <- clean_tab_f(pdf_raw[[4]], Table3_9_name, "tons")
rm(Table3_9_name)

# Table 3.10
Table3_10_name <- c("Local", "Composite", "Recycled", "Hybrid", "Total_maize")
NACAL[["Table3_10"]] <- clean_tab_f(pdf_raw[[5]], Table3_10_name, "ha")
rm(Table3_10_name)

# Table 3.11
Table3_11_name <- c("Rice", "Millet", "Sorghum")
NACAL[["Table3_11"]] <- clean_tab_f(pdf_raw[[6]], Table3_11_name, "ha")
rm(Table3_11_name)

# Table 3.12
Table3_12_name <- c("Sweet_potatoes", "Irish_potatoes", "Cassava")
NACAL[["Table3_12"]] <- clean_tab_f(pdf_raw[[7]], Table3_12_name, "ha")
rm(Table3_12_name)

# Table 3.13
Table3_13_name <- c("Groundnuts", "Ground_beans", "Soya_beans", "Pigeon_peas", "Cow_peas", "Ordinary_beans")
NACAL[["Table3_13"]] <- clean_tab_f(pdf_raw[[8]], Table3_13_name, "ha")
rm(Table3_13_name)

# Table 3.14
Table3_14_name <- c("Cotton", "Tobacco", "Sunflower")
NACAL[["Table3_14"]] <- clean_tab_f(pdf_raw[[9]], Table3_14_name, "ha")
rm(Table3_14_name)

# Table 5.2
Table5_2_name <- c("Cattle", "Goats", "Sheep", "Pigs", "Chicken")
NACAL[["Table5_2"]] <- clean_tab_f(pdf_raw[[10]], Table5_2_name, "number")
rm(Table5_2_name)

# Table 5.3
Table5_3_name <- c("Donkeys", "Rabbits", "Guinea_pigs", "Ducks", "Guinea_fowls", "Doves", "Turkeys")
NACAL[["Table5_3"]] <- clean_tab_f(pdf_raw[[11]], Table5_3_name, "number")
rm(Table5_3_name)

### COMBINE DATA
# Read adm mappping
MWI2ADM <- read_excel(file.path(dataPath, "Processed/MWI/Mappings/Mappings_MWI.xlsx"), sheet = "MWI2ADM") %>%
  dplyr::select(adm1, adm1_as) %>%
  na.omit

# Read crop mapping
MWI_as2GLOBIOM_crop <- read_excel(file.path(dataPath, "Processed/MWI/Mappings/Mappings_MWI.xlsx"), sheet = "MWI_as2GLOBIOM_crop") %>%
  dplyr::select(crop_as, ALLPRODUCT) %>%
  na.omit()

# Read animal mapping
MWI_as2GLOBIOM_anim <- read_excel(file.path(dataPath, "Processed/MWI/Mappings/Mappings_MWI.xlsx"), sheet = "MWI_as2GLOBIOM_anim") %>%
  dplyr::select(crop_as = anim_as, ALLPRODUCT= ANIMALS) %>%
  na.omit()

# Combined crop and animal mapping
MWI_as2GLOBIOM <- bind_rows(MWI_as2GLOBIOM_anim, MWI_as2GLOBIOM_crop)
  

# Aggregate production and regroup  
NACAL <- bind_rows(NACAL) %>%
  mutate(adm1_as = ifelse(adm1_as == "NKHOTAKOTA", "NKHOTA KOTA", adm1_as)) %>% # repair coding that occurs in two differnt forms in NACAL
  dplyr::filter(unit == "tons") %>%
  left_join(MWI2ADM) %>%
  left_join(MWI_as2GLOBIOM) %>%
  filter(!is.na(ALLPRODUCT)) %>% # Filter out non-mapped crops
  group_by(ALLPRODUCT, adm1, unit) %>%
  summarize(value = sum(value, na.rm = T))

write_csv(NACAL, file.path(dataPath , "Processed/MWI/Agricultural_statistics/NACAL.csv"))
