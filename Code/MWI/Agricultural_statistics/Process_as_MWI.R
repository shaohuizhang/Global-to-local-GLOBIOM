#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to process agricultural statistics
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


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### EXTRACT TABLES FROM NACAL REPORT
# Select pdf
doc <- file.path(dataPath, "Data\\MWI\\Raw\\Agricultural_statistics\\Other\\National_statistics\\Nacal_Report.pdf")

# Extract tables
pdf_raw <- extract_tables(doc, pages = c(102:110, 131, 132), method = "data.frame")

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
           adm2_as = trimws(District)) %>%
    dplyr::select(-District) %>%
    gather(crop_lvst_as, value, -unit, -adm2_as) %>%
    mutate(adm2_as = toupper(adm2_as),
           crop_lvst_as = toupper(crop_lvst_as))
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

# Combine
NACAL <- bind_rows(NACAL) 

# OBTAIN ADM AND CROP_LVST LIST
# Save adm list
# Need to correct NKHOTAKOTA into NKHOTA KOTA
as_MWI_adm2_list <- NACAL %>%
  dplyr::select(adm2_as) %>%
  mutate(adm2_as = ifelse(adm2_as == "NKHOTAKOTA", "NKHOTA KOTA", adm2_as)) %>%
  unique

write_csv(as_MWI_adm2_list, file.path(dataPath, "Data/MWI/Processed/Mappings/as_MWI_adm2_list.csv"))

# Save crop_lvst list
as_MWI_crop_lvst_list <- NACAL %>%
  dplyr::transmute(crop_lvst_as) %>%
  unique %>%
  arrange(crop_lvst_as)

write_csv(as_MWI_crop_lvst_list, file.path(dataPath, "Data/MWI/Processed/Mappings/as_MWI_crop_lvst_list.csv"))

### PROCESS NACAL
# Read adm mappping
MWI2adm <- read_excel(file.path(dataPath, "Data\\MWI\\Processed/Mappings/Mappings_MWI.xlsx"), sheet = "MWI2adm") %>%
  filter(year == 2000) %>%
  dplyr::select(adm2, adm2_as) %>%
  na.omit

# Read crop and livestock mapping
MWI_as2crop_lvst <- read_excel(file.path(dataPath, "Data\\MWI\\Processed/Mappings/Mappings_MWI.xlsx"), sheet = "MWI_as2crop_lvst") %>%
  dplyr::select(crop_lvst_as, short_name) %>%
  na.omit()

# Aggregate production and regroup  
as <- NACAL %>%
  mutate(adm2_as = ifelse(adm2_as == "NKHOTAKOTA", "NKHOTA KOTA", adm2_as)) %>% # repair coding that occurs in two differnt forms in NACAL
  left_join(MWI2adm) %>%
  left_join(MWI_as2crop_lvst) %>%
  filter(!is.na(short_name)) %>% # Filter out non-mapped crops
  group_by(short_name, adm2, unit) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(year = 2007, 
         adm_level = 2,
         variable = dplyr::recode(unit, "tons" = "production", "ha" = "area"),
         source = "as") %>%
  rename(adm = adm2)

# Write file
write_csv(as, file.path(dataPath , "Data/MWI/Processed/Agricultural_statistics/as_MWI.csv"))

