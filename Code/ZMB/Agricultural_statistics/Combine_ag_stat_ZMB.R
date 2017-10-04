#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to analyse and combine agricultural statistics data from different sources
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
p_load("countrycode", "imputeTS")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### SOURCE FUNCTIONS
source(file.path(root, "Code/Support/functions.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)
options(max.print=1000000) # more is printed on screen


### LOAD MAPPINGS
# Regional mapping
adm_map <- read_excel(file.path(dataPath, "Data/ZMB/Processed/Mappings/Mappings_ZMB.xlsx"), sheet = "ZMB2adm") %>%
  filter(year == 2000)

crop_lvst <- read_excel(file.path(dataPath, "Data\\Mappings\\Mappings.xlsx"), sheet = "crop_lvst") %>%
  dplyr::select(short_name) %>%
  na.omit()


### LOAD DATA
# FAOSTAT
FAOSTAT <- read_csv(file.path(dataPath, "Data/ZMB/processed/Agricultural_statistics/FAOSTAT_ZMB.csv"))

# Agro-Maps
am <- read_csv(file.path(dataPath, "Data/ZMB/Processed/Agricultural_statistics/am_ZMB.csv"))

# CropSTAT
cs <- read_csv(file.path(dataPath, "Data/ZMB/Processed/Agricultural_statistics/cs_ZMB.csv"))

# Agricultural statistics


### COMBINE ADM DATA AND ADD ID
# Filter out adm that are not relevant
ag_stat <- bind_rows(am, cs, FAOSTAT) %>%
  filter(variable == "area") %>%
  mutate(id = paste(source, adm_level, sep = "_"))


### CORRECTIONS
ag_stat_upd <- ag_stat

# See Compare_ag_stat_ZMB.r for analysis
# bean and cowp are presented in cs and am data but not in FAOSTAT => We assume they are part of opul in FAOSTAT 
ag_stat_upd <- ag_stat_upd %>%
  mutate(short_name = ifelse(short_name == "bean", "opul", short_name),
         short_name = ifelse(short_name == "cowp", "opul", short_name)) %>%
  group_by(year, adm, short_name, unit, variable, adm_level, source, id) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  ungroup()

# am values for Sorgum are extremely high in 2001, while all other values are very similar to cs => use cs values for 2001
cs_sorg <- filter(ag_stat_upd, year == 2001, short_name == "sorg", id == "cs_1")
sorg_fix <- filter(ag_stat_upd, year == 2001, short_name == "sorg", id == "am_1") %>%
  mutate(value =cs_sorg$value[match(adm, cs_sorg$adm)])
ag_stat_upd[ag_stat_upd$year == 2001 & ag_stat_upd$short_name == "sorg" & ag_stat_upd$id == "am_1",] <- sorg_fix

# Impute am values
am_imp <- filter(ag_stat_upd, id == "am_1") 
am_imp_sy_ey <- am_imp %>%
  group_by(short_name, adm) %>%
  mutate(sy = min(year),
         ey = max(year)) %>%
  dplyr::select(short_name, adm, sy, ey) %>%
  unique

# Impute missing values
# Dataframe with all country combinations from start year to end year
base <- expand.grid(year = c(min(am_imp$year):max(am_imp$year)), 
                    short_name = unique(am_imp$short_name), adm = unique(am_imp$adm), stringsAsFactors = FALSE)

# Function to impute values
impute_f <- function(df){
  title = unique(paste(df$adm, df$short_name, sep = "_"))
  print(title)
  df <- arrange(df, year)
  #Impute
  #imp <- na.kalman(df$value)
  imp <- na.interpolation(df$value) # use simple interpolation
  plotNA.imputations(df$value, imp, main = title)
  # Combine
  df$value <- imp
  df$id = "am_1"
  df$variable = "area"
  df$unit = "ha"
  df$adm_level = 1
  df$source[is.na(df$source)] <- "impute"
  return(df)
}

# Impute NA values
am_imp <- left_join(base, am_imp) %>%
  left_join(.,am_imp_sy_ey) %>%
  group_by(adm, short_name) %>%
  filter(year >= sy & year <= ey) %>%
  do(impute_f(.))

# Check if values are still missing for key years
am_check <- am_imp %>%
  group_by(short_name, adm) %>%
  filter(sy >2000 | ey <2010)

# Impute missing values by simply taking last available value
am_imp <- bind_rows(
  am_check %>%
    group_by(short_name, adm) %>%
    do(fill_val_f(., 2000, "past")),
  am_check %>%
    group_by(short_name, adm) %>%
    do(fill_val_f(., 2010, "future")),
  am_imp) %>%
  dplyr::select(-ey, -sy)
  

### PLOT RESULTS
# Plot function
df <- filter(am_imp, adm == "NORTHERN")
p_crop_adm <- function(df) {
  p = ggplot(data = df, aes(x = year, y = value)) +
    geom_line(colour = "red") +
    geom_smooth(se = F) +
    facet_wrap(~short_name, scales = "free") +
    labs(title = unique(df$adm),
         y = "ha",
         x ="") +
    scale_y_continuous(labels=comma) +
    theme_bw() +
    theme(text = element_text(size=10))
}

# Create plots
fig_crop_adm <- am_imp %>%
  group_by(adm) %>%
  do(plot = p_crop_adm(.))

pdf(file = file.path(dataPath, "Data/ZMB/Processed/Agricultural_statistics/Graphs/fig_am_imp.pdf"), width = 15, height = 12)
fig_crop_adm$plot
dev.off()


### 

### HARMONISE ADM TOTAL WITH NATIONAL TOTAL FROM FAOSTAT
# Create new ag_stat_upd
rm(ag_stat_upd)
ag_stat_upd <- bind_rows(am_imp, FAOSTAT) %>%
  filter(variable == "area") %>%
  mutate(id = paste(source, adm_level, sep = "_"))

# Create FAO adm0 data: Calculate averages for 1999-2001
FAOSTAT_2000 <- ag_stat_upd %>%
  filter(id %in% c("FAOSTAT_0"), year %in% c(1999:2001)) %>%
  group_by(short_name, id, variable, adm_level, adm) %>%
  summarize(value = mean(value)) %>%
  ungroup() %>%
  dplyr::select(adm, value, short_name, adm_level, variable)

# Create adm area data
adm_2000 <-  ag_stat_upd %>%
  ungroup() %>%
  filter(id == "am_1", year %in% c(1999:2001)) %>%
  group_by(short_name, id, variable, adm_level, adm) %>%
  summarize(value = mean(value)) %>%
  ungroup() %>%
  group_by(short_name, variable) %>%
  mutate(adm0 = sum(value)) %>%
  left_join(transmute(FAOSTAT_2000, short_name, variable, FAOSTAT = value)) %>%
  mutate(value = value/adm0*FAOSTAT) %>%
  dplyr::select(adm, value, short_name, adm_level, variable) %>%
  ungroup()

# Combine and plot
ag_stat_2000 <- bind_rows(FAOSTAT_2000, adm_2000)


### ADD ZERO FOR CROPS THAT ARE NOT PRODUCED IN AN ADM. 
# check coverage
xtabs(~ adm + short_name, data = filter(ag_stat_2000, adm_level == 1))

# Fill in zero
ag_stat_2000_adm <- filter(ag_stat_2000, adm_level == 1) %>%
  spread(short_name, value, fill = 0) %>%
  gather(short_name, value, -adm, -adm_level, -variable)

# Replace
ag_stat_2000 <- filter(ag_stat_2000, !(adm_level == 1)) %>%
  bind_rows(ag_stat_2000_adm)


### RECODE CROPS AND ADD LC CLASS
# Recode teas and coff to teas_coff because they are combined in lc mapping
ag_stat_2000 <- ag_stat_2000 %>%
  mutate(short_name = dplyr::recode(short_name, "teas" = "teas_coff", "coff" = "teas_coff")) %>%
  ungroup() %>%
  group_by(short_name, adm, adm_level, variable) %>%
  summarize(value = sum(value, na.rm = T))

# Add lc classes
ag_stat_2000 <- ag_stat_2000 %>%
  left_join(.,lc2crop_lvst)

### SAVE
write_csv(ag_stat_2000, file.path(dataPath, "Data/ZMB/Processed/Agricultural_statistics/ag_stat_2000_ZMB.csv"))
