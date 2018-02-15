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


### SET COUNTRY
source("Code/ZMB/Set_country.R")


### LOAD DATA
# FAOSTAT
faostat <- read_csv(file.path(dataPath, paste0("Data/", iso3c_sel, "/processed/Agricultural_statistics/faostat_crops_", iso3c_sel, ".csv")))

# Agro-Maps
am <- read_csv(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/am_", iso3c_sel, ".csv")))

# CropSTAT
cs <- read_csv(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/cs_", iso3c_sel, ".csv")))

# Agricultural statistics
as <- read_csv(file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/as_", iso3c_sel, ".csv")))



### COMBINE ADM DATA AND ADD ID
# Select area and filter out zeros
ag_stat <- bind_rows(am, cs, as, faostat) %>%
  filter(variable == "area",
         value != 0) %>%
  mutate(id = paste(source, adm_level, sep = "_"))
summary(ag_stat)

### CONCLUSIONS FROM Compare_ag_stat.R

# We decided to use 'as' data as source for subnational information because it provides longer time series and highest coverage
# Apart from sunflowers and cassava the series are very similar.
# Some data is missing and needs to be imputed.

# (1) Data for cott, cowp, pota and toba is largely incomplete => use national faostat data
# (2) beans are presented in as and am data but not in FAOSTAT => We assume they are part of opul in FAOSTAT. 
# (3) There is a spike in the sorgum data for 2001 in the as and am data which is clearly wrong. We replace by cs values.
# (4) After 2008 data is missing for cass. Also the 2008 values do not make sense (near zero)
# and the total is much higher in comparison than am and faostat so we use cassava from cs.


### CORRECTIONS
# (1) Remove cott, pota and toba
ag_stat <- ag_stat %>%
  filter(!short_name %in% c("cott", "pota", "toba", "cowp"),
         !(short_name == "cass" & year == 2008))

# (2) Reclassify beans 
ag_stat <- ag_stat %>%
  mutate(short_name = ifelse(short_name == "bean", "opul", short_name)) %>%
  group_by(year, adm, short_name, unit, variable, adm_level, source, id) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  ungroup()

# (3) Fix sorghum
cs_sorg <- filter(ag_stat, year == 2001, short_name == "sorg", id == "cs_1")
sorg_fix <- filter(ag_stat, year == 2001, short_name == "sorg", id == "as_1") %>%
  mutate(value =cs_sorg$value[match(adm, cs_sorg$adm)])
ag_stat[ag_stat$year == 2001 & ag_stat$short_name == "sorg" & ag_stat$id == "as_1",] <- sorg_fix
rm(sorg_fix, cs_sorg)

# (4) Use cassave from cs
cass_fix <- filter(ag_stat, short_name == "cass", id == "cs_1") %>%
  mutate(source = "as", id = "as_1")
ag_stat <- ag_stat %>%
  filter(!(short_name == "cass"& id == "as_1")) %>%
  bind_rows(cass_fix)


### IMPUTE MISSING AS VALUES
# For some values am and cs provide data while as does not. First preference is as and then cs to add information
as_imp <- ag_stat %>%
  filter(source %in% c("as","cs", "am")) %>%
  dplyr::select(-id) %>%
  spread(source, value) %>%
  mutate(as = ifelse(!is.na(as), as, ifelse(!is.na(am), am, cs))) %>%
  dplyr::select(-am, -cs) %>%
  rename(value = as) %>%
  mutate(source = "as") %>%
  na.omit


# Linear imputation
# Dataframe with all country combinations from start year to end year
base <- expand.grid(year = c(min(as_imp$year):max(as_imp$year)), 
                    short_name = unique(as_imp$short_name), adm = unique(as_imp$adm), stringsAsFactors = FALSE)

# Start end end year
as_imp_sy_ey <- as_imp %>%
  group_by(short_name, adm) %>%
  summarize(sy = min(year),
         ey = max(year)) %>%
  ungroup()


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
  df$variable = "area"
  df$unit = "ha"
  df$adm_level = 1
  df$source[is.na(df$source)] <- "impute" # ADD SOURCE HERE
  return(df)
}

# Impute NA values
as_imp <- left_join(base, as_imp) %>%
  left_join(.,as_imp_sy_ey) %>%
  group_by(adm, short_name) %>%
  filter(year >= sy & year <= ey) %>%
  do(impute_f(.))

# Check if values are still missing for key years
as_check <- as_imp %>%
  group_by(short_name, adm) %>%
  filter(sy >2000 | ey <2010)

# Impute missing values by simply taking last available value
as_imp <- bind_rows(
  as_check %>%
    group_by(short_name, adm) %>%
    do(fill_val_f(., 2000, "past")),
  as_check %>%
    group_by(short_name, adm) %>%
    do(fill_val_f(., 2010, "future")),
  as_imp) %>%
  dplyr::select(-ey, -sy)
rm(as_check, as_imp_sy_ey, base)  


### PLOT RESULTS
# Plot function
df <- filter(as_imp, adm == "NORTHERN")
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
fig_crop_adm <- as_imp %>%
  group_by(adm) %>%
  do(plot = p_crop_adm(.))

pdf(file = file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/Agricultural_statistics/Graphs/fig_am_imp.pdf")), width = 15, height = 12)
fig_crop_adm$plot
dev.off()


### HARMONISE ADM TOTAL WITH NATIONAL TOTAL FROM FAOSTAT
# Create FAO adm0 data: Calculate averages for 1999-2001
FAOSTAT_2000 <- faostat %>%
  filter(year %in% c(1999:2001), variable == "area") %>%
  group_by(short_name, variable, adm_level, adm) %>%
  summarize(value = mean(value)) %>%
  ungroup() %>%
  dplyr::select(adm, value, short_name, adm_level, variable)

# Compare adm total with FAOSTAT
# Good match apart from cassava
comp_2000 <- as_imp %>%
  ungroup() %>%
  filter(year %in% c(1999:2001)) %>%
  group_by(short_name, variable, adm_level, adm) %>%
  summarize(value = mean(value)) %>%
  ungroup() %>%
  group_by(short_name, variable) %>%
  summarize(adm0 = sum(value)) %>%
  left_join(transmute(FAOSTAT_2000, short_name, variable, FAOSTAT = value)) %>%
  gather(source, value, -short_name, -variable)


ggplot() +
  geom_col(data = comp_2000, aes(y = value, x = source, fill = source)) +
  facet_wrap(~short_name, scales = "free")

# Harmonize
adm_2000 <-  as_imp %>%
  ungroup() %>%
  filter(year %in% c(1999:2001)) %>%
  group_by(short_name, variable, adm_level, adm) %>%
  summarize(value = mean(value)) %>%
  ungroup() %>%
  group_by(short_name, variable) %>%
  mutate(adm0 = sum(value)) %>%
  left_join(transmute(FAOSTAT_2000, short_name, variable, FAOSTAT = value)) %>%
  mutate(value = value/adm0*FAOSTAT) %>%
  dplyr::select(adm, value, short_name, adm_level, variable) %>%
  ungroup() 

### ADD ZERO FOR CROPS THAT ARE NOT PRODUCED IN AN ADM. 
# check coverage
xtabs(~ adm + short_name, data = adm_2000)

# Fill in zero
adm_2000 <- adm_2000 %>%
  spread(short_name, value, fill = 0) %>%
  gather(short_name, value, -adm, -adm_level, -variable)

# Combine
ag_stat_2000 <- bind_rows(FAOSTAT_2000, adm_2000) %>%
  dplyr::select(-variable)

### SHARE OF SUBNATIONAL CROPS IN TOTAL
# Crops covered by adm statistics
unique(ag_stat_2000$short_name[ag_stat_2000$adm != iso3c_sel])

# Crops not covered by adm statistics
setdiff(
  unique(ag_stat_2000$short_name[ag_stat_2000$adm == iso3c_sel]),
  unique(ag_stat_2000$short_name[ag_stat_2000$adm != iso3c_sel]))

# Share of crops covered by adm statistics
sum(ag_stat_2000$value[ag_stat_2000$adm != iso3c_sel])/
      sum(ag_stat_2000$value[ag_stat_2000$adm == iso3c_sel]) *100

# Area not allocated to adm
sum(ag_stat_2000$value[ag_stat_2000$adm == iso3c_sel]) - 
  sum(ag_stat_2000$value[ag_stat_2000$adm != iso3c_sel])


### SAVE
# lu_adm
saveRDS(ag_stat_2000, file.path(dataPath, paste0("Data/", iso3c_sel, "/Processed/GAMS/lu_adm_2000_", iso3c_sel, ".rds"))) 

# Crop list
crop_list <- ag_stat_2000 %>%
  filter(adm == iso3c_sel) %>%
  dplyr::select(short_name, adm) %>%
  unique
write_csv(crop_list, file.path(paste0(dataPath, "/Data/ZMB/Processed/Mappings/Total_crop_lvst_list_2000_", iso3c_sel, ".csv")))
