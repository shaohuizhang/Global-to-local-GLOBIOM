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
p_load("countrycode")


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
# bean and cowp are presented in cs and am data but not in FAOSTAT => We assume they are part of opul in FAOSTAT 
ag_stat <- ag_stat %>%
  mutate(short_name = ifelse(short_name == "bean", "opul", short_name),
         short_name = ifelse(short_name == "cowp", "opul", short_name)) %>%
  group_by(year, adm, short_name, unit, variable, adm_level, source, id) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  ungroup()
  
# am values for Sorgum are extremely high in 2001, while all other values are very similar to cs => use cs values for 2001
cs_sorg <- filter(ag_stat, year == 2001, short_name == "sorg", id == "cs_1")
sorg_fix <- filter(ag_stat, year == 2001, short_name == "sorg", id == "am_1") %>%
  mutate(value =cs_sorg$value[match(adm, cs_sorg$adm)])
ag_stat[ag_stat$year == 2001 & ag_stat$short_name == "sorg" & ag_stat$id == "am_1",] <- sorg_fix


### COMPARE AREA AT COUNTRY LEVEL
# Might be large differences simply because crop coverage is different
# aggregate adm1 tot adm0
area_adm0 <- ag_stat %>%
  filter(variable %in% c("area")) %>%
  group_by(year, adm_level, unit, variable, source) %>%
  summarize( value = sum(value, na.rm = T)) %>%
  mutate(id = paste(adm_level, source, sep = "_"))

# Figure
fig_area_adm0 <- ggplot(data = area_adm0, aes(x = year, y = value, colour = id)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  labs(title = "Area comparison between FAOSTAT, am, as and cs",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(text = element_text(size=10))

fig_area_adm0


### COMPARE AREA AT CROP AND COUNTRY LEVEL
# aggregate adm1 tot adm0
crop_area_adm0 <- ag_stat %>%
  group_by(id, year, adm_level, short_name, unit, variable, source) %>%
  summarize( value = sum(value, na.rm = T))

# Area comparison over time
fig_area_crop_adm0 <- ggplot(data = filter(crop_area_adm0, variable == "area"), aes(x = year, y = value, colour = id)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  facet_wrap(~short_name, scales = "free") +
  labs(title = "Area comparison between FAOSTAT, am, as and cs",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(text = element_text(size=10))

fig_area_crop_adm0


### COMPARE AREA AT CROP AND ADM1 LEVEL
# Select data
crop_area_adm1 <- ag_stat %>%
  filter(variable %in% c("area"), adm_level == 1) %>%
  mutate(id = paste(adm_level, source, sep = "_"))

# Plot function
p_crop_area_adm1 <- function(df) {
  p = ggplot(data = df, aes(x = year, y = value, colour = id)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  facet_wrap(~short_name, scales = "free") +
  labs(title = unique(df$adm),
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(text = element_text(size=10))
}

# Create plots
fig_crop_area_adm1 <- crop_area_adm1 %>%
  group_by(adm) %>%
  do(plot = p_crop_area_adm1(.))

pdf(file = file.path(dataPath, "Data/ZMB/Processed/Agricultural_statistics/Graphs/fig_area_adm.pdf"), width = 15, height = 12)
fig_crop_area_adm1$plot
dev.off()

### CHECK DATA AVAILABILITY
period <- c(2000, 2010)

# FAOSTAT:ok
FAOSTAT_check <- ag_stat %>%
  filter(id == "FAOSTAT_0", year %in% period) %>%
  group_by(short_name) %>%
  mutate(n=n()) %>%
  filter(n < length(period))

# am: missing for 2000 and 2010
# Excludes combinations where both years are missing
am_check <- ag_stat %>%
  filter(id == "am_1", year %in% period) %>%
  group_by(short_name, adm) %>%
  mutate(n=n()) %>%
  filter(n < length(period))

# cs: missing after 2004
cs_check <- ag_stat %>%
  filter(id == "cs_1", year %in% period) %>%
  group_by(short_name, adm) %>%
  mutate(n=n()) %>%
  filter(n < length(period))


### CONCLUSIONS
# We decided to use am data as source for subnational information because it provides longer time series, also covering 2010
# A comparison with cs data shows that the series are similar and the largest crops are covered.
# am data needs to be imputed for 2010.
