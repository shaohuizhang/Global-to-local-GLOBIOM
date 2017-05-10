#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Script to powerpoint presentation
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "maps")
# Additional packages
p_load("ReporteRs")


### DETERMINE ROOT PATH
root <- find_root(is_rstudio_project)

### SET DATAPATH
source(file.path(root, "Code/get_dataPath.r"))


### SOURCE MAPS
source(file.path(root, "code/MWI/Maps/Key_maps_MWI.r"))
source(file.path(root, "code/Zambezi/Maps/Key_maps_Zambezi.r"))

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### CREATE POWERPOINT BASED ON TEMPLATE
# http://davidgohel.github.io/ReporteRs/index.html
pptx <- pptx(template = file.path(root, "Presentation/IIASA_template.pptx"))
slide.layouts(pptx)
slide.layouts(pptx, "Picture with Caption")

#### ADD FIGURES
#
pptx <- addSlide(pptx, slide.layout = "Picture with Caption")
pptx <-addTitle(pptx, 'my new graph')
pptx <-addPlot(pptx, print, x = fig_adm2_zambezi)

writeDoc(pptx, file.path(root, "Presentation/test.pptx"))
