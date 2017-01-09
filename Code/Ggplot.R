##################################################################################################'
#' PROJECT: ggplot theme
#' Purpose: Standard themes for maps and figures - test
##################################################################################################'

### PACKAGES
BasePackages<- c("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer")
lapply(BasePackages, library, character.only = TRUE)
#SpatialPackages<-c("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
#lapply(SpatialPackages, library, character.only = TRUE)
#AdditionalPackages <- c("WDI")
lapply(AdditionalPackages, library, character.only = TRUE)

### SET WORKING DIRECTORY
wdPath<-"~/Github/FoodFutures"
setwd(wdPath)

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

#### THEME
# https://www.r-bloggers.com/how-to-create-a-ggplot-theme-unicorn-edition/

theme_unicorn <- function(base_size=12, font=NA){
  
  txt <- element_text(size = base_size+2, colour = "black", face = "plain")
  bold_txt <- element_text(size = base_size+2, colour = "black", face = "bold")
  
  theme_classic(base_size = base_size, base_family = font)+
    theme(
      ###### clean up!
      legend.key = element_blank(), 
      strip.background = element_blank(), 
      ########### text basics
      text = txt, 
      plot.title = txt, 
      
      axis.title = txt, 
      axis.text = txt, 
      
      legend.title = bold_txt, 
      legend.text = txt ) +
    
    ############## AXIS lines
    theme(
      
      axis.line.y = element_line(colour = "pink", size = 1, linetype = "dashed"),
      axis.line.x = element_line(colour = "pink", size = 1.2,linetype = "dashed"),
      #### remove Tick marks
      axis.ticks=element_blank(),
      
      ### legend  top and no title!
      legend.position = "top", 
      legend.title = element_blank(),
      legend.key = element_rect(fill = "lightskyblue1", color = "lightskyblue1"),
      legend.background = element_rect( fill = "lightskyblue1",color = "pink", size = 0.5,linetype = "longdash"),
      
      ## background
      plot.background = element_rect(fill = "lightskyblue1",colour = "pink",size = 0.5, linetype = "longdash")
    )
}