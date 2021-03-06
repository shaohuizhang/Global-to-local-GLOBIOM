#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Set country
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("countrycode")

### SET COUNTRY
iso3c_sel <- "MWI"
country_sel <- countrycode(iso3c_sel, "iso3c", "country.name")
iso3n_sel <- countrycode(iso3c_sel, "iso3c", "iso3n")
