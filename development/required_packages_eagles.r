# Check that required packages are installed ----

is_installed <- function(mypkg) {
  mypkg %in% installed.packages()[,1]
}

install_missing_packages <- function(mypkg,
                                     repos = "https://ftp.acc.umu.se/mirror/CRAN/",
                                     dependencies = TRUE) {

  for (i in 1:length(mypkg)) {
    if (is.installed(mypkg[i]) == FALSE) {
      install.packages(
        pkgs = mypkg[i],
        lib =.Library,
        repos = repos,
        dependencies = dependencies)
    }
  }
}

pkgs <- c(
  "rgdal",
  "gdalUtils",
  "leafem",
  "leaflet",
  "leaflet.extras",
  "sp",
  "maps",
  "mapdata",
  "maptools",
  "mapview",
  "raster",
  "geosphere",
  "spatstat",
  "mapview",
  "htmlwidgets",
  "rgeos",
  "spatialEco",
  "sf",
  "lwgeom",
  "tmap",
  "tidyverse")

is_installed(pkgs)
install_missing_packages(pkgs)

# Load required packages ----
# pkgs <- as.list(c(pkgs))
# lapply(pkgs, require, character.only = TRUE)
