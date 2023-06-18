# https://r-pkgs.org

library(devtools)
library(usethis)

p <- "W:/PROJEKT/R/eagles"
#usethis::create_package(p, check_name = FALSE)

devtools::load_all()

# Must run document() to add export functions to NAMESPACE
devtools::document()
devtools::install()

devtools::test()

# Document data:
# https://r-pkgs.org/data.html

p <- "W:/PROJEKT/R/cyclar"
#usethis::create_package(p, check_name = FALSE)

usethis::use_mit_license()

use_git_config(user.name = "peterhellstrom", user.email = "peter.hellstrom@nrm.se")
usethis::use_git()
usethis::use_github()
# GitHub API error (401): Bad credentials

usethis::create_github_token()

use_readme_rmd()
build_readme()

devtools::load_all()

# Must run document() to add export functions to NAMESPACE
devtools::document()
devtools::install()

devtools::test()

# Document data:
# https://r-pkgs.org/data.html

install_github("peterhellstrom/cyclar")


## Load package ----
library(eagles)

## Data sets ----
storrutor
ekorutor
fastighetsblad
wms_layers_data
tms_layers_data

st_as_sf(storrutor, coords = c("easting", "northing"), crs = 3021)

## Functions ----
# How should functions be written - include source package in code?
# Use native pipe or not?
# Depends in DESCRIPTION file - loads all packages, can this be avoided?
# Can Imports be used?
tmp <- function(.x, ...) {
  .x %>%
    dplyr::select(...)
}

ekorutor %>% tmp(ruta_id)

round_up(9.45)
lm_basemaps()
swe_tiles(tile_providers = tms_layers_data)
index_to_sf(storrutor, easting, northing, 50000, 50000, 3021) %>%
  mapview()
