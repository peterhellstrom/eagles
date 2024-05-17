# https://r-pkgs.org

# devtools::install_github("r-lib/devtools")
# devtools::install_github("r-lib/usethis")

library(devtools) # can be added to .Rprofile startup file

# p <- "W:/projects/R/eagles"
# usethis::create_package(p, check_name = FALSE)

load_all()

# Must run document() to add export functions to NAMESPACE
document()
install()

chk_eagles <- check()
glimpse(chk_eagles)
names(chk_eagles)

test()

# Document data:
# https://r-pkgs.org/data.html

# usethis::create_package(p, check_name = FALSE)

usethis::use_mit_license()

use_git_config(user.name = "peterhellstrom", user.email = "peter.hellstrom@nrm.se")
usethis::use_git()
usethis::use_github()
# GitHub API error (401): Bad credentials

usethis::create_github_token()

use_readme_rmd()
build_readme()

# Ignore ----
usethis::use_build_ignore(c("backup", "data-raw", "development", "examples"))

# Document data:
# https://r-pkgs.org/data.html

install_github("peterhellstrom/eagles")

## Load package ----
library(eagles)

## Data sets ----
usethis::use_data_raw()

storrutor
ekorutor
fastighetsblad
wms_layers_data
tms_layers_data
rc_species_list

storrutor |>
  st_as_sf(coords = c("easting", "northing"), crs = 3021) |>
  mapview::mapview()

storrutor |>
  mutate(
    geometry = map2(
      easting, northing,
      \(x, y) grid_cell(x, y, 50000, 50000)
    )
  ) |>
  st_as_sf(crs = 3021) |>
  mapview::mapview()

eagles::round_up(9.45)
eagles::lm_basemaps()
eagles::swe_tiles(tile_providers = tms_layers_data)
eagles::rc_species_list()
