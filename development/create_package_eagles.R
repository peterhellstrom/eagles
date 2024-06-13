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

chk_pkg <- check()
dplyr::glimpse(chk_pkg)
names(chk_pkg)

test()

# Document data:
# https://r-pkgs.org/data.html

# usethis::create_package(p, check_name = FALSE)

usethis::use_mit_license()

use_git_config(user.name = "peterhellstrom", user.email = "peter.hellstrom@nrm.se")
usethis::use_git()
usethis::use_github()

usethis::create_github_token()

use_readme_rmd()
build_readme()

# Ignore ----
usethis::use_build_ignore(c("backup", "data-raw", "development", "examples"))

# Imports ----
usethis::use_package("dplyr", min_version = TRUE)
usethis::use_package("purrr", min_version = TRUE)

usethis::use_package("sf", min_version = TRUE)

usethis::use_package("RODBC", min_version = TRUE)
usethis::use_package("DBI", min_version = TRUE)

# '::' or ':::' imports not declared from:
# 'RJDBC' 'RODBCext' 'broom' 'data.table' 'getPass' 'glue' 'gratia'
# 'janitor' 'jsonlite' 'lubridate' 'mgcv' 'odbc' 'pdftools' 'readr'
# 'readxl' 'rlang' 'rstudioapi' 'rvest' 'sifr' 'stringi' 'stringr'
# 'tibble' 'tidyr' 'tidyselect' 'xml2'

# Suggests ----
usethis::use_package("ggplot2", "Suggests")

usethis::use_tidy_description()

# Document data:
# https://r-pkgs.org/data.html

install_github("peterhellstrom/eagles")

## Load package ----
library(eagles)

## Data sets ----
usethis::use_data_raw()
rc_species_list
