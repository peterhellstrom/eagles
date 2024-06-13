# https://r-pkgs.org
# devtools::install_github("r-lib/devtools")
# devtools::install_github("r-lib/usethis")

library(devtools) # can be added to .Rprofile startup file

# p <- "W:/projects/R/eagles"
# usethis::create_package(p, check_name = FALSE)

load_all()

# Must run document() to add export functions to NAMESPACE
document()

chk_pkg <- check()
dplyr::glimpse(chk_pkg)
names(chk_pkg)

test()

# Document data:
# https://r-pkgs.org/data.html

# usethis::create_package(p, check_name = FALSE)

usethis::use_mit_license()

use_git_config(
  user.name = "peterhellstrom",
  user.email = "peter.hellstrom@nrm.se"
)

usethis::use_git()
usethis::use_github()

usethis::create_github_token()

use_readme_rmd()
build_readme()

# Ignore ----
usethis::use_git_ignore(c("backup", "development"))
usethis::use_build_ignore(c("backup", "data-raw", "development", "examples"))

# Imports ----
usethis::use_package("broom", min_version = TRUE)
usethis::use_package("data.table", min_version = TRUE)
usethis::use_package("DBI", min_version = TRUE)
usethis::use_package("dplyr", min_version = TRUE)
usethis::use_package("getPass", min_version = TRUE)
usethis::use_package("glue", min_version = TRUE)
usethis::use_package("gratia", min_version = TRUE)
usethis::use_package("janitor", min_version = TRUE)
usethis::use_package("jsonlite", min_version = TRUE)
usethis::use_package("lubridate", min_version = TRUE)
usethis::use_package("mgcv", min_version = TRUE)
usethis::use_package("odbc", min_version = TRUE)
usethis::use_package("pdftools", min_version = TRUE)
usethis::use_package("purrr", min_version = TRUE)
usethis::use_package("readr", min_version = TRUE)
usethis::use_package("readxl", min_version = TRUE)
usethis::use_package("RJDBC", min_version = TRUE)
usethis::use_package("rlang", min_version = TRUE)
usethis::use_package("RODBC", min_version = TRUE)
usethis::use_package("rstudioapi", min_version = TRUE)
usethis::use_package("rvest", min_version = TRUE)
usethis::use_package("sf", min_version = TRUE)
usethis::use_package("stringi", min_version = TRUE)
usethis::use_package("stringr", min_version = TRUE)
usethis::use_package("tibble", min_version = TRUE)
usethis::use_package("tidyr", min_version = TRUE)
usethis::use_package("tidyselect", min_version = TRUE)
usethis::use_package("xml2", min_version = TRUE)

# Dependencies on non-CRAN packages:
usethis::use_dev_package("RODBCext", remote = "github::zozlak/RODBCext")
usethis::use_dev_package("sifr", remote = "github::s-fleck/sifr")
usethis::use_dev_package("swecoords", remote = "github::peterhellstrom/swecoords")

# Suggests ----
usethis::use_package("ggplot2", "Suggests")

usethis::use_tidy_description()

# Document data:
# https://r-pkgs.org/data.html

# Install package ----
install()

# install_github("peterhellstrom/eagles")

## Load package ----
library(eagles)

## Data sets ----
usethis::use_data_raw()
rc_species_list
