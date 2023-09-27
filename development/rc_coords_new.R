library(tidyverse)
library(sf)
library(eagles)

coords_rc <- function(
    latitude,
    longitude,
    outformat = c("dm", "dms"),
    pad_right = TRUE,
    pad = " ",
    max_lon_width = 5) {

  latlon <- data.frame(
    lat = latitude,
    lon = longitude
  ) |>
    dplyr::mutate(
      lat_hemisphere = dplyr::case_when(
        latitude >= 0 ~ "N",
        TRUE ~ "S"
      ),
      lon_hemisphere = dplyr::case_when(
        longitude >= 0 ~ "E",
        TRUE ~ "W"
      ),
      dplyr::across(
        c(lat, lon), \(x) round_dd(abs(x), outformat),
        .names = "{.col}_round"
      )
    )

  latlon <- latlon |>
    mutate(
      lat_round = case_when(
        outformat == "dm" & pad_right ~ str_pad(lat_round, side = "right", pad = " ", width = 6),
        TRUE ~ lat_round
      ),
      lon_round = case_when(
        outformat == "dm" & !pad_right ~ str_pad(lon_round, side = "left", pad = " ", width = 5),
        outformat == "dm" & pad_right & str_length(lon_round) == 4 ~ str_pad(lon_round, side = "both", pad = " ", width = 7),
        outformat == "dm" & pad_right & str_length(lon_round) == 5 ~ str_pad(lon_round, side = "right", pad = " ", width = 7),
        outformat == "dms" ~ stringr::str_pad(lon_round, width = 7, side = "left", pad = " ")
      )
    ) |>
    dplyr::mutate(
      lat_lon_round = stringr::str_c(
        lat_round, lat_hemisphere,
        lon_round, lon_hemisphere
      )
    )

  latlon |>
    dplyr::pull(lat_lon_round)
}

coords_rc(64.110602, 15.46875, "dm")
coords_rc(64.110602, 15.46875, "dms")
coords_rc(64.110602, 15.46875, "dm", FALSE)
coords_rc(64.110602, 15.46875, "dm")
coords_rc(50.110602, -15.46875, "dm")
coords_rc(50.110602, -152.46875, "dm", FALSE)
coords_rc(50.110602, -152.46875, "dms", FALSE)
coords_rc(-50.110602, 150.46875, "dm", FALSE)
coords_rc(-50.110602, 150.46875, "dm")

# Many
coords_rc(
  c(64.110602, 67.496745, -12.1234),
  c(15.46875, 18.28125, 125.2345),
  "dm")

coords_rc(
  c(64.110602, 67.496745, -12.1234),
  c(15.46875, 18.28125, 125.2345),
  "dms")

coords_rc(
  c(64.110602, 67.496745, -12.1234),
  c(15.46875, 18.28125, 125.2345),
  "dm", pad_right = FALSE)

# outformat as vector? NO! Not yet...
coords_rc(
  rep(64.110602, 2),
  rep(15.46875, 2),
  c("dm", "dms"))

# Rounding "spill-over" effects from seconds to minutes?
coords_rc(60.549853, 17.175407, "dm")
coords_rc(60.549853, 17.175407, "dms") # 603259N 171031E

# Developer's case
latitude <- c(64.110602, 67.496745, 65.25, -42.89)
longitude <- c(15.46875, 18.28125, 125.12344, -16.34)
outformat <- "dm"
outformat <- "dms"
max_lon_width <- 5
pad_right <- TRUE

width <- 6
pad <- " "
side <- "right"
sep <- " "

# round_dd dependency

eagles::round_dd(-9, outformat = "dm")
eagles::round_dd(-9, outformat = "dms")

str_pad(c("1152","11528", "115281", "1152812"), side = "both", pad = "*", width = 7)
stri_pad_both(c("1152","11528", "115281", "1152812"), pad = "*", width = ceiling(0.9 * getOption("width")))

stri_pad_both(c("1152","11528", "115281", "1152812"), pad = "*", width = ceiling(1.1 * 7))

# Longitude values

# case_dm, pad_right = FALSE
str_pad("1152", side = "left", pad = " ", width = 5)

# case dm, pad_right = TRUE
str_pad("1152", side = "both", pad = " ", width = 7)
str_pad("11521", side = "right", pad = " ", width = 7)

# case dms
str_pad("115212", side = "left", pad = " ", width = 7)

# Latitude values

# case_dm, pad_right = FALSE
# do nothing

# case_dm, pad_right = TRUE
str_pad("1152", side = "right", pad = " ", width = 6)

# case_dms
# do nothing








str_pad("115223", side = "both", pad = " ", width = 7)
str_pad("2115223", side = "both", pad = " ", width = 7)

str_pad("1528", side = "both", pad = " ", width = 7)
str_pad("152812", side = "both", pad = " ", width = 7)
str_pad("1528", side = "left", pad = " ", width = 5)

