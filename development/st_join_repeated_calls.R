library(tidyverse)
library(sf)

dsn <- "E:/Maps/Naturvard/Naturvard.gpkg"
dsn_wtse <- "W:/projects/data/havsorn.gpkg"

query_sql <- list(
  "SELECT lankod, lanbok, lannamn, geom FROM admin_lan_fastk",
  "SELECT kommunkod, kommunnamn, geom FROM admin_kommun_fastk",
  "SELECT landskapskod, landskapbok, landskapnamn, geom FROM admin_landskap_fran_distrikt",
  "SELECT distriktskod, distriktsnamn, geom FROM admin_distrikt",
  "SELECT area_name AS rapportomrade, geom FROM admin_rapportomrade_artportalen"
)

query_sql <- list(
  "SELECT lankod AS lan_id, geom FROM admin_lan_fastk",
  "SELECT kommunkod AS kommun_id, geom FROM admin_kommun_fastk",
  "SELECT landskapskod AS landskap_id, distriktskod AS distrikt_id, geom FROM admin_distrikt",
  "SELECT area_id AS rapportomrade_id, geom FROM admin_rapportomrade_artportalen"
)

query_sql <- list(
  "SELECT lanbok AS lan, geom FROM admin_lan_fastk",
  "SELECT kommunnamn AS kommun, geom FROM admin_kommun_fastk",
  "SELECT landskapbok AS landskap, distriktsnamn AS distrikt, geom FROM admin_distrikt",
  "SELECT area_name AS rapportomrade, geom FROM admin_rapportomrade_artportalen"
)

# Import administrative units ----
admin_boundaries <- purrr::map(
  query_sql,
  \(x) sf::read_sf(dsn, query = x)
)

# Import territories and nests ----
lkd <- eagles::wtse_sites(spatial = TRUE, db_package = "DBI", encoding = "") |>
  dplyr::select(Lokalkod, Lokalnamn)

nests <- sf::read_sf(dsn_wtse, "wpt_nests")

# Join territories/nests and administrative units ----
test_1 <- lkd |>
  eagles::st_join_n(admin_boundaries, c("Lokalkod", "Lokalnamn"))

test_2 <- lkd |>
  eagles::st_join_n_loop(admin_boundaries)

test_3 <- nests |>
  dplyr::select(name) |>
  eagles::st_join_n(admin_boundaries, "name")

all.equal(test_1, test_2)
test_1; test_2; test_3

# test_1 |>
#   # dplyr::count(lanbok, landskapbok)
#   dplyr::count(lan_id, landskap_id)

lkd_nas <- test_1 |>
  dplyr::filter(if_any(where(is.character), \(x) is.na(x)))

mapview::mapview(lkd_nas)
