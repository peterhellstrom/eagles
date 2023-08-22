# Direct transformation RT 90 <-> SWEREF 99
# using Gauss-Krügers projection

# https://www.lantmateriet.se/sv/Kartor-och-geografisk-information/gps-geodesi-och-swepos/Om-geodesi/Geodesitjanster/enkel-koordinattransformation/
# Inspired by javascripts from:
# http://latlong.mellifica.se/
# http://mellifica.se/geodesi/gausskruger.js
# https://rl.se/rt90

# https://www.tidyverse.org/blog/2019/06/rlang-0-4-0/#a-simpler-interpolation-pattern-with

# Last updated 2023-02-23

# To do: tidy up input and output parameters, handle order x & y
# in a more consistent way.

#' @export
deg_to_rad <- function(angle) angle * (pi / 180)

#' @export
rad_to_deg <- function(radians) radians * (180 / pi)

# Comments on ellipsoid() and swedish_parameters().
# Previous versions were non-vectorized, as
# they internally called switch. Changed from switch
# to dplyr::case_mean, which is really fast in R (in comparison to switch),
# but when included in Shiny app,
# it is the opposite... Explanation: performance of switch and case_mean
# do scale differently.
# case_mean takes almost the same execution time irrespective of sample size
# (and is really useful and fast for data sets with many rows),
# but switch is in fact faster than case_mean for small sample sizes!
# This became evident when using geodetic_to_grid in a Shiny app,
# under such circumstances use the new p argument in grid_to_geodetic
# and geodetic_to_grid which allows the user to pre-define a data frame
# with transformation parameter values in order to speed up the
# transformation calculations.

#' @export
ellipsoid <- function(.x) {
  dplyr::case_match(
    .x,
    "GRS 80" ~ data.frame(a = 6378137, f = 1 / 298.257222101),
    "Bessel 1841" ~ data.frame(a = 6377397.155, f = 1 / 299.1528128))
}

#' @export
swedish_parameters <- function(crs) {

  dplyr::case_match(
    crs,
    # RT90 parameters, "GRS 80" ellipsoid. Använd dessa vid direkt transformering <-> SWEREF99
    3845 ~ data.frame(# rt90_7.5_gon_v
      ellipsoid("GRS 80"),
      lon_of_origin = 11 + 18.375/60, lat_of_origin = 0,
      scale = 1.000006000000, false_northing = -667.282, false_easting = 1500025.141),
    3846 ~ data.frame(# rt90_5.0_gon_v
      ellipsoid("GRS 80"),
      lon_of_origin = 13 + 33.376/60, lat_of_origin = 0,
      scale = 1.000005800000, false_northing = -667.130, false_easting = 1500044.695),
    3847 ~ data.frame(# rt90_2.5_gon_v
      ellipsoid("GRS 80"),
      lon_of_origin = 15 + 48/60 + 22.624306/3600, lat_of_origin = 0,
      scale = 1.00000561024, false_northing = -667.711, false_easting = 1500064.274),
    3848 ~ data.frame(# rt90_0.0_gon_v
      ellipsoid("GRS 80"),
      lon_of_origin = 18 + 3.378/60, lat_of_origin = 0,
      scale = 1.000005400000, false_northing = -668.844, false_easting = 1500083.521),
    3849 ~ data.frame(# rt90_2.5_gon_o
      ellipsoid("GRS 80"),
      lon_of_origin = 20 + 18.379/60, lat_of_origin = 0,
      scale = 1.000005200000, false_northing = -670.706, false_easting = 1500102.765),
    3850 ~ data.frame(# rt90_5.0_gon_o
      ellipsoid("GRS 80"),
      lon_of_origin = 22 + 33.380/60, lat_of_origin = 0,
      scale = 1.000004900000, false_northing = -672.557, false_easting = 1500121.846),
    # RT90 parameters, "Bessel 1841" ellipsoid
    3019 ~ data.frame(# bessel_rt90_7.5_gon_v
      ellipsoid("Bessel 1841"),
      lon_of_origin = 11 + 18/60 + 29.8/3600, lat_of_origin = 0,
      scale = 1, false_northing = 0, false_easting = 1500000),
    3020 ~ data.frame(# bessel_rt90_5.0_gon_v
      ellipsoid("Bessel 1841"),
      lon_of_origin = 13 + 33/60 + 29.8/3600, lat_of_origin = 0,
      scale = 1, false_northing = 0, false_easting = 1500000),
    3021 ~ data.frame(# bessel_rt90_2.5_gon_v
      ellipsoid("Bessel 1841"),
      lon_of_origin = 15 + 48/60 + 29.8/3600, lat_of_origin = 0,
      scale = 1, false_northing = 0, false_easting = 1500000),
    3022 ~ data.frame(# bessel_rt90_0.0_gon_v
      ellipsoid("Bessel 1841"),
      lon_of_origin = 18 + 3/60 + 29.8/3600, lat_of_origin = 0,
      scale = 1, false_northing = 0, false_easting = 1500000),
    3023 ~ data.frame(# bessel_rt90_2.5_gon_o
      ellipsoid("Bessel 1841"),
      lon_of_origin = 20 + 18/60 + 29.8/3600, lat_of_origin = 0,
      scale = 1, false_northing = 0, false_easting = 1500000),
    3024 ~ data.frame(# bessel_rt90_5.0_gon_o
      ellipsoid("Bessel 1841"),
      lon_of_origin = 22 + 33/60 + 29.8/3600, lat_of_origin = 0,
      scale = 1, false_northing = 0, false_easting = 1500000),
    # SWEREF99TM and SWEREF99 ddmm parameters, "GRS 80" ellipsoid
    3006 ~ data.frame(# sweref99_tm
      ellipsoid("GRS 80"),
      lon_of_origin = 15.00, lat_of_origin = 0,
      scale = 0.9996, false_northing = 0, false_easting = 500000),
    3007 ~ data.frame(# sweref99_1200
      ellipsoid("GRS 80"),
      lon_of_origin = 12.00, lat_of_origin = 0,
      scale = 1, false_northing = 0, false_easting = 150000),
    3008 ~ data.frame(# sweref99_1330
      ellipsoid("GRS 80"),
      lon_of_origin = 13.50, lat_of_origin = 0,
      scale = 1, false_northing = 0, false_easting = 150000),
    3012 ~ data.frame(# sweref99_1415
      ellipsoid("GRS 80"),
      lon_of_origin = 14.25, lat_of_origin = 0,
      scale = 1, false_northing = 0, false_easting = 150000),
    3009 ~ data.frame(# sweref99_1500
      ellipsoid("GRS 80"),
      lon_of_origin = 15.00, lat_of_origin = 0,
      scale = 1, false_northing = 0, false_easting = 150000),
    3013 ~ data.frame(# sweref99_1545
      ellipsoid("GRS 80"),
      lon_of_origin = 15.75, lat_of_origin = 0,
      scale = 1, false_northing = 0, false_easting = 150000),
    3010 ~ data.frame(# sweref99_1630
      ellipsoid("GRS 80"),
      lon_of_origin = 16.50, lat_of_origin = 0,
      scale = 1, false_northing = 0, false_easting = 150000),
    3014 ~ data.frame(# sweref99_1715
      ellipsoid("GRS 80"),
      lon_of_origin = 17.25, lat_of_origin = 0,
      scale = 1, false_northing = 0, false_easting = 150000),
    3011 ~ data.frame(# sweref99_1800
      ellipsoid("GRS 80"),
      lon_of_origin = 18.00, lat_of_origin = 0,
      scale = 1, false_northing = 0, false_easting = 150000),
    3015 ~ data.frame(# sweref99_1845
      ellipsoid("GRS 80"),
      lon_of_origin = 18.75, lat_of_origin = 0,
      scale = 1, false_northing = 0, false_easting = 150000),
    3016 ~ data.frame(# sweref99_2015
      ellipsoid("GRS 80"),
      lon_of_origin = 20.25, lat_of_origin = 0,
      scale = 1, false_northing = 0, false_easting = 150000),
    3017 ~ data.frame(# sweref99_2145
      ellipsoid("GRS 80"),
      lon_of_origin = 21.75, lat_of_origin = 0,
      scale = 1, false_northing = 0, false_easting = 150000),
    3018 ~ data.frame(# sweref99_2315
      ellipsoid("GRS 80"),
      lon_of_origin = 23.25, lat_of_origin = 0,
      scale = 1, false_northing = 0, false_easting = 150000)
  )
}

# y = ?, x = ?
#' @export
grid_to_geodetic <- function(
    y, x, crs,
    colnames = c("latitude", "longitude"),
    p = NULL) {

  if (is.null(p)) {
    p <- swedish_parameters(crs)
  }

  e2 <- p$f * (2 - p$f)
  n <- p$f / (2 - p$f)
  a_roof <- p$a / (1 + n) * (1 + n^2/4 + n^4/64)

  delta1 <- n / 2 - 2 * n^2/3 + 37 * n^3/96 - n^4/360
  delta2 <- n^2/48 + n^3/15 - 437 * n^4/1440
  delta3 <- 17 * n^3/480 - 37 * n^4/840
  delta4 <- 4397 * n^4/161280

  Astar <- e2 + e2^2 + e2^3 + e2^4
  Bstar <- -(7 * e2^2 + 17 * e2^3 + 30 * e2^4) / 6
  Cstar <- (224 * e2^3 + 889 * e2^4) / 120
  Dstar <- -(4279 * e2^4) / 1260

  lambda_zero <- deg_to_rad(p$lon_of_origin)
  xi <- (x - p$false_northing) / (p$scale * a_roof)
  eta <- (y - p$false_easting) / (p$scale * a_roof)

  xi_prim <- xi -
    delta1 * sin(2 * xi) * cosh(2 * eta) -
    delta2 * sin(4 * xi) * cosh(4 * eta) -
    delta3 * sin(6 * xi) * cosh(6 * eta) -
    delta4 * sin(8 * xi) * cosh(8 * eta)

  eta_prim <- eta -
    delta1 * cos(2 * xi) * sinh(2 * eta) -
    delta2 * cos(4 * xi) * sinh(4 * eta) -
    delta3 * cos(6 * xi) * sinh(6 * eta) -
    delta4 * cos(8 * xi) * sinh(8 * eta)

  phi_star <- asin(sin(xi_prim) / cosh(eta_prim))
  delta_lambda <- atan(sinh(eta_prim) / cos(xi_prim))

  lon_radian <- lambda_zero + delta_lambda
  lat_radian <- phi_star + sin(phi_star) * cos(phi_star) *
    (Astar +
       Bstar * sin(phi_star)^2 +
       Cstar * sin(phi_star)^4 +
       Dstar * sin(phi_star)^6)

  latitude <- rad_to_deg(lat_radian)
  longitude <- rad_to_deg(lon_radian)

  out <- data.frame(latitude, longitude)
  setNames(out, colnames)

}

# y = ?, x = ?
#' @export
geodetic_to_grid <- function(
    longitude, latitude, crs,
    colnames = c("y", "x"),
    p = NULL) {

  if (is.null(p)) {
    p <- swedish_parameters(crs)
  }

  e2 <- p$f * (2 - p$f)
  n <- p$f / (2 - p$f)
  a_roof <- p$a / (1 + n) * (1 + n^2/4 + n^4/64)

  A <- e2
  B <- (5 * e2^2 - e2^3) / 6
  C <- (104 * e2^3 - 45 * e2^4) / 120
  D <- (1237 * e2^4) / 1260

  beta1 <- n/2 - 2 * n^2/3 + 5 * n^3/16 + 41 * n^4/180
  beta2 <- 13 * n^2/48 - 3 * n^3/5 + 557 * n^4/1440
  beta3 <- 61 * n^3/240 - 103 * n^4/140
  beta4 <- 49561 * n^4/161280

  phi <- deg_to_rad(latitude)
  lambda <- deg_to_rad(longitude)
  lambda_zero <- deg_to_rad(p$lon_of_origin)

  phi_star <- phi - sin(phi) * cos(phi) *
    (A +
       B * sin(phi)^2 +
       C * sin(phi)^4 +
       D * sin(phi)^6)

  delta_lambda <- lambda - lambda_zero
  xi_prim <- atan(tan(phi_star) / cos(delta_lambda))
  eta_prim <- atanh(cos(phi_star) * sin(delta_lambda))

  x <- p$scale * a_roof * (
    xi_prim +
      beta1 * sin(2 * xi_prim) * cosh(2 * eta_prim) +
      beta2 * sin(4 * xi_prim) * cosh(4 * eta_prim) +
      beta3 * sin(6 * xi_prim) * cosh(6 * eta_prim) +
      beta4 * sin(8 * xi_prim) * cosh(8 * eta_prim)) +
    p$false_northing

  y <- p$scale * a_roof * (
    eta_prim +
      beta1 * cos(2 * xi_prim) * sinh(2 * eta_prim) +
      beta2 * cos(4 * xi_prim) * sinh(4 * eta_prim) +
      beta3 * cos(6 * xi_prim) * sinh(6 * eta_prim) +
      beta4 * cos(8 * xi_prim) * sinh(8 * eta_prim)) +
    p$false_easting

  out <- data.frame(x, y)
  setNames(out, colnames)
}

#' @export
detect_crs <- function(north, east) {
  dplyr::case_when(
    between(north, 6100000, 7700000) & between(east, 1200000, 1900000) ~ 3847,
    between(north, 6000000, 7700000) & between(east, 200000, 1000000) ~ 3006,
    between(north, 54, 70) & between(east, 10, 26) ~ 4326)
}

# Wrapper function(s) for direct transformation
#' @export
grid_to_grid <- function(
    data, y, x,
    from_epsg, to_epsg,
    round = FALSE,
    round_grid = 0, round_degrees = 6,
    colnames_new = c("y", "x")) {

  out <- data %>%
    mutate(
      grid_to_geodetic({{y}}, {{x}}, {{from_epsg}}))

  out <- out %>%
    mutate(
      case_when(
        {{from_epsg}} == to_epsg ~ data.frame({{y}}, {{x}}) %>%
          set_names(colnames_new),
        TRUE ~ geodetic_to_grid(longitude, latitude, {{to_epsg}}, colnames_new))
      )

  if (round) {
    out <- out %>%
      mutate(
        across(all_of(colnames_new), ~ round(., round_grid)),
        across(c(latitude, longitude), ~ round(., round_degrees)))
  }
  out
}

# 1) convert everything to geodetic
# 2) convert from geodetic to grid based on vector with crs's.
# append crs to standardized column name
# all datavalues are transformed
# 3) retain original data, i.e. replace transformed data
# with original input data if input crs == output crs
# 4) extras: possible to customize column names

#' @export
swe_transform <- function(
    ost, nord, crs_from, crs_to = c(3006, 3847),
    colnames_geodetic = c("latitude", "longitude"),
    colnames_grid_prefix = c("north", "east"),
    colnames_grid = map(
      crs_to,
      ~ str_c(colnames_grid_prefix, .x, sep = "_")),
    round = FALSE,
    round_grid = 0, round_degrees = 6,
    keep_original = TRUE) {

  d_geodetic <- grid_to_geodetic({{ost}}, {{nord}}, crs_from, colnames_geodetic)

  if (keep_original) {
    d_geodetic[[colnames_geodetic[1]]] <-
      ifelse(is.na(d_geodetic[[colnames_geodetic[1]]]), {{nord}}, d_geodetic[[colnames_geodetic[1]]])

    d_geodetic[[colnames_geodetic[2]]] <-
      ifelse(is.na(d_geodetic[[colnames_geodetic[2]]]), {{ost}}, d_geodetic[[colnames_geodetic[2]]])
  }

  d_grid <- purrr::map2(
    crs_to, colnames_grid,
    ~ geodetic_to_grid(d_geodetic[,2], d_geodetic[,1], .x, .y))

  d_out <- dplyr::bind_cols(d_geodetic, d_grid)

  if (keep_original) {
    if (length(crs_from) == 1) crs_from <- rep(crs_from, nrow(d_out))
    for (i in seq_along(crs_to)) {
      d_out[[colnames_grid[[i]][1]]] <- ifelse(crs_from == crs_to[i], nord, d_out[[colnames_grid[[i]][1]]])
      d_out[[colnames_grid[[i]][2]]] <- ifelse(crs_from == crs_to[i], ost, d_out[[colnames_grid[[i]][2]]])
    }
  }

  if (round) {
    d_out <- d_out %>%
      mutate(
        across(all_of(unlist(colnames_grid)), ~ round(., round_grid)),
        across(all_of(colnames_geodetic), ~ round(., round_degrees)))
  }

  d_out

}
