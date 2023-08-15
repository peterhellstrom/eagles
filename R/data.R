#' Swedish Land Survey Data
#'
#' A subset of data from the Swedish Land Survey Data
#' Report ...
#'
#' @format ## `storrutor`
#' A data frame with 252 rows and 5 columns, 50 x 50 km grid cells covering Sweden in RT90 2.5 gon V (EPSG:3021):
#' \describe{
#'   \item{ruta_id}{Grid cell, first two numeric characters gives Northing position, last alphabetic character gives Easting Position}
#'   \item{ruta}{Alternative representation of ruta_id, removes leading zero where present.}
#'   \item{namn}{Geographic name of grid cell}
#'   \item{northing, easting}{Geographic coordinates in EPSG:3021 of lower left corner of grid cell}
#'   ...
#' }
#' @source ## Grid cells name were extracted from the Swedish Land Survey Data software Kartex, discontinued in 2015.
"storrutor"
