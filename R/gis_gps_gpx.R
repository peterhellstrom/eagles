# Extensions ----
#' @export
gpx_categ_n <- function(.x, extension = "gpxx_WaypointExtension", index) {
  word_list = str_split(.x[[extension]], '\\s+')
  gsub(x = sapply(word_list, "[", index), pattern = "</gpxx:Category>", replacement = "")
}

#' @export
gpx_categ <- function(.x) {
  .x %>%
    mutate(
      Category =
        unglue_vec(
          gpxx_WaypointExtension,
          "{}<gpxx:Category>{x}</gpxx:Category>{}"),
      CreationTime =
        unglue_vec(
          ctx_CreationTimeExtension,
          "{}<ctx:CreationTime>{x}</ctx:CreationTime>{}")) %>%
    select(-gpxx_WaypointExtension, -wptx1_WaypointExtension, -ctx_CreationTimeExtension) %>%
    select(Category, name, ele, time, sym, cmt, desc, CreationTime)
}

# custom gpx-creation functions

## Creates a track point list ----
#' @export
gpx_trkpt <- function(lat, lon, ele = NULL, time = NULL){
  trkpt <- str_c("<trkpt lat=", double_quote(lat), " lon=", double_quote(lon), ">")
  if (!is.null(ele) && !is.na(ele)) trkpt <- c(trkpt, str_c("<ele>", ele, "</ele>"))
  ## check time is a in character with format %Y-%m-%dT%H:%M:%sZ (UTC time zone)
  if (!is.null(time) && !is.na(time)) trkpt <- c(trkpt, str_c("<time>", time, "</time>"))
  trkpt <- c(trkpt, "</trkpt>")
  return(trkpt)
}

## creates track ----
#' @export
gpx_trk <- function(df, name = NULL) {
  trk <- "<trk>"
  if (!is.null(name)) trk <- c(trk, str_c("<name>", name, "</name>"))
  trk <- c(trk, "<trkseg>")
  list_resu <- pmap(df, gpx_trkpt) %>% unlist()
  trk <- c(trk, list_resu, "</trkseg>", "</trk>")
  return(trk)
}

## creates the start of gpx file ----
#' @export
gpx_header <- function(
    creator = "R - pep"){

  header <- c(
    "<?xml version='1.0' encoding='UTF-8' ?>",
    str_c("<gpx version=", double_quote("1.1"), " creator=", double_quote(creator)),
    "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"",
    "xmlns=\"http://www.topografix.com/GPX/1/1\"",
    "xsi:schemaLocation=\"http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd\">"
  )

  return(header)
}

## creates the end of a gpx file ----
#' @export
gpx_end <- function(){
  return("</gpx>")
}

# xml2 ----
#' @export
add_gpx <-  function(
    creator = "R - pep") {

  xml_new_root(
    "gpx",
    version = "1.1",
    creator = creator,
    "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
    "xmlns" = "http://www.topografix.com/GPX/1/1",
    "xsi:schemaLocation" = "http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd",
    "xmlns:gpxtpx" = "http://www.garmin.com/xmlschemas/TrackPointExtension/v1",
    "xmlns:gpxx" = "http://www.garmin.com/xmlschemas/GpxExtensions/v3",
    "xmlns:trp" = "http://www.garmin.com/xmlschemas/TripExtensions/v1")
}

#' @export
add_trk <- function(
    parent,
    df,
    name = NULL,
    desc = NULL,
    display_color = "Red") {

  # Add main track node
  trk_node <- xml_add_child(parent, "trk")
  # Add name and description
  if (!is.null(name)) xml_add_child(trk_node, "name", name)
  if (!is.null(desc)) xml_add_child(trk_node, "desc", desc)

  # Add extensions (development section)
  ext_node <- xml_add_child(trk_node, "extensions")
  ext_node_trkext <- xml_add_child(ext_node, "gpxx:TrackExtension")
  xml_add_child(ext_node_trkext, "gpxx:DisplayColor", display_color)

  # Track segment
  trkseg_node <- xml_add_child(trk_node, "trkseg")

  # Track points
  purrr::walk2(
    .x = df$lat,
    .y = df$lon,
    ~ xml_add_child(trkseg_node, .value = "trkpt", lat = .x, lon = .y))

  # for (k in 1:n_points) {
  #   xml_add_child(trkseg_node, "trkpt")
  # }

  trkpt_nodes <- xml_find_all(trkseg_node, ".//trkpt")

  # create attributes lon, lat
  # xml_set_attr(trkpt_nodes, "lat", df$lat)
  # xml_set_attr(trkpt_nodes, "lon", df$lon)

  # create nodes ele and time if they exist
  # but also necessary to check that all values are NOT NA or NULL
  if ("ele" %in% names(df)) xml_add_child(trkpt_nodes, "ele", df$ele)
  if ("time" %in% names(df)) xml_add_child(trkpt_nodes, "time", df$time)

  return(invisible(parent))
}

#' @export
add_rte <- function(
    parent,
    df,
    name = NULL,
    desc = NULL,
    display_color = "Red") {

  # Add main track node
  rte_node <- xml_add_child(parent, "rte")
  # Add name and description
  if (!is.null(name)) xml_add_child(rte_node, "name", name)
  if (!is.null(desc)) xml_add_child(rte_node, "desc", desc)

  # Add extensions
  ext_node <- xml_add_child(rte_node, "extensions")
  ext_node_rteext <- xml_add_child(ext_node, "gpxx:RouteExtension")
  xml_add_child(ext_node_rteext, "gpxx:IsAutoNamed", "false")
  xml_add_child(ext_node_rteext, "gpxx:DisplayColor", display_color)

  # Track segment
  #trkseg_node <- xml_add_child(trk_node, "trkseg")

  # Route points
  purrr::walk2(
    .x = df$lat,
    .y = df$lon,
    ~ xml_add_child(rte_node, .value = "rtept", lat = .x, lon = .y))

  rtept_nodes <- xml_find_all(rte_node, ".//rtept")

  # create nodes ele and time if they exist
  # but also necessary to check that all values are NOT NA or NULL
  if ("ele" %in% names(df)) xml_add_child(rtept_nodes, "ele", df$ele)
  if ("time" %in% names(df)) xml_add_child(rtept_nodes, "time", df$time)
  if ("name" %in% names(df)) xml_add_child(rtept_nodes, "name", df$name)
  if ("sym" %in% names(df)) xml_add_child(rtept_nodes, "sym", df$sym)

  #
  xml_add_child(rtept_nodes, "extensions")

  rtept_ext_nodes <- xml_find_all(rtept_nodes, ".//extensions")
  xml_add_child(rtept_ext_nodes, "trp:ViaPoint")
  xml_add_child(rtept_ext_nodes, "gpxx:RoutePointExtension")

  xml_add_child(xml_find_all(rtept_ext_nodes, ".//trp:ViaPoint"),
                "trp:CalculationMode", "Direct")
  xml_add_child(xml_find_all(rtept_ext_nodes, ".//gpxx:RoutePointExtension"),
                "gpxx:Subclass", "000000000000FFFFFFFFFFFFFFFFFFFFFFFF")

  return(invisible(parent))
}
