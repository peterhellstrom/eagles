# To do:
# Kontrollera max-zoom gränser!
# tile sizes?
# worldcopyJump? Continuous world?
# maxZoom för 3006 = 18, maxZoom för 3857 = 23. Specificeras via resolutions i leafletCRS!
# hur hittas tillgängliga zoom-nivåer?
# layers control, går det att skapa underrubriker och undergrupper?
# Använda "egna" lager inte bara i leaflet, utan går det att göra även i mapview?
# tillämpning i shiny?
# Check: do lm_basemaps work also for CRS:3857 and not only CRS:3006?

#' @export
leaflet_crs_3006 <- function() {
  leaflet::leafletCRS(
    crsClass = "L.Proj.CRS",
    code = "EPSG:3006",
    proj4def = "+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
    #resolutions = 2^(12:-1), # 4096 down to 0.5
    resolutions = c(
      4096, 2048, 1024, 512, 256, 128, 64, 32, 16, 8, 4, 2, 1, 0.5,
      0.25, 0.15, 0.1, 0.05, 0.01
    ),
    origin = c(-1200000, 8500000)
  )
}

#' @export
wms_add <- function(
    map,
    group,
    url,
    layers,
    format = "image/png",
    transparent = TRUE,
    opacity = 1,
    styles = "",
    ...) {

  leaflet::addWMSTiles(
    map = map,
    group = group,
    baseUrl = url,
    layers = layers,
    options = leaflet::WMSTileOptions(
      format = format,
      transparent = transparent,
      opacity = opacity,
      styles = styles,
      ...)
  )
}

#' @export
wms_sources <- function(
    path = "inst/extdata/wms_sources.xlsx",
    layers_sheet = "wms_layers",
    url_sheet = "wms_url",
    sep = ", ") {

  wms_layers <- readxl::read_excel(path = path, sheet = layers_sheet)
  wms_url <- readxl::read_excel(path = path, sheet = url_sheet)

  wms_layers <- wms_layers |>
    dplyr::left_join(wms_url, dplyr::join_by(url_id)) |>
    dplyr::relocate(url, .before = url_id) |>
    tidyr::replace_na(list(styles = "")) |>
    dplyr::mutate(
      dplyr::across(c(layers, styles), \(x) stringr::str_split(x, sep))
    )

  wms_layers |>
    dplyr::filter(!url_id %in% c("allmanna_kartor", "lm_geodata_intern"))
}

#' @export
lm_basemaps <- function(
    .data = NULL,
    crs = leaflet_crs_3006(),
    wms_layers = wms_layers_data,
    setView_lng = 16.15043,
    setView_lat = 59.60423,
    setView_zoom = 2,
    addOverlayGroups = NULL,
    collapseLayersControl = FALSE,
    add_extras = TRUE) {

  # Create empty leaflet map
  m <- leaflet::leaflet(
    data = .data,
    options = leaflet::leafletOptions(
      worldCopyJump = FALSE,
      crs = crs
    )
  ) |>
    leaflet::setView(
      lng = setView_lng,
      lat = setView_lat,
      zoom = setView_zoom
    )

  # Add layers
  for (i in seq_len(nrow(wms_layers))) {
    m <- with(wms_layers, {
      m |>
        wms_add(
          group = group[[i]],
          url = url[[i]],
          layers = layers[[i]],
          styles = styles[[i]],
          opacity = opacity[[i]]
        )
    }
    )
  }

  # Create layers control
  m <- m |>
    leaflet::addLayersControl(
      position = 'bottomright',
      baseGroups =
        wms_layers |>
        dplyr::filter(type == "baseGroups") |>
        dplyr::pull(group) |>
        base::unique(),
      overlayGroups = c(
        wms_layers |>
          dplyr::filter(type == "overlayGroups") |>
          dplyr::pull(group) |>
          base::unique(),
        addOverlayGroups),
      options = leaflet::layersControlOptions(
        collapsed = collapseLayersControl)
    ) |>
    leaflet::hideGroup(
      wms_layers |>
        dplyr::filter(type == "overlayGroups") |>
        dplyr::pull(group) |>
        base::unique()
    )

  if (add_extras) {
    m <- m |>
      lm_basemaps_add_extras()
  }

  m
}

#' @export
lm_basemaps_old <- function(
    .data = NULL,
    crs = leaflet_crs_3006(),
    topowebb_url = "https://minkarta.lantmateriet.se/map/topowebb/?",
    ortofoto_url = "https://minkarta.lantmateriet.se/map/ortofoto/?",
    hojdmodell_url = "https://minkarta.lantmateriet.se/map/hojdmodell/?",
    fastigheter_url = "https://minkarta.lantmateriet.se/map/fastighetsindelning/?",
    historiska_ortofoton_url = "https://api.lantmateriet.se/historiska-ortofoton/wms/v1/token/a2c113be5a3969e03dbd50222d3e2550/?",
    marktacke_url = "https://nvpub.vic-metria.nu/arcgis/services/NMD2018/MapServer/WMSServer",
    setView_lng = 16.15043,
    setView_lat = 59.60423,
    setView_zoom = 2,
    addOverlayGroups = NULL,
    CollapseLayerscontrol = FALSE) {

  m <- leaflet::leaflet(
    data = .data,
    options = leaflet::leafletOptions(
      worldCopyJump = FALSE, crs = crs)
  ) |>
    leaflet::setView(
      lng = setView_lng,
      lat = setView_lat,
      zoom = setView_zoom
    )

  # Add base groups
  m <- m |>
    wms_add(
      group = "LM topowebb",
      url = topowebb_url,
      layers = "topowebbkartan") |>
    wms_add(
      group = "LM topowebb nedtonad",
      url = topowebb_url,
      layers = "topowebbkartan_nedtonad") |>
    wms_add(
      group = "LM topowebb + terrängskuggning",
      url = hojdmodell_url,
      layers = "terrangskuggning") |>
    wms_add(
      group = "LM topowebb + terrängskuggning",
      url = topowebb_url,
      layers = "topowebbkartan", opacity = "0.75") |>
    wms_add(
      group = "LM topowebb med gränser",
      url = topowebb_url,
      layers = "topowebbkartan") |>
    wms_add(
      group = "LM topowebb med gränser",
      url = fastigheter_url,
      layers = c("granser", "text"),
      styles = c("ljusbakgrund", "ljusbakgrund")) |>
    wms_add(
      group = "LM ortofoto 0.16",
      url = ortofoto_url,
      layers = "Ortofoto_0.16") |>
    wms_add(
      group = "LM ortofoto",
      url = ortofoto_url,
      layers = c("Ortofoto_0.5", "Ortofoto_0.4", "Ortofoto_0.25", "Ortofoto_0.16")) |>
    wms_add(
      group = "LM ortofoto + terrängskuggning",
      url = hojdmodell_url,
      layers = "terrangskuggning") |>
    wms_add(
      group = "LM ortofoto + terrängskuggning",
      url = ortofoto_url,
      layers = c("Ortofoto_0.5", "Ortofoto_0.4", "Ortofoto_0.25", "Ortofoto_0.16"),
      opacity = 0.75) |>
    wms_add(
      group = "LM ortofoto med gränser",
      url = ortofoto_url,
      layers = c("Ortofoto_0.5", "Ortofoto_0.4", "Ortofoto_0.25", "Ortofoto_0.16")) |>
    wms_add(
      group = "LM ortofoto med gränser",
      url = fastigheter_url,
      layers = c("granser", "text"),
      styles = c("morkbakgrund", "morkbakgrund")) |>
    wms_add(
      group = "LM ortofoto IR 0.5",
      url = ortofoto_url,
      layers = "Ortofoto_IR") |>
    wms_add(
      group = "LM terrängskuggning",
      url = hojdmodell_url,
      layers = "terrangskuggning") |>
    wms_add(
      group = "LM terränglutning",
      url = hojdmodell_url,
      layers = "terranglutning") |>
    wms_add(
      group = "LM Historiska ortofoton 1960",
      url = historiska_ortofoton_url,
      layers = "OI.Histortho_60") |>
    wms_add(
      group = "LM Historiska ortofoton 1975",
      url = historiska_ortofoton_url,
      layers = "OI.Histortho_75") |>
    wms_add(
      group = "NV Nationella marktäckedata",
      url = marktacke_url,
      layers = "0")

  # Add overlay groups
  m <- m |>
    wms_add(
      group = "Fastighetsgränser (röda)",
      url = fastigheter_url,
      layers = c("granser", "text"),
      styles = c("ljusbakgrund", "ljusbakgrund")) |>
    wms_add(
      group = "Fastighetsgränser (gula)",
      url = fastigheter_url,
      layers = c("granser", "text"),
      styles = c("morkbakgrund", "morkbakgrund")) |>
    wms_add(
      group = "Fastighetsgränser (registerkarta)",
      url = fastigheter_url,
      layers = c("granser", "text"),
      styles = c("registerkarta", "registerkarta"))

  # Add layers control
  m <- leaflet::m |>
    addLayersControl(
      position = 'bottomright',
      baseGroups =
        c("LM topowebb", "LM topowebb nedtonad",
          "LM topowebb + terrängskuggning",
          "LM topowebb med gränser",
          "LM ortofoto 0.16", "LM ortofoto",
          "LM ortofoto + terrängskuggning",
          "LM ortofoto med gränser", "LM ortofoto IR 0.5",
          "LM terrängskuggning", "LM terränglutning",
          "LM Historiska ortofoton 1960",
          "LM Historiska ortofoton 1975",
          "NV Nationella marktäckedata"),
      overlayGroups = c(
        "Fastighetsgränser (röda)",
        "Fastighetsgränser (gula)",
        "Fastighetsgränser (registerkarta)",
        addOverlayGroups),
      options = leaflet::layersControlOptions(
        collapsed = CollapseLayerscontrol)) |>
    leaflet::hideGroup(c(
      "Fastighetsgränser (röda)",
      "Fastighetsgränser (gula)",
      "Fastighetsgränser (registerkarta)")
    )
  m
}

#' @export
lm_basemaps_add_extras <- function(map) {
  map |>
    leafem::addMouseCoordinates(
      epsg = 3006,
      proj4string = "+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
      native.crs = TRUE
    ) |>
    #mapview::addMouseCoordinates(style = "detailed") |>
    leaflet.extras::addControlGPS(
      options = leaflet.extras::gpsOptions(
        activate = FALSE,
        autoCenter = TRUE,
        maxZoom = 12)
    ) |>
    leaflet::addScaleBar(
      position = "bottomleft",
      options = scaleBarOptions(metric = TRUE, imperial = FALSE)
    ) |>
    leaflet::addMeasure(
      position = "topleft",
      primaryLengthUnit = "meters",
      secondaryLengthUnit = "kilometers",
      primaryAreaUnit = "sqmeters",
      secondaryAreaUnit = "hectares"
    ) |>
    leaflet.extras::addFullscreenControl() |>
    leaflet::addMiniMap(
      tiles = "https://karta.raa.se/lmtopowebb/1.0.0/topowebb_nedtonad/default/3006/{z}/{y}/{x}.png",
      toggleDisplay = TRUE,
      minimized = TRUE
    ) |>
    leaflet::addEasyButton(
      leaflet::easyButton(
        icon = "fa-crosshairs",
        title = "Locate Me",
        onClick = leaflet::JS("function(btn, map){ map.locate({setView: true}); }")
      )
    )
}

#' @export
remove_na <- function(x) {
  x[!is.na(x)]
}

#' @export
options_sublist <- function(x, y = c("service", "group", "urlTemplate")) {
  c(x[names(x) %in% y], options = list(x[!names(x) %in% y]))
}

#' @export
tms_sources <- function(
    path = "inst/extdata/wms_sources.xlsx",
    layers_sheet = "tms") {

  .data <- readxl::read_excel(
    path = path, sheet = layers_sheet
  )

  .data |>
    purrr::transpose() |>
    purrr::map(remove_na) |>
    purrr::map(options_sublist)

}

#' @export
swe_tiles <- function(
    tile_providers = tms_layers_data,
    lng = 15,
    lat = 62,
    zoom = 4) {

  m <- leaflet::leaflet() |>
    leaflet::setView(
      lng = lng, lat = lat, zoom = zoom
    )

  for (i in seq_along(tile_providers)) {
    m <- m |>
      leaflet::addTiles(
        urlTemplate = tile_providers[[i]]$urlTemplate,
        options = base::do.call("tileOptions", tile_providers[[i]]$options),
        group = tile_providers[[i]]$group
      )
  }

  m <- m |>
    leaflet::addLayersControl(
      position = 'bottomright',
      baseGroups = base::sapply(tile_providers, "[[", "group"),
      #baseGroups = purrr::map_chr(tile_providers, ~.x$group)
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) |>
    leafem::addMouseCoordinates()

  m
}

#' @export
simpleLeaflet <- function(long, lat, popup, fitbounds = TRUE, zoom = 13) {

  d <- data.frame(
    long, lat, name = popup) |>
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326)

  d_coords <- sf::st_coordinates(d)

  m <- swe_tiles(tile_providers = tms_layers_data) |>
    leaflet::setView(mean(d_coords[, 1]), mean(d_coords[, 2]), zoom) |>
    leaflet::addMarkers(long, lat, popup = popup)
    if (fitbounds) {
      m <- m |>
        leaflet::fitBounds(
          lng1 = min(d_coords[, 1]),
          lat1 = min(d_coords[, 2]),
          lng2 = max(d_coords[, 1]),
          lat2 = max(d_coords[, 2])
        )
    }
    m
  }
