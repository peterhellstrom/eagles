# Avrinningsområden nedströms/uppströms ----

# Ange ett AROID och lokalisera alla delavrinningsområden.
# Returnerar vektor med AROID.

# direction = "down":
# nedströms till mynning i havet.
# Endast huvudflödet returneras, inga biflöden etc.

# direction = "up":
# Returnerar hela den del av huvudavrinningsområdet som ligger ovanför
# angivet AROID.

#' @export
daro_flode <- function(aroid, direction = c("down", "up"), data = daro) {

  direction <- match.arg(direction)

  out <- list()
  i <- 1
  out[[i]] <- aroid

  if (direction == "down") {
    while (!rlang::is_empty(aroid)) {
      aroid <- data[data$"AROID" == aroid,]$OMRID_NED
      i <- i + 1
      out[[i]] <- aroid }
    # Remove final AROID, since this is (always) in the sea
    inds <- head(unlist(out), -1)
    daro_flode_sf(data, inds, add_flow_id = TRUE)
    #data[data$"AROID" %in% inds,]
  }

  else if (direction == "up") {
    while (!rlang::is_empty(aroid)) {
      aroid <- data[data$"OMRID_NED" %in% aroid,]$AROID
      i <- i + 1
      out[[i]] <- aroid }

    inds <- unlist(out)
    data[data$"AROID" %in% inds,]
  }
}

# Extrahera polygoner från delavrinningsområden i flödesordning ----
# OBS! Flödesordningen egentligen avsedd att använda nedströms,
# nu blir den ju lite "missvisande" vid sökning uppströms.

#' @export
daro_flode_sf <- function(data, index, add_flow_id = FALSE) {

  out <- data |>
    dplyr::slice(
      base::match(
        index,
        data |> dplyr::pull(AROID)
      )
    )

  if (add_flow_id) {
    out <- out |>
      dplyr::mutate(
        FLODEID = dplyr::row_number()
      ) |>
      dplyr::relocate(FLODEID)
  }
  out
}

# Gruppera avrinningsområden, baserat på attribut ----
# (attribut i standardiserat format, se exempel nedan)
# OBS! ger INTE överlappande avrinningsområden
# (jmf med skiktet SVARO i SMHI i Svenskt Vattenarkiv), eftersom de
# överlappande polygonerna är tillgängliga i just SVARO.
#' @export
aroid_group <- function(.x, .field) {

  x <- map_dfr(
    .x$aroid |> rlang::set_names(),
    \(x) daro_flode(x, direction = "up"),
    .id = "aroid_grp"
  )

  x <- x |>
    inner_join(.x, join_by(aroid_grp == aroid))

  x_s <- x |>
    dplyr::group_by(AROID) |>
    dplyr::arrange(AROID) |>
    # varje ID får endast förekomma en gång,
    # ==> polygoner ska inte överlappa.
    # jmf mot SVARO som har överlappande polygoner
    # vilken polygon som har överordnad prioritet anges av fältet ordn
    # (hur fältet ordn ska definieras bör beskrivas...)
    dplyr::slice_min(ordn, n = 1) |>
    dplyr::ungroup()

  # group_by renders error message in ms_dissolve
  # dplyr::group_by(test_name)
  x_s
}

## Slå ihop delavriningsområden baserat på fältet namn i attributtabell ----
# input: output from aroid_group
# dissolve & join-fält är hårdkodat här!
# Var tidigare namn, men det funkar ju inte om två olika områden
# har samma namn, ändrade till grans_aroid
#' @export
aroid_group_dissolve <- function(.x, .x_attr, .field = aroid_grp, .attr_field = aroid) {
  .x |>
    dplyr::group_by( {{ .field }}) |>
    dplyr::summarize() |>
    dplyr::inner_join(
       .x_attr, join_by( {{ .field }} == {{ .attr_field }})
    ) |>
    # dplyr::arrange(grupp, ordn) |>
    { \(.) dplyr::mutate(., area = sf::st_area(.)) }() |>
    dplyr::mutate(area = units::set_units(area, km^2))
}

## Havsområden ----

### Kombinera havsområden med delavriningsområden ----

# Blir ofta små "slivers" mellan polygoner för olika data-set
# som måste bort, ett alternativ är att använda snap-funktioner

# st_snap tar extremt lång tid för hela data-setet ?!?!?
# ==> kör på mindre ytor och sammanfoga

# Testa med st_snap_to_grid, ett snabbare alternativ, men hur
# blir resultatet?

# nngeo::remove_holes-funktionen lyckas inte ta bort hål/slivers
# som tangerar kanterna på polygonerna. Samma gäller för sfheaders::sf_remove_holes
# som annars är en snabbare funktion.
#' @export
havso_combine_by_id <- function(
    havso, daro, hid,
    union = TRUE,
    union_method = c("union", "dissolve")) {

  union_method <- match.arg(union_method)

  havso_sel <- havso |>
    dplyr::filter(HID == hid)

  daro_sel <- daro |>
    dplyr::filter(OMRID_NED == hid) |>
    dplyr::rename(c("HID" = "OMRID_NED"))

  havso_daro_sel <- dplyr::bind_rows(havso_sel, daro_sel) |>
    tidyr::fill(TYP_NFS06, TYPOMRKUST)

  if (union) {
    if (union_method == "union") {
      havso_daro_sel <- havso_daro_sel |>
        sf::st_snap(x = ., y = ., tolerance = 0.0001) |>
        sf::st_union() |>
        sf::st_sf() |>
        dplyr::mutate(HID = hid)

    } else if (union_method == "dissolve") {
      havso_daro_sel <- havso_daro_sel |>
        sf::st_snap(x = ., y = ., tolerance = 0.0001) |>
        rmapshaper::ms_dissolve(field = "HID") |>
        dplyr::mutate(HID = hid)
    }
  }

  havso_daro_sel

}
