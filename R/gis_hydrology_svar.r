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
daro_flode <- function(aroid, direction = c("down", "up")) {

  direction <- match.arg(direction)

  out <- list()
  i <- 1
  out[[i]] <- aroid

  if (direction == "down") {
    while (!rlang::is_empty(aroid)) {
      aroid <- daro_d[daro_d$"AROID" == aroid,]$OMRID_NED
      i <- i + 1
      out[[i]] <- aroid }
    # Remove final AROID, since this is (always) in the Sea
    head(unlist(out), -1) }

  else if (direction == "up") {
    while (!rlang::is_empty(aroid)) {
      aroid <- daro_d[daro_d$"OMRID_NED" %in% aroid,]$AROID
      i <- i + 1
      out[[i]] <- aroid }

    unlist(out) }
}

# Extrahera polygoner från delavrinningsområden i flödesordning ----
# OBS! Flödesordningen egentligen avsedd att använda nedströms,
# nu blir den ju lite "missvisande" vid sökning uppströms.

#' @export
daro_flode_sf <- function(.x) {
  daro %>%
    slice(match(.x, daro %>% pull(AROID))) %>%
    mutate(FLODEID = row_number()) %>%
    relocate(FLODEID)
}

# Gruppera avrinningsområden, baserat på attribut ----
# (attribut i standardiserat format, se exempel nedan)
# OBS! ger INTE överlappande avrinningsområden
# (jmf med skiktet SVARO i SMHI i Svenskt Vattenarkiv), eftersom de
# överlappande polygonerna är tillgängliga i just SVARO.
#' @export
aroid_group <- function(.x) {

  x <- lapply(.x$grans_aroid, daro_flode, direction = "up")
  names(x) <- .x$grans_aroid

  x <- enframe(x, name = "grans_aroid", value = "AROID") %>%
    unnest(cols = AROID) %>%
    inner_join(.x, by = "grans_aroid")

  x_s <- x %>%
    group_by(AROID) %>%
    arrange(AROID) %>%
    # varje ID får endast förekomma en gång,
    # ==> polygoner ska inte överlappa.
    # jmf mot SVARO som har överlappande polygoner
    # vilken polygon som har överordnad prioritet anges av fältet ordn
    # (hur fältet ordn ska definieras bör beskrivas...)
    slice_min(ordn, n = 1) %>%
    ungroup()

  x_daro <- daro %>%
    inner_join(x_s, by = "AROID")
  #group_by(test_name) # group_by renders error message in ms_dissolve
  x_daro
}

## Slå ihop delavriningsområden baserat på fältet namn i attributtabell ----
# input: output from aroid_group
# dissolve & join-fält är hårdkodat här!
# Var tidigare namn, men det funkar ju inte om två olika områden
# har samma namn, ändrade till grans_aroid
#' @export
aroid_group_dissolve <- function(.x, .x_attr, .field = "grans_aroid") {
  x_daro_dslv <- .x %>%
    rmapshaper::ms_dissolve(field = .field) %>%
    inner_join(.x_attr, by = .field) %>%
    arrange(grupp, ordn) %>%
    mutate(area = st_area(.)) %>%
    mutate(area = set_units(area, km^2))
  x_daro_dslv
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
havso_combine_by_id <- function(havso, daro, hid,
                                union = TRUE,
                                union_method = c("union", "dissolve")) {

  union_method <- match.arg(union_method)

  havso_sel <- havso %>%
    filter(HID == hid)

  daro_sel <- daro %>%
    filter(OMRID_NED == hid) %>%
    rename(c("HID" = "OMRID_NED"))

  havso_daro_sel <- bind_rows(havso_sel, daro_sel) %>%
    fill(TYP_NFS06, TYPOMRKUST)

  if (union) {
    if (union_method == "union") {
      havso_daro_sel <- havso_daro_sel %>%
        st_snap(x = ., y = ., tolerance = 0.0001) %>%
        st_union() %>%
        st_sf() %>%
        mutate(HID = hid)

    } else if (union_method == "dissolve") {
      havso_daro_sel <- havso_daro_sel %>%
        st_snap(x = ., y = ., tolerance = 0.0001) %>%
        rmapshaper::ms_dissolve(field = "HID") %>%
        mutate(HID = hid)
    }
  }

  havso_daro_sel

}
