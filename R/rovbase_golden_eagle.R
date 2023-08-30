# kungsorn_cols_swe <- function(.x, .y, .value) .y[match(.value, .x)] # general version
# kungsorn_cols_swe(.value = "Territorium", .y = kolumner$field_name_sv, .x = kolumner$field_name_no)
# hard-coded version
# Men vad händer om det potentiellt finns flera träffar???? Bör testas...
#' @export
kungsorn_cols_swe <- function(.x) {
  rovbase_reports_columns$field_name_sv[match(.x, rovbase_reports_columns$field_name_no)]
}

#' @export
kungsorn_load_report <- function(.x) {
    readxl::read_excel(.x) |>
    dplyr::rename_with(kungsorn_cols_swe)
}

# x <- map(fls_import, read_excel)
# Läs in till lista
# x <- map(fls_import, kungsorn_load_report)
# names(x) <- str_replace(basename(fls_import), ".xlsx", "")

# Skapa "enskilda objekt" från listan x
# observera att listan x finns kvar, förbrukar alltså dubbelt med minne
# går det att köra assign direkt?
# for (i in seq_along(x)) assign(names(x)[[i]], x[[i]])

#' @export
kungsorn_load_report2 <- function(.x, .file_extension = ".xlsx$") {
  .data <- .x |>
    readxl::read_excel() |>
    dplyr::rename_with(kungsorn_cols_swe)

  .new_name <- stringr::str_replace(basename(.x), .file_extension, "")
  base::assign(.new_name, .data, envir = parent.frame())
}

# for (i in seq_along(fls_import)) kungsorn_load_report2(fls_import[[i]])

# input .x is a tibble created by the function find_rovbase_reports
# det är alltså originalfilerna hämtade från Rovbase som läses in här,
# inte de omdöpta som också kan skapas av find_rovbase_reports
# dock är det enkelt att ändra funktionen om önskvärt.

#' @export
kungsorn_bind_reports <- function(
    .x,
    .value,
    .sub_dir = "data_rovbase") {

  .x |>
    dplyr::filter(stringr::str_detect(report_short_name, .value)) |>
    dplyr::mutate(file_path = file.path(.sub_dir, file)) |>
    dplyr::pull(file_path) |>
    purrr::map(kungsorn_load_report) |>
    dplyr::bind_rows()
}

# Alternativ metod
# https://stackoverflow.com/questions/50138295/replace-column-values-with-column-name-using-dplyrs-transmute-all
# map2_df(boplatser_habitat, names(boplatser_habitat), ~  replace(.x, .x == "Ja", .y))

# Sammanställningar ----
## Komplettera län ----
# Requires input data in environment, lan_s_n
#' @export
complete_lan <- function(.x, ...) {
  .x |>
    dplyr::relocate(lanbok) |>
    dplyr::mutate(
      lanbok = base::factor(lanbok, levels = lan_s_n |> dplyr::pull(lanbok))
    ) |>
    tidyr::complete(lanbok, ...) |>
    dplyr::right_join(
      lan_s_n |>
        dplyr::select(-c(X, Y, lannamn)),
      dplyr::join_by(lanbok)
    ) |>
    dplyr::relocate(land, forvaltningsomrade, lankod, lanbok)
}

## Revir ----
#' @export
sum_revir <- function(.x = boplats_join, .y = revir_join, survey_year = NULL) {

  # .x = boplats
  # .y = revir
  if (is.null(survey_year)) survey_year <- max({{.x}}$`År, ny boplats`, na.rm = TRUE)

  b <- {{.x}} |>
    # Koppla startår för reviret till boplats-data
    dplyr::left_join({{.y}} |>
                dplyr::select(LokalID, Startår_revir = Startår),
              dplyr::join_by(LokalID)) |>
    # Inkludera enbart revir som helt säkert inte var kända vid angivet år
    dplyr::filter(Startår_revir <= survey_year | is.na(Startår_revir))

  # Hantera boplatser som registrerats efter angivet år, men ingen info finns om
  # startår. `År registrerat` är inte säkert, kan ju handla om ett revir/bo som
  # lagts till ur ett "historiskt" perspektiv
  # Revir där samtliga bon har registrerat EFTER angivet inventeringsår.
  # Vi vet ju inte om reviret var känt
  # vid angivet inventeringsår, även om det inte fanns ngn angiven boplats
  # detta år! Startår MÅSTE fyllas i på revirnivå för att reda ut detta problem!
  # Ta bort eller låt dessa vara kvar?
  b <- b |>
    dplyr::filter(is.na(`År, ny boplats`) | `År, ny boplats` <= survey_year)

  #
  b <- b |>
    dplyr::bind_rows(
      revir_join |>
        dplyr::anti_join(boplats_join, dplyr::join_by(LokalID)) |>
        dplyr::filter(is.na(Startår) | Startår <= survey_year) |>
        dplyr::select(LokalID))
  b
}

# z <- sum_revir(survey_year = 2023) |>
#   add_wt(LokalID, lankod, lanbok)
#
# z |>
#   dplyr::count(lankod, lanbok, wt = wt) |>
#   janitor::adorn_totals()

# Hantera revir utan geografisk info

## Boplatser ----

### Boplatser per revir ----
# Okända bon: Leta efter termen 'okän' i Boplats- och Kommentarsfälten
# Notera att man också kan använda funktionen regex för att leta "case-insensitive"
# boplats |>
#  dplyr::filter_at(vars(Boplats, Kommentar),
#            dplyr::any_vars(stringr::str_detect(., regex("okän*", ignore_case = TRUE))))

# OBS! Kolla även i kommentarsfältet efter okända bon, det finns några där som
# inte är angivna i fältet "Boplats"!
# OBS! Här summeras alla boplatser, oavsett fynd- eller registreringsår!

#' @export
boplatser_per_revir <- function(.x = boplats, ..., survey_year = NULL) {

  boplats <- .x %>%
    group_by(LokalID, ...)

  if (is.null(survey_year)) survey_year <- max(.x$`År, ny boplats`, na.rm = TRUE)

  boplats %>%
    summarize(
      n_bon = sum(
        is.na(`År, ny boplats`) | `År, ny boplats` <= survey_year),
      n_okända_bon = sum(
        str_detect(Boplats, "okän|Okän") & `År, ny boplats` <= survey_year |
          str_detect(Boplats, "okän|Okän") & is.na(`År, ny boplats`), na.rm = TRUE),
      n_kända_bon = n_bon - n_okända_bon,
      n_borta = sum(`Boplats borta` & `År, ny boplats` <= survey_year |
                      `Boplats borta` & is.na(`År, ny boplats`), na.rm = TRUE),
      n_befintliga_bon = n_kända_bon - n_borta,
      min_år_ny = my_min_max(`År, ny boplats`, min),
      max_år_ny = my_min_max(`År, ny boplats`, max)) %>%
    relocate(n_kända_bon, .after = n_bon) %>%
    dplyr::mutate(
      bostatus = case_when(
        n_okända_bon == 0 & n_befintliga_bon > 0 & n_kända_bon > 0 ~ "Känt bo",
        n_okända_bon == 0 & n_befintliga_bon == 0 & n_kända_bon > 0 ~ "(Tidigare) känt bo",
        n_okända_bon > 0 & n_befintliga_bon > 0 & n_kända_bon > 0 ~ "Känt och okänt bo",
        n_okända_bon > 0 & n_befintliga_bon == 0 & n_kända_bon > 0 ~ "(I dagsläget) okänt bo",
        n_okända_bon > 0 & n_befintliga_bon == 0 & n_kända_bon == 0 ~ "Okänt bo",
        n_okända_bon == 0 & n_befintliga_bon == 0 & n_kända_bon == 0 ~ paste0("Upptäckt efter ", survey_year))
    )
}

# Exempel
# boplatser_per_revir()
#
# boplatser_per_revir(survey_year = 2019) %>%
#   filter(n_bon == 0) %>%
#   print(n = Inf)
#
# boplatser_per_revir(survey_year = 2019) %>%
#   filter(min_år_ny < 2019) %>%
#   print(n = Inf)
#
# boplatser_per_revir(survey_year = 2022) %>%
#   count(bostatus)

# boplatser_per_revir(boplats)

# tibble(survey_year = 2017:2022) %>%
#   map(.x = .$survey_year, .f = ~boplatser_per_revir(survey_year = .x)) %>%
#   map(., nrow) %>%
#   unlist()

#' @export
sum_boplats <- function(.x, ..., survey_year = NULL) {

  if (is.null(survey_year)) survey_year <- max(.x$`År, ny boplats`, na.rm = TRUE)

  .x %>%
    group_by(...) %>%
    summarize(
      n_Boplatser =
        sum(is.na(`År, ny boplats`) | `År, ny boplats` <= survey_year, na.rm = TRUE),
      n_Känd = sum(`Boplats känd` == TRUE & (is.na(`År, ny boplats`) | `År, ny boplats` <= survey_year), na.rm = TRUE),
      n_Okänd = sum(`Boplats känd` != TRUE & (is.na(`År, ny boplats`) | `År, ny boplats` <= survey_year), na.rm = TRUE),
      n_Nya = sum(`År, ny boplats` == survey_year, na.rm = TRUE))
}

## Årsposter ----
### Rudimentär funktion för att sammanställa per år ----
#' @export
count_arspost <- function(arspost, År = 2021, ...) {
  arspost %>%
    filter(År %in% {{År}}) %>%
    count(..., Slutstatus, .drop = FALSE)
}
# Behöver tillämpa annan metod för att få med rad- och kolumnsummor

### Sammanställ årsposter per år + pivot ----
# Använder right join för att få med samtliga utfall
#' @export
arspost_cnt <- function(.x, year, pivot_wider = TRUE, ...) {
  f <- .x %>%
    filter(År %in% {{year}}) %>%
    count(...) %>%
    #arrange(desc(Instruktion)) %>%
    rename(Antal = n) %>%
    replace_na(list(Antal = 0))

  # Pivot
  if (pivot_wider) {
    f <- f %>%
      pivot_wider(names_from = Slutstatus, values_from = Antal)
  }
  f
}

### Summera viktade årsposter ----
# ... används för geografiska grupperande variabler
# Lokalisera revir med boplatser som delas över administrativ gräns
# (antingen länsgräns eller landsgrans),
# dvs. revir som har fler än ett län kopplat till boplatser
# Beräkna variabeln w som används som vikt vid summeringen av poster
# Revir som har boplatser i 2 administrativa enheter (=län) ==> w = 0.5,
# om reviret delas av tre län ==> w = 1/3 osv.

# Går det att generalisera denna funktion för att summera
# olika variabler (dvs. ersätta `Slutstatus` med {{variabel}})
#' @export
sum_arspost_wt <- function(boplats, arspost, group_by_year = TRUE, ...) {

  wt <- add_wt({{boplats}}, LokalID, ...)

  arspost_wt <- {{arspost}} %>%
    inner_join(wt, by = "LokalID")

  if (group_by_year) {
    arspost_wt %>%
      count(..., År, Slutstatus, wt = wt, .drop = FALSE, name = "n_status") }
  else {
    arspost_wt %>%
      count(..., Slutstatus, wt = wt, .drop = FALSE, name = "n_status") }
}

# Använd sum_status_wt, pivot_wider och använd sedan denna
# för att beräkna summerande kolumner.
# Bör ändras så att kända revir inkluderas
#' @export
add_arspost_totals <- function(.x) {
  .x %>%
    mutate(
      `Inventerade revir` =
        rowSums(select(., c(`Reviret inventerat ej fastställt besatt`:`Lyckad häckning, flygg unge`,
                            `Osäker häckning`:`Lyckad häckning`)), na.rm = TRUE),
      `Besatta revir` =
        rowSums(select(., c(`Besatt revir`:`Lyckad häckning, flygg unge`,
                            `Ingen häckning`:`Lyckad häckning`)), na.rm = TRUE),
      `Lyckade häckningar` =
        rowSums(select(., starts_with("Lyckad häckning")), na.rm = TRUE),
      `Häckande par` =
        rowSums(select(., c(`Häckning med okänt resultat`:`Lyckad häckning, flygg unge`,
                            `Häckningsförsök påvisat`:`Lyckad häckning`)), na.rm = TRUE))
}

# Reproduktion ----
# OBS! Enbart summera levande ungar oberoende av andra kolumner KAN bli fel!
# Finns t.ex. ett fåtal kullar med ungar i norska kategoring "Häckning påvisat"

# Antal kullar
# Antal ungar
# Antal kullar med flygga ungar (OBS! hela kullen behöver INTE vara flygg)
# Antal flygga ungar
# Frekvens fördelning, kullstorlek (totalt, samt dela upp a) Samtliga ungar observerade == Ja b) Samtliga ungar observerade == Nej
# Ungar indelat i åldersklasser (baserat på slutstatus i årsposter, men data på individuella ungar finns i besök...)

## Summera antal ungar ----
#' @export
sum_kullar <- function(arspost) {

  {{arspost}} %>%
    summarize(
      # Ej "viktat"
      summa_ungar = sum(`Levande ungar observerade`, na.rm = TRUE),
      n_kullar = sum(`Levande ungar observerade` > 0, na.rm = TRUE),
      # Summering av flygga ungar [utan hänsyn till slutstatus]
      summa_ungar_flygga = sum(`Flygga ungar, ålder > 50 dagar`, na.rm = TRUE),
      n_kullar_flygga = sum(`Flygga ungar, ålder > 50 dagar` > 0, na.rm = TRUE),
      # Frekvensfördelning, antal kullar med 1,2,3 ungar
      # (oavsett om samtliga ungar observerats eller ej)
      n_1_kull = sum(`Levande ungar observerade` == 1, na.rm = TRUE),
      n_2_kull = sum(`Levande ungar observerade` == 2, na.rm = TRUE),
      n_3_kull = sum(`Levande ungar observerade` == 3, na.rm = TRUE),
      # Samtliga ungar observerade
      n_1_kull_sakra = sum(`Levande ungar observerade` == 1 & `Samtliga ungar observerade` == "Ja", na.rm = TRUE),
      n_2_kull_sakra = sum(`Levande ungar observerade` == 2 & `Samtliga ungar observerade` == "Ja", na.rm = TRUE),
      n_3_kull_sakra = sum(`Levande ungar observerade` == 3 & `Samtliga ungar observerade` == "Ja", na.rm = TRUE),
      # Samtliga ungar EJ SÄKERT observerade
      n_1_kull_osakra = sum(`Levande ungar observerade` == 1 & `Samtliga ungar observerade` != "Ja", na.rm = TRUE),
      n_2_kull_osakra = sum(`Levande ungar observerade` == 2 & `Samtliga ungar observerade` != "Ja", na.rm = TRUE),
      n_3_kull_osakra = sum(`Levande ungar observerade` == 3 & `Samtliga ungar observerade` != "Ja", na.rm = TRUE),
      # Summera ungar efter ålder
      # OBS! Baserat på slutstatus, inte från data på besöksnivå
      summa_ungar_30 = sum(`Levande ungar observerade`[Slutstatus == "Lyckad häckning, unge <30 dagar"], na.rm = TRUE),
      summa_ungar_30_50 = sum(`Levande ungar observerade`[Slutstatus == "Lyckad häckning, unge 30-50 dagar"], na.rm = TRUE),
      summa_ungar_50 = sum(`Levande ungar observerade`[Slutstatus == "Lyckad häckning, unge >50 dagar ej flygg"], na.rm = TRUE),
      summa_ungar_flygga = sum(`Levande ungar observerade`[Slutstatus == "Lyckad häckning, flygg unge"], na.rm = TRUE),
      # "Slaskkategori" för norsk metodik
      summa_ungar_ovriga = sum(`Levande ungar observerade`[Slutstatus == "Lyckad häckning"], na.rm = TRUE) +
        sum(`Levande ungar observerade`[Slutstatus == "Häckningsförsök påvisat"], na.rm = TRUE),
      .groups = "drop")
}

# Summera viktat:
# Beskriv skillnader mot funktionen ovan!

# År existerar inte i boplats-data, utan måste adderas i ett senare steg!
# Därför används argumentet group_by_year
#' @export
sum_kullar_wt <- function(boplats, arspost, group_by_year = TRUE, ...) {

  wt <- add_wt({{boplats}}, LokalID, ...)

  arspost_wt <- {{arspost}} %>%
    inner_join(wt, by = "LokalID")

  if (group_by_year) {
    arspost_wt <- arspost_wt %>%
      group_by(År, ...)
  } else {
    arspost_wt <- arspost_wt %>%
      group_by(...)
  }

  arspost_wt %>%
    summarize(
      # "Viktad" beräkning
      summa_ungar = sum(`Levande ungar observerade` * wt, na.rm = TRUE),
      n_kullar = sum(wt[`Levande ungar observerade` > 0], na.rm = TRUE),
      # Summering av flygga ungar [utan hänsyn till slutstatus]
      summa_ungar_flygga = sum(`Flygga ungar, ålder > 50 dagar` * wt, na.rm = TRUE),
      n_kullar_flygga = sum(wt[`Flygga ungar, ålder > 50 dagar` > 0], na.rm = TRUE),
      # Frekvensfördelning, antal kullar med 1,2,3 ungar
      # oavsett om samtliga ungar observerats eller ej!
      n_1_kull = sum(wt[`Levande ungar observerade` == 1], na.rm = TRUE),
      n_2_kull = sum(wt[`Levande ungar observerade` == 2], na.rm = TRUE),
      n_3_kull = sum(wt[`Levande ungar observerade` == 3], na.rm = TRUE),
      # Samtliga ungar observerade
      n_1_kull_sakra = sum(wt[`Levande ungar observerade` == 1 & `Samtliga ungar observerade` == "Ja"], na.rm = TRUE),
      n_2_kull_sakra = sum(wt[`Levande ungar observerade` == 2 & `Samtliga ungar observerade` == "Ja"], na.rm = TRUE),
      n_3_kull_sakra = sum(wt[`Levande ungar observerade` == 3 & `Samtliga ungar observerade` == "Ja"], na.rm = TRUE),
      # Samtliga ungar EJ SÄKERT observerade
      n_1_kull_osakra = sum(wt[`Levande ungar observerade` == 1 & `Samtliga ungar observerade` != "Ja"], na.rm = TRUE),
      n_2_kull_osakra = sum(wt[`Levande ungar observerade` == 2 & `Samtliga ungar observerade` != "Ja"], na.rm = TRUE),
      n_3_kull_osakra = sum(wt[`Levande ungar observerade` == 3 & `Samtliga ungar observerade` != "Ja"], na.rm = TRUE),
      # Summera ungar efter ålder (baserat på slutstatus)
      summa_ungar_30 = sum(
        `Levande ungar observerade`[Slutstatus == "Lyckad häckning, unge <30 dagar"] *
          wt[Slutstatus == "Lyckad häckning, unge <30 dagar"], na.rm = TRUE),
      summa_ungar_30_50 = sum(
        `Levande ungar observerade`[Slutstatus == "Lyckad häckning, unge 30-50 dagar"] *
          wt[Slutstatus == "Lyckad häckning, unge 30-50 dagar"], na.rm = TRUE),
      summa_ungar_50 = sum(
        `Levande ungar observerade`[Slutstatus == "Lyckad häckning, unge >50 dagar ej flygg"] *
          wt[Slutstatus == "Lyckad häckning, unge >50 dagar ej flygg"], na.rm = TRUE),
      summa_ungar_flygga = sum(
        `Levande ungar observerade`[Slutstatus == "Lyckad häckning, flygg unge"] *
          wt[Slutstatus == "Lyckad häckning, flygg unge"], na.rm = TRUE),
      # "Slaskkategori" för norsk metodik
      summa_ungar_ovriga = sum(
        `Levande ungar observerade`[Slutstatus == "Lyckad häckning"] *
          wt[Slutstatus == "Lyckad häckning"], na.rm = TRUE) +
        sum(
          `Levande ungar observerade`[Slutstatus == "Häckningsförsök påvisat"] *
            wt[Slutstatus == "Häckningsförsök påvisat"], na.rm = TRUE),
      .groups = "drop")
}

## Medelkull ----
#' @export
add_kullstorlek <- function(.x) {
  .x %>%
    mutate(
      medelkull =
        case_when(n_kullar > 0 ~ summa_ungar / n_kullar,
                  TRUE ~ NA_real_),
      medelkull_alla2 =
        case_when(n_kullar > 0 ~ (n_1_kull + n_2_kull*2 + n_3_kull*3) / (n_1_kull + n_2_kull + n_3_kull),
                  TRUE ~ NA_real_),
      medelkull_sakra =
        case_when(n_kullar > 0 ~ (n_1_kull_sakra + n_2_kull_sakra*2 + n_3_kull_sakra*3) / (n_1_kull_sakra + n_2_kull_sakra + n_3_kull_sakra),
                  TRUE ~ NA_real_),
      medelkull_osakra =
        case_when(n_kullar > 0 ~ (n_1_kull_osakra + n_2_kull_osakra*2 + n_3_kull_osakra*3) / (n_1_kull_osakra + n_2_kull_osakra + n_3_kull_osakra),
                  TRUE ~ NA_real_))
}
