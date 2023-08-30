# Förberedelser: Ladda hem rapporter från Rovbase.
# För arbetsområdet kungsörn är inte alla tillgängliga rapporter nödvändiga.
# Det är rapporterna lokaler, boplatser, årsposter samt besök som behövs.
# För samtliga rapporter förutom lokaler, ska både norsk och svensk version laddas hem,
# eftersom revir som delas med Norge ENBART finns med i de norska rapporterna.

# Av 11 tillgängliga rapporter är 7 nödvändiga.
# OBS! Ladda hem rapporterna genom att välja samtliga fält i rapportskaparen,
# koden nedan fungerar inte om
# användaren gör en egen design på rapporten.

# I tabellen nedan framgår rapporternas namn
# och vilka som är obligatoriska samt (i viss mån) vad tabellerna innehåller.
# (Se Excel-filen 'kungsorn_rapporter_info.xlsx' för utförligare info om rapporternas uppbyggnad).
# Då rapportfilernas struktur och fältnamn för norsk och svensk instruktion skiljer sig krävs
# efterbearbetning för att slå ihop filerna till bearbetningsbara data.

# Namnen på rapportfilerna från Rovbase innehåller info om användare samt nedladdningsdatum,
# men ingen information om vilken rapport det handlar om.
# Därför behöver innehållet i rapporterna granskas för att kunna bestämma
# vilken rapport som är vilken. Detta görs genom att bestämma antalet kolumner
# i rapporterna (samt eventuellt om vissa fältnamn förekommer i rapporten). Antalet kolumner
# är i de olika tabllerna är f.n. unikt.
# Men detta har varierat beroende på förändringar i Rovbase,
# därför kan vissa fält behöva lokaliseras för att korrekt identifiera rapporterna.

# Jag använder fälten "Instruks" och "År" tillsammans med antal kolumner för att
# bestämma vilken rapport som är vilken.

# OBS! Om rapporternas struktur ändras i Rovbase kommer denna kod inte att fungera.
# Detta kan uppstå om a) användaren väljer att göra en egen rapport där inte alla tillgängliga fält
# har valts (jag förutsätter alltså att samtliga tillgängliga fält har valts vid skapandet av rapporten!)
# eller b) rapportens struktur har ändrats i Rovbase.

# Funktion som identifierar nedladdade rapporter ----
# NOTE: wrong behaviour if copy_files = TRUE and delete_old = FALSE
# This is an impossible action, so files are overwritten anyway.
#' @export
find_rovbase_reports <- function(
    data_dir,
    user,
    only_obligatory = TRUE,
    copy_files = FALSE,
    import = FALSE,
    delete_old = FALSE,
    pattern = utils::glob2rx(paste0(user, "*.xlsx")),
    data_dir_copy = "rapporter_senaste"
    ) {

  # Kontrollera att angiven mapp dit rapporter laddats ned finns
  if (!dir.exists(data_dir)) {
    stop("Angiven arbetsmapp för nedladdade rapporter saknas")
  } else {
    # Sök efter rapport-filer (xlsx-format) skapade av angiven användare:
    fls <- tibble::tibble(
      file = list.files(
        data_dir,
        pattern = pattern,
        full.names = FALSE)
    )
  }

  # Om mappen finns, men filer saknas (alternativt har fel användarnamn):
  if(nrow(fls) == 0) {
    stop(
      paste0(
        "Det finns inga nedladdade rapporter skapade av användaren ",
        user, " på angiven sökväg!\n")
    )
  }

  # Om filer finns, men antalet är för lågt - kolla vilka filer som saknas:
  if(nrow(fls) < 7) {
    cat(
      paste0(
        "Minst 7 rapporter skapade av användaren ", user, " behövs!\n",
        7 - nrow(fls), " rapporter saknas.\n")
    )
  }

  # Läs in kolumnrubriker från hittade Excel-rapporter
  # använd argumentet n_max = 0 i readxl::read_excel.
  # Extrahera metadata från rapporterna
  # OBS! Vad händer om det finns flera versioner av samma rapport
  # med olika datumstämplar?
  fls <- fls |>
    dplyr::mutate(
      columns = purrr::map(file.path(data_dir, file), \(x) {
        readxl::read_excel(x, n_max = 0) |>
          colnames() } )
    ) |>
    dplyr::mutate(
      n_columns = lengths(columns),
      user_name = stringr::str_sub(file, 1, 4),
      # user_name = stringr::str_extract(file, "[A-Z]*"),
      # first_column = purrr::map_chr(columns, dplyr::first),
      # instruks_field = purrr::map_lgl(columns, \(x) "Instruks" %in% x),
      # year_field = purrr::map_lgl(columns, \(x) "År" %in% x),
      download_dttm = stringr::str_sub(file, 5, -6) |>
        base::strptime("%d%m%Y%H%M%S%OS", tz = "")
    )

  fls <- fls |>
    dplyr::left_join(
      rovbase_reports |>
        dplyr::select(report_nr, report_short_name:n_columns),
      dplyr::join_by(n_columns))

  fls <- fls |>
    tidyr::drop_na(report_nr) |>
    dplyr::group_by(report_short_name) |>
    # Sortera så att nyaste rapporterna kommer först
    dplyr::arrange(report_short_name, dplyr::desc(download_dttm)) |>
    dplyr::mutate(version = dplyr::row_number())

  # Välj bara senaste versionen, ignorera i förekommande fall äldre versioner
  fls_use <- fls |>
    dplyr::slice_head() |>
    dplyr::arrange(report_nr)

  fls_old <- fls |>
    dplyr::anti_join(fls_use, dplyr::join_by(file)) |>
    dplyr::arrange(report_nr)

  # Filtrera metadata-tabellen om endast obligatoriska tabeller ska inkluderas
  if (only_obligatory) {
    fls_use <- fls_use |>
      dplyr::filter(obligatory)
  }

  cat(
    paste0(nrow(fls_use), " rapportfiler har hittats.\n")
    )

  # Vilka rapporter är obligatoriska? Tala om för användaren vilka som finns,
  # om status är OK eller om det saknas filer, i så fall vilka!
  if (nrow(fls_use) < 7) {

    cat(
      paste0(
        7 - nrow(fls_use),
        " rapport(er) skapade av användaren ", user, " saknas!\n",
        "Rapporter behöver exporteras från Rovbase.\n")
    )

    missing_reports <- rovbase_reports |>
      dplyr::filter(obligatory) |>
      dplyr::select(report_name_rovbase, report_short_name) |>
      dplyr::anti_join(
        fls_use,
        dplyr::join_by(report_short_name))

    return(missing_reports)

    stop("Import av data har avbrutits pga att det saknas Excel-rapportfiler!\n")

  } else {
    cat("Samtliga nödvändiga rapporter hittades!\n")
  }

  if (delete_old) {
    if (nrow(fls_old) > 0) {
      cat(nrow(fls_old), "gamla filer tas bort!\n")
      unlink(file.path(data_dir, fls_old$file))
    } else {
      cat("Finns inga äldre versioner att ta bort!\n")
    }
  }

  if (copy_files) {

    # OBS! Arbetsflödet med att namnge och flytta filer känns inte säkert ur datasäkerhetssynpunkt

    # Kopiera rapportfiler till mappen för 'senaste rapporter'
    # Observera att befintliga filer skrivs över!
    # Tanken är att inget arbete ska göras i själva Excel-filerna,
    # utan att all bearbetning ska ske automatiserat i R.

    fls_copy_from <- file.path(data_dir, fls_use$file)

    data_dir_latest <- file.path(dirname(data_dir), data_dir_copy)
    if (!dir.exists(data_dir_latest)) dir.create(data_dir_latest)

    fls_copy_to <- file.path(data_dir_latest, paste0(fls_use$report_short_name, ".xlsx"))

    cat("Rapporterna kopieras med ändrade namn till:\n", data_dir_latest,"\n")

    file.copy(
      from = fls_copy_from,
      to = fls_copy_to,
      overwrite = TRUE,
      copy.date = TRUE)
  }

  if (import) {
    # Läs in data för fortsatt bearbetning
    import_data <- purrr::map(
      file.path(data_dir, fls_use$file),
      readxl::read_excel) |>
      rlang::set_names(fls_use$report_short_name)
    import_data
  } else {
    fls_use |> dplyr::ungroup()
  }

}

# Exempel på användning ----

## Ange sökväg till mapp där rapporterna från Rovbase laddats ned ----
#data_dir <- "./data_rovbase/rapporter_nedladdade"
## Ange användarnamn, fyra tecken, finns i filnamnet på de nedladdade rapportfilerna.
#user <- "PEHE"

## Olika varianter ----
### Visa enbart vilka filer som finns och vilka rapporter det handlar om. ----
# Saknas rapporter returnerar funktionen vilka obligatoriska rapporter som måste
# hämtas från rapportverktyget i Rovbase

# only_obligatory = FALSE visar även ej helt nödvändiga rapporter som laddats ned
#x_info <- find_rovbase_reports(data_dir, user, only_obligatory = FALSE,
#                               copy_files = FALSE, import = FALSE)
#x_info

# only obligatory = TRUE returnerar enbart de filer som verkligen behövs
#x_info <- find_rovbase_reports(data_dir, user, only_obligatory = TRUE,
#                               copy_files = FALSE, import = FALSE)
#x_info

### Radera äldre versioner av filer via argumentet delete_old ----
#x_info <- find_rovbase_reports(data_dir, user, only_obligatory = TRUE,
#                               copy_files = FALSE, import = FALSE, delete_old = TRUE)
#x_info

### Kopiera data till filer med andra namn ----
# Argumentet copy_files kopierar filerna med användarvänliga namn till mappen "./data_rovbase/rapporter_senaste",
# placeras i roten på aktuell arbetsmapp
# Det är oftast detta steg man vill göra, för att sedan bearbeta data från rapportfilerna
#x_copy <- find_rovbase_reports(data_dir, user, only_obligatory = TRUE,
#                               copy_files = TRUE, import = FALSE)
#x_copy

### Importera data ----
# Argumentet import importerar data från Excel filer till en lista med tibbles
#x_data <- find_rovbase_reports(data_dir, user, only_obligatory = TRUE,
#                               copy_files = TRUE, import = TRUE)
#
#names(x_data)
#length(x_data)
#lengths(x_data)
