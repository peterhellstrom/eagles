# INFO ----
# Uppdaterad 2021-10-06 av Peter Hellström, MFÖ, NRM

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
# tillvägagångssättet redovisas i filen {xxx}.r (OBS! Work in progress, kod behöver städas)

# Namnen på rapportfilerna från Rovbase innehåller info om användare samt nedladdningsdatum,
# men ingen information om vilken rapport det handlar om.
# Därför behöver innehållet i rapporterna granskas för att kunna bestämma
# vilken rapport som är vilken. Detta görs genom att bestämma antalet kolumner
# i rapporterna, samt om vissa fältnamn förekommer i rapporten. Antalet kolumner
# är inte unikt, därför behövs även vissa fält lokaliseras för att identifiera rapporterna.

# Jag använder fälten "Instruks" och "År" tillsammans med antal kolumner för att
# bestämma vilken rapoprt som är vilken.
# OBS! Om rapporternas struktur ändras i Rovbase kommer denna kod inte att fungera.
# Detta kan uppstå om a) användaren väljer att göra en egen rapport där inte alla tillgängliga fält
# har valts (jag förutsätter alltså att samtliga tillgängliga fält har valts vid skapandet av rapporten!)
# eller b) rapportens struktur har ändrats av supporten på Rovbase.
# Därför är det önskvärt att supporten på Rovbase meddelar om/när rapporterna förändras så att
# vi kan uppdatera rutinerna.

# Funktion som identifierar nedladdade rapporter ----
# NOTE: wrong behaviour if copy_files = TRUE and delete_old = FALSE
# This is an impossible action, so files are overwritten anyway.
#' @export
find_rovbase_reports <- function(wd, user, only_obligatory = TRUE,
                                 copy_files = FALSE, import = FALSE,
                                 delete_old = FALSE) {

  # Kontrollera att angiven mapp dit rapporter laddats ned finns
  if (!dir.exists(wd)) {
    stop("Angiven arbetsmapp för nedladdade rapporter saknas")
  } else {
    # Sök efter rapport-filer (xlsx-format) skapade av angiven användare:
    fls <- list.files(wd, pattern = glob2rx(paste0(user, "*.xlsx")), full.names = FALSE)
  }

  # Om mappen finns, men filer saknas (alternativt har fel användarnamn):
  if(length(fls) == 0) {
    stop(
      paste0("Det finns inga nedladdade rapporter skapade av användaren ",
             user,
             " på angiven sökväg!\n"))
  }

  # Om filer finns, men antalet är för lågt - kolla vilka filer som saknas:
  if(length(fls) < 7) {
    cat(
      paste0("Minst 7 rapporter skapade av användaren ",
             user,
             " behövs!\n", 7 - length(fls),
             " rapporter saknas.",
             " Vi tar nu reda på vilka rapporter som saknas.\n"))
  }

  # Läs in kolumnrubriker från hittade Excel-rapporter
  # använd argumentet n_max = 0 i read_excel
  fls_m <- map(file.path(wd, fls), read_excel, n_max = 0)
  #names(fls_m) <- str_match(fls, "(.*)\\..*$")[,2]
  #names(fls_m) <- fls
  fls_colnames <- map(fls_m, colnames)

  # Extrahera metadata från rapporterna
  # OBS! Vad händer om det finns flera versioner av samma rapport
  # med olika datumstämplar?
  report_file_info_all <-
    tibble(file_name = fls) %>%
    mutate(
      n_columns = map_int(fls_m, ncol),
      #n_rows = map_int(fls_m, nrow),
      user_name = str_sub(fls, 1, 4),
      first_column = fls_colnames %>%
        sapply(., "[[", 1),
      instruks_field = map_lgl(fls_colnames, has_field_name, "Instruks"),
      year_field = map_lgl(fls_colnames, has_field_name, "År"),
      download_dttm = str_sub(file_name, 5, -6) %>%
        strptime(., "%d%m%Y%H%M%S%OS", tz = ""),
      report_short_name =
        case_when(
          n_columns == 31 & instruks_field == TRUE & year_field == FALSE ~ "besok_no",
          n_columns == 34 & instruks_field == FALSE & year_field == FALSE ~ "besok_sv",
          n_columns == 32 & instruks_field == TRUE & year_field == FALSE ~ "boplats_no",
          n_columns == 35 & instruks_field == FALSE & year_field == FALSE ~ "boplats_sv",
          n_columns == 56 & instruks_field == TRUE & year_field == TRUE ~ "arspost_besok_no",
          n_columns == 54 & instruks_field == FALSE & year_field == TRUE ~ "arspost_besok_sv",
          n_columns == 76 & instruks_field == TRUE & year_field == TRUE ~ "arspost_besok_boplats_no",
          n_columns == 78 & instruks_field == FALSE & year_field == TRUE ~ "arspost_besok_boplats_sv",
          n_columns == 28 & instruks_field == TRUE & year_field == TRUE ~ "arspost_no",
          n_columns == 22 & instruks_field == FALSE & year_field == TRUE ~ "arspost_sv",
          n_columns == 8 & instruks_field == TRUE & year_field == FALSE ~ "lokaler")) %>%
    select(-c(first_column, instruks_field, year_field)) %>%
    inner_join(rovbase_reports %>%
                 select(report_short_name, report_nr, report_name_rovbase, obligatory),
               by = "report_short_name") %>%
    group_by(report_short_name) %>%
    # Sortera så att nyaste rapporterna kommer först
    arrange(report_short_name, desc(download_dttm)) %>%
    mutate(version = row_number())

  # Välj bara senaste versionen, ignorera i förekommande fall äldre versioner
  report_file_info_use <- report_file_info_all %>%
    slice(head = 1) %>%
    arrange(report_nr)

  report_file_info_old <- report_file_info_all %>%
    anti_join(report_file_info_use, by = "file_name")

  # Filtrera metadata-tabellen om endast obligatoriska tabeller ska inkluderas
  if (only_obligatory) {
    report_file_info_use <- report_file_info_use %>%
      filter(obligatory == TRUE)
  }

  cat(paste0(nrow(report_file_info_use), " rapportfiler har hittats.\n"))

  # Vilka rapporter är obligatoriska? Tala om för användaren vilka som finns,
  # om status är OK eller om det saknas filer, i så fall vilka!
  if (nrow(report_file_info_use) < 7) {

    cat(
      paste0(7 - nrow(report_file_info_use),
             " rapport(er) skapade av användaren ", user, " saknas!\n",
             "Rapporter behöver exporteras från Rovbase.\n"))

    missing_reports <- rovbase_reports %>%
      filter(obligatory == TRUE) %>%
      select(report_name_rovbase, report_short_name) %>%
      anti_join(report_file_info_use, by ="report_short_name")

    return(missing_reports)

    stop("Import av data har avbrutits pga att det saknas Excel-rapportfiler!\n")

  } else {
    cat("Samtliga nödvändiga rapporter hittades!\n")
  }

  if (delete_old) {
    if (nrow(report_file_info_old) > 0) {
      cat(nrow(report_file_info_old), "gamla filer tas bort!\n")
      unlink(file.path(wd, report_file_info_old$file_name))
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

    fls_copy_from <- file.path(wd, report_file_info_use$file_name)

    wd_latest <- file.path(dirname(wd), "rapporter_senaste")
    if (!dir.exists(wd_latest)) dir.create(wd_latest)

    fls_copy_to <- file.path(wd_latest, paste0(report_file_info_use$report_short_name, ".xlsx"))

    cat("Rapporterna kopieras med ändrade namn till:\n", wd_latest,"\n")

    file.copy(from = fls_copy_from, to = fls_copy_to,
      overwrite = TRUE, copy.date = TRUE)
  }

  if (import) {
    # Läs in data för fortsatt bearbetning
    import_data <- map(file.path(wd, report_file_info_use$file_name), read_excel)
    names(import_data) <- report_file_info_use$report_short_name
    import_data
  } else {
    report_file_info_use
  }

}

# Exempel på användning ----

## Ange sökväg till mapp där rapporterna från Rovbase laddats ned ----
#wd <- "./data_rovbase/rapporter_nedladdade"
## Ange användarnamn, fyra tecken, finns i filnamnet på de nedladdade rapportfilerna.
#user <- "PEHE"

## Olika varianter ----
### Visa enbart vilka filer som finns och vilka rapporter det handlar om. ----
# Saknas rapporter returnerar funktionen vilka obligatoriska rapporter som måste
# hämtas från rapportverktyget i Rovbase

# only_obligatory = FALSE visar även ej helt nödvändiga rapporter som laddats ned
#x_info <- find_rovbase_reports(wd, user, only_obligatory = FALSE,
#                               copy_files = FALSE, import = FALSE)
#x_info

# only obligatory = TRUE returnerar enbart de filer som verkligen behövs
#x_info <- find_rovbase_reports(wd, user, only_obligatory = TRUE,
#                               copy_files = FALSE, import = FALSE)
#x_info

### Radera äldre versioner av filer via argumentet delete_old ----
#x_info <- find_rovbase_reports(wd, user, only_obligatory = TRUE,
#                               copy_files = FALSE, import = FALSE, delete_old = TRUE)
#x_info

### Kopiera data till filer med andra namn ----
# Argumentet copy_files kopierar filerna med användarvänliga namn till mappen "./data_rovbase/rapporter_senaste",
# placeras i roten på aktuell arbetsmapp
# Det är oftast detta steg man vill göra, för att sedan bearbeta data från rapportfilerna
#x_copy <- find_rovbase_reports(wd, user, only_obligatory = TRUE,
#                               copy_files = TRUE, import = FALSE)
#x_copy

### Importera data ----
# Argumentet import importerar data från Excel filer till en lista med tibbles
#x_data <- find_rovbase_reports(wd, user, only_obligatory = TRUE,
#                               copy_files = TRUE, import = TRUE)
#
#names(x_data)
#length(x_data)
#lengths(x_data)
