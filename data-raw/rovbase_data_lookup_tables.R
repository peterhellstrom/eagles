# Kolumner i kungsörnsrapporter (Excel-format) från Rovbase ----
rovbase_reports_columns <- readxl::read_excel(
  file.path("..", "..", "data", "nrm_golden_eagle", "kungsorn_rapporter_info.xlsx"),
  sheet = "kolumner",
  range = "A1:K112")

usethis::use_data(rovbase_reports_columns, overwrite = TRUE)

# Kolla Rovbase! Stämmer ordning etc.
besok_status <- tibble::tribble(
  ~status, ~sort_order, ~aktivitet,
  # Svensk metodik
  "Ingen observation av kungsörn", 1, FALSE,
  "Observation av kungsörn", 2, TRUE,
  "Besatt revir", 3, TRUE,
  "Misslyckad häckning", 4, TRUE,
  "Lyckad häckning, unge <30 dagar", 5, TRUE,
  "Lyckad häckning, unge 30-50 dagar", 6, TRUE,
  "Lyckad häckning, unge >50 dagar ej flygg", 7, TRUE,
  "Lyckad häckning, flygg unge", 8, TRUE,
  "Lyckad häckning, historiskt data - okänd ålder", 9, TRUE,
  # Norsk metodik, ekstensiv
  "Osäker häckning", 10, FALSE,
  "Häckningsförsök inte påvisat", 11, TRUE,
  "Häckningsförsök påvisat", 12, TRUE,
  "Lyckad häckning", 13, TRUE,
  "Ingen aktivitet", 14, FALSE)

# Lista över tillgängliga rapporter i arbetsområde 'Kungsörn' i Rovbase ----
# Ändringar införd 2022-02-03.
# Efter uppdatering av rapporter 2022-02-03, kan rapporterna unikt identifieras efter kolumnantal.
# Detta var inte fallet tidigare, då några olika rapporter hade samma antal kolumner.
rovbase_reports <- tibble::tribble(
  ~report_nr, ~report_id, ~instruks, ~report_name_rovbase, ~report_short_name, ~obligatory, ~n_columns, ~coordinates,
  1, "A01", "no", "(S) Kongeørn besøk, norsk instruks", "besok_no", TRUE, 31, TRUE,
  2, "A02", "sv", "(S) Kongeørn besøk, svensk instruks", "besok_sv", TRUE, 34, TRUE,
  3, "A03", "no", "(S) Kongeørn reirplass, norsk instruks", "boplats_no", TRUE, 32, TRUE,
  4, "A04", "sv", "(S) Kongeørn reirplass, svensk instruks", "boplats_sv", TRUE, 35, TRUE,
  5, "A05", "no", "(S) Kongeørn årspost, besøk - norsk instruks", "arspost_besok_no", FALSE, 56, TRUE,
  6, "A06", "sv", "(S) Kongeørn årspost, besøk - svensk instruks", "arspost_besok_sv", FALSE, 53, TRUE,
  7, "A07", "no", "(S) Kongeørn årspost, besøk, reirplass - norsk instruks", "arspost_besok_boplats_no", FALSE, 76, TRUE,
  8, "A08", "sv", "(S) Kongeørn årspost, besøk, reirplass - svensk instruks", "arspost_besok_boplats_sv", FALSE, 77, TRUE,
  9, "A09", "no", "(S) Kongeørn, norsk instruks", "arspost_no", TRUE, 28, FALSE,
  10, "A10", "sv", "(S) Kongeørn, svensk instruks", "arspost_sv", TRUE, 21, FALSE,
  11, "A11", "sv_no", "(S) Lokalitet", "lokaler", TRUE, 8, FALSE)

usethis::use_data(rovbase_reports, overwrite = TRUE)
