library(usethis)

path <- "W:/projects/data/ringing/KodlistaF3-230322.mdb"
query <- "SELECT * FROM Artlista"

con <- eagles::db_connect_to_access(path, package = "DBI")
rc_species_list <- DBI::dbGetQuery(con, query) |> tibble::as_tibble()
DBI::dbDisconnect(con)

usethis::use_data(rc_species_list, overwrite = TRUE)
