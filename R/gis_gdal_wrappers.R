#' @export
copy_from_gpkg <- function(
    dsn_source,
    dsn_destination,
    driver = c("GPKG", "OpenFileGDB"),
    filter_expr = NULL) {

  driver <- match.arg(driver)

  f <- gpkg_contents(dsn_source) |>
    dplyr::rename(src_driver = driver)

  if (!is.null(filter_expr)) {
    f <- f |>
      dplyr::filter(stringr::str_detect(name, filter_expr))
  }

  f_copy <- f |>
    dplyr::mutate(
      command = dplyr::case_when(
        driver == "GPKG" ~ glue::glue("ogr2ogr -f {driver} {dsn_destination} -a_srs EPSG:{srs_id} -overwrite {dsn_source} {name}"),
        driver == "OpenFileGDB" ~ glue::glue("ogr2ogr -f {driver} {dsn_destination} -a_srs EPSG:{srs_id} -overwrite {dsn_source} {name} -mapFieldType Integer64=Integer")
      ))


  # walk(f_copy$command, \(x) system(command = "cmd.exe", input = x, show.output.on.console = TRUE))

  # Or send as one command to cmd
  # (Check https://stackoverflow.com/questions/8055371/how-do-i-run-two-commands-in-one-line-in-windows-cmd)
  cmd_n <- stringr::str_c(f_copy$command, collapse = " & ")

  system(
    command = "cmd.exe",
    input = cmd_n,
    show.output.on.console = TRUE)
}
