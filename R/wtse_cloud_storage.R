#' @export
jwd_folder_names <- function(
    .x,
    pattern = "([A-Z])([0-9]{3})([A-Zi])?|((?<=_)(.+)(?=\\_))|([0-9]{4}$)") {

  str_extract_all(.x, pattern = pattern) %>%
    do.call(rbind, .) %>%
    as.data.frame() %>%
    set_names(c("lokalkod", "lokalnamn", "survey_year")) %>%
    mutate(survey_year = as.numeric(survey_year))
}
