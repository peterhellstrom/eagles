# Bottenviken = Bvi, Region: 1, Kommunkod: 2409 (Robertsfors), 2482 (Skellefteå), 25*
# N Bottenhavet = N Bh, Region: 1, Kommunkod: 2132 (Hudiksvall), 2184 (Nordanstig), 22*, 2401 (Nordmaling), 2480 (Umeå)
# S Bottenhavet = S Bh, Region: 1, Kommunkod: 2180 (Gävle), 2182 (Söderhamn), 03*
# N Egentliga Östersjön = N EgÖ, Region: 1 (Kommunkod: 01*, 04*)
# S Egentliga Östersjön = S EgÖ, Region: 1, (Kommunkod: 05*, 08*, 09*, 10*, 1287, 1290, 1291, 1270, 1286, 1264, 1287)
# Västerhavet = Vh, Region: 4 (Kommunkod: 13*, 14*, 1233, 1280, 1262, 1261, 1282, 1283, 1284, 1292, 1278)
# Inlandet, syd- och mellan-Sverige = Inl S-M, Region: 2
# Inlandet, norra Sverige = Inl N, Region: 3

# Lookup Subregion, based on given Region and Kommun
# Kommun is given as kommunkod, NOT kommunnamn


#' Title
#'
#' @param .region
#' @param .kommun
#'
#' @returns
#' @export
#'
#' @examples
wtse_add_region_from_kommun <- function(.region, .kommun) {

  # Check if Region = 1 (Kust), then proceed to check if Kommun is any of the four in X-county
  # that divides N Bh and S Bh. If Region is 2, 3, 4, Kommun is not necessary to define subregion

  region_str <- dplyr::case_when(
    {{ .region }} == 1 ~ dplyr::if_else(
      stringr::str_detect( {{ .kommun }}, "^21|^12|^24", negate = TRUE),
      stringr::str_sub( {{ .kommun }}, 1, 2),
      {{ .kommun }}
    ),
    .default = {{ .region }}
  )

  dplyr::case_match(
    region_str,
    c("05", "08", "09", "10") ~ "S EgÖ",
    c("1233", "1272", "1290", "1291", "1270", "1286", "1264", "1287") ~ "S EgÖ",
    c("01", "04") ~ "N EgÖ",
    c("03", "2180", "2182") ~ "S Bh",
    c("2132", "2184", "22", "2401", "2480") ~ "N Bh",
    c("2409", "2482", "25") ~ "Bvi",
    "2" ~ "Inl S-M",
    "3" ~ "Inl N",
    "4" ~ "Vh"
  ) |>
    base::factor(
      levels = c("Vh", "S EgÖ", "N EgÖ", "S Bh", "N Bh", "Bvi", "Inl S-M", "Inl N")
    )

}

#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
wtse_region_reformat <- function(x) {
  dplyr::case_when(
    x == "S EgÖ" ~ "Eg. Östersjön, södra",
    x == "N EgÖ" ~ "Eg. Östersjön, norra",
    x == "S Bh" ~ "Bottenhavet, södra",
    x == "N Bh" ~ "Bottenhavet, norra",
    x == "Bvi" ~ "Bottenviken",
    x == "Vh" ~ "Västerhavet",
    x == "Inl N" ~ "Inland N",
    x == "Inl S-M" ~ "Inland S-M",
    TRUE ~ NA_character_
  )
}


#' Title
#'
#' @param separate 
#' @param use_factors 
#'
#' @returns
#' @export
#'
#' @examples
wtse_add_region_from_county <- function(
    separate = TRUE, 
    use_factors = TRUE
) {
  
  out <- tibble::tribble(
    ~RegionID, ~Huvudnivå, ~`Undernivå 1`, ~`Undernivå 2`, ~LanBokstav, ~LanKod,
    1, "Östersjön", "Eg Östersjön", "Götaland", "M, K, H, I, E", "12, 10, 08, 09, 05",
    1, "Östersjön", "Eg Östersjön", "Svealand", "D, AB", "04, 01",
    1, "Östersjön", "Bottniska viken", "Bottenhavet", "C, X, Y", "03, 21, 22",
    1, "Östersjön", "Bottniska viken", "Bottenviken", "AC, BD", "24, 25",
    4, "Västerhavet", "Södra Västerhavet", "Öresund", "M", "12",
    4, "Västerhavet", "Norra Västerhavet", "Kattegat", "N", "13",
    4, "Västerhavet", "Norra Västerhavet", "Skagerrak", "O", "14",
    2, "Inland", "Inlandet i söder", "Götaland", "M, K, N, F, G, H, E, O", "12, 10, 13, 06, 07, 08, 05, 14",
    2, "Inland", "Inlandet i söder", "Svealand", "AB, C, D, S, T, U, W", "01, 03, 04, 17, 18, 19, 20",
    # Note that entire county of X (Gävleborg) has been assigned to RegionID = 3,
    # but southern part of this county has RegionID = 2 in database!
    3, "Inland", "Inlandet i norr", "Nedre Norrland", "X, Y, Z", "21, 22, 23",
    3, "Inland", "Inlandet i norr", "Övre Norrland", "AC, BD", "24, 25"
  )
  
  if (separate) {
    out <- out |> 
      tidyr::separate_longer_delim(
        cols = c(LanBokstav, LanKod), 
        delim = ", "
      )
  }
  
  if (use_factors) {
    # NOTE! Undernivå 2 is not a factor yet...
    out <- out |> 
      dplyr::mutate(
        Huvudnivå = factor(
          Huvudnivå, 
          levels = c("Östersjön", "Västerhavet", "Inland")
        ),
        `Undernivå 1` = factor(
          `Undernivå 1`, 
          levels = c(
            "Eg Östersjön", "Bottniska viken", 
            "Södra Västerhavet", "Norra Västerhavet", 
            "Inlandet i söder", "Inlandet i norr"
          )
        )
      )
  }
  out
}

