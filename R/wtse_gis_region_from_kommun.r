# Bottenviken = Bvi, Region: 1, Kommunkod: 2409 (Robertsfors), 2482 (Skellefteå), 25*
# N Bottenhavet = N Bh, Region: 1, Kommunkod: 2132 (Hudiksvall), 2184 (Nordanstig), 22*, 2401 (Nordmaling), 2480 (Umeå)
# S Bottenhavet = S Bh, Region: 1, Kommunkod: 2180 (Gävle), 2182 (Söderhamn), 03*
# N Egentliga Östersjön = N EgÖ, Region: 1 (Kommunkod: 01*, 04*)
# S Egentliga Östersjön = S EgÖ, Region: 1, (Kommunkod: 05*, 08*, 09*, 10*, 1287, 1290, 1291, 1270, 1286, 1264, 1287)
# Västerhavet = Vh, Region: 4 (Kommunkod: 13*, 14*, 1233, 1280, 1262, 1261, 1282, 1283, 1284, 1292, 1278)
# Inlandet, syd- och mellan-Sverige = Inl S-M, Region: 2
# Inlandet, norra Sverige = Inl N, Region: 3

# Vectorised switch function
#' @export
region_switch <- Vectorize(function(x) {
  switch(
    as.character(x),
    "05"   = "S EgÖ",
    "08"   = "S EgÖ",
    "09"   = "S EgÖ",
    "10"   = "S EgÖ",
    "1233" = "S EgÖ",
    "1272" = "S EgÖ",
    "1290" = "S EgÖ",
    "1291" = "S EgÖ",
    "1270" = "S EgÖ",
    "1286" = "S EgÖ",
    "1264" = "S EgÖ",
    "1287" = "S EgÖ",
    "01"   = "N EgÖ",
    "04"   = "N EgÖ",
    "03"   = "S Bh",
    "2180" = "S Bh",
    "2182" = "S Bh",
    "2132" = "N Bh",
    "2184" = "N Bh",
    "22"   = "N Bh",
    "2401" = "N Bh",
    "2480" = "N Bh",
    "2409" = "Bvi",
    "2482" = "Bvi",
    "25"   = "Bvi",
    "2"    = "Inl S-M",
    "3"    = "Inl N",
    "4"    = "Vh")
},
"x"
)

# Lookup Subregion, based on given Region and Kommun
# Kommun is given as kommunkod, NOT kommunnamn

#' @export
region_from_kommun <- function(
    Region,
    Kommun,
    unlist = TRUE,
    names = FALSE
) {
  # Check if Region = 1 (Kust), then proceed to check if Kommun is any of the four in X-county
  # that divides N Bh and S Bh. If Region is 2, 3, 4, Kommun is not necessary to define subregion
  x <- ifelse(
    Region == 1,
    ifelse(
      !Kommun %in% grep(Kommun, pattern="^21|^12|^24", value = TRUE),
      substr(Kommun, 1, 2),
      Kommun),
    Region)
  # Call vectorised switch function
  out <- region_switch(x)
  # Replace NULL (returned by switch) with NA
  out[sapply(out, is.null)] <- NA
  # Optional: Remove [default] (or keep) names
  if (!names) names(out) <- NULL
  # Optional: unlist (default)
  if (unlist) {
    unlist(out)
  } else {
    out
  }
}

#' @export
reformat_region <- function(x) {
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
