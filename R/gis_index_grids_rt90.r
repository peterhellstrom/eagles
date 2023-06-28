# https://github.com/kjgrahn/rubin

#' @export
rubin_width <- function(x) {
    y <- as.character(x)
    switch(y, "1" = 4, "10" = 3, "100" = 2, "1000" = 1)
}

# I funktionerna nedan gäller:
# y = northing, x = easting
# alltså enligt ett matematiskt koordinatsystem.
# Notera dock att Lantmäteriet använder ett geodetiskt system där X = Northing och Y = Easting.

# Indexsystem i RT90
# 2 huvudsakliga rutstorlekar: 50 x 50 km (storruta), 5 x 5 km (småruta)
# Storrutor anges med ett numeriskt värde i sydlig-nordlig riktning (1-32) och
# ett alfabetiskt i västlig-östlig riktning (kapitaler, A-N, vilket ger 14 kolumner)
# Storrutor delas även in i kvartsrutor, 25 x 25 km, där kvadranterna benämns NV, NO, SV, SO
# Smårutor/ekorutor anges med 0-9 i syd-nordlig riktning och a-j i väst-östlig riktning,
# en storruta består av 100 smårutor (även kallade ekorutor efter ekonomiska kartbladet)

# OBS! Använd inte ceiling() för att avrunda siffror uppåt!
# Fungerar ju om en punkt ligger helt inne i en cell och inte tangerar
# någon av ytterlinjernas hörn. Men om vi matar in nedre vänstra hörnet,
# blir ju resultatet 0!

# Dependencies: stringr

#' @export
rt90_index <- function(.x, .y = NULL, .grid_size = 5000,
                       pad0 = FALSE, space = TRUE, caps = FALSE,
                       rubin = FALSE, rubin_num = 1000, rubin_space = TRUE) {

  # Allowed grid sizes
  gs <- c(100, 50, 25, 10, 5, 1) * 1000

  g <- .grid_size
  stopifnot(all(g %in% gs))

  sep <- if_else(space, " ", "")

  # Extract coordinates if input is an sf-object
  if (class(.x)[1] == "sf") {
    if (!sf::st_crs(.x)$epsg %in% c(3021, 3847)) {
      stop("coordinate reference system must be epsg:3021 or epsg:3847")
    }
    .xy <- st_extract_pt_coords(.x); .x <- .xy$.x; .y <- .xy$.y
  }

	if (any(nchar(as.integer(.y)) != 7)) stop("y-coordinate must be given with 7 digits")
	if (any(nchar(as.integer(.x)) != 7)) stop("x-coordinate must be given with 7 digits")

	y_min <- 6100000; y_max <- 7700000
	x_min <- 1200000; x_max <- 1900000

	.y <- if_else(.y < y_min | .y >= y_max, NA, .y)
	.x <- if_else(.x < x_min | .x >= x_max, NA, .x)

	# Storruta (50 x 50 km)
	bk_50 <- str_c(
	  floor(1 + (.y - y_min) / 50000),
	  LETTERS[floor(1 + (.x - x_min) / 50000)])

	bk_50 <- case_when(
	  pad0 == TRUE ~ str_pad(bk_50, width = 3, pad = 0),
	  TRUE ~ bk_50)

	# Småruta (5 x 5 km)
	bk_5 <- str_c(
	  floor((.y %% 50000) / 5000),
	  letters[floor(1 + (.x %% 50000) / 5000)])

	bk_5 <- case_when(
	  caps == TRUE ~ toupper(bk_5),
	  TRUE ~ bk_5)

	# RUBIN-koordinater
	if (rubin == FALSE) {
		rubin_str <- ""
	} else {
		# Beräkning av RUBIN-kod
		# nr & er avser avstånd från södra resp. västra kanten av ekorutan i antal METER
		# y - (5000 * floor((y / 5000)))
		nrubin <- str_pad(
		  floor(.y %% 5000 / rubin_num),
		  width = rubin_width(rubin_num),
		  pad = "0")

		erubin <- str_pad(
		  floor(.x %% 5000 / rubin_num),
		  width = rubin_width(rubin_num),
		  pad = "0")

		if (rubin_space == FALSE) {
			rubin_str <- str_c(nrubin, erubin)
		} else {
			rubin_str <- str_c(" ", nrubin, erubin)
		}
	}

	case_when(
	  .grid_size == 1000 ~ str_c(bk_50, bk_5,
	                             str_c(
	                               floor((.y %% 5000) / 1000),
	                               floor((.x %% 5000) / 1000)),
	                             sep = sep),
	  .grid_size == 5000 ~ str_c(bk_50, sep, bk_5, rubin_str),
	  .grid_size == 25000 ~ str_c(bk_50,
	                              quadrant_cardinal(
	                                floor(1 + (.y - y_min) / 50000),
	                                floor(1 + (.y - y_min) / 50000)),
	                              sep = sep),
	  .grid_size == 50000 ~ bk_50,
	  TRUE ~ NA
	)
}

#' @export
add_rt90_index <- function(data, .grid_size = c(50000, 25000, 5000), .prefix = "grid", ...) {
  data %>%
    bind_cols(
      map(.grid_size, ~ rt90_index(.data, .grid_size = .x, ...)) %>%
        setNames(str_c(.prefix, "_", str_replace(.grid_size/1000, "\\.", "\\_"))) %>%
        bind_cols())
}


# Test case:
# (point 2 is on purpose outside the reference grid!)
# data <- st_sf(a = 1:3,
#               geom = st_sfc(
#                 st_point(c(1582696, 6583013)),
#                 st_point(c(1199547, 6524265)),
#                 st_point(c(1691235, 7396695))), crs = 3847)
# mapview::mapview(data)

# rt90_index(data)
# add_rt90_index(data)
# rt90_index(data, .grid_size = 1000)
# rt90_index(data, .grid_size = 1000, rubin = TRUE)
# rt90_index(data, rubin = TRUE, rubin_num = 100)
# rt90_index(data, rubin = TRUE, rubin_num = 1)

# input variable grid should in future versions be parsed
# with regular expressions, and not with substr()
# Should add grid size 25 here as well, possibly also RUBIN coordinates
	# x: indexruta, längd 5 tecken t.ex. 09E2g

#' @export
index_rt90 <- function(grid, .grid_size = 5000) {

	nstor <- as.numeric(substr(grid, 1, 2))
	estor <- substr(grid, 3, 3)

	# Northing: X = (storrutaX * 50000) + 6100000 + (5000 * bladX)
	# Easting: y = ((storrutaYs position i alfabetet - 1) * 50000) + 1200000 + (5000 * (bladYs position i alfabetet -1))

	if (.grid_size == 5000) {
	  nekon <- as.numeric(substr(grid, 4, 4))
	  # eekon must be supplied as lower case
	  eekon <- tolower(substr(grid, 5, 5))

		out <- data.frame(
			northing = ((nstor - 1) * 50000) + (nekon * 5000) + 6100000,
			easting = as.numeric((((gtools::asc(estor) - 64) - 1) * 50000) +
			                       (((gtools::asc(eekon) - 96) - 1) * 5000) + 1200000)
		)
	} else if (.grid_size == 50000) {
		out <- data.frame(
			northing = ((nstor - 1) * 50000) + 6100000,
			easting = (((gtools::asc(estor) - 64) - 1) * 50000) + 1200000
		)
	}
	return(out)
}
