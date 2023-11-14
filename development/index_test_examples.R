places <- sf::read_sf(
  "E:/Maps/Ortnamn/ortnamn_sverige.gpkg",
  query = "SELECT * FROM ortnamn LIMIT 2500;"
)

places <- sf::st_sf(
  id = 1:2,
  name = c("Naturhistoriska riksmuseet, Stockholm", "Naturhistoriska museet, Göteborg"),
  geom = sf::st_sfc(
    sf::st_point(c(673510, 6585100)),
    sf::st_point(c(318150, 6398320))
  ),
  crs = 3006)

places <- sf::read_sf(
  file.path("/vsizip", "vsicurl", "https://geodata.naturvardsverket.se/nedladdning/naturvardsregistret/NP.zip/NP/NP_polygon.shp")
  ) |>
  dplyr::select(NVRID, NAMN, URSBESLDAT) |>
  dplyr::arrange(URSBESLDAT) |>
  sf::st_centroid()

x <- 673510
y <- 6585100

sweref99_index(x, y, .grid_size = 5000)
sweref99_index_alphanum(x, y, .grid_size = 5000)

places_xy <- places |>
  eagles::sfc_as_cols() |>
  st_drop_geometry()

sweref99_index(places_xy$x, places_xy$y, .grid_size = 5000)

grid_sizes <- c(100000, 50000, 25000, 10000, 5000, 2500, 1000, 500)

grid_sizes_named <- c(100000, 50000, 10000, 5000, 1000) |>
  rlang::set_names(c("100km", "50km", "10km", "5km", "1km"))

purrr::map_chr(
  grid_sizes,
  \(g) sweref99_index(x, y, .grid_size = g))

purrr::map_chr(
  c(100000, 50000, 10000, 5000, 1000),
  \(g) sweref99_index_alphanum(x, y, .grid_size = g))

places |>
  sweref99_index(.grid_size = 5000)

sweref99_index(places, .grid_size = 5000)

mutate(
  places,
  grid_5_km = sweref99_index(places, .grid_size = 5000),
)

places %>%
  mutate(
    grid_5_km = sweref99_index(.x = ., .grid_size = 5000)
  )

places |>
  {(\(.) mutate(
    .,
    grid_01_km = sweref99_index(., .grid_size = 1000),
    grid_05_km = sweref99_index(., .grid_size = 5000),
    grid_10_km = sweref99_index(., .grid_size = 10000),
    grid_50_km = sweref99_index(., .grid_size = 50000),
    grid_100_km = sweref99_index(., .grid_size = 100000)
  )
  )}()

# Do NOT use .data as argument, since it is confused with the .data pronoun !?!?
.data_ <- places
.prefix <- "ruta"
.suffix <- "_km"
.grid_size <- 5000

dplyr::mutate(
  .data_,
  "{.prefix}_{stringr::str_replace(.grid_size / 1000, ',|\\\\.', '_')}{.suffix}" := sweref99_index(.data_, .grid_size = .grid_size),
  .keep = "all"
)

places |>
  add_index_1(
    .grid_size = 5000,
    .fn = sweref99_index,
    .prefix = "grid",
    .suffix = "_km"
  )

places |>
  st_transform(3847) |>
  add_index_1(
    .grid_size = 5000,
    .fn = rt90_index,
    .prefix = "grid",
    .suffix = "_km"
  )

places |>
  add_index(
    .grid_size = grid_sizes,
    .fn = sweref99_index,
    .prefix = "ruta",
    .suffix = "_km"
  )

places |>
  add_index(
    .grid_size = c(100000, 50000, 10000, 5000, 1000),
    .fn = sweref99_index_alphanum,
    .prefix = "ruta",
    .suffix = "_km"
  )

places |>
  st_transform(3847) |>
  add_index(
    .grid_size = c(50, 25, 5, 1) * 1000,
    .fn = rt90_index,
    .prefix = "grid",
    .suffix = "_km"
  ) |>
  print(n = Inf)

purrr::map_dfr(
  grid_sizes_named,
  \(g) sweref99_index_alphanum(places, .grid_size = g)
)

dplyr::bind_cols(
  places,
  purrr::map_dfr(
    grid_sizes_named,
    \(g) sweref99_index_alphanum(places, .grid_size = g)
  )
)

sweref99_index_alphanum(places, .grid_size = 10000)

places |>
  sweref99_index_alphanum(.grid_size = 10000)

sweref99_index_alphanum(places, .grid_size = 10000, fastighetsblad = TRUE)

places |>
  add_sweref99_index(.prefix = "ruta")

places |>
  add_sweref99_index(.grid_size = c(10000, 5000))

places |>
  st_transform(3847) |>
  rt90_index(.grid_size = 25000)

places |>
  st_transform(3847) |>
  add_rt90_index(.grid_size = c(50000, 25000, 5000))


# Test case, RT90:
# (point 2 is on purpose outside the reference grid!)
data_ <- st_sf(
  a = 1:3,
  geom = st_sfc(
    st_point(c(1582696, 6583013)),
    st_point(c(1199547, 6524265)),
    st_point(c(1691235, 7396695))),
  crs = 3847)

mapview::mapview(data_)

rt90_index(data_)
add_rt90_index(data_)
rt90_index(data_, .grid_size = 1000)
rt90_index(data_, .grid_size = 1000, rubin = TRUE)
rt90_index(data_, rubin = TRUE, rubin_num = 100)
rt90_index(data_, rubin = TRUE, rubin_num = 1)

# Askö forskningsstation
# WGS84
# 58°49'24.1"N 17°38'13.1"E
# WGS84 DDM
# 58°49.401'N 17°38.219'E
# WGS84 decimal (lat, lon)
# 58.82335, 17.636983
# RT90 (nord, öst)
# 6523806, 1605815
# SWEREF99 TM (nord, öst)
# 6523381, 652251

map(c(1000, 5000, 50000), \(x) rt90_index(1605815, 6523806, x))

storrutor |> filter(ruta == "9I")
ekorutor |> filter(ruta == "9I 4b")

purrr::map_chr(
  c("dm", "dms"),
  \(x) eagles::rc_coords(58.82335, 17.636983, x)
)
