library(usethis)
library(eagles)
library(stringi)

# Create data sets ----
wms_layers_data <- wms_sources()
tms_layers_data <- tms_sources()

# Export data sets ----
use_data(wms_layers_data, overwrite = TRUE)
use_data(tms_layers_data, overwrite = TRUE)

# Load package ----
devtools::load_all()

# Check for non-ASCII characters ----
# Does it work for *.rda files as well?
tools::showNonASCIIfile("data/test_data_unicode.rda")
tools::showNonASCIIfile("data/tms_layers_data.rda")

# Unicode escaping ----
storrutor

storrutor |> filter(namn == "Örnahusen")
storrutor |> filter(namn == "\u00d6rnahusen")
stri_escape_unicode("Örnahusen")

# Check encoding ----
storrutor |>
  select(where(is.character)) |>
  map(\(x) Encoding(x) <- "UTF-8")

dat1 <- storrutor %>%
  mutate(across(where(is.character), ~ `Encoding<-`(.x, "UTF-8")))

dat1 |>
  select(where(is.character)) |>
  map(\(x) Encoding(x))

all(stri_enc_isutf8(dat1$namn))
all(stri_enc_isutf8(storrutor$namn))
