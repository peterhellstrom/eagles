library(usethis)
library(eagles)

x <- indexrutor_namn()
#map_dfr(x, ~.x, .id = "grid_source")

storrutor <- x$storrutor
ekorutor <- x$ekorutor
fastighetsblad <- x$fastighetsblad
wms_layers_data <- wms_sources()
tms_layers_data <- tms_sources()

use_data(storrutor, overwrite = TRUE)
use_data(ekorutor, overwrite = TRUE)
use_data(fastighetsblad, overwrite = TRUE)
use_data(wms_layers_data, overwrite = TRUE)
use_data(tms_layers_data, overwrite = TRUE)
