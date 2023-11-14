Sys.time()
format(Sys.time(), tz = Sys.timezone(location = TRUE), usetz = TRUE)
as.POSIXlt(Sys.time(), "UTC")
lubridate::now(tz = "UTC")

datetime_format <- "%Y-%m-%dT%H:%M:%SZ"
base::format(base::Sys.time(), datetime_format, tz = "UTC")
base::format(lubridate::now(tz = "UTC"), datetime_format)

time_to_gpx_time <- function(
    x = base::Sys.time(),
    datetime_format = "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC",
    usetz = FALSE) {
  
  base::format(
    x, 
    datetime_format, 
    tz = tz, 
    usetz = usetz
  )
}

x <- time_to_gpx_time()
x

base::strptime(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") |> 
  base::as.POSIXct() |> 
  base::format(tz = base::Sys.timezone(location = TRUE), usetz = TRUE)

lubridate::parse_date_time(
  x, 
  "%Y-%m-%d %H:%M:%S", 
  tz = "UTC"
) |> 
  format(
    tz = Sys.timezone(location = TRUE),
    usetz = TRUE
  )
