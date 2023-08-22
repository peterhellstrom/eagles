# https://stackoverflow.com/questions/46327431/facet-wrap-add-geom-hline

# facet_wrap add geom_hline
StatMeanLine <- ggplot2::ggproto("StatMeanLine", ggplot2:::Stat,
  compute_group = function(data, scales) {
    transform(data, yintercept=mean(y))
  },
  required_aes = c("x", "y")
)

#' @export
stat_mean_line <- function(mapping = NULL, data = NULL, geom = "hline",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatMeanLine, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
