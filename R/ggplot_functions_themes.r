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

#' @export
theme_ge <- function (base_size = 12, base_family = "Times New Roman",
	base_line_size = rel(0.5), base_rect_size = rel(0.5)) {

	theme_grey(base_size = base_size, base_family = base_family,
        base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
        theme(
			panel.background = element_blank(),
			panel.border = element_blank(),
			panel.grid = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = "grey85", colour = "grey20"),
			legend.key = element_rect(fill = "white", colour = NA),
			legend.direction = "horizontal",
			legend.position = "bottom",
			legend.justification = "left",
			legend.text = element_text(size = rel(1)),
			axis.line = element_line(colour = "black"),
			axis.text.x = element_text(size = base_size, colour = "black", angle = 45, hjust = 1, vjust = 1, margin = margin(t = 2.5, r = 0, b = 0, l = 0)),
			axis.text.y = element_text(size = base_size, colour = "black", hjust = 1, margin = margin(t = 0, r = 5, b = 0, l = 0)),
			title = element_text(face = "bold", size = rel(1), color = "black"),
			plot.caption = element_text(face = "italic", size = rel(0.75), color = "black", hjust = 1, vjust = 7),
			complete = TRUE)
}

#' @export
theme_ge2 <- function (base_size = 14, base_family = "Times New Roman",
	base_line_size = rel(0.5), base_rect_size = rel(0.5)) {

    theme_bw(base_size = base_size, base_family = base_family,
        base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
        theme(
			panel.border = element_blank(),
			panel.grid.major = element_blank(),
			panel.grid.minor = element_blank(),
			axis.line = element_line(colour = "black", size = rel(1)),
			legend.key = element_blank(),
			strip.background = element_rect(fill = "white", colour = "black"),
			plot.margin = margin(t = 10, r = 10, b = 20, l = 20),
			axis.title.x = element_text(size = rel(1.25), vjust = -3.5),
			axis.title.y = element_text(size = rel(1.25), angle = 90, vjust = 5),
			axis.text.x = element_text(size = rel(1.25), colour = "black"),
			axis.text.y = element_text(size = rel(1.25), colour = "black"),
			complete = TRUE)
}

#theme_bw <- function (base_size = 11, base_family = "", base_line_size = base_size/22,
#    base_rect_size = base_size/22) {
#    theme_grey(base_size = base_size, base_family = base_family,
#        base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
#        theme(
#			panel.background = element_rect(fill = "white", colour = NA),
#			panel.border = element_rect(fill = NA, colour = "grey20"),
#			panel.grid = element_line(colour = "grey92"),
#           panel.grid.minor = element_line(size = rel(0.5)),
#            strip.background = element_rect(fill = "grey85", colour = "grey20"),
#			legend.key = element_rect(fill = "white", colour = NA),
#			complete = TRUE)
#}

# https://stackoverflow.com/questions/46327431/facet-wrap-add-geom-hline

