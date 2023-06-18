gam_plot_trend <- function(data, x, y, model,
                           scale_x_min = 1965, scale_x_max = 2020, scale_x_by = 5,
                           scale_y_min = 0, scale_y_max = 2, scale_y_by = 0.2,
                           rect_y_min = 0.97, rect_y_max = 1.75, 
                           rect_fill = "palegreen3", rect_alpha = 0.2,
                           fit_line_color = "grey", fit_line_color_incr = "blue",
                           fit_line_color_decr = "red",
                           fit_line_size_decr = 1.5, fit_line_size_incr = 1.5,
                           fit_line_alpha = 1,
                           hline_intercept = 1.31, hline_size = 1,
                           hline_color = "orange", hline_linetype = "dashed",
                           point_size = 2.5, point_fill = "steelblue", 
                           point_color = "black", point_shape = 21, point_alpha = 1,
                           axis_text_size = 16, axis_title_size = 16,
                           xlab = NULL, ylab = "Productivity",
                           n_pts_predict = 300) {
  data %>% 
    ggplot(aes({{x}}, {{y}})) + 
    scale_x_continuous(breaks = seq(scale_x_min, scale_x_max, scale_x_by)) + 
    scale_y_continuous(breaks = seq(scale_y_min, scale_y_max, scale_y_by)) + 
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = rect_y_min, ymax = rect_y_max, 
             fill = rect_fill, alpha = rect_alpha) + 
    geom_hline(aes(yintercept = hline_intercept), 
               linetype = hline_linetype, 
               color = hline_color, 
               size = hline_size) + 
    geom_line(data = gam_predict(model, n_pts = n_pts_predict), 
              aes(x = {{x}}, y = predict, 
                  color = variable, 
                  size = variable), 
              alpha = fit_line_alpha,
              lineend = "round", na.rm = TRUE) + 
    scale_size_manual(values = 
                        c(1, fit_line_size_decr, fit_line_size_decr)) + 
    scale_colour_manual(values = 
                          c(fit_line_color, fit_line_color_decr, fit_line_color_incr)) + 
    geom_point(size = point_size, alpha = point_alpha, 
               shape = point_shape, fill = point_fill, color = point_color,
               na.rm = TRUE) + 
    labs(x = xlab, y = ylab) + 
    theme(legend.position = "none",
          axis.title.y = element_text(color = "black", size = axis_title_size, face = "bold"),
          axis.text.x = element_text(size = axis_title_size, color = "black"),
          axis.text.y = element_text(size = axis_title_size, color = "black"))
}