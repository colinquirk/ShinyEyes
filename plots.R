make_trace_plot <- function(input, chunk_data, brush_data, colors) {
  p <- ggplot()
  
  if (input$x_variable != "" & input$y_variable != "" & input$sample_variable != "") {
    p <- ggplot(chunk_data, aes_string(x = input$sample_variable)) +
      geom_line(aes_string(y = input$x_variable), color = "red") +
      geom_line(aes_string(y = input$y_variable), color = "blue")
  }
  
  if (nrow(brush_data$windows) > 0) {
    names(colors) <- c(input$window_1_name, input$window_2_name, input$window_3_name)
    
    p <- p + geom_rect(data = brush_data$windows,
                       aes(x = NULL, xmin = xmin, xmax = xmax, fill = window_name),
                       ymin = -Inf, ymax = Inf, alpha = 0.3) +
      scale_fill_manual(values = colors) +
      theme(legend.position="bottom")
  }
  p
}

make_gaze_gif <- function(render_gif, input, chunk_data) {
  if (render_gif) {
    gaze_x_scale_min <- as.numeric(input$gaze_x_scale_min)
    gaze_x_scale_max <- as.numeric(input$gaze_x_scale_max)
    gaze_y_scale_min <- as.numeric(input$gaze_y_scale_min)
    gaze_y_scale_max <- as.numeric(input$gaze_y_scale_max)
    n_frames <- as.numeric(input$n_frames)
    
    p <- ggplot(chunk_data, aes_string(x = input$x_variable, y = input$y_variable)) +
      annotate(geom = "text", label = "+",
               x = gaze_x_scale_max/2,
               y = gaze_y_scale_max/2, size = 15) +
      geom_point() +
      coord_equal(xlim = c(gaze_x_scale_min, gaze_x_scale_max),
                  ylim = c(gaze_y_scale_min, gaze_y_scale_max),
                  expand = FALSE) +
      transition_time(!!sym(input$sample_variable)) +
      labs(title = "Sample: {round(frame_time)}",
           x = "",
           y = "")
    
    anim_save("outfile.gif", animate(p, nframes = n_frames))
    list(src = "outfile.gif", contentType = "image/gif")
  } else {
    ggsave("outfile.png", ggplot(), width = 1, height = 1, units = "in")
    list(src = "outfile.png", contentType = "image/gif")
  }
}