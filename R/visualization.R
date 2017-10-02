
pad <- function(x, optima, range, size) {
  buffer <- range * size / 2
  if (optima == 'max') {
    x + buffer
  } else {
    x - buffer
  }
}


render_body <- function(df, body_name, size) {
  body_pos <- filter(df, body == body_name)
  
  annotation_custom(image_dict[[body_name]],
                    xmin = pad(body_pos$x, 'min', 3, size),
                    xmax = pad(body_pos$x, 'max', 3, size),
                    ymin = pad(body_pos$y, 'min', 3, size),
                    ymax = pad(body_pos$y, 'max', 3, size))
}

#' Create a static plot of the solar system
#'
#' @param variables
#'
#' @return a plot
#'
#' @examples
plot_system <- function(df) {
  qp <- qplot(-1.5:1.5, -1.5:1.5, geom = "blank")
  
  earth_plot <- render_body(df, 'earth', .13)
  sun_plot <- render_body(df, 'sun', .4)
  opts <- theme(panel.background = element_rect(fill = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank())
  
  return(qp + sun_plot + earth_plot + opts)
}

