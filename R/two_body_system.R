
library(animation)
library(ggplot2)
library(dplyr)
library(png)
library(grid)

rm(list=ls())

# constants ----------------------------------------------------------

G = 6.67408e-11     # Newton's gravitational constant (m3 kg-1 s-2)
M = 1.989e30        # mass of the sun (kg)
dt = 8.64e4         # time step -one day (seconds)
au = 1.496e11       # astronaumical unit, au (m)

# initializations ----------------------------------------------------

x = 1.496e11             # (m)
y = 0                    # (m)
vx_prev_half = 0         # (m/s)
vy_prev_half = 2.978e4   # (m/s)

# rescale to au's ----------------------------------------------------

G = G / au^3
x = x / au
y = y / au
vx_prev_half = vx_prev_half / au
vy_prev_half = vy_prev_half / au

# images -------------------------------------------------------------

earth <- readPNG("img/earth.png") %>%
     rasterGrob(interpolate=FALSE)

sun <- readPNG("img/sun.png") %>%
     rasterGrob(interpolate=FALSE)

image_dict <- list('earth'=earth,
                   'sun'=sun)

# functions ----------------------------------------------------------

pad <- function(x, optima, range, size) {
     buffer <- range * size / 2
     if (optima == 'max') {
          x + buffer
     } else{
          x - buffer
     }

}


render_body <- function(df, body_name, size) {
     body_pos <- filter(df, body == body_name)

     annotation_custom(image_dict[[body_name]],
                       xmin=pad(body_pos$x, 'min', 3, size),
                       xmax=pad(body_pos$x, 'max', 3, size),
                       ymin=pad(body_pos$y, 'min', 3, size),
                       ymax=pad(body_pos$y, 'max', 3, size))
}

#' Create a static plot of the solar system
#'
#' @param variables
#'
#' @return a plot
#'
#' @examples
plot_system <- function(df) {
     qp <- qplot(-1.5:1.5, -1.5:1.5, geom="blank")

     earth_plot <- render_body(df, 'earth', .13)
     sun_plot <- render_body(df, 'sun', .4)
     opts <- theme(panel.background = element_rect(fill = "black"),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line=element_blank(),
                   axis.text.x=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank())

     return(qp + sun_plot + earth_plot + opts)
}


#' execute one iteration of the leapfrog algorithm
#'
#' @param x
#' @param y
#' @param vx_prev_half
#' @param vy_prev_half
#'
#' @return list of next sequence of xy positions and velocities
#' @export
#'
#' @examples
execute_leapfrog <- function(x, y, vx_prev_half, vy_prev_half) {

     # compute future positions
     vx_next_half <- vx_prev_half - dt * G * M * x * (x^2 + y^2) ^ (-3/2)
     x_next <- x + dt * vx_next_half

     vy_next_half <- vy_prev_half - dt * G * M * y * (x^2 + y^2) ^ (-3/2)
     y_next <- y + dt * vy_next_half

     return(list(x_next, y_next, vx_next_half, vy_next_half))
}


# run simulation -----------------------------------------------------

saveGIF(interval = .05,
        movie.name = "img/2_body_system.gif",
        expr = {

             for (i in 1:365){

                  # plot
                  df <- data.frame(rbind(c(x=x, y=y),
                                         c(x=0, y=0)))
                  df$body <- c('earth', 'sun')

                  print(plot_system(df))

                  # run leapfrog algorithm
                  next_sequence <- execute_leapfrog(x, y, vx_prev_half, vy_prev_half)

                  # update positions
                  x <- next_sequence[[1]]
                  y <- next_sequence[[2]]
                  vx_prev_half <- next_sequence[[3]]
                  vy_prev_half <- next_sequence[[4]]

             }
        }
)

