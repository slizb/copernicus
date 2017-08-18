
library(animation)
library(ggplot2)

rm(list=ls())

# constants ----------------------------------------------------------

G = 6.67408e-11     # Newton's gravitational constant (m3 kg-1 s-2)
M = 1.989e30        # mass of the sun (kg)
dt = 8.64e4         # time step -one day (seconds)


# initializations ----------------------------------------------------

x = 1.496e11             # (m)
y = 0                    # (m)
vx_prev_half = 0         # (m/s)
vy_prev_half = 2.978e4   # (m/s)


# functions ----------------------------------------------------------


#' Create a static plot of the solar system
#'
#' @param variables
#'
#' @return a plot
#'
#' @examples
plot_system <- function(df) {
     gg <- ggplot(df, aes(x=x, y=y, color=body ) )
     points <- geom_point()
     x_lim <- xlim(-2e11, 2e11)
     y_lim <- ylim(-2e11, 2e11)

     return(gg + points + x_lim + y_lim)
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

