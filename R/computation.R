
library(animation)
library(ggplot2)
library(dplyr)


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
execute_leapfrog <- function(x, y, vx_prev_half, vy_prev_half, constants) {
  G <- constants$G
  M <- constants$M
  dt <- constants$dt
  
  # compute future positions
  vx_next_half <- vx_prev_half - dt * G * M * x * (x^2 + y^2) ^ (-3/2)
  x_next <- x + dt * vx_next_half
  
  vy_next_half <- vy_prev_half - dt * G * M * y * (x^2 + y^2) ^ (-3/2)
  y_next <- y + dt * vy_next_half
  
  return(list(x_next, y_next, vx_next_half, vy_next_half))
}

# inits <- set_initializations()

# run simulation -----------------------------------------------------

# saveGIF(interval = .05,
#         movie.name = "img/2_body_system.gif",
#         expr = {
# 
#              for (i in 1:365){
# 
#                   # plot
#                   df <- data.frame(rbind(c(x=x, y=y),
#                                          c(x=0, y=0)))
#                   df$body <- c('earth', 'sun')
# 
#                   print(plot_system(df))
# 
#                   # run leapfrog algorithm
#                   next_sequence <- execute_leapfrog(x, y, vx_prev_half, vy_prev_half)
# 
#                   # update positions
#                   x <- next_sequence[[1]]
#                   y <- next_sequence[[2]]
#                   vx_prev_half <- next_sequence[[3]]
#                   vy_prev_half <- next_sequence[[4]]
# 
#              }
#         }
# )

