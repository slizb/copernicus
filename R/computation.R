
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
