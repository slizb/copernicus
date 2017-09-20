

AU <- 1.496e11 # astronaumical unit conversion


set_constants <- function(G = 6.67408e-11 / AU^3,
                          M = 1.989e30,
                          dt = 8.64e4) {
  constants <- list("G" = G,
                    "M" = M,
                    "dt" = dt)
  return(constants)
}


set_initializations <- function(earth_x = 1.496e11 / AU,
                                earth_y = 0,
                                earth_vx = 0,
                                earth_vy = 2.978e4 / AU) {
  initializations <- list("earth_x" = earth_x,
                          "earth_y" = earth_y,
                          "earth_vx" = earth_vx,
                          "earth_vy" = earth_vy)
  return(initializations)
}

