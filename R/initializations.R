

AU <- 1.496e11 # astronaumical unit conversion


set_constants <- function(G = 6.67408e-11 / AU^3,
                          M = 1.989e30,
                          dt = 8.64e4) {
  constants <- list("G" = G,
                    "M" = M,
                    "dt" = dt)
  return(constants)
}

celestial_body <- function(position, velocity, mass) {
  # screen user inputs
  assertthat::assert_that(is.numeric(position))
  assertthat::assert_that(is.numeric(velocity))
  assertthat::assert_that(is.numeric(mass))
  assertthat::assert_that(length(position) == 2)
  assertthat::assert_that(length(velocity) == 2)
  assertthat::assert_that(length(mass) == 1)
  
  body_list <- list('position' = position,
                    'velocity' = velocity,
                    'mass' = mass)
  return(body_list)
}

# todo: create seperate method to initialize sun / earth
# todo: create seperate method to initialize earth / moon
# todo: create seperate method to initialize sun / earth / moon

solar_system <- function() {
  system_list <- list("sun" = celestial_body(c(0, 0), c(0, 0), 1.989e30),
                      "mercury" = celestial_body(c(5.79e10, 0), c(0, 4.74e4), 3.3e23),
                      "venus" = celestial_body(c(1.082e11, 0), c(0, 3.5e4), 4.87e24),
                      "earth" = celestial_body(c(1.496e11, 0), c(0, 2.978e4), 5.972e24),
                      "mars" = celestial_body(c(2.279e11, 0), c(0, 2.41e4), 6.42e23),
                      "jupiter" = celestial_body(c(7.786e11, 0), c(0, 1.31e4), 1.9e27),
                      "saturn" = celestial_body(c(1.433e12, 0), c(0, 9.6e3), 5.69e26),
                      "uranus" = celestial_body(c(2.873e12, 0), c(0, 6.8e3), 8.68e25),
                      "neptune" = celestial_body(c(4.495e12, 0), c(0, 5.4e3), 1.03e26),
                      "pluto" = celestial_body(c(5.906e12, 0), c(0, 4.74e3), 1.46e22) )
  
  convert_element_to_au <- function(x, element) {
    x[[element]] <- x[[element]] / AU
    return(x)
  }
  
  lapply(system_list, convert_element_to_au, 'position')
  lapply(system_list, convert_element_to_au, 'velocity')

}

# todo: adapt initialize_system() so it can unpack the solar_system() et. al funcs
initialize_system <- function(...) {
  inits <- "!!!"
  user_inits <- list(...)
  for (name in names(user_inits)) {
    # todo: make assertions about shape / class
    inits[[name]] <- user_inits[[name]]
  }
  return(inits)
}
