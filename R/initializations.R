

AU <- 1.496e11 # astronaumical unit conversion


set_constants <- function(G = 6.67408e-11 / AU^3,
                          M = 1.989e30,
                          dt = 8.64e4) {
  constants <- list("G" = G,
                    "M" = M,
                    "dt" = dt)
  return(constants)
}

# maybe todo: extract function for setting an individual body's initial state
# todo: create seperate method to initialize sun / earth
# todo: create seperate method to initialize earth / moon
# todo: create seperate method to initialize sun / earth / moon
# todo: adapt set_inits() so it can unpack the solar_system()

solar_system <- function() {
  system_list <- list("sun" = list('position' = c(0, 0),
                                   'velocity' = c(0, 0),
                                   'mass' = 1.989e30),
                      "mercury" = list('position' = c(5.79e10, 0),
                                       'velocity' = c(0, 4.74e4),
                                       'mass' = 3.3e23),
                      "venus" = list('position' = c(1.082e11, 0),
                                     'velocity' = c(0, 3.5e4),
                                     'mass' = 4.87e24),
                      "earth" = list('position' = c(1.496e11, 0),
                                     'velocity' = c(0, 2.978e4),
                                     'mass' = 5.972e24),
                      "mars" = list('position' = c(2.279e11, 0),
                                    'velocity' = c(0, 2.41e4),
                                    'mass' = 6.42e23),
                      "jupiter" = list('position' = c(7.786e11, 0),
                                       'velocity' = c(0, 1.31e4),
                                       'mass' = 1.9e27),
                      "saturn" = list('position' = c(1.433e12, 0),
                                      'velocity' = c(0, 9.6e3),
                                      'mass' = 5.69e26),
                      "uranus" = list('position' = c(2.873e12, 0),
                                      'velocity' = c(0, 6.8e3),
                                      'mass' = 8.68e25),
                      "neptune" = list('position' = c(4.495e12, 0),
                                       'velocity' = c(0, 5.4e3),
                                       'mass' = 1.03e26),
                      "pluto" = list('position' = c(5.906e12, 0),
                                     'velocity' = c(0, 4.74e3),
                                     'mass' = 1.46e22) )
  
  convert_element_to_au <- function(x, element) {
    x[[element]] <- x[[element]] / AU
    return(x)
  }
  
  lapply(system_list, convert_element_to_au, 'position')
  lapply(system_list, convert_element_to_au, 'velocity')

}

initialize_system <- function(...) {
  inits <- "!!!"
  user_inits <- list(...)
  for (name in names(user_inits)) {
    # todo: make assertions about shape / class
    inits[[name]] <- user_inits[[name]]
  }
  return(inits)
}
