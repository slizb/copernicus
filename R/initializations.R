

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
                    'mass' = mass,
                    'body' = TRUE)
  return(body_list)
}

sun <- function() {
  celestial_body(position = c(0, 0), 
                 velocity = c(0, 0), 
                 mass = 1.989e30)
}

mercury <- function() {
  celestial_body(position = c(5.79e10, 0), 
                 velocity = c(0, 4.74e4), 
                 mass = 3.3e23)
}

venus <- function() {
  celestial_body(position = c(1.082e11, 0), 
                 velocity = c(0, 3.5e4), 
                 mass = 4.87e24)  
}

earth <- function() {
  celestial_body(position = c(1.496e11, 0), 
                 velocity = c(0, 2.978e4), 
                 mass = 5.972e24)
}

mars <- function() {
  celestial_body(position = c(2.279e11, 0), 
                 velocity = c(0, 2.41e4), 
                 mass = 6.42e23)
}

jupiter <- function() {
  celestial_body(position = c(7.786e11, 0), 
                 velocity = c(0, 1.31e4), 
                 mass = 1.9e27)
}

saturn <- function() {
  celestial_body(position = c(1.433e12, 0), 
                 velocity = c(0, 9.6e3), 
                 mass = 5.69e26)
}

uranus <- function() {
  celestial_body(position = c(2.873e12, 0), 
                 velocity = c(0, 6.8e3), 
                 mass = 8.68e25)
}

neptune <- function() {
  celestial_body(position = c(4.495e12, 0), 
                 velocity = c(0, 5.4e3), 
                 mass = 1.03e26)
}

pluto <- function() {
  celestial_body(position = c(5.906e12, 0), 
                 velocity = c(0, 4.74e3), 
                 mass = 1.46e22) 
}

# todo: create seperate method to initialize sun / earth
# todo: create seperate method to initialize earth / moon
# todo: create seperate method to initialize sun / earth / moon

solar_system <- function() {
  system_list <- list("sun" = sun(),
                      "mercury" = mercury(),
                      "venus" = venus(),
                      "earth" = earth(),
                      "mars" = mars(),
                      "jupiter" = jupiter(),
                      "saturn" = saturn(),
                      "uranus" = uranus(),
                      "neptune" = neptune(),
                      "pluto" = pluto() )
  
  convert_element_to_au <- function(x, element) {
    x[[element]] <- x[[element]] / AU
    return(x)
  }
  
  lapply(system_list, convert_element_to_au, 'position')
  lapply(system_list, convert_element_to_au, 'velocity')

}


initialize_system <- function(...) {
  inits <- list()
  user_inits <- list(...)
  user_names <- names(user_inits)
  
  for (i in 1:length(user_inits)) {
    is_body <- user_inits[[i]]$body
    if (isTRUE(is_body)) {
      name <- user_names[i]
      inits[[name]] <- user_inits[[name]]
    } else {
      sub_system <- user_inits[[i]]
      for (name in names(sub_system)) {
        inits[[name]] <- sub_system[[name]]
      }
    }
  }
  return(inits)
}
