

AU <- function() {
  # astronaumical unit conversion
  1.496e11 
}


set_constants <- function(G = 6.67408e-11 / AU()^3,
                          M = 1.989e30,
                          dt = 8.64e4) {
  constants <- list("G" = G,
                    "M" = M,
                    "dt" = dt)
  return(constants)
}

is_celestial_body <- function(x) {
  if (is.list(x)) {
    has_position <- length(x$position) == 2
    has_velocity <- length(x$velocity) == 2
    has_mass <- length(x$mass) == 1
    has_proper_elements <- has_position & has_velocity & has_mass
    has_proper_types_list <- lapply(X = x[c('position', 'velocity', 'mass')],
                                    FUN = is.numeric)
    has_proper_types_int <- min(unlist(has_proper_types_list))
    has_proper_types <- as.logical(has_proper_types_int)
    answer <- has_proper_elements & has_proper_types
  } else {
    answer <- FALSE
  }
  return(answer)
}

initialize_system <- function(...) {
  inits <- list()
  user_inits <- list(...)
  user_names <- names(user_inits)
  
  for (i in 1:length(user_inits)) {
    assertthat::assert_that(is.list(user_inits[[i]]))
    if (is_celestial_body(user_inits[[i]])) {
      name <- user_names[i]
      inits[[name]] <- user_inits[[name]]
    } else {
      sub_system <- user_inits[[i]]
      for (i in length(sub_system)) {
        list_element <- sub_system[[i]]
        assertthat::assert_that(is_celestial_body(list_element))
        name <- names(sub_system)[i]
        inits[[name]] <- sub_system[[name]]
      }
    }
  }
  return(inits)
}

celestial_body <- function(position, velocity, mass) {
  # screen user inputs
  assertthat::assert_that(is.numeric(position))
  assertthat::assert_that(is.numeric(velocity))
  assertthat::assert_that(is.numeric(mass))
  assertthat::assert_that(length(position) == 2)
  assertthat::assert_that(length(velocity) == 2)
  assertthat::assert_that(length(mass) == 1)
  assertthat::assert_that(mass > 0)
  
  body_list <- list('position' = position,
                    'velocity' = velocity,
                    'mass' = mass,
                    'body' = TRUE)
  return(body_list)
}

sun <- function() {
  celestial_body(position = c(0, 0) / AU(), 
                 velocity = c(0, 0) / AU(), 
                 mass = 1.989e30)
}

mercury <- function() {
  celestial_body(position = c(5.79e10, 0) / AU(), 
                 velocity = c(0, 4.74e4) / AU(), 
                 mass = 3.3e23)
}

venus <- function() {
  celestial_body(position = c(1.082e11, 0) / AU(), 
                 velocity = c(0, 3.5e4) / AU(), 
                 mass = 4.87e24)  
}

earth <- function() {
  celestial_body(position = c(1.496e11, 0) / AU(), 
                 velocity = c(0, 2.978e4) / AU(), 
                 mass = 5.972e24)
}

mars <- function() {
  celestial_body(position = c(2.279e11, 0) / AU(), 
                 velocity = c(0, 2.41e4) / AU(), 
                 mass = 6.42e23)
}

jupiter <- function() {
  celestial_body(position = c(7.786e11, 0) / AU(), 
                 velocity = c(0, 1.31e4) / AU(), 
                 mass = 1.9e27)
}

saturn <- function() {
  celestial_body(position = c(1.433e12, 0) / AU(), 
                 velocity = c(0, 9.6e3) / AU(), 
                 mass = 5.69e26)
}

uranus <- function() {
  celestial_body(position = c(2.873e12, 0) / AU(), 
                 velocity = c(0, 6.8e3) / AU(), 
                 mass = 8.68e25)
}

neptune <- function() {
  celestial_body(position = c(4.495e12, 0) / AU(), 
                 velocity = c(0, 5.4e3) / AU(), 
                 mass = 1.03e26)
}

pluto <- function() {
  celestial_body(position = c(5.906e12, 0) / AU(), 
                 velocity = c(0, 4.74e3) / AU(), 
                 mass = 1.46e22) 
}

solar_system <- function() {
  list("sun" = sun(),
       "mercury" = mercury(),
       "venus" = venus(),
       "earth" = earth(),
       "mars" = mars(),
       "jupiter" = jupiter(),
       "saturn" = saturn(),
       "uranus" = uranus(),
       "neptune" = neptune(),
       "pluto" = pluto() )
}

