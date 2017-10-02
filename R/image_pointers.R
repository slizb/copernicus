

load_image <- function(im_path) {
  grid::rasterGrob(png::readPNG(im_path), interpolate = FALSE)
}


create_image_dict <- function() {
  sun <- load_image("copernicus/img/sun.png")
  mercury <- load_image("copernicus/img/mercury.png")
  venus <- load_image("copernicus/img/venus.png")
  earth <- load_image("copernicus/img/earth.png") 
  mars <- load_image("copernicus/img/mars.png")
  jupiter <- load_image("copernicus/img/jupiter.png")
  saturn <- load_image("copernicus/img/saturn.png")
  uranus <- load_image("copernicus/img/uranus.png")
  neptune <- load_image("copernicus/img/neptune.png")
  pluto <- load_image("copernicus/img/pluto.png")
  
  image_dict <- list('sun' = sun,
                     'mercury' = mercury,
                     'venus' = venus,
                     'earth' = earth,
                     'mars' = mars,
                     'jupiter' = jupiter,
                     'saturn' = saturn,
                     'uranus' = uranus,
                     'neptune' = neptune,
                     'pluto' = pluto)
  return(image_dict)
}
