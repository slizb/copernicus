
load_image <- function(rel_path) {
  package_root <- devtools::package_file()
  im_path <- paste0(package_root, rel_path)
  grid::rasterGrob(png::readPNG(im_path), interpolate = FALSE)
}


create_image_dict <- function() {
  
  image_dict <- list('sun' = load_image("/img/sun.png"),
                     'mercury' = load_image("/img/mercury.png"),
                     'venus' = load_image("/img/venus.png"),
                     'earth' = load_image("/img/earth.png"),
                     'mars' = load_image("/img/mars.png"),
                     'jupiter' = load_image("/img/jupiter.png"),
                     'saturn' = load_image("/img/saturn.png"),
                     'uranus' = load_image("/img/uranus.png"),
                     'neptune' = load_image("/img/neptune.png"),
                     'pluto' = load_image("/img/pluto.png"))
  return(image_dict)
}
