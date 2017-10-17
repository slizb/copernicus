
library(copernicus)

test_that("load_image() returns a rastergrob object", {
  sun_png <- load_image("/img/sun.png")
  expect_is(sun_png, "rastergrob")
})

test_that("create_image_dict() loads all body images", {
  image_dict <- create_image_dict()
  expect_is(image_dict, "list")
  expect_length(image_dict, 10)
})
