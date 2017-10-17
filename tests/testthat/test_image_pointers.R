
library(copernicus)

test_that("load_image() returns a rastergrob object", {
  sun_png = load_image("/img/sun.png")
  expect_is(sun_png, "rastergrob")
})
