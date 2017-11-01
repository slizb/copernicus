
library(copernicus)

test_that("celestial_body() fails given invalid data", {
  expect_error(celestial_body(c(0,0), c(0,0) ))
  expect_error(celestial_body(c(0,0), NA, 5))
  expect_error(celestial_body(NULL, c(0,0), 5))
  expect_error(celestial_body(c(0,0), c(0,0), -5))
})

test_that("celestial_body() works given valid data", {
  expect_is(celestial_body(c(0,0), c(2,5), 234), "list")
  expect_is(celestial_body(c(0,-3), c(2,-5.24), 234), "list")
})

test_that("solar_system() initializes all of the planets and sun", {
  solar_system <- solar_system()
  expect_is(solar_system, "list")
  expect_length(solar_system, 10)
})
