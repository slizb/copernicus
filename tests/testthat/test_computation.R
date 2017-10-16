
library(copernicus)

test_that("execute_leapfrog() fails given invalid data", {
  expect_error(execute_leapfrog())
  expect_error(execute_leapfrog(NA, NA, NA, NA, NA))
  expect_error(execute_leapfrog(NULL, NULL, NULL, NULL, NULL))
  expect_error(execute_leapfrog('a', 'b', 'c', 'd', 'e'))
})

test_that("execute_leapfrog() works given valid data", {
  constants <- set_constants()
  expect_is(execute_leapfrog(1,1,1,1, constants), "list")
  expect_is(execute_leapfrog(0,2,4,6, constants), "list")
  expect_is(execute_leapfrog(-10,50,1000,3e6, constants), "list")
})

