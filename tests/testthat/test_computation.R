
library(copernicus)

test_that("execute_leapfrog() fails given invalid data", {
     expect_error(execute_leapfrog())
     expect_error(execute_leapfrog(NA))
     expect_error(execute_leapfrog(NULL))
     expect_error(execute_leapfrog('asdf'))
})

test_that("execute_leapfrog() works given valid data", {
     expect_is(execute_leapfrog(1,1,1,1), "list")
     expect_is(execute_leapfrog(0,2,4,6), "list")
     expect_is(execute_leapfrog(-10,50,1000,3e6), "list")
})

