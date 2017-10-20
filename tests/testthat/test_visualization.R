
library(copernicus)

test_that("plot_system() fails given invalid data", {
  expect_error(plot_system(NA))
  expect_error(plot_system('asdf'))
  expect_error(plot_system(123))
})

test_that("plot_system() returns a plot given valid data", {
  df <- data.frame(rbind(c(x = 1, y = 1),
                         c(x = 0, y = 0)))
  df$body <- c('bob', 'martin')
  a_plot <- plot_system(df)
  
  expect_is(a_plot, "ggplot")
  
})

# test render_body()

# test pad()
test_that("pad() fails given invalid inputs", {
  expect_error(pad())
  expect_error(pad(3, 'max', 1, size = 'a'))
})

test_that("pad() works given valid inputs", {
  expect_is(pad(3, 'max', 1, 4), 'numeric')
  expect_is(pad(2.5, 'min', 1.3, 0.8), 'numeric')
  expect_is(pad(345, 'min', 8923, 467), 'numeric')
})
