
library(copernicus)

test_that("plot_system() fails given invalid data", {
     expect_error(plot_system(NA))
     expect_error(plot_system('asdf'))
     expect_error(plot_system(123))
})

test_that("plot_system() returns a plot given valid data", {
     df <- data.frame(rbind(c(x=1, y=1),
                            c(x=0, y=0)))
     df$body <- c('bob', 'martin')
     a_plot <- plot_system(df)

     expect_is(a_plot, "ggplot")

})

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

