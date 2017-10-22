context("welch")

x <- c(31, 18, 17, 16, 37, 16, 32, 13, 14, 49, 25, 19, 13, 32, 27)
y <- c(15, 17, 13, 25, 22, 20, 24, 12, 23, 15, 20, 18)

test_that("welch acepta vectores numericos",{
  expect_equal(welch(x,y), welch(x,y))
})

test_that("welch no permite otra cosa que no sea numeros",{
  expect_error(welch(x,"hahahaha"))
})
