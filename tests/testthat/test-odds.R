library(gambleR)

context("Odds")

test_that("conversion fractional -> decimal", {
  expect_equal(to.decimal(c("2/1", "5/3", "1/4")), c(3, 8/3, 5/4))
})

test_that("conversion decimal -> fractional", {
  expect_equal(to.fractional(c(3, 2+2/3, 1.25)), c("2/1", "5/3", "1/4"))
})




implied.probability(c("2/1", "5/3", "1/4"))
implied.probability(c(3, 2+2/3, 1.25))