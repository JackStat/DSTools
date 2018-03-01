context("Testing SamplingDist")

test_that("length of SamplingDist does not matche the parameter", {
  ss <- SamplingDist(samples = 51)
  expect_equal(length(ss), 51)
})


test_that("Devations from population mean are extreme", {
  ss <- SamplingDist(samples = 5)
  expect_true(all(abs(ss) <= 5))
})



test_that("Error messages are not working properly",{
  testthat::expect_error(SamplingDist(0)
    , "The number of samples needs to be greater than 0")

  testthat::expect_error(SamplingDist(sd = 0)
    , "The standard devaition cannot be != 0")
})


