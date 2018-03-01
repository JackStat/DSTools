context("Testing SamplingDist")

test_that("length of SamplingDist does not matche the parameter", {
  ss <- SamplingDist(samples = 51)
  expect_equal(length(ss), 51)
})


test_that("Devations from population mean are extreme", {
  ss <- SamplingDist(samples = 5)
  expect_true(all(abs(ss) <= 5))
})
