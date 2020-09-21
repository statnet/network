context("test-indexing")

test_that("proper error messages for out of bounds indexing",{
  nw <- network.initialize(10)
  expect_error(nw[1,100], "subscript out of bounds")
  expect_error(nw[1,100] <- 1, "subscript out of bounds")
  expect_error(nw[100,1], "subscript out of bounds")
  expect_error(nw[100,1] <- 1, "subscript out of bounds")
})
