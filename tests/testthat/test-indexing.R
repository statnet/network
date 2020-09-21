context("test-indexing")

test_that("proper error messages for out of bounds indexing (unipartite)",{
  nw <- network.initialize(10)
  expect_error(nw[1,100], "subscript out of bounds")
  expect_error(nw[1,100] <- 1, "subscript out of bounds")
  expect_error(nw[100,1], "subscript out of bounds")
  expect_error(nw[100,1] <- 1, "subscript out of bounds")
})

test_that("proper error messages (or lack thereof) for out of bounds indexing (bipartite)",{
  nw <- network.initialize(10, bipartite=3, directed=FALSE)
  expect_error(nw[1,3], "subscript out of bounds")
  expect_error(nw[1,3] <- 1, "subscript out of bounds")
  expect_error(nw[4,5], "subscript out of bounds")
  expect_error(nw[4,5] <- 1, "subscript out of bounds")

  expect_error(nw[4,1], NA)
  expect_error(nw[5,3], NA)
})

test_that("wildcard assignment (bipartite)",{
  nw <- network.initialize(10, bipartite=3, directed=FALSE)
  nw[1,] <- 1
  expect_equal(network.edgecount(nw), 7) # 7

  nw[,4] <- 1
  expect_equal(network.edgecount(nw), 9) # 7 + 3 - 1

  nw[,] <- 1
  expect_equal(network.edgecount(nw), 21) # 3*7
})
