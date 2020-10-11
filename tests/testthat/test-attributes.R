# Testing checking/setting/getting of vertex/edge/network attributes

test_that("getting non-existent vertex attribute returns (vector of) NAs with a warning", {
  net <- network.initialize(4)
  net[1,2] <- net[2,3] <- 1
  expect_warning(
    r <- get.vertex.attribute(net, "no.such.attribute")
  )
  expect_identical(
    r,
    rep(NA, network.size(net))
  )
})

test_that("getting non-existent edge attribute returns (vector of) NAs with a warning", {
  net <- network.initialize(4)
  net[1,2] <- net[2,3] <- 1
  expect_warning(
    r <- get.edge.attribute(net, "no.such.attribute")
  )
  expect_null(r)
})

test_that("getting non-existent network attribute returns (vector of) NAs with a warning", {
  net <- network.initialize(4)
  net[1,2] <- 1
  expect_warning(
    r <- get.network.attribute(net, "no.such.attribute")
  )
  expect_null(r)
})
