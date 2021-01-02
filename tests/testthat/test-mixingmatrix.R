
# Naive mixing matrix
stupid_mm <- function(net, a, ...) {
  edb <- as.data.frame(net, unit="edges")
  vdb <- as.data.frame(net, unit="vertices")
  edb$.head.a <- vdb[[a]][match(edb$.head, vdb$vertex.names)]
  edb$.tail.a <- vdb[[a]][match(edb$.tail, vdb$vertex.names)]
  with(edb, table(From = .tail.a, To = .head.a, ...))
}

data(emon, package="network")
data(flo, package="network")
flonet <- as.network(flo, directed=FALSE)
set.seed(666)
flonet %v% "a" <- sample(c(1,2,NA), network.size(flonet), replace=TRUE)
# plot(flonet, vertex.col = "a")

# Directed networks -------------------------------------------------------




test_that("mixingmatrix() just works on a directed network", {
  mm <- mixingmatrix(emon$Texas, "Location")
  expect_type(mm$matrix, "integer")
  expect_identical(
    mm$matrix,
    stupid_mm(emon$Texas, "Location", exclude=NULL)
  )
})

test_that("directed: rows and cols for NA on attribute are always shown", {
  skip("Skip until #42 is resolved")
  mm <- mixingmatrix(emon$MtSi, "Formalization")
  expect_type(mm$matrix, "integer")
  expect_identical(
    mm$matrix,
    stupid_mm(emon$MtSi, "Formalization", exclude=NULL)
  )
})






# Undirected networks -----------------------------------------------------


test_that("mixingmatrix() just works on a undirected network", {
  net <- network.initialize(4, directed=FALSE)
  net[1,2] <- net[1,3] <- 1
  net %v% "a" <- c(1,1, 2,2)
  mm <- mixingmatrix(net, "a")
  expect_type(mm$matrix, "integer")
  expect_identical(
    mm$matrix,
    structure(
      matrix(as.integer(c(1,1,1,0)), 2, 2),
      dimnames = list(From = 1:2, To = 1:2),
      class = "table"
    )
  )
})


test_that("undirected: rows and cols for NA on attribute are always shown", {
  skip("Skip until #42 is resolved")
  mm <- mixingmatrix(flonet, "a")
  expect_type(mm$matrix, "integer")
  expect_identical(
    mm$matrix,
    stupid_mm(flonet, "a", exclude=NULL)
  )
})


# Bipartite networks ------------------------------------------------------


test_that("mixingmatrix() just works on a bipartite network", {
  am <- matrix(0, 5, 5)
  am[1,3] <- am[2,4] <- 1
  net <- as.network(am, directed=FALSE, bipartite=3)
  net %v% "mode" <- c(1,1,2,2,2)
  net %v% "a" <- c(1,2,1,2,1,2)
  mm <- mixingmatrix(net, "mode")
  expect_type(mm$matrix, "integer")
  expect_identical(
    mm$matrix,
    structure(
      matrix(c(2L, 0L), 2, 1),
      dimnames = list(From = 1:2, To=2),
      class = "table"
    )
  )
})







# Testing new mixingmatrix() ----------------------------------------------
# 
# As per statnet/network#32

data(flo, package="network")
net <- as.network(flo, directed=FALSE)
set.seed(666)
net %v% "a" <- sample(c(1,2,NA), network.size(net), replace=TRUE)
mixingmatrix(net, "a")

dinet <- as.network(flo, directed=TRUE)
set.seed(666)
dinet %v% "a" <- sample(c(1,2,NA), network.size(dinet), replace=TRUE)
mm <- mixingmatrix(dinet, "a")
