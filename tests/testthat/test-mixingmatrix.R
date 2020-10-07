data(emon, package="network")
data(flo, package="network")
flonet <- as.network(flo, directed=FALSE)
set.seed(666)
flonet %v% "a" <- sample(c(1,2,NA), network.size(flonet), replace=TRUE)
# plot(flonet, vertex.col = "a")




# Directed networks -------------------------------------------------------

test_that("mixingmatrix() just works on a directed network", {
  expect_silent(
    mm <- mixingmatrix(emon$Texas, "Location")
  )
  expect_type(mm, "integer")
  expect_s3_class(mm, c("mixingmatrix", "table"), exact=TRUE)
  expect_equivalent(
    mm,
    structure(c(18L, 35L, 5L, 38L, 76L, 1L, 9L, 3L, 1L), .Dim = c(3L, 3L), 
              class = c("mixingmatrix", "table"), directed = TRUE, bipartite = FALSE)
  )
  expect_true(attr(mm, "directed"))
  expect_true(is.directed(mm))
  expect_false(attr(mm, "bipartite"))
  expect_false(is.bipartite(mm))
})

test_that("directed: rows and cols for NA on attribute are always shown", {
  skip("Wait and update when #42 is resolved")
  mm <- mixingmatrix(emon$MtSi, "Formalization")
  expect_type(mm$matrix, "integer")
  expect_identical(
    mm$matrix,
    stupid_mm(emon$MtSi, "Formalization", exclude=NULL)
  )
  
  net <- network.initialize(2, directed=TRUE)
  net %v% "a" <- c(1,NA)
  net[1,2] <- 1
  mm <- mixingmatrix(net, "a")
  expect_type(mm$matrix, "integer")
  expect_identical(
    mm$matrix,
    structure(
      matrix(as.integer(c(0, 1), 1, 2)),
      dimnames = list(From=1, To=c("1", "<NA>")),
      class = "table"
    )
  )
  
})





# Undirected networks -----------------------------------------------------


test_that("mixingmatrix() just works on a undirected network", {
  net <- network.initialize(4, directed=FALSE)
  net[1,2] <- net[1,3] <- 1
  net %v% "a" <- c(1,1, 2,2)
  mm <- mixingmatrix(net, "a")
  expect_type(mm, "integer")
  expect_equivalent(
    mm,
    structure(
      matrix(as.integer(c(1,1,1,0)), 2, 2),
      dimnames = list(From = 1:2, To = 1:2),
      class = c("mixingmatrix", "table")
    )
  )
  expect_false(attr(mm, "directed"))
  expect_false(is.directed(mm))
  expect_false(attr(mm, "bipartite"))
  expect_false(is.bipartite(mm))
})


test_that("undirected: rows and cols for NA on attribute are always shown", {
  skip("Wait and update when #42 is resolved")
  mm <- mixingmatrix(flonet, "a")
  expect_type(mm$matrix, "integer")
  expect_identical(
    mm$matrix,
    stupid_mm(flonet, "a", exclude=NULL)
  )
})


# Bipartite networks ------------------------------------------------------

am <- matrix(0, 5, 5)
am[1,3] <- am[2,4] <- 1
net <- as.network(am, directed=FALSE, bipartite=2)
# Mode attribute matches the partition
net %v% "mode" <- c("circle", "square")[c(1,1,2,2,2)]
# Attribute 'a' defined for all nodes in both partitions
net %v% "common" <- c("black", "red")[c(1,2,1,2,1,2)]
# plot(net, vertex.col="a", vertex.cex = 3, vertex.sides = (net %v% "mode") * 4)
# Attribute present only for circle nodes
set.vertex.attribute(net, "radius", 1:2, 1:2)
# Attribute present only for square nodes
set.vertex.attribute(net, "side_length", c(2,3,2), 1:3)

test_that("mixingmatrix() just works on a bipartite network - partition attribute", {
  expect_silent(
    mm <- mixingmatrix(net, "mode")
  )
  expect_type(mm, "integer")
  expect_false(is.directed(mm))
  expect_true(is.bipartite(mm))
  expect_equivalent(
    mm,
    structure(matrix(2, 1, 1), class="mixingmatrix")
  )
})  


test_that("mixingmatrix() just works on a bipartite network - common attribute", {
  # Attribute matching the partition
  expect_silent(
    mm <- mixingmatrix(net, "common")
  )
  expect_type(mm, "integer")
  expect_false(is.directed(mm))
  expect_true(is.bipartite(mm))
  expect_equivalent(
    mm,
    structure(matrix(c(1,0,0,1), 2, 2), class="mixingmatrix")
  )
})  


test_that("mixingmatrix() just works on a bipartite network - two partition-specific attributes", {
  skip("Not yet implemented")
  expect_silent(
    mm <- mixingmatrix(net, c("radius", "side_length"))
  )
  expect_type(mm, "integer")
  expect_false(is.directed(mm))
  expect_true(is.bipartite(mm))
})
