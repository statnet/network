# Directed networks -------------------------------------------------------

test_that("mixingmatrix() just works on a directed network", {
  net <- network.initialize(4, directed=TRUE)
  net[1,2] <- net[3,4] <- 1
  net %v% "a" <- c(1,1,2,2)
  mm <- mixingmatrix(net, "a")
  expect_type(mm, "integer")
  expect_s3_class(mm, c("mixingmatrix", "table"), exact=TRUE)
  expect_true(is.directed(mm))
  expect_false(is.bipartite(mm))
})

test_that("mixingmatrix() works on emon$Texas (directed)", {
  data(emon, package="network")
  a <- get.vertex.attribute(emon$Texas, "Location")
  el <- as.matrix(emon$Texas, matrix.type="edgelist")
  emm <- table(From=a[el[,1]], To=a[el[,2]])
  expect_equivalent(
    as.integer(mixingmatrix(emon$Texas, "Location")),
    as.integer(emm)
  )
})

test_that("NA rows & cols are present for emon$MtSi unless useNA='no'", {
  mm.no <- mixingmatrix(emon$MtSi, "Formalization", useNA="no")
  expect_type(mm.no, "integer")
  expect_identical(dim(mm.no), c(2L,2L))
  
  mm.default <- mixingmatrix(emon$MtSi, "Formalization")
  mm.ifany <- mixingmatrix(emon$MtSi, "Formalization", useNA="ifany")
  mm.always <- mixingmatrix(emon$MtSi, "Formalization", useNA="always")
  expect_identical(mm.ifany, mm.default)
  expect_identical(mm.ifany, mm.always)
  expect_identical(dim(mm.ifany), c(3L, 3L))
  expect_identical(
    mm.default,
    structure(
      c(19L, 4L, 1L, 4L, 0L, 0L, 4L, 1L, 0L), 
      .Dim = c(3L,  3L), 
      .Dimnames = list(From = c("1", "2", NA), To = c("1", "2",  NA)), 
      class = c("mixingmatrix", "table"), 
      directed = TRUE, 
      bipartite = FALSE
    )
  )
} )

test_that("mixingmatrix(directed with categories without incident ties)", {
  net <- network.initialize(4, directed = TRUE)
  net %v% "a" <- c(1,1,2,3)
  net[1,2] <- net[1,3] <- 1 # no ties incident on a=3
  mm <- mixingmatrix(net, "a")
  expect_type(mm, "integer")
  expect_equivalent(
    mm,
    structure(
      matrix(as.integer(c(1,0,0, 1,0,0, 0,0,0)), 3, 3),
      dimnames = list(From=1:3, To=1:3),
      class = c("mixingmatrix", "table")
    )
  )
})


test_that("mixingmatrx() warns on exclude=NULL", {
  net <- network.initialize(4, directed=TRUE)
  net[1,2] <- net[3,4] <- 1
  net %v% "a" <- c(1,1,2,2)
  expect_warning(
    r <- mixingmatrix(net, "a", exclude=NULL),
    regexp = "passing `exclude=NULL`"
  )
  expect_identical(r, mixingmatrix(net, "a"))
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
  expect_false(is.directed(mm))
  expect_false(is.bipartite(mm))
})


test_that("NA rows & cols are shown for undirected net unless useNA='no'", {
  net <- network.initialize(2, directed=FALSE)
  net %v% "a" <- c(1, NA)
  net[1,2] <- 1

  mm.default <- mixingmatrix(net, "a")
  mm.ifany <- mixingmatrix(net, "a", useNA="ifany")
  mm.always <- mixingmatrix(net, "a", useNA="always")
  expect_identical(mm.default, mm.ifany)
  expect_identical(mm.default, mm.always)
  expect_identical(
    mm.default,
    structure(
      c(0L, 1L, 1L, 0L),
      .Dim = c(2L, 2L),
      class = c("mixingmatrix",  "table"),
      .Dimnames = list(From = c("1", NA), To = c("1", NA)),
      directed = FALSE, 
      bipartite = FALSE
    )
  )

  mm.no <- mixingmatrix(net, "a", useNA="no")
  expect_type(mm.no, "integer")
  expect_identical(dim(mm.no), c(1L, 1L))
})



# Bipartite networks ------------------------------------------------------

am <- matrix(0, 5, 5)
am[1,3] <- am[1,4] <- am[2,3] <- am[2,5] <-  1
net <- as.network(am, directed=FALSE, bipartite=2)
net %v% "mode" <- c(1,1,2,2,2)
net %v% "a" <- c(1,2,3,4,4)
net %v% "withNA" <- c(1,2,NA, 4,NA)
set.vertex.attribute(net, "p1", value = c(20, 30), v = 1:2)
set.vertex.attribute(net, "p2", value = c(0.1, 0.2, 0.1), v = 3:5)
# plot(net, vertex.col="mode", displaylabels=TRUE)

test_that("mixingmatrix for bipartite net with expand.bipartite=FALSE is correct", {
  # On `mode` so all ties between groups
  expect_silent(
    mm <- mixingmatrix(net, "mode", expand.bipartite = FALSE)
  )
  expect_type(mm, "integer")
  expect_false(is.directed(mm))
  expect_true(is.bipartite(mm))
  expect_equivalent(
    mm,
    structure(
      matrix(4L, 1, 1),
      dimnames = list(From = 1, To = 2),
      class = "mixingmatrix"
    )
  )
  
  # On `a`
  expect_silent(
    mm <- mixingmatrix(net, "a", expand.bipartite = FALSE)
  )
  expect_type(mm, "integer")
  expect_false(is.directed(mm))
  expect_true(is.bipartite(mm))
  expect_equivalent(
    mm,
    structure(
      matrix(as.integer(c(1,1, 1,1)), 2, 2),
      dimnames = list(From = 1:2, To=3:4),
      class = "mixingmatrix"
    )
  )
})


test_that("mixingmatrix for bipartite net with expand.bipartite=TRUE is correct", {
  # On `mode`
  expect_silent(
    mm <- mixingmatrix(net, "mode", expand.bipartite = TRUE)
  )
  expect_type(mm, "integer")
  expect_equivalent(
    mm,
    structure(
      matrix(as.integer(c(0,0, 4,0)), 2, 2),
      dimnames = list(From = 1:2, To=1:2),
      class = "mixingmatrix"
    )
  )
  # On `a`
  expect_silent(
    mm <- mixingmatrix(net, "a", expand.bipartite = TRUE)
  )
  expect_identical(dim(mm), c(4L, 4L))
  expect_identical(
    as.integer(mm),
    as.integer(c(0,0,0,0, 0,0,0,0, 1,1,0,0, 1,1,0,0))
  )
})


test_that("NA rows & cols are shown for bipartite net unless useNA='no'", {
  expect_silent(
    mm.default <- mixingmatrix(net, "withNA")
  )
  expect_silent(
    mm.no <- mixingmatrix(net, "withNA", useNA="no")
  )
  expect_silent(
    mm.always <- mixingmatrix(net, "withNA", useNA="always")
  )
  expect_identical(mm.default, mm.always)
  expect_identical(
    as.integer(mm.default),
    as.integer(c(1,0,0, 1,2,0))
  )
  expect_identical(dim(mm.no), c(2L, 1L))
  expect_identical(
    as.integer(mm.no),
    as.integer(c(1, 0))
  )
})
