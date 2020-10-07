data(emon, package="network")


# Directed networks -------------------------------------------------------

test_that("mixingmatrix() just works on a directed network", {
  net <- network.initialize(4, directed=TRUE)
  net[1,2] <- net[3,4] <- 1
  net %v% "a" <- c(1,1,2,2)
  expect_identical(
    mixingmatrix(net, "a")$matrix,
    structure(
      matrix(as.integer(c(1,0,0,1)), 2, 2),
      dimnames = list(From=1:2, To=1:2),
      class = "table"
    )
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

test_that("mixingmatrix() works on emon$Texas (directed)", {
  data(emon, package="network")
  a <- get.vertex.attribute(emon$Texas, "Location")
  el <- as.matrix(emon$Texas, matrix.type="edgelist")
  emm <- table(From=a[el[,1]], To=a[el[,2]])
  expect_identical(
    mixingmatrix(emon$Texas, "Location")$matrix,
    emm
  )
})



test_that("directed: rows and cols for NA on attribute are always shown", {
  skip("Wait and update when #42 is resolved")
  mm <- mixingmatrix(emon$MtSi, "Formalization")
  expect_type(mm$matrix, "integer")
  expect_identical(
    mm$matrix,
    stupid_mm(emon$MtSi, "Formalization")
  )
} )

test_that("mixingmatrix(directed with categories without incident ties)", {
  net <- network.initialize(4, directed = TRUE)
  net %v% "a" <- c(1,1,2,3)
  net[1,2] <- net[1,3] <- 1 # no ties incident on a=3
  mm <- mixingmatrix(net, "a")
  expect_type(mm$matrix, "integer")
  expect_identical(
    mm$matrix,
    structure(
      matrix(as.integer(c(1,0,0, 1,0,0, 0,0,0)), 3, 3),
      dimnames = list(From=1:3, To=1:3),
      class = "table"
    )
  )
})


test_that("mixingmatrx() responds correctly to useNA= and exclude=NULL", {
  net <- network.initialize(2, directed=TRUE)
  net %v% "a" <- c(1,NA)
  net[1,2] <- 1
  for(useNA in c("no", "always", "ifany")) {
    mm <- mixingmatrix(net, "a", useNA = useNA)
    expect_type(mm$matrix, "integer")
    expect_identical(
      mm$matrix,
      switch(
        useNA,
        no = structure(
          matrix(as.integer(0), 1, 1),
          dimnames = list(From=1, To=1),
          class = "table"
        ),
        always = structure(
          matrix(as.integer(c(0,0, 1,0)), 2, 2),
          dimnames = list(From=c(1,NA), To=c(1, NA)),
          class = "table"
        ),
        ifany = structure(
          matrix(as.integer(c(0, 1)), 1, 2),
          dimnames = list(From=1, To=c(1, NA)),
          class = "table"
        )
      )
    )
  }
  expect_identical(
    mixingmatrix(net, "a", exclude=NULL)$matrix,
    structure(
      matrix(as.integer(c(0, 1)), 1, 2),
      dimnames = list(From=1, To=c(1, NA)),
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
  net <- network.initialize(2, directed=FALSE)
  net %v% "a" <- c(1, NA)
  net[1,2] <- 1
  mm <- mixingmatrix(net, "a")
  expect_type(mm$matrix, "integer")
  expect_identical(
    mm$matrix,
    structure(
      matrix(as.integer(c(1,1,0, 1,0,0, 0,0,0)), 3, 3),
      dimnames = list(From=1:3, To=1:3),
      class = "table"
    )
  )
})




net <- network.initialize(2, directed=FALSE)
net %v% "a" <- c(1,NA)
net[1,2] <- 1
for(useNA in c("no", "always")) {
  test_that(
    paste0("mixingmatrx(undir net with NA on attribute) responds correctly to useNA=", sQuote(useNA)),
    {
      expect_silent(
        mm <- mixingmatrix(net, "a", useNA = useNA)
      )
      # Expected output
      emm <- switch(
        useNA,
        no = structure(
          matrix(as.integer(0), 1, 1),
          dimnames = list(From=1, To=1),
          class = "table"
        ),
        always = structure(
          matrix(as.integer(c(0,1, 1,0)), 2, 2),
          dimnames = list(From=c(1,NA), To=c(1, NA)),
          class = "table"
        )
      )
      expect_type(mm$matrix, "integer")
      expect_identical(
        mm$matrix,
        emm
      )
    }
  )
}
test_that("mixingmatrx(undir net with NA on attributes) responds correctly to exclude=NULL", {
  # Expected: matri with row for 1 and column for NA
  a <- get.vertex.attribute(net, "a")
  el <- as.matrix(net, matrix.type="edgelist")
  emm <- table(From=a[el[,1]], To=a[el[,2]], exclude=NULL)
  expect_silent(
    mm <- mixingmatrix(net, "a", exclude=NULL)
  )
  expect_identical(mm$matrix, emm)
})










# Bipartite networks ------------------------------------------------------

am <- matrix(0, 5, 5)
am[1,3] <- am[1,4] <- am[2,3] <- am[2,5] <-  1
net <- as.network(am, directed=FALSE, bipartite=2)
net %v% "mode" <- c(1,1,2,2,2)
net %v% "a" <- c(1,2,3,4,4)
# plot(net, vertex.col="mode")

test_that("mixingmatrix(bipartite with heter value sets, expand.bipartite=FALSE)", {
  # On `mode`
  expect_silent(
    mm <- mixingmatrix(net, "mode", expand.bipartite = FALSE)
  )
  expect_type(mm$matrix, "integer")
  expect_identical(
    mm$matrix,
    structure(
      matrix(4L, 1, 1),
      dimnames = list(From = 1, To = 2),
      class = "table"
    )
  )
  # On `a`
  expect_silent(
    mm <- mixingmatrix(net, "a", expand.bipartite = FALSE)
  )
  expect_type(mm$matrix, "integer")
  expect_identical(
    mm$matrix,
    structure(
      matrix(as.integer(c(1,1, 1,1)), 2, 2),
      dimnames = list(From = 1:2, To=3:4),
      class = "table"
    )
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
})


test_that("mixingmatrix(bipartite with heter value sets, expand.bipartite=FALSE)", {
  # On `mode`
  expect_silent(
    mm <- mixingmatrix(net, "mode", expand.bipartite = TRUE)
  )
  expect_type(mm$matrix, "integer")
  expect_identical(
    mm$matrix,
    structure(
      matrix(as.integer(c(0,0, 4,0)), 2, 2),
      dimnames = list(From = 1:2, To=1:2),
      class = "table"
    )
  )
  # On `a`
  expect_silent(
    mm <- mixingmatrix(net, "a", expand.bipartite = TRUE)
  )
})


test_that("mixingmatrix(bipartite with categories without incident ties)", {
  am <- matrix(0, 5, 5)
  am[1,3] <- am[2,4] <- 1
  net <- as.network(am, directed=FALSE, bipartite=2)
  net %v% "a" <- c(1,2,30,40,99)
  expect_silent(
    mm <- mixingmatrix(net, "a")
  )
  expect_type(mm, "integer")
  expect_false(is.directed(mm))
  expect_true(is.bipartite(mm))
})
