# Directed networks

stupid_mm <- function(net, a, ...) {
  edb <- as.data.frame(net, unit="edges")
  vdb <- as.data.frame(net, unit="vertices")
  edb$.head.a <- vdb[[a]][match(edb$.head, vdb$vertex.names)]
  edb$.tail.a <- vdb[[a]][match(edb$.tail, vdb$vertex.names)]
  # +0 to have numeric not integer
  with(edb, table(From = .tail.a, To = .head.a, ...))
}

data(emon)
emm <- stupid_mm(emon$Texas, "Location", exclude=NULL)

test_that("mixingmatrix() works on a directed network", {
  mm <- mixingmatrix(emon$Texas, "Location")
  expect_identical(
    mm$matrix,
    emm
  )
})

# Undirected networks


