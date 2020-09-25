context("test-mixingmatrix")

# Directed network
data(emon)
edb <- as.data.frame(emon$Texas, unit="edges")
vdb <- as.data.frame(emon$Texas, unit="vertices")
edb$.head.location <- vdb$Location[match(edb$.head, vdb$vertex.names)]
edb$.tail.location <- vdb$Location[match(edb$.tail, vdb$vertex.names)]
# +0 to have numeric not integer
emm <- with(edb, table(From = .tail.location, To = .head.location, exclude=NULL)) + 0
  

test_that("mixingmatrix() works on a directed network", {
  mm <- mixingmatrix(emon$Texas, "Location")
  expect_identical(
    mm$matrix,
    emm
  )
})


