# ----- checks for network edgecount ------

test<-network.initialize(4)
# directed
expect_equal(network.dyadcount(test),12)
# undirected
test%n%'directed'<-FALSE
expect_equal(network.dyadcount(test),6)

# loops allowed
test%n%'loops'<-TRUE
#undirected
expect_equal(network.dyadcount(test),10)
# directed
test%n%'directed'<-TRUE
expect_equal(network.dyadcount(test),16)

# directed bipartite
test%n%'loops'<-FALSE
test%n%'bipartite'<-1
expect_equal(network.dyadcount(test),6)

# undirected bipartite
test%n%'directed'<-FALSE
expect_equal(network.dyadcount(test),3)

# NA values
test[1,2]<-NA
expect_equal(network.dyadcount(test,na.omit = TRUE),2)


# ----- checks for dyads eids -----

data(emon)
el<-as.matrix.network.edgelist(emon[[1]])
expect_equal(get.dyads.eids(emon[[1]],el[,1],el[,2]),as.list(1:83))
expect_equal(get.dyads.eids(emon[[1]],el[5:10,1],el[5:10,2]),as.list(5:10))
expect_error(get.dyads.eids(emon[[1]],1,2:3),regexp = 'heads and tails vectors must be the same length')
expect_error(get.dyads.eids(network.initialize(0),1,2),regexp = 'invalid vertex id in heads or tails vector')

mult<-network.initialize(5,multi=TRUE)
add.edges(mult,1,2)
add.edges(mult,1,2)
expect_warning(expect_true(is.na(get.dyads.eids(mult,1,2)[[1]])),regexp = 'multiple edge ids for dyad')

expect_equal(get.dyads.eids(network.initialize(0),numeric(0),numeric(0)), list())
expect_equal(get.dyads.eids(network.initialize(5),tails=1:2,heads=3:4),list(numeric(0),numeric(0)))

# check oposite matching for undirected nets
undir<-network.initialize(3,directed=FALSE)
undir[1,2]<-1
expect_equal(get.dyads.eids(undir,2,1),list(1))
expect_equal(get.dyads.eids(undir,1,2),list(1))


undir%n%'directed'<-TRUE
expect_equal(get.dyads.eids(undir,2,1),list(integer(0)))
expect_equal(get.dyads.eids(undir,1,2),list(1))

expect_equal(get.dyads.eids(undir,2,1,neighborhood='in'),list(1))
expect_equal(get.dyads.eids(undir,1,2,neighborhood='in'),list(integer(0)))

nw <- network.initialize(10, directed = FALSE)
el <- matrix(c(1,2,3,5,2,9,9,10,6,7),ncol=2,byrow=TRUE)
nw[el]<-1
expect_identical(get.dyads.eids(nw, el[,1], el[,2], na.omit = FALSE), as.list(seq_len(NROW(el))))
expect_identical(get.dyads.eids(nw, el[,1], el[,2], na.omit = TRUE), as.list(seq_len(NROW(el))))
nw[el[2,1],el[2,2]] <- NA
nw[el[5,1],el[5,2]] <- NA
expect_identical(get.dyads.eids(nw, el[,1], el[,2], na.omit = FALSE), as.list(seq_len(NROW(el))))
expect_identical(get.dyads.eids(nw, el[,1], el[,2], na.omit = TRUE), list(1L, integer(0), 3L, 4L, integer(0)))
delete.edges(nw, 2)
expect_identical(get.dyads.eids(nw, el[,1], el[,2], na.omit = FALSE), list(1L, integer(0), 3L, 4L, 5L))
expect_identical(get.dyads.eids(nw, el[,1], el[,2], na.omit = TRUE), list(1L, integer(0), 3L, 4L, integer(0)))
delete.edges(nw, 3)
expect_identical(get.dyads.eids(nw, el[,1], el[,2], na.omit = FALSE), list(1L, integer(0), integer(0), 4L, 5L))
expect_identical(get.dyads.eids(nw, el[,1], el[,2], na.omit = TRUE), list(1L, integer(0), integer(0), 4L, integer(0)))
