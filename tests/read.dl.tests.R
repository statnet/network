# tests for the dl file format reader

# create tempfiles for testing
# these examples are from http://www.analytictech.com/networks/dataentry.htm

basicEdgelist<-tempfile('el',fileext='.dl')
cat("DL n=5
format = edgelist1
labels:
george, sally, jim, billy, jane
data:
1 2
1 3
2 3
3 1
4 3 
",file=basicEdgelist)

basicElNet<-read.dl(basicEdgelist)
if(!all(
  network.size(basicElNet)==5,
  network.edgecount(basicElNet)==5,
  all(as.matrix.network.edgelist(basicElNet)==structure(c(1L, 1L, 2L, 3L, 4L, 2L, 3L, 3L, 1L, 3L), .Dim = c(5L,  2L), n = 5, vnames = c("george", "sally", "jim", "billy", "jane" )))
  )){
  stop('read.dl did not parse dl file correctly for basicEdgelist test')
}

basicEdgelistEmbedded<-tempfile('elEmbedded',fileext='.dl')
cat("DL n=5
format = edgelist1
labels embedded:
data:
george sally
george jim
sally jim
billy george
jane jim
",file=basicEdgelistEmbedded)
basicElEmbedNet<-read.dl(basicEdgelistEmbedded)

if(!all(as.matrix(basicElEmbedNet)==structure(c(0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 1, 1, 0, 1, 0), .Dim = c(5L, 5L), .Dimnames = list(c("george",  "sally", "billy", "jane", "jim"), c("george", "sally", "billy",  "jane", "jim"))))){
  stop("read.dl did not parse dl file correctly for basicEdgelistEmbedded test")
}

basicEdgelistEmbeddedOrdered<-tempfile('elEmbedded',fileext='.dl')
cat("DL n=5
format = edgelist1
labels:
george, sally, jim, billy, jane
labels embedded:
data:
george sally
george jim
sally jim
billy george
jane jim 
",file=basicEdgelistEmbeddedOrdered)
basicElEmbedOrdNet<-read.dl(basicEdgelistEmbeddedOrdered)
if(!all(network.vertex.names(basicElEmbedOrdNet)==c("george","sally","jim","billy","jane"))){
  stop('read.dl did not prase dl file correctly to preserve id order of basicEdgelistEmbeddedOrdered example')
}

# read from a URL a DL file with slightly different header formatting

#proPro<-read.dl("http://moreno.ss.uci.edu/pro-pro.dat")

    
