# logical isolated vertices in a network.
#
is.isolated<-function(x){
  #Check to be sure we were called with a network
  if(!is.network(x))
    stop("is.isolated requires an argument of class network.")

  xedges <- as.vector(as.matrix.network(x,matrix.type="edgelist"))
  !((1:network.size(x)) %in% xedges)
}
