#' Determine if specified vertices of a network have any edges (are not
#' isolates)
#' 
#' Returns a logical value for each specified vertex, indicating if it has any
#' incident (in or out) edges.  Checks all vertices by default
#' 
#' 
#' @aliases is.isolate
#' @param net a \code{\link{network}} object to be queried
#' @param v integer vector of vertex ids to check
#' @return returns a logical vector with the same length as v, with TRUE if the
#' vertex is involved in any edges, FALSE if it is an isolate.
#' @author skyebend
#' @examples
#' 
#' test<-network.initialize(5)
#' test[1,2]<-1
#' has.edges(test)
#' has.edges(test,v=5)
#' 
#' @export has.edges
has.edges<-function(net,v=seq_len(network.size(net))){
  if(network.size(net)==0){
    return(logical(0))
  }
  if(any(v < 1) | any(v > network.size(net))){
    stop("'v' argument must be a valid vertex id in is.isolate")
  }
  ins<-sapply(net$iel[v],length)
  outs<-sapply(net$oel[v],length)
  return(ins+outs != 0)
}
