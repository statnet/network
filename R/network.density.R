#' Compute the Density of a Network
#' 
#' \code{network.density} computes the density of its argument.
#' 
#' The density of a network is defined as the ratio of extant edges to
#' potential edges. We do not currently consider edge values; missing edges are
#' omitted from extent (but not potential) edge count when
#' \code{na.omit==TRUE}.
#' 
#' @param x an object of class \code{network}
#' @param na.omit logical; omit missing edges from extant edges when assessing
#' density?
#' @param discount.bipartite logical; if \code{x} is bipartite, should
#' \dQuote{forbidden} edges be excluded from the count of potential edges?
#' @return The network density.
#' @section Warning : \code{network.density} relies on network attributes (see
#' \link{network.indicators}) to determine the properties of the underlying
#' network object.  If these are set incorrectly (e.g., multiple edges in a
#' non-multiplex network, network coded with directed edges but set to
#' \dQuote{undirected}, etc.), surprising results may ensue.
#' @author Carter T. Butts \email{buttsc@@uci.edu}
#' @seealso \code{\link{network.edgecount}}, \code{\link{network.size}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{http://www.jstatsoft.org/v24/i02/}
#' 
#' Wasserman, S. and Faust, K.  (1994).  \emph{Social Network Analysis: Methods
#' and Applications.} Cambridge: Cambridge University Press.
#' @keywords graphs
#' @examples
#' 
#' #Create an arbitrary adjacency matrix
#' m<-matrix(rbinom(25,1,0.5),5,5)
#' diag(m)<-0
#' 
#' g<-network.initialize(5)    #Initialize the network
#' network.density(g)          #Calculate the density
#' 
#' @export network.density
network.density<-function(x,na.omit=TRUE,discount.bipartite=FALSE){
  if(!is.network(x))
    stop("network.density requires a network object.")
  if(network.size(x)==0){
    warning("Density is not well-defined for networks of order 0.")
    return(NaN)
  }
  if(is.multiplex(x))
    warning("Network is multiplex - no general way to define density.  Returning value for a non-multiplex network (hope that's what you wanted).\n")
  ec<-network.edgecount(x,na.omit=na.omit)
  n<-network.size(x)
  bip<-x%n%"bipartite"
  if(is.hyper(x)){
    if((bip>=0)&&(discount.bipartite)){
      pe<-choose(bip,1:bip)*choose(n-bip,1:(n-bip))*(1+is.directed(x))
    }else{
      if(has.loops(x))
        pe<-sum(choose(n,1:n))^(1+is.directed(x))
      else
        pe<-sum(choose(n,1:n))/(1+!is.directed(x))
    }
  }else{
    if((bip>=0)&&(discount.bipartite)){
      pe<-bip*(n-bip)*(1+is.directed(x))
    }else{
      pe<-n*(n-1)/(1+!is.directed(x))+(has.loops(x)*network.size(x))
    }
  }  
  ec/pe
}
