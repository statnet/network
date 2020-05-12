######################################################################
#
# misc.R
#
# Written by Carter T. Butts <buttsc@uci.edu>; portions contributed by
# David Hunter <dhunter@stat.psu.edu> and Mark S. Handcock
# <handcock@u.washington.edu>.
#
# Last Modified 02/26/13
# Licensed under the GNU General Public License version 2 (June, 1991)
# or greater
#
# Part of the R/network package
#
# This file contains various network routines which don't fit anywhere
# else (generally, utilities and the like).
#
# Contents:
#
#   as.color
#   mixingmatrix
#   network.density
#   is.color
#   is.discrete
#   is.discrete.character
#   is.discrete.numeric
#   print.mixingmatrix
#   which.matrix.type
#
######################################################################


# Given a vector of non-colors, try to coerce them into some reasonable color
# format.  This may not work well, but what the hell....




#' Internal Network Package Functions
#' 
#' Internal network functions.
#' 
#' Most of these are not to be called by the user.
#' 
#' @name network-internal
#' 
#' @aliases + - * +.default -.default *.default summary.character
#' print.summary.character
#' @param x an object to be designated either discrete or continuous, or a
#' network.
#' @param y a network or something coercible to one.
#' @param \dots further arguments passed to or used by methods.
#' @seealso network
#' @keywords internal



# Return the density of the given network.  (This probably won't stay in
# this package....
#

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

# has.edges  checks if any of the specified vertex ids have edges (are not isolates)


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


#Returns TRUE if x is a character in a known color format
#' @rdname as.color
#' @export
is.color<-function(x){
  xic<-rep(FALSE,length(x))         #Assume not a color by default
  
  xc<-sapply(x,is.character)        #Must be a character string
  #For characters, must be a named color or a #RRGGBB/#RRGGBBAA sequence
  xic[xc]<-(x[xc]%in%colors())| ((nchar(x[xc])%in%c(7,9))&(substr(x[xc],1,1)=="#"))
  xic[is.na(x)]<-NA                 #Missing counts as missing
  #Return the result
  xic
}

#' @rdname network-internal
#' @export
is.discrete.numeric<-function(x){
 (is.numeric(x)|is.logical(x)) && mean(duplicated(x)) > 0.8
}

#' @rdname network-internal
#' @export
is.discrete.character<-function(x){
 (is.character(x)|is.logical(x)) && mean(duplicated(x)) > 0.8
}

#' @rdname network-internal
#' @export
is.discrete<-function(x){
 (is.numeric(x)|is.logical(x)|is.character(x)) && mean(duplicated(x)) > 0.8
}






#' Heuristic Determination of Matrix Types for Network Storage
#' 
#' \code{which.matrix.type} attempts to choose an appropriate matrix expression
#' for a \code{network} object, or (if its argument is a matrix) attempts to
#' determine whether the matrix is of type adjacency, incidence, or edgelist.
#' 
#' The heuristics used to determine matrix types are fairly arbitrary, and
#' should be avoided where possible.  This function is intended to provide a
#' modestly intelligent fallback option when explicit identification by the
#' user is not possible.
#' 
#' @param x a matrix, or an object of class \code{network}
#' @return One of \code{"adjacency"}, \code{"incidence"}, or \code{"edgelist"}
#' @author David Hunter \email{dhunter@@stat.psu.edu}
#' @seealso \code{\link{as.matrix.network}}, \code{\link{as.network.matrix}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{http://www.jstatsoft.org/v24/i02/}
#' @keywords graphs
#' @examples
#' 
#'   #Create an arbitrary adjacency matrix
#'   m<-matrix(rbinom(25,1,0.5),5,5)
#'   diag(m)<-0
#' 
#'   #Can we guess the type?
#'   which.matrix.type(m)
#' 
#'   #Try the same thing with a network
#'   g<-network(m)
#'   which.matrix.type(g)
#'   which.matrix.type(as.matrix.network(g,matrix.type="incidence"))
#'   which.matrix.type(as.matrix.network(g,matrix.type="edgelist"))
#' 
#' @export which.matrix.type
which.matrix.type<-function(x)
{
    if (!is.network(x)) {
        if (is.character(x<-as.matrix(x))){ 
          if (diff(dim(x))==0)
            out<-"adjacency"
          else if (dim(x)[2]==2)
            out<-"edgelist"
          else
            out<-"bipartite"
        }else if (!is.numeric(x))  
            out<-NA
        else if (diff(dim(x))==0)  
            out<-"adjacency"
        else if (max(abs(x),na.rm=TRUE)==1 && max(abs(x-as.integer(x)),na.rm=TRUE)==0)
            out<-"bipartite"
        else if (max(abs(x-as.integer(x))[,1:2],na.rm=TRUE)==0 && min(x[,1:2],na.rm=TRUE)>0)
            out<-"edgelist"
        else
            out<-NA
    }
    else {  # Very ad-hoc criteria for choosing; choice can be overridden.
	if (is.hyper(x))
            out<-"incidence"
        else if ((n<-x$gal$n)<14 || x$gal$mnext>n*n/2)
            out<-"adjacency"
        else
            out<-"edgelist"
    }
    out
}
