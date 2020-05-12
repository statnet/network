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
