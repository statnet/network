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


#Given a vector of non-colors, try to coerce them into some reasonable
#color format.  This may not work well, but what the hell....


#' Transform vector of values into color specification
#' 
#' Convenience function to convert a vector of values into a color
#' specification.
#' 
#' Behavior of \code{as.color} is as follows: \itemize{ \item integer numeric
#' values: unchanged, (assumed to corespond to values of R's active
#' \code{\link{palette}}) \item integer real values: will be translated to into
#' grayscale values ranging between the max and min \item factor: integer
#' values corresponding to factor levels will be used \item character: if
#' values are valid colors (as determined by \code{is.color}) they will be
#' returned as is.  Otherwise converted to factor and numeric value of factor
#' returned. }
#' 
#' The optional \code{opacity} parameter can be used to make colors partially
#' transparent (as a shortcut for \code{\link{adjustcolor}}.  If used, colors
#' will be returned as hex rgb color string (i.e. \code{"#00FF0080"})
#' 
#' The \code{is.color} function checks if each character element of \code{x}
#' appears to be a color name by comparing it to \code{\link{colors}} and
#' checking if it is an HTML-style hex color code.  Note that it will return
#' FALSE for integer values.
#' 
#' These functions are used for the color parameters of
#' \code{\link{plot.network}}.
#' 
#' @param x vector of numeric, character or factor values to be transformed
#' @param opacity optional numeric value in the range 0.0 to 1.0 used to
#' specify the opacity/transparency (alpha) of the colors to be returned. 0
#' means fully opaque, 1 means fully transparent.
#' @return For \code{as.color}, a vector integer values (corresponding to color
#' palette values) or character color name. For \code{is.color}, a logical
#' vector indicating if each element of x appears to be a color
#' @examples
#' 
#' 
#' as.color(1:3)
#' as.color(c('a','b','c'))
#' 
#' # add some transparency
#' as.color(c('red','green','blue'),0.5) # gives "#FF000080", "#00FF0080", "#0000FF80"
#' 
#' is.color(c('red',1,'foo',NA,'#FFFFFF55'))
#' 
#' @export as.color
as.color<-function(x,opacity=1.0){
  if(opacity > 1 | opacity < 0){
    stop('opacity parameter must be a numeric value in the range 0 to 1')
  }
  colors<-x
  #Numeric rule: if integer leave as-is, otherwise convert to grayscale
  if(is.numeric(x)){
    if(any(x!=round(x),na.rm=TRUE)){
      colors<-gray((x-min(x))/(max(x)-min(x)))
    }else
      colors<-x
  }
  #Factor rule: categorical colorings
  if(is.factor(x)){
    colors<-match(levels(x)[x],levels(x))
  }
  #Character rule: if colors, retain as colors; else categorical
  if(is.character(x)){
    if(all(is.color(x)))
      colors<-x
    else{
      colors<-match(x,sort(unique(x)))
    }
  }
  # add transparency if not 1
  if(opacity < 1){
    colors<-grDevices::adjustcolor(colors,alpha.f=opacity)
  }
  return(colors)
}


#Return the mixing matrix for a network object, on a given attribute.  This
#is a relocated function from the ergm package; it probably belongs elsewhere,
#but is needed for the summary.network method (and in that sense is basic
#enough to include.
#' Internal Network Package Functions
#' 
#' Internal network functions.
#' 
#' Most of these are not to be called by the user.
#' 
#' @name network-internal
#' 
#' @aliases + - * +.default -.default *.default summary.character
#' print.summary.character print.mixingmatrix
#' @param object a network or some other data structure for which a mixing
#' matrix is meaningful.
#' @param x an object to be designated either discrete or continuous, or a
#' network.
#' @param attrname a vertex attribute name.
#' @param y a network or something coercible to one.
#' @param \dots further arguments passed to or used by methods.
#' @seealso network
#' @keywords internal
#' @export
mixingmatrix <- function(object, ...) UseMethod("mixingmatrix")

#' @rdname network-internal
#' @export
mixingmatrix.network <- function(object, attrname, ...) {
  nw <- object
  if(missing(attrname)){
    stop("attrname argument is missing. mixingmatrix() requires an an attribute name")
  }
  if(network.size(nw)==0){
    warning("mixing matrices not well-defined for graphs with no vertices.")
    type<-"directed"
    if(is.bipartite(nw))
      type<-"bipartite"
    tabu<-matrix(nrow=0,ncol=0)
    ans<-list(type=type,matrix=tabu)
    class(ans)<-"mixingmatrix"
    return(ans)
  }
  nodecov <- unlist(get.vertex.attribute(nw, attrname))
  u<-sort(unique(nodecov))
  # nodecovnum <- match(nodecov, u)
  el <- as.matrix.network.edgelist(nw)
  type <- "directed"
  if (is.bipartite(nw)) { # must have heads < tails now
    if (is.directed(nw)) 
      cat("Warning:  Bipartite networks are currently\n",
          "automatically treated as undirected\n")
    type <- "bipartite"
    rowswitch <- apply(el, 1, function(x) x[1]>x[2])
    el[rowswitch, 1:2] <- el[rowswitch, 2:1]
    nb1 <- get.network.attribute(nw,"bipartite")
    From <- c(u, nodecov[el[,1]])
    To <- c(u, nodecov[el[,2]])
  }else{
    From <- c(u, nodecov[el[,1]])
    To <- c(u, nodecov[el[,2]])
  }
  tabu <- table(From, To)  # Add u,u diagonal to ensure each 
  # value is represented, then subtract it later
  diag(tabu) <- diag(tabu) - 1
  if(!is.directed(nw) && !is.bipartite(nw)){
    type <- "undirected"
    tabu <- tabu + t(tabu)
    diag(tabu) <- diag(tabu)/2
  }
  ans <- list(type=type, matrix=tabu)
  class(ans) <- "mixingmatrix"
  ans
}


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


#Print method for mixingmatrix objects
#' @export print.mixingmatrix
#' @export
print.mixingmatrix <- function(x, ...) {
  m <- x$mat
  rn <- rownames(m)
  cn <- colnames(m)  
  if (x$type == "undirected") {
    dimnames(m) <- list(rn, cn)
    cat("Note:  Marginal totals can be misleading\n",
        "for undirected mixing matrices.\n")
  } else {
    total <- apply(m,1,sum)
    m <- cbind(m,total)
    total <- apply(m,2,sum)
    m <- rbind(m,total)
    rn <- c(rn, "Total")
    cn <- c(cn, "Total")
    if (x$type == "bipartite")
      dimnames(m) <- list(B1 = rn,B2 = cn)
    else
      dimnames(m) <- list(From = rn,To = cn)
  }
  print(m)
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
