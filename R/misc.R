######################################################################
#
# misc.R
#
# Written by Carter T. Butts <buttsc@uci.edu>; portions contributed by
# David Hunter <dhunter@stat.psu.edu> and Mark S. Handcock
# <handcock@u.washington.edu>.
#
# Last Modified 06/08/21
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
#   is.discrete
#   is.discrete.character
#   is.discrete.numeric
#   which.matrix.type
#
######################################################################

#' Transform vector of values into color specification
#' 
#' Convenience function to convert a vector of values into a color
#' specification.
#' 
#' @param x vector of numeric, character or factor values to be transformed
#' @param opacity optional numeric value in the range 0.0 to 1.0 used to specify
#'   the opacity/transparency (alpha) of the colors to be returned. 0 means
#'   fully opaque, 1 means fully transparent.
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
#' @return For \code{as.color}, a vector integer values (corresponding to color
#'   palette values) or character color name. For \code{is.color}, a logical
#'   vector indicating if each element of x appears to be a color
#' 
#' @rdname as.color
#' @export
#' 
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


# Mixing matrix -----------------------------------------------------------

#' Mixing matrix
#' 
#' Return the mixing matrix for a network, on a given attribute.
#' 
#' @param object a network or some other data structure for which a mixing
#'   matrix is meaningful.
#' @param ... further arguments passed to or used by methods.
#' 
#' @rdname mixingmatrix
#' @export

mixingmatrix <- function(object, ...) UseMethod("mixingmatrix")


# Return the mixing matrix for a network object, on a given attribute.  This is
# a relocated function from the ergm package; it probably belongs elsewhere, but
# is needed for the summary.network method (and in that sense is basic enough to
# include.




#' @rdname mixingmatrix
#'
#' @param attrname a vertex attribute name.
#' @param expand.bipartite logical; if `object` is bipartite, should we return
#'   the *square* mixing matrix representing every level of `attrname` against
#'   every other level, or a *rectangular* matrix considering only levels
#'   present in each bipartition?
#' @param useNA one of "ifany", "no" or "always". Argument passed to
#'   \code{\link{table}}. By default (\code{useNA = "ifany"}) if there are any
#'   \code{NA}s on the attribute corresponding row \emph{and} column will be
#'   contained in the result. See Details.
#' @param ... arguments passed to \code{\link{table}}.
#'
#' @details Handling of missing values on the attribute \code{attrname} almost
#'   follows similar logic to \code{\link{table}}. If there are \code{NA}s on
#'   the attribute and \code{useNA="ifany"} (default) the result will contain
#'   both row and column for the missing values to ensure the resulting matrix
#'   is square (essentially calling \code{\link{table}} with
#'   \code{useNA="always"}). Also for that reason passing \code{exclude}
#'   parameter with \code{NULL}, \code{NA} or \code{NaN} is ignored with a
#'   warning as it may break the symmetry.
#'
#' @return Function `mixingmatrix()` returns an object of class `mixingmatrix`
#'   extending `table` with a cross-tabulation of edges in the `object`
#'   according to the values of attribute `attrname` for the two incident
#'   vertices. If `object` is a *directed* network rows correspond to the "tie
#'   sender" and columns to the "tie receiver". If `object` is an *undirected*
#'   network there is no such distinction and the matrix is symmetrized. In both
#'   cases the matrix is square and all the observed values of the attribute
#'   `attrname` are represented in rows and columns. If `object` is a
#'   *bipartite* network and `expand.bipartite` is `FALSE` the resulting matrix
#'   does not have to be square as only the actually observed values of the
#'   attribute are shown for each partition, if `expand.bipartite` is `TRUE` the
#'   matrix will be square.
#'
#' @export
#' @examples
#' # Interaction ties between Lake Pomona SAR organizations by sponsorship type
#' # of tie sender and receiver (data from Drabek et al. 1981)
#' data(emon)
#' mixingmatrix(emon$LakePomona, "Sponsorship")
mixingmatrix.network <- function(object, attrname, useNA = "ifany", expand.bipartite=FALSE, ...) {
  nw <- object
  if(missing(attrname)){
    stop("attrname argument is missing. mixingmatrix() requires an an attribute name")
  }
  if(!(attrname %in% list.vertex.attributes(object)))
    stop("vertex attribute ", sQuote(attrname), " not found in network ",
         sQuote(deparse(substitute(object))))
  if(network.size(nw)==0L){
    warning("mixing matrices not well-defined for graphs with no vertices.")
    return(as.mixingmatrix(
      matrix(nrow=0L, ncol=0L),
      directed = is.directed(object),
      bipartite = is.bipartite(object)
    ))
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
    rowswitch <- apply(el, 1L, function(x) x[1L]>x[2L])
    el[rowswitch, 1L:2L] <- el[rowswitch, 2L:1L]
    nb1 <- get.network.attribute(nw,"bipartite")
    if(!expand.bipartite) u <- sort(unique(nodecov[1L:nb1]))
    From <- factor(nodecov[el[,1L]], levels=u)
    if(!expand.bipartite) u <- sort(unique(nodecov[(nb1+1L):network.size(nw)]))
    To <- factor(nodecov[el[,2L]], levels=u)
  }else{
    From <- factor(nodecov[el[,1L]], levels=u)
    To <- factor(nodecov[el[,2L]], levels=u)
  }
  if(any(is.na(nodecov)) && useNA == "ifany") useNA <- "always"
  dots <- list(...)
  if("exclude" %in% names(dots) && (is.null(dots$exclude) | any(is.na(dots$exclude)) | any(is.nan(dots$exclude)))) {
    warning("passing `exclude=NULL` to table() is not supported, ignoring")
    dots$exclude <- NULL
  }
  tabu <- do.call(table, c(list(From=From, To=To, useNA=useNA), dots))
  if(!is.directed(nw) && !is.bipartite(nw)){
    type <- "undirected"
    tabu <- tabu + t(tabu)
    diag(tabu) <- diag(tabu)%/%2L
  }
  as.mixingmatrix(
    tabu,
    directed = is.directed(object),
    bipartite = is.bipartite(object)
  )
}

#' @rdname mixingmatrix
#' 
#' @note The `$` and `[[` methods are included only for backward-compatiblity
#'   reason and will become defunct in future releases of the package.
#' 
#' @export
"[[.mixingmatrix" <- function(x, ...) {
  .Deprecated(
    new = "mixingmatrix",
    msg = "Mixing matrix objects now extend class \"table\". The `[[` method is deprecated and will be removed from future releases of the package. See ?mixingmatrix for details."
  )
  x <- .to_oldmm(x)
  NextMethod()
}


#' @rdname mixingmatrix
#' 
#' @param name name of the element to extract, one of "matrix" or "type"
#'
#' @export
"$.mixingmatrix" <- function(x, name) {
  .Deprecated(
    new = "mixingmatrix",
    msg = "Mixing matrix objects now extend class \"table\". The `$` method is deprecated and will be removed from future releases of the package. See ?mixingmatrix for details."
  )
  x <- .to_oldmm(x)
  NextMethod()
}


.to_oldmm <- function(x) {
  directed <- attr(x, "directed")
  bipartite <- attr(x, "bipartite")
  list(
    matrix = structure(as.integer(x), dimnames=dimnames(x), dim=dim(x)),
    type = if(bipartite) "bipartite" else if(directed) "directed" else "undirected"
  )
}


# A non-exported constructor of mixingmatrix objects
# 
# @param mat matrix with the actual cross-tabulation
# @param directed logical if the network is directed
# @param bipartite logical if the netwoek is bipartite
# @param ... other arguments currently ignored
# 
# @return The matrix with attributes `directed` and `bipartite` of class
#   `mixingmatrix` inheriting from `table`.

as.mixingmatrix <- function(mat, directed, bipartite, ...) {
  # Test/check/symmetrize here?
  structure(
    mat,
    directed = directed,
    bipartite = bipartite,
    class = c("mixingmatrix", "table")
  )
}


#' @rdname mixingmatrix
#' 
#' @return Functions `is.directed()` and `is.bipartite()` return `TRUE` or
#'   `FALSE`. The values will be identical for the input network `object`.
#' 
#' @export
is.directed.mixingmatrix <- function(x, ...) attr(x, "directed")

#' @rdname mixingmatrix
#' @export
is.bipartite.mixingmatrix <- function(x, ...) attr(x, "bipartite")


#' @rdname mixingmatrix
#' 
#' @param x mixingmatrix object
#' 
#' @export
print.mixingmatrix <- function(x, ...) {
  m <- x
  rn <- rownames(x)
  cn <- colnames(x)  
  if (!attr(x, "directed")) {
    dimnames(m) <- list(rn, cn)
    on.exit(
      message("Note:  Marginal totals can be misleading for undirected mixing matrices.")  
    )
  } else {
    dimnames(m) <- if(attr(x, "bipartite")) list(B1 = rn, B2 = cn) else list(From = rn, To = cn)
    m <- stats::addmargins(m)
  }
  m <- structure(
    m,
    directed = attr(x, "directed"),
    bipartite = attr(x, "bipartite"),
    class = "table"
  )
  print(m)
}


# network.density ---------------------------------------------------------

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
#' \doi{10.18637/jss.v024.i02}
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
#' @rdname network.density
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

# has.edges ---------------------------------------------------------------

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
#' @rdname has.edges
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

# is.color ----------------------------------------------------------------

#' @rdname as.color
#' 
#' @return \code{as.color()} returns TRUE if x is a character in a known color format.
#' 
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



#' Internal Network Package Functions
#' 
#' Internal network functions.
#' 
#' Most of these are not to be called by the user.
#' 
#' @name network-internal
#' 
#' @param x an object to be designated either discrete or continuous, or a
#' network.
#' @param y a network or something coercible to one.
#' 
#' @seealso network
#' 
#' @keywords internal

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









# which.matrix.type -------------------------------------------------------

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
#' \doi{10.18637/jss.v024.i02}
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
#' @rdname which.matrix.type
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
        else if (NROW(x)==0)  #For a 0-row matrix, an empty edgelist is the best bet...
            out<-"edgelist"
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
