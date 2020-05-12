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
