#  File R/edgelist.R in package network, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2003-2015 Statnet Commons
#######################################################################

# the edgelist functions have been copied in from ergm

#' @export
as.edgelist <- function(x, ...){
  UseMethod("as.edgelist")
} 


# convert a network into an ergm-style sorted edgelist using
# as.edgelist.matrix and as.matrix.network.edgelist

#' @name as.edgelist
#'
#' @title Convert a network object into a numeric edgelist matrix
#' 
#' @description Constructs an edgelist in a sorted format with defined attributes.
#' 
#' @details Constructs a edgelist matrix or tibble from a network, sorted tails-major
#' order, with tails first, and, for undirected networks, tail < head.  This
#' format is required by some reverse-depending packages (e.g. \code{ergm})
#' 
#' The \code{\link{as.matrix.network.edgelist}} provides similar functionality
#' but it does not enforce ordering or set the \code{edgelist} class and so
#' should be slightly faster.
#' 
#' \code{is.edgelist} tests if an object has the class \code{'edgelist'}
#' 
#' 
#' @aliases edgelist
#' @param x a \code{network} object with additional class added indicating how
#' it should be dispatched.
#' @param output return type: a \code{\link{matrix}} or a \code{\link[tibble]{tibble}};
#' see \code{\link{as.matrix.network}} for the difference.
#' @param attrname optionally, the name of an edge attribute to use for edge
#' values; may be a vector of names if \code{output="tibble"}
#' @param as.sna.edgelist logical; should the edgelist be returned in edgelist
#' form expected by the sna package? Ignored if \code{output="tibble"}
#' @param n integer number of vertices in network, value passed to the 'n' flag
#' on edgelist returned
#' @param vnames vertex names (defaults to vertex ids) to be attached to
#' edgelist for sna package compatibility
#' @param directed logical; is network directed, value passed to the 'directed'
#' flag on edgelist returned
#' @param bipartite logical or integer; is network bipartite, value passed to
#' the 'bipartite' flag on edgelist returned
#' @param loops logical; are self-loops allowed in network?, value passed to
#' the 'loops' flag on edgelist returned
#' @param \dots additional arguments to other methods
#' @return A matrix in which the first two columns are integers giving the tail
#' (source) and head (target) vertex ids of each edge. The matrix will be given
#' the class \code{edgelist}.
#' 
#' The edgelist has additional attributes attached to it: \itemize{ \item
#' \code{attr(,"n")} the number of vertices in the original network
#' 
#' \item \code{attr(,"vnames")} the names of vertices in the original network
#' 
#' \item \code{attr(,"directed")} logical, was the original network directed
#' 
#' \item \code{attr(,"bipartite")} was the original network bipartite
#' 
#' \item \code{attr(,"loops")} does the original network contain self-loops }
#' 
#' Note that if the \code{attrname} attribute is used the resulting edgelist
#' matrix will have three columns.  And if \code{attrname} refers to a
#' character attribute, the resulting edgelist matrix will be character rather
#' than numeric unless \code{output="tibble"}.
#' 
#' @note NOTE: this function was moved to network from the ergm package in
#' network version 1.13
#' @seealso See also \code{\link{as.matrix.network.edgelist}}
#' @examples
#' 
#'    data(emon)
#'    as.edgelist(emon[[1]])
#'    as.edgelist(emon[[1]],output="tibble")
#'    # contrast with unsorted columns of
#'    as.matrix.network.edgelist(emon[[1]])
#'   
#' @export
as.edgelist.network <- function(x, attrname = NULL, as.sna.edgelist = FALSE, output=c("matrix","tibble"), ...){
  output <- match.arg(output)
  switch(output,
         matrix = as.edgelist(as.matrix.network.edgelist(x, attrname=attrname,
                                                         as.sna.edgelist=as.sna.edgelist,...),
                              n=network.size(x),
                              directed=is.directed(x),
                              bipartite=ifelse(is.bipartite(x),x%n%"bipartite",FALSE),
                              loops=has.loops(x),
                              vnames=network.vertex.names(x)),
         tibble = as.edgelist(as_tibble(x, attrnames=attrname,...),
                              n=network.size(x),
                              directed=is.directed(x),
                              bipartite=ifelse(is.bipartite(x),x%n%"bipartite",FALSE),
                              loops=has.loops(x),
                              vnames=network.vertex.names(x))
         )
}

#' @rdname as.edgelist
#' @export as.edgelist.matrix
#' @export
as.edgelist.matrix <- function(x, n, directed=TRUE, bipartite=FALSE, loops=FALSE, vnames=seq_len(n),...){
  tails <- as.integer(x[,1])
  heads <- as.integer(x[,2])
  if(!directed) {
    tails <- pmin(t <- tails, heads)
    heads <- pmax(t, heads)
  }
  keep <- rep(TRUE, length(tails))
  if(!loops) {
    keep <- keep & (tails != heads)
  }
  if(bipartite) {
    keep <- keep & ((tails <= bipartite) != (heads <= bipartite))
  }
  x <- x[keep,,drop=FALSE]
  tails <- tails[keep]
  heads <- heads[keep]
  x[,1:2] <- cbind(tails, heads)
  x <- unique(x[order(tails, heads),,drop=FALSE])
  attr(x,"n") <- as.integer(n)
  attr(x,"vnames")<- vnames
  attr(x,"directed") <- as.logical(directed)
  attr(x,"bipartite") <- if(is.numeric(bipartite)) as.integer(bipartite) else bipartite
  attr(x,"loops") <- as.logical(loops)
  class(x)<-c('matrix_edgelist','edgelist',class(x))
  x
}

#' @rdname as.edgelist
#' @export
as.edgelist.tbl_df <- function(x, n, directed=TRUE, bipartite=FALSE, loops=FALSE, vnames=seq_len(n),...){
  if(!directed){
    x$.tail <- pmin(t <- x$.tail, x$.head)
    x$.head <- pmax(t, x$.head) # .tail has been clobbered.
  }
  if(!loops) x <- x[x$.tail!=x$.head,]
  if(bipartite) x <- x[(x$.tail<=bipartite)!=(x$.head<=bipartite),]
  x <- unique(x[order(x$.tail, x$.head),])
  attr(x,"n") <- as.integer(n)
  attr(x,"vnames")<- vnames
  attr(x,"directed") <- as.logical(directed)
  attr(x,"bipartite") <- if(is.numeric(bipartite)) as.integer(bipartite) else bipartite
  attr(x,"loops") <- as.logical(loops)
  class(x)<-c('tibble_edgelist','edgelist',class(x))
  x
}

#' @rdname as.edgelist
#' @export is.edgelist
is.edgelist<-function(x){
  inherits(x,"edgelist")
}


