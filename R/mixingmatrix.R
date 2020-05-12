#' Mixing matrix
#' 
#' Return the mixing matrix for a network, on a given attribute.
#' 
#' @param object a network or some other data structure for which a mixing
#'   matrix is meaningful.
#' @param ... further arguments passed to or used by methods.
#' 
#' 
#' @include constructors.R
#' @export

mixingmatrix <- function(object, ...) UseMethod("mixingmatrix")


# Return the mixing matrix for a network object, on a given attribute.  This is
# a relocated function from the ergm package; it probably belongs elsewhere, but
# is needed for the summary.network method (and in that sense is basic enough to
# include.




#' @rdname mixingmatrix
#' 
#' @param attrname a vertex attribute name.
#' 
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
    u<-sort(unique(nodecov[0:nb1]))
    From <- c(u, nodecov[el[,1]])
    u<-sort(unique(nodecov[(nb1+1):network.size(nw)]))
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






#' @rdname mixingmatrix
#' 
#' @param x mixingmatrix object
#' 
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
