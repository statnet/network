######################################################################
#
# constructors.R
#
# Written by Carter T. Butts <buttsc@uci.edu>; portions contributed by
# David Hunter <dhunter@stat.psu.edu> and Mark S. Handcock
# <handcock@u.washington.edu>.
#
# Last Modified 02/26/13
# Licensed under the GNU General Public License version 2 (June, 1991)
# or later
#
# Part of the R/network package
#
# This file contains various routines for the construction of network
# and edge objects.
#
# Contents:
#
#   network
#   network.adjacency
#   network.copy
#   network.edgelist
#   network.incidence
#   network.initialize
#
######################################################################


# Basic network constructor.  Converts a single matrix to a network class
# object.  The matrix must be in one of three formats:  adjacency,
# incidence, or edgelist.
#
# MSH added bipartite
#



#' @rdname network
#' @export network
network<-function(x, vertex.attr=NULL, vertex.attrnames=NULL,
                directed=TRUE, hyper=FALSE, loops=FALSE,
                multiple=FALSE, bipartite=FALSE, ...)
{
  #Initialize the network object
  g<-as.network(x,directed=directed,hyper=hyper,loops=loops,
              multiple=multiple,bipartite=bipartite,...)
  #Add vertex attributes, if needed
  if(!is.null(vertex.attr)){
    #Create vertex attribute names, if needed
    if(is.null(vertex.attrnames)){
      if(!is.null(names(vertex.attr)))
        vertex.attrnames<-names(vertex.attr)
      else{
        vertex.attrnames<-1:length(vertex.attr)
	warning("Vertex attribute names not given; making some up.")
      }
    }
    #Add the attributes
    for(i in 1:length(vertex.attr))
      g<-set.vertex.attribute(g,vertex.attrnames[[i]],vertex.attr[[i]])
  }
# xnames <- get.vertex.attribute(g,"vertex.names")
# if(!is.null(xnames) & any(!is.na(xnames))){ g <- xnames }
  #Return the result
  g  
}

# Construct a network's edge set, using an a bipartite adjacency matrix as input.
#
#' @name edgeset.constructors
#'
#' @title Edgeset Constructors for Network Objects
#'
#' @description These functions convert relational data in matrix form to 
#'   network edge sets.
#'
#' @details Each of the above functions takes a \code{network} and a matrix
#'   as input, and modifies the supplied \code{network} object by adding the
#'   appropriate edges.  \code{network.adjacency} takes \code{x} to be an 
#'   adjacency matrix; \code{network.edgelist} takes \code{x} to be an edgelist
#'   matrix; and \code{network.incidence} takes \code{x} to be an incidence
#'   matrix.  \code{network.bipartite} takes \code{x} to be a two-mode 
#'   adjacency matrix where rows and columns reflect each respective mode 
#'   (conventionally, actors and events); If \code{ignore.eval==FALSE}, 
#'   (non-zero) edge values are stored as edgewise attributes with name 
#'   \code{names.eval}.  The \code{edge.check} argument can be added via 
#'   \code{\dots} and will be passed to \code{\link{add.edges}}.
#' 
#' Edgelist matrices to be used with \code{network.edgelist} should have one 
#'   row per edge, with the first two columns indicating the sender and 
#'   receiver of each edge (respectively).  Edge values may be provided in 
#'   additional columns. The edge attributes will be created with names 
#'   corresponding to the column names unless alternate names are provided via 
#'   \code{names.eval}. The vertices specified in the first two columns, which
#'   can be characters, are added to the network in default sort order. The 
#'   edges are added in the order specified by the edgelist matrix.
#'   
#' Incidence matrices should contain one row per vertex, with one column per 
#'   edge. A non-zero entry in the matrix means that the edge with the id 
#'   corresponding to the column index will have an incident vertex with an
#'   id corresponding to the row index. In the directed case, negative cell
#'   values are taken to indicate tail vertices, while positive values 
#'   indicate head vertices. 
#' 
#' Results similar to \code{network.adjacency} can also be obtained by means 
#'   of extraction/replacement operators.  See the associated man page for 
#'   details.
#'
#' @param x a matrix containing edge information
#' @param g an object of class \code{network}
#' @param ignore.eval logical; ignore edge value information in x?
#' @param names.eval a name for the edge attribute under which to store edge
#'   values, if any
#' @param \dots possible additional arguments (such as \code{edge.check})
#'
#' @return Invisibly, an object of class \code{network}; these functions modify
#'   their argument in place.
#' 
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing 
#'   Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  
#'   \url{http://www.jstatsoft.org/v24/i02/}
#'
#' @author Carter T. Butts \email{buttsc@uci.edu} and David Hunter 
#'   \email{dhunter@stat.psu.edu}
#' 
#' 
#' @seealso \code{\link{loading.attributes}}, \code{\link{network}}, 
#'   \code{\link{network.initialize}}, \code{\link{add.edges}}, 
#'   \code{\link{network.extraction}}
#' @examples
#' #Create an arbitrary adjacency matrix
#' m<-matrix(rbinom(25,1,0.5),5,5)
#' diag(m)<-0
#' 
#' g<-network.initialize(5)    #Initialize the network
#' network.adjacency(m,g)      #Import the edge data
#' 
#' #Do the same thing, using replacement operators
#' g<-network.initialize(5)
#' g[,]<-m
#' 
#' # load edges from a data.frame via network.edgelist
#' edata <-data.frame(
#'   tails=c(1,2,3),
#'   heads=c(2,3,1),
#'   love=c('yes','no','maybe'),
#'   hate=c(3,-5,2),
#'   stringsAsFactors=FALSE
#'   )
#' 
#' g<-network.edgelist(edata,network.initialize(4),ignore.eval=FALSE)
#' as.sociomatrix(g,attrname='hate')
#' g%e%'love'
#' 
#' # load edges from an incidence matrix
#' inci<-matrix(c(1,1,0,0, 0,1,1,0, 1,0,1,0),ncol=3,byrow=FALSE)
#' inci
#' g<-network.incidence(inci,network.initialize(4,directed=FALSE))
#' as.matrix(g)
#' 
#' # load in biparite dataframe with weights
#' bipMat<-data.frame(
#'         event1=c(1,2,1,0),
#'         event2=c(0,0,3,0),
#'         event3=c(1,1,0,4),
#'         row.names=c("a","b","c","d"))
#' net<-network(bipMat,matrix.type='bipartite',ignore.eval=FALSE,names.eval='pies')
#' as.matrix(net,attername='pies')
#' 
#' 
#' 
#' @keywords classes graphs
#' @export
network.bipartite<-function(x, g, ignore.eval=TRUE, names.eval=NULL, ...){
  #Set things up to edit g in place
  gn<-substitute(g)
  #Build head/tail lists; note that these cannot be hypergraphic or
  #multiplex, since our data is drawn from an adjacency matrix
  nactors <- dim(x)[1]
  nevents <- dim(x)[2]
  n <- nactors + nevents
  #Add names if available
  if(!is.null(colnames(x)) & !is.null(rownames(x))){
    g <- set.vertex.attribute(g,"vertex.names",c(rownames(x),colnames(x)))
  }
  # convert x into a matrix
  x<-as.matrix(x)
  
  X <- matrix(0,ncol=n,nrow=n)
# diag(X) <- 0
  X[1:nactors, nactors+(1:nevents)] <- x
  X[nactors+(1:nevents), 1:nactors] <- t(x)
  X[row(X)<col(X)]<-0            #Clear above-diagonal entries.
  x <- X
  missing <- is.na(x)
  x[missing] <- 1
#
  x<-as.vector(x)
  n<-network.size(g)
  e<-(0:(n*n-1))[x!=0] 
  if(ignore.eval){
    ev<-as.list(as.logical(missing[x!=0]))
    en<-replicate(length(ev),list("na"))
  }else{
    xv<-x
    ev<-apply(cbind(as.list(as.logical(missing[x!=0])),as.list(xv[x!=0])),1, as.list)
    en<-replicate(length(ev),list(list("na",names.eval)))
  }
  if(sum(x!=0)>0)
    add.edges(g, as.list(1+e%%n), as.list(1+e%/%n),
              names.eval=en, vals.eval=ev, ...)
  #Patch up g on exit for in-place modification
  if(.validLHS(gn,parent.frame())){
    on.exit(eval.parent(call('<-',gn,g)))
  }
  invisible(g)
}


# Construct a network's edge set, using an adjacency matrix as input.
#
#' @rdname edgeset.constructors
#' @export
network.adjacency<-function(x, g, ignore.eval=TRUE, names.eval=NULL, ...){
  # check that dimension of g is appropriate for x
  if (nrow(x)!=ncol(x)){
    stop('the network.adjacency constructor expects its matrix argument to be square (same number of rows and columns)')
  }
  if (network.size(g) != nrow(x)){
    stop('the network.adjacency constructor requires that the size of its network argument (',network.size(g),') matches the dimensions of the matrix argument (',nrow(x),' by ',ncol(x),')')
  }
  
  #Set things up to edit g in place
  gn<-substitute(g)
  #Build head/tail lists; note that these cannot be hypergraphic or
  #multiplex, since our data is drawn from an adjacency matrix
  if(!is.directed(g)){
    missingE <- is.na(x) | is.na(t(x))
    x[missingE] <- 1
    #Be sure to pick up nonzero entries for which x[i,j]=-x[j,i].
    x[x==-t(x)]<-abs(x)[x==-t(x)]  
    x<-(x+t(x))/2                  #Symmetrize matrix.
    x[row(x)<col(x)]<-0            #Clear above-diagonal entries.
  }else{
    missingE <- is.na(x)
    x[missingE] <- 1
  }
  
  # if the na.rm value is specified and TRUE, don't include those missing edges after all
  # some ugliness to pull names from ...
  dotNames<-as.list(substitute(list(...)))[-1L]
  if('na.rm'%in%dotNames){
    na.rm<-list(...)[[match('na.rm',dotNames)]]
    if (na.rm){
      x[missingE]<-0
    }
  }
  
  if(!has.loops(g)){ # if it doesn't have loops, replace the diagonal
    diag(x)<-0
  }
  x<-as.vector(x)
  n<-network.size(g)
  e<-(0:(n*n-1))[x!=0] 
  if(ignore.eval){
    ev<-as.list(as.logical(missingE[x!=0]))
    en<-replicate(length(ev),list("na"))
  }else{
    xv<-x
    xv[missingE]<-NA
    ev<-apply(cbind(as.list(as.logical(missingE[x!=0])),as.list(xv[x!=0])),1, as.list)
    en<-replicate(length(ev),list(c("na",names.eval)))
  }
  # Add names if available
  if(!is.null(colnames(x))){
   g <- set.vertex.attribute(g,"vertex.names", colnames(x))
  }else{
    if(!is.null(rownames(x))){
      g <- set.vertex.attribute(g,"vertex.names", rownames(x))
    }
  }
  if(sum(x!=0)>0)
    add.edges(g, as.list(1+e%%n), as.list(1+e%/%n),
              names.eval=en, vals.eval=ev, ...)
  #Patch up g on exit for in-place modification
  if(.validLHS(gn,parent.frame())){
    on.exit(eval.parent(call('<-',gn,g)))
  }
  invisible(g)
}


# Construct and a return a network object which is a copy of x
#
#' @rdname network
#' @export
network.copy<-function(x){
  #Verify that this is a network object
  if(!is.network(x))
    stop("network.copy requires an argument of class network.\n")
  #Duplicate and return
  y<-.Call(copyNetwork_R,x)
  y
}


# Construct a network's edge set, using an edgelist matrix as input.
#
#' @rdname edgeset.constructors
#' @export
network.edgelist<-function(x, g, ignore.eval=TRUE, names.eval=NULL, ...){
  #Set things up to edit g in place
  gn<-substitute(g)
  l<-dim(x)[2]
  #Traverse the edgelist matrix, adding edges as we go.
  if((l>2)&&(!ignore.eval)){		#Use values if present...
    #if names not given, try to use the names from data frame
    if (is.null(names.eval)){
      names.eval<-names(x)[3:l]
    }
    #if it is still null, its going to crash, so throw an informative error
    if (is.null(names.eval)){
      stop("unable to add attribute values to edges because names are not provided for each attribute (names.eval=NULL)")
    }
    edge.check<-list(...)$edge.check 
    eattrnames <-lapply(seq_len(NROW(x)),function(r){as.list(names.eval)})
   # eattrvals <-apply(x[,3:l,drop=FALSE]
    eattrvals <-lapply(seq_len(NROW(x)),function(r){as.list(x[r,3:l,drop=FALSE])})
    g<-add.edges(g,as.list(x[,1]),as.list(x[,2]),eattrnames,eattrvals,edge.check=edge.check)
  }else{				#...otherwise, don't.
    edge.check<-list(...)$edge.check      
    g<-add.edges(g,as.list(x[,1]),as.list(x[,2]),edge.check=edge.check)
  }
  #Patch up g on exit for in-place modification
  if(.validLHS(gn,parent.frame())){
    on.exit(eval.parent(call('<-',gn,g)))
  }
  invisible(g)
}


# Construct a network's edge set, using an incidence matrix as input.
#
#' @rdname edgeset.constructors
#' @export
network.incidence<-function(x, g, ignore.eval=TRUE, names.eval=NULL, ...){
  #Set things up to edit g in place
  gn<-substitute(g)
  n<-network.size(g)
  edge.check<-list(...)$edge.check      
  #Traverse the incidence matrix, adding edges as we go.
  for(i in 1:dim(x)[2]){
    #Construct the head and tail sets
    if(is.directed(g)){
      if(any(is.na(x[,i])))
        stop("Missing data not allowed for directed incidence matrices.\n")
      head<-(1:n)[x[,i]>0]
      tail<-(1:n)[x[,i]<0]
      missing<-FALSE
    }else{
      missing<-any(is.na(x[,i]))
      x[,i][is.na(x[,i])]<-1
      head<-(1:n)[x[,i]!=0]
      if(is.hyper(g))
        tail<-head
      else{                 #If dyadic, use only the first two nonzero entries
        tail<-head[1]
        head<-head[2]
      }
    }
    if(length(head)*length(tail)==0)
      stop("Supplied incidence matrix has empty head/tail lists. (Did you get the directedness right?)")
    #Get edge values, if needed
    if(ignore.eval){
      en<-"na"
      ev<-missing
    }else{
      if(!is.directed(g))
        ev<-list(missing,x[x[,i]!=0,i][1])
      else
        ev<-list(missing,abs(x[x[,i]!=0,i][1]))
      if(is.null(names.eval))
        en<-list("na",NULL)
      else
        en<-list("na",names.eval)
    }
    #Add the edge to the graph
    g<-add.edge(g,tail,head,names.eval=en,vals.eval=ev,edge.check=edge.check)
  }
  #Patch up g on exit for in-place modification
  if(.validLHS(gn,parent.frame())){
    on.exit(eval.parent(call('<-',gn,g)))
  }
  invisible(g)
}

# Initialize a new network object.
# MSH added bipartite
#


#' Initialize a Network Class Object
#' 
#' Create and initialize a \code{network} object with \code{n} vertices.
#' 
#' Generally, \code{network.initialize} is called by other constructor
#' functions as part of the process of creating a network.
#' 
#' @param n the number of vertices to initialize
#' @param directed logical; should edges be interpreted as directed?
#' @param hyper logical; are hyperedges allowed?
#' @param loops logical; should loops be allowed?
#' @param multiple logical; are multiplex edges allowed?
#' @param bipartite count; should the network be interpreted as bipartite? If
#' present (i.e., non-NULL) it is the count of the number of actors in the
#' first mode of the bipartite network. In this case, the overall number of
#' vertices is equal to the number of 'actors' (first mode) plus the number of
#' `events' (second mode), with the vertex.ids of all actors preceeding all
#' events. The edges are then interpreted as nondirected.
#' @return An object of class \code{network}
#' @author Carter T. Butts \email{buttsc@@uci.edu}
#' @seealso \code{\link{network}}, \code{\link{as.network.matrix}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{http://www.jstatsoft.org/v24/i02/}
#' @keywords classes graphs
#' @examples
#' 
#' g<-network.initialize(5)  #Create an empty graph on 5 vertices
#' 
#' @export network.initialize
network.initialize<-function(n,directed=TRUE,hyper=FALSE,loops=FALSE,multiple=FALSE,bipartite=FALSE){
  #If we have a negative number of vertices, we have a problem...
  n<-round(n)
  if(n<0)
    stop("Network objects cannot be of negative order.")
  #Create the base-level lists
  g<-list()
  g$mel<-list()
  g$gal<-list()
  #Create the required network attributes
  g$gal$n<-n
  g$gal$mnext<-1
  g$gal$directed<-directed
  g$gal$hyper<-hyper
  g$gal$loops<-loops
  g$gal$multiple<-multiple
  g$gal$bipartite<-bipartite
  #Populate the vertex attribute lists, endpoint lists, etc.
  if(n>0){
    g$val<-replicate(n,list())
    g$iel<-replicate(n,vector(mode="integer"))
    g$oel<-replicate(n,vector(mode="integer"))
  }else{
    g$val<-vector(length=0,mode="list")
    g$iel<-vector(length=0,mode="list")
    g$oel<-vector(length=0,mode="list")
  }
  #Set the class
  class(g)<-"network"
  #Set the required vertex attribute
  if(n>0)
    g<-set.vertex.attribute(g,"na",rep(FALSE,n),1:n)
  #Create default vertex names
  if(n>0)
    network.vertex.names(g)<-1:n
  #Return
  g
}
