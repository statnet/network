######################################################################
#
# access.R
#
# Written by Carter T. Butts <buttsc@uci.edu>; portions contributed by
# David Hunter <dhunter@stat.psu.edu> and Mark S. Handcock
# <handcock@u.washington.edu>.
#
# Last Modified 02/24/19
# Licensed under the GNU General Public License version 2 (June, 1991)
# or greater
#
# Part of the R/network package
#
# This file contains various routines for accessing network class objects.
#
# Contents:
#
#   add.edge
#   add.edges
#   add.vertices
#   delete.edge.attribute
#   delete.edges
#   delete.network.attribute
#   delete.vertex.attribute
#   delete.vertices
#   get.edge.attribute
#   get.edge.value
#   get.edgeIDs
#   get.edges
#   get.inducedSubgraph
#   get.network.attribute
#   get.neighborhood
#   get.vertex.attribute
#   has.loops
#   is.adjacent
#   is.bipartite
#   is.directed
#   is.hyper
#   is.multiplex
#   is.network
#   list.edge.attributes
#   list.network.attributes
#   list.vertex.attributes
#   network.dyadcount
#   network.edgecount
#   network.naedgecount
#   network.size
#   network.vertex.names
#   network.vertex.names<-
#   permute.vertexIDs
#   set.edge.attribute
#   set.edge.value
#   set.network.attribute
#   set.vertex.attribute
#   valid.eids
#
######################################################################


#Add a single edge to a network object.
# S3 method dispatch for add edge

#' @name add.edges
#'
#' @title Add Edges to a Network Object
#' 
#' @description Add one or more edges to an existing network object.
#' 
#' @details The edge checking procedure is very slow, but should always be employed when
#' debugging; without it, one cannot guarantee that the network state is
#' consistent with network level variables (see
#' \code{\link{network.indicators}}). For example, by default it is possible to
#' add multiple edges to a pair of vertices.
#' 
#' Edges can also be added/removed via the extraction/replacement operators.
#' See the associated man page for details.
#' 
#' @aliases add.edges.network add.edge.network
#' @param x an object of class \code{network}
#' @param tail for \code{add.edge}, a vector of vertex IDs reflecting the tail
#' set for the edge to be added; for \code{add.edges}, a list of such vectors
#' @param head for \code{add.edge}, a vector of vertex IDs reflecting the head
#' set for the edge to be added; for \code{add.edges}, a list of such vectors
#' @param names.eval for \code{add.edge}, an optional list of names for edge
#' attributes; for \code{add.edges}, a list of length equal to the number of
#' edges, with each element containing a list of names for the attributes of
#' the corresponding edge
#' @param vals.eval for \code{add.edge}, an optional list of edge attribute
#' values (matching \code{names.eval}); for \code{add.edges}, a list of such
#' lists
#' @param edge.check logical; should we perform (computationally expensive)
#' tests to check for the legality of submitted edges?
#' @param ...  additional arguments
#' @return Invisibly, \code{add.edge} and \code{add.edges} return pointers to
#' their modified arguments; both functions modify their arguments in place..
#' @note \code{add.edges} and \code{add.edge} were converted to an S3 generic
#' funtions in version 1.9, so they actually call \code{add.edges.network} and
#' \code{add.edge.network} by default, and may call other versions depending on
#' context (i.e. when called with a \code{networkDynamic} object).
#' @author Carter T. Butts \email{buttsc@@uci.edu}
#' @seealso \code{\link{network}}, \code{\link{add.vertices}},
#' \code{\link{network.extraction}}, \code{\link{delete.edges}},
#' \code{\link{network.edgelist}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{http://www.jstatsoft.org/v24/i02/}
#' @keywords classes graphs
#' @examples
#' 
#' #Initialize a small, empty network
#' g<-network.initialize(3)
#' 
#' #Add an edge
#' add.edge(g,1,2)
#' g
#' 
#' #Can also add edges using the extraction/replacement operators
#' #note that replacement operators are much slower than add.edges()
#' g[,3]<-1
#' g[,]
#' 
#' #Add multiple edges with attributes to a network
#' 
#' # pretend we just loaded in this data.frame from a file
#' # Note: network.edgelist() may be simpler for this case
#' elData<-data.frame(
#'   from_id=c("1","2","3","1","3","1","2"),
#'   to_id=c("1", "1", "1", "2", "2", "3", "3"),
#'   myEdgeWeight=c(1, 2, 1, 2, 5, 3, 9.5),
#'   someLetters=c("B", "W", "L", "Z", "P", "Q", "E"),
#'   edgeCols=c("red","green","blue","orange","pink","brown","gray"),
#'   stringsAsFactors=FALSE
#' )
#' 
#' valueNet<-network.initialize(3,loops=TRUE)
#' 
#' add.edges(valueNet,elData[,1],elData[,2],
#'     names.eval=rep(list(list("myEdgeWeight","someLetters","edgeCols")),nrow(elData)), 
#'     vals.eval=lapply(1:nrow(elData),function(r){as.list(elData[r,3:5])}))
#' 
#' list.edge.attributes(valueNet)
#' 
#' 
#' @export
add.edge<-function(x, tail, head, names.eval=NULL, vals.eval=NULL, edge.check=FALSE, ...){
  xn<-substitute(x)
  UseMethod("add.edge") 
  if(.validLHS(xn,parent.frame())){  #If x not anonymous, set in calling env 
    on.exit(eval.parent(call('<-',xn,x)))
  }
  invisible(x) 
} 

#' @export add.edge.network
#' @export
add.edge.network<-function(x, tail, head, names.eval=NULL, vals.eval=NULL, edge.check=FALSE, ...){ 
  xn<-substitute(x)
  if(.validLHS(xn,parent.frame())){  #If x not anonymous, set in calling env 
    on.exit(eval.parent(call('<-',xn,x)))
  }
  x<-.Call(addEdge_R,x,tail,head,names.eval,vals.eval,edge.check)
  invisible(x)
}

# S3 method dispatch for add.edges

#' @rdname add.edges
#' @export add.edges
add.edges<-function(x, tail, head, names.eval=NULL, vals.eval=NULL, ...){
  xn<-substitute(x)
  UseMethod("add.edges") 
  if(.validLHS(xn,parent.frame())){  #If x not anonymous, set in calling env 
    on.exit(eval.parent(call('<-',xn,x)))
  }
  invisible(x) 
} 


# Add multiple edges to network x.  Tail must be a list, each element of
# which is the tail set for a given edge (ditto for head).  If edge values
# are provided, they must be given similarly as lists of lists.
#' @export add.edges.network
#' @export
add.edges.network<-function(x, tail, head, names.eval=NULL, vals.eval=NULL, ...){
  #Ensure that the inputs are set up appropriately 
  if(!is.list(tail))
    tail<-as.list(tail)
  if(!is.list(head))
    head<-as.list(rep(head,length=length(tail)))
  if(is.null(names.eval))
    names.eval<-replicate(length(tail),NULL)
  else if(!is.list(names.eval))
    names.eval<-as.list(rep(names.eval,length=length(tail)))
  if(is.null(vals.eval))
    vals.eval<-replicate(length(tail),NULL)
  else if(!is.list(vals.eval))
    vals.eval<-as.list(rep(vals.eval,length=length(names.eval)))
  if(length(unique(c(length(tail),length(head),length(names.eval), length(vals.eval))))>1)
    stop("head, tail, names.eval and vals.eval lists passed to add.edges must be of the same length!\n")
  edge.check<-list(...)$edge.check
  if(is.null(edge.check))
    edge.check<-FALSE
  #Pass the inputs to the C side
  xn<-substitute(x)
  x<-.Call(addEdges_R,x,tail,head,names.eval,vals.eval,edge.check)
  if(.validLHS(xn,parent.frame())){  #If x not anonymous, set in calling env 
    on.exit(eval.parent(call('<-',xn,x)))
  }
  invisible(x)
}



# S3 method dispatch for add.vertices


#' Add Vertices to an Existing Network
#' 
#' \code{add.vertices} adds a specified number of vertices to an existing
#' network; if desired, attributes for the new vertices may be specified as
#' well.
#' 
#' New vertices are generally appended to the end of the network (i.e., their
#' vertex IDs begin with \code{network.size(x)} an count upward).  The one
#' exception to this rule is when \code{x} is bipartite and
#' \code{last.mode==FALSE}.  In this case, new vertices are added to the end of
#' the first mode, with existing second-mode vertices being permuted upward in
#' ID.  (\code{x}'s \code{bipartite} attribute is adjusted accordingly.)
#' 
#' Note that the attribute format used here is based on the internal
#' (vertex-wise) storage method, as opposed to the attribute-wise format used
#' by \code{\link{network}}.  Specifically, \code{vattr} should be a list with
#' one entry per new vertex, the ith element of which should be a list with an
#' element for every attribute of the ith vertex.  (If the required \code{na}
#' attribute is not given, it will be automatically created.)
#' 
#' @aliases add.vertices.network
#' @param x an object of class \code{network}
#' @param nv the number of vertices to add
#' @param vattr optionally, a list of attributes with one entry per new vertex
#' @param last.mode logical; should the new vertices be added to the last
#' (rather than the first) mode of a bipartite network?
#' @param ... possible additional arguments to add.vertices
#' @return Invisibly, a pointer to the updated \code{network} object;
#' \code{add.vertices} modifies its argument in place.
#' @note \code{add.vertices} was converted to an S3 generic funtion in version
#' 1.9, so it actually calls \code{add.vertices.network} by default and may
#' call other versions depending on context (i.e. when called with a
#' \code{networkDynamic} object).
#' @author Carter T. Butts \email{buttsc@@uci.edu}
#' @seealso \code{\link{network}}, \code{\link{get.vertex.attribute}},
#' \code{\link{set.vertex.attribute}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{http://www.jstatsoft.org/v24/i02/}
#' @keywords classes graphs
#' @examples
#' 
#' #Initialize a network object
#' g<-network.initialize(5)
#' g
#' 
#' #Add five more vertices
#' add.vertices(g,5)
#' g
#' 
#' #Create two more, with attributes
#' vat<-replicate(2,list(is.added=TRUE,num.added=2),simplify=FALSE)
#' add.vertices(g,2,vattr=vat)
#' g%v%"is.added"       #Values are only present for the new cases
#' g%v%"num.added"
#' 
#' #Add to a bipartite network
#' bip <-network.initialize(5,bipartite=3)
#' get.network.attribute(bip,'bipartite') # how many vertices in first mode?
#' add.vertices(bip,3,last.mode=FALSE)
#' get.network.attribute(bip,'bipartite')
#' 
#' @export add.vertices
add.vertices<-function(x, nv, vattr=NULL, last.mode=TRUE, ...){
  xn<-substitute(x)
  UseMethod("add.vertices") 
  if(.validLHS(xn,parent.frame())){  #If x not anonymous, set in calling env 
    on.exit(eval.parent(call('<-',xn,x)))
  }
  invisible(x) 
}

# Add nv vertices to network x.  Vertex attributes (in addition to those which
# are required) are to be provided in vattr; vattr must be a list containing
# nv elements, each of which is equal to the desired val[i] entry.
#' @export add.vertices.network
#' @export
add.vertices.network<-function(x, nv, vattr=NULL, last.mode=TRUE, ...){ 
  #Check to be sure we were called with a network
  if(!is.network(x))
    stop("add.vertices requires an argument of class network.\n")
  #Check the vertex attributes, to be sure that they are legal
  if(!is.null(vattr)){
    if(is.list(vattr))
      vattr<-rep(vattr,length=nv)
    else
      vattr<-as.list(rep(vattr,length=nv))
  }
  #Perform the addition
  xn<-substitute(x)
  if(nv>0){
    if(.validLHS(xn,parent.frame())){  #If x not anonymous, set in calling env 
      on.exit(eval.parent(call('<-',xn,x)))
    }
    if(last.mode||(!is.bipartite(x))){
      x<-.Call(addVertices_R,x,nv,vattr)
    }else{
      
      nr<-nv
      nc<-0
      nnew<-nr+nc
      nold<-network.size(x)
      bip<-x%n%"bipartite"
      x<-.Call(addVertices_R, x, nv, vattr)
      
      if(nr>0){
        if(bip>0)
          orow<-1:bip
        else
          orow<-NULL
        if(nold-bip>0)
          ocol<-(bip+1):nold
        else
          ocol<-NULL
        
        ncol<-NULL
        nrow<-(nold+nnew-nr+1):(nold+nnew)
        permute.vertexIDs(x,c(orow,nrow,ocol,ncol))
        set.network.attribute(x,"bipartite",bip+nr)
      }
    }
  }

  invisible(x)
}


# Remove all instances of the specified attribute(s) from the edge set
#

#' @name attribute.methods
#'
#' @title Attribute Interface Methods for the Network Class
#'
#' @description These methods get, set, list, delete, and check for attributes
#'   at the network, edge, and vertex level.
#'
#' @details The \code{list.attributes} functions return the names of all edge,
#'   network, or vertex attributes (respectively) in the network.  All 
#'   attributes need not be defined for all elements; the union of all extant
#'   attributes for the respective element type is returned.
#'
#' The \code{get.attribute} functions look for an edge, network, or vertex 
#'   attribute (respectively) with the name \code{attrname}, returning its
#'   values.  Note that, to retrieve an edge attribute from all edges within
#'   a network \code{x}, \code{x$mel} should be used as the first argument to
#'   \code{get.edge.attribute}; \code{get.edge.value} is a convenience function
#'   which does this automatically. As of v1.7.2, if a \code{network} object is
#'   passed to \code{get.edge.attribute} it will automatically call 
#'   \code{get.edge.value} instead of returning NULL. When the parameters 
#'   \code{na.omit},  or \code{deleted.edges.omit} are used, the position index
#'   of the attribute values returned will not correspond to the vertex/edge 
#'   id.  To preserved backward compatibility, if the edge attribute 
#'   \code{attrname} does not exist for any edge, \code{get.edge.attribute} 
#'   will still return \code{NULL} even if \code{null.na=TRUE}
#'
#' \code{network.vertex.names} is a convenience function to extract the 
#'   \code{"vertex.names"} attribute from all vertices.
#'
#' The \code{set.attribute} functions allow one to set the values of edge, 
#'   network, or vertex attributes.  \code{set.edge.value} is a convenience
#'   function which allows edge attributes to be given in adjacency matrix 
#'   form, and the assignment form of \code{network.vertex.names} is likewise
#'   a convenient front-end to \code{set.vertex.attribute} for vertex names.  
#'   The \code{delete.attribute} functions, by contrast, remove the named 
#'   attribute from the network, from all edges, or from all vertices (as 
#'   appropriate).  If \code{attrname} is a vector of attribute names, each
#'   will be removed in turn.  These functions modify their arguments in place,
#'   although a pointer to the modified object is also (invisibly) returned.
#'
#' Additional practical example of how to load and attach attributes are on the
#'   \code{\link{loading.attributes}} page. 
#'
#' Some attribute assignment/extraction can be performed conveniently through 
#'   the various extraction/replacement operators, although they may be less 
#'   efficient.  See the associated man page for details.
#'
#'
#' @param x an object of class \code{network}, or a list of edges 
#'          (possibly \code{network$mel}) in \code{get.edge.attribute}.
#' @param el Deprecated; use \code{x} instead.
#' @param attrname the name of the attribute to get or set.
#' @param unlist logical; should retrieved attribute values be 
#'   \code{\link{unlist}}ed prior to being returned?
#' @param na.omit logical; should retrieved attribute values corresponding to 
#'   vertices/edges marked as 'missing' be removed?
#' @param deleted.edges.omit logical: should the elements corresponding to 
#'   deleted edges be removed?
#' @param null.na logical; should \code{NULL} values (corresponding to vertices
#'   or edges with no values set for the attribute) be replaced with \code{NA}s
#'   in output?
#' @param value values of the attribute to be set; these should be in 
#'   \code{vector} or \code{list} form for the \code{edge} and \code{vertex}
#'   cases, or \code{matrix} form for \code{set.edge.value}.
#' @param e IDs for the edges whose attributes are to be altered.
#' @param v IDs for the vertices whose attributes are to be altered.
#' @param ... additional arguments
#'
#' @return For the \code{list.attributes} methods, a vector containing 
#'   attribute names.  For the \code{get.attribute} methods, a list containing
#'   the values of the attribute in question (or simply the value itself, for 
#'   \code{get.network.attribute}).  For the \code{set.attribute} and 
#'   \code{delete.attribute} methods, a pointer to the updated \code{network}
#'   object.
#' @note  As of version 1.9 the \code{set.vertex.attribute} function can accept
#'   and modify multiple attributes in a single call to improve efficiency.  
#'   For this case \code{attrname} can be a list or vector of attribute names 
#'   and \code{value} is a list of values corresponding to the elements of 
#'   \code{attrname} (can also be a list of lists of values if elements in v 
#'   should have different values). 
#' @seealso \code{\link{loading.attributes}},\code{\link{network}}, 
#'   \code{\link{as.network.matrix}}, \code{\link{as.sociomatrix}}, 
#'   \code{\link{as.matrix.network}}, \code{\link{network.extraction}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#'   Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  
#'   \url{http://www.jstatsoft.org/v24/i02/}
#' @author Carter T. Butts \email{buttsc@uci.edu}
#' @examples
#' #Create a network with three edges
#' m<-matrix(0,3,3)
#' m[1,2]<-1; m[2,3]<-1; m[3,1]<-1
#' g<-network(m)
#' 
#' #Create a matrix of values corresponding to edges
#' mm<-m
#' mm[1,2]<-7; mm[2,3]<-4; mm[3,1]<-2
#' 
#' #Assign some attributes
#' set.edge.attribute(g,"myeval",3:5)
#' set.edge.value(g,"myeval2",mm)
#' set.network.attribute(g,"mygval","boo")
#' set.vertex.attribute(g,"myvval",letters[1:3])
#' network.vertex.names(g) <- LETTERS[1:10]
#' 
#' #List the attributes
#' list.edge.attributes(g)
#' list.network.attributes(g)
#' list.vertex.attributes(g)
#' 
#' #Retrieve the attributes
#' get.edge.attribute(g$mel,"myeval")  #Note the first argument!
#' get.edge.value(g,"myeval")          #Another way to do this
#' get.edge.attribute(g$mel,"myeval2") 
#' get.network.attribute(g,"mygval")
#' get.vertex.attribute(g,"myvval")
#' network.vertex.names(g)
#' 
#' #Purge the attributes
#' delete.edge.attribute(g,"myeval")
#' delete.edge.attribute(g,"myeval2")
#' delete.network.attribute(g,"mygval")
#' delete.vertex.attribute(g,"myvval")
#' 
#' #Verify that the attributes are gone
#' list.edge.attributes(g)
#' list.network.attributes(g)
#' list.vertex.attributes(g)
#' 
#' #Note that we can do similar things using operators
#' g %n% "mygval" <- "boo"               #Set attributes, as above
#' g %v% "myvval" <- letters[1:3]
#' g %e% "myeval" <- mm
#' g[,,names.eval="myeval"] <- mm          #Another way to do this
#' g %n% "mygval"                        #Retrieve the attributes
#' g %v% "myvval"
#' g %e% "mevval"
#' as.sociomatrix(g,"myeval")              # Or like this
#' 
#' @keywords classes graphs
#' @export delete.edge.attribute
delete.edge.attribute <- function(x, attrname, ...) {
  UseMethod("delete.edge.attribute")
}

#' @rdname attribute.methods
#' @export
delete.edge.attribute.network <- function(x, attrname, ...) {
  #Remove the edges
  xn<-substitute(x)
  x<-.Call(deleteEdgeAttribute_R,x,attrname)
  if(.validLHS(xn,parent.frame())){  #If x not anonymous, set in calling env 
    on.exit(eval.parent(call('<-',xn,x)))
  }
  invisible(x)
}
 
 
# Remove specified edges from the network.
#
#' @name deletion.methods
#' 
#' @title Remove Elements from a Network Object
#'
#' @description \code{delete.edges} removes one or more edges (specified by 
#'   their internal ID numbers) from a network; \code{delete.vertices} 
#'   performs the same task for vertices (removing all associated edges in 
#'   the process).
#'
#' @details Note that an edge's ID number corresponds to its order within 
#'   \code{x$mel}.  To determine edge IDs, see \code{\link{get.edgeIDs}}.  
#'   Likewise, vertex ID numbers reflect the order with which vertices are
#'   listed internally (e.g., the order of \code{x$oel} and \code{x$iel}, or 
#'   that used by \code{as.matrix.network.adjacency}).  When vertices are 
#'   removed from a network, all edges having those vertices as endpoints are
#'   removed as well.  When edges are removed, the remaining edge ids are NOT
#'   permuted and \code{NULL} elements will be left on the list of edges, which
#'   may complicate some functions that require eids (such as 
#'   \code{\link{set.edge.attribute}}).  The function \code{\link{valid.eids}}
#'   provides a means to determine the set of valid (non-NULL) edge ids. 
#'
#' Edges can also be added/removed via the extraction/replacement operators.
#'   See the associated man page for details.
#'
#' @param x an object of class \code{network}.
#' @param eid a vector of edge IDs.
#' @param vid a vector of vertex IDs.
#'   
#' @return Invisibly, a pointer to the updated network; these functions modify
#'   their arguments in place.
#'
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing 
#'   Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  
#'   \url{http://www.jstatsoft.org/v24/i02/}
#' @author Carter T. Butts \email{buttsc@uci.edu}
#' 
#' @seealso \code{\link{get.edgeIDs}}, \code{\link{network.extraction}}, 
#'   \code{\link{valid.eids}}
#' @examples
#' #Create a network with three edges
#' m<-matrix(0,3,3)
#' m[1,2]<-1; m[2,3]<-1; m[3,1]<-1
#' g<-network(m)
#' 
#' as.matrix.network(g)
#' delete.edges(g,2)              #Remove an edge
#' as.matrix.network(g)
#' delete.vertices(g,2)           #Remove a vertex
#' as.matrix.network(g)
#' 
#' #Can also remove edges using extraction/replacement operators
#' g<-network(m)
#' g[1,2]<-0                      #Remove an edge
#' g[,]
#' g[,]<-0                        #Remove all edges
#' g[,]
#' 
#' @keywords classes graphs
#' @export
delete.edges<-function(x,eid){
  #Check to be sure we were called with a network
  if(!is.network(x))
    stop("delete.edges requires an argument of class network.")
  xn<-substitute(x)
  if(length(eid)>0){
    #Perform a sanity check
    if((min(eid)<1)|(max(eid)>length(x$mel)))
      stop("Illegal edge in delete.edges.\n")
    #Remove the edges
    x<-.Call(deleteEdges_R,x,eid)
    if(.validLHS(xn,parent.frame())){  #If x not anonymous, set in calling env 
      on.exit(eval.parent(call('<-',xn,x)))
    }
  }
  invisible(x)
}

# Remove the specified network-level attribute(s)
#
#' @rdname attribute.methods
#' @export
delete.network.attribute <- function(x, attrname, ...) {
  UseMethod("delete.network.attribute")
}

#' @rdname attribute.methods
#' @export
delete.network.attribute.network <- function(x, attrname, ...){
  #Remove the edges
  xn<-substitute(x)
  x<-.Call(deleteNetworkAttribute_R,x,attrname)
  if(.validLHS(xn,parent.frame())){  #If x not anonymous, set in calling env 
    on.exit(eval.parent(call('<-',xn,x)))
  }
  invisible(x)
}


# Remove all instances of the specified attribute(s) from the vertex set
#
#' @rdname attribute.methods
#' @export
delete.vertex.attribute <- function(x, attrname, ...) {
  UseMethod("delete.vertex.attribute")
}

#' @rdname attribute.methods
#' @export
delete.vertex.attribute.network <- function(x, attrname, ...) {
  #Remove the attribute (or do nothing, if there are no vertices)
  if(network.size(x)>0){
    xn<-substitute(x)
    x<-.Call(deleteVertexAttribute_R,x,attrname)
    if(.validLHS(xn,parent.frame())){  #If x not anonymous, set in calling env 
      on.exit(eval.parent(call('<-',xn,x)))
    }
  }
  invisible(x)
}


# Remove specified vertices (and associated edges) from the network.
#
#' @rdname deletion.methods
#' @export delete.vertices
delete.vertices<-function(x,vid){
  #Check to be sure we were called with a network
  if(!is.network(x))
    stop("delete.vertices requires an argument of class network.")
  #Remove any vids which are out of bounds
  vid<-vid[(vid>0)&(vid<=network.size(x))]
  #Do the deed, if still needed
  xn<-substitute(x)
  if(length(vid)>0){
    if(is.bipartite(x)){  #If bipartite, might need to adjust mode 1 count
      m1v<-get.network.attribute(x,"bipartite")  #How many mode 1 verts?
      set.network.attribute(x,"bipartite",m1v-sum(vid<=m1v))
    }
    x<-.Call(deleteVertices_R,x,vid)
    if(.validLHS(xn,parent.frame())){  #If x not anonymous, set in calling env 
      on.exit(eval.parent(call('<-',xn,x)))
    }
  }
  invisible(x)
}


# Retrieve a specified edge attribute from edge list or network x.  The attribute
# is returned as a list, unless unlist is TRUE. 
# if deleted.edges.omit is TRUE, then only attribute values on existing (non-null) edges will be returned.
# if na.omit is TRUE, than values corresponding to 'missing' edges (edges with attribute 'na' set to TRUE) should be ommited. (NULL edgs count as not-missing)
# If null.na is TRUE, then values corresponding to  edges for which the attribute name was never set will be set to NA.  Otherwise, they will be NULL, which means they will be included when unlist=TRUE 
#
#' @rdname attribute.methods
#' @export
get.edge.attribute <- function(x, ..., el) {
  if(!missing(el)) {
    warning("Argument ", sQuote("el"), " to ", sQuote("get.edge.attribute"), " is deprecated and will be removed in a future version.  Use ", sQuote("x"), " instead.")
    UseMethod("get.edge.attribute", object = el)
  } else {
    UseMethod("get.edge.attribute", object = x)
  }
}

#' @rdname attribute.methods
#' @export
get.edge.attribute.network <- function(x, attrname, unlist=TRUE, na.omit=FALSE, null.na=FALSE, deleted.edges.omit=FALSE, ..., el) {
  if(is.network(x) && !has.edge.attribute(x, attrname)) {
    warning(
      paste0("there is no attribute ", sQuote(attrname), " in ",
             sQuote(deparse(substitute(x))))
    )
  }

  if(!missing(el)) x <- el
  
  if (is.network(x)) x <- x$mel

  if (!is.list(x))
    stop("x must be a network object or a list.")

  if (!is.character(attrname))
    stop("attrname must be a character vector.")

  if (!is.logical(unlist) || !is.logical(na.omit) || !is.logical(null.na) ||
      !is.logical(deleted.edges.omit))
    stop("na.omit, null.na, deleted.edges.omit must be a logical vector.")

  edges <- .Call(getEdgeAttribute_R,x,attrname,na.omit,null.na,deleted.edges.omit)

  if(unlist)
    unlist(edges)
  else
    edges
}

#' @rdname attribute.methods
#' @export
get.edge.attribute.list <- get.edge.attribute.network

# Retrieve a specified edge attribute from all edges in x.
#
#' @rdname attribute.methods
#' @export
get.edge.value <- function(x, ...) {
  UseMethod("get.edge.value")
}

#' @rdname attribute.methods
#' @export
get.edge.value.network <- function(x, attrname, unlist=TRUE, na.omit=FALSE, null.na=FALSE, deleted.edges.omit=FALSE, ...){
  get.edge.attribute(x,attrname,unlist,na.omit,null.na,deleted.edges.omit)
}

#' @rdname attribute.methods
#' @export
get.edge.value.list <- get.edge.value.network

# Retrieve the ID numbers for all edges incident on v, in network x.  
# Outgoing or incoming edges are specified by neighborhood, while na.omit 
# indicates whether or not missing edges should be omitted.  The return value
# is a vector of edge IDs.
#

#' @name get.edges
#'
#' @title Retrieve Edges or Edge IDs Associated with a Given Vertex
#' 
#' @description \code{get.edges} retrieves a list of edges incident on a given vertex;
#' \code{get.edgeIDs} returns the internal identifiers for those edges,
#' instead.  Both allow edges to be selected based on vertex neighborhood and
#' (optionally) an additional endpoint.
#' 
#' @details By default, \code{get.edges} returns all out-, in-, or out- and in-edges
#' containing \code{v}.  \code{get.edgeIDs} is identical, save in its return
#' value, as it returns only the ids of the edges.  Specifying a vertex in
#' \code{alter} causes these edges to be further selected such that alter must
#' also belong to the edge -- this can be used to extract edges between two
#' particular vertices.  Omission of missing edges is accomplished via
#' \code{na.omit}.  Note that for multiplex networks, multiple edges or edge
#' ids can be returned.
#' 
#' The function \code{get.dyads.eids} simplifies the process of looking up the
#' edge ids associated with a set of 'dyads' (tail and head vertex ids) for
#' edges. It only is intended for working with non-multiplex networks and will
#' return a warning and \code{NA} value for any dyads that correspond to
#' multiple edges. The value \code{numeric(0)} will be returned for any dyads
#' that do not have a corresponding edge.
#' 
#' @param x an object of class \code{network}
#' @param v a vertex ID
#' @param alter optionally, the ID of another vertex
#' @param neighborhood an indicator for whether we are interested in in-edges,
#' out-edges, or both (relative to \code{v}). defaults to \code{'combined'} for
#' undirected networks
#' @param na.omit logical; should we omit missing edges?
#' @param tails a vector of vertex ID for the 'tails' (v) side of the dyad
#' @param heads a vector of vertex ID for the 'heads' (alter) side of the dyad
#' @return For \code{get.edges}, a list of edges.  For \code{get.edgeIDs}, a
#' vector of edge ID numbers. For \code{get.dyads.eids}, a list of edge IDs
#' corresponding to the dyads defined by the vertex ids in \code{tails} and
#' \code{heads}
#' @author Carter T. Butts \email{buttsc@@uci.edu}
#' @seealso \code{\link{get.neighborhood}}, \code{\link{valid.eids}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{http://www.jstatsoft.org/v24/i02/}
#' @keywords classes graphs
#' @examples
#' 
#' #Create a network with three edges
#' m<-matrix(0,3,3)
#' m[1,2]<-1; m[2,3]<-1; m[3,1]<-1
#' g<-network(m)
#' 
#' get.edges(g,1,neighborhood="out")
#' get.edgeIDs(g,1,neighborhood="in")
#' 
#' @export get.edgeIDs
get.edgeIDs<-function(x, v, alter=NULL, neighborhood=c("out","in","combined"), na.omit=TRUE){
  #Check to be sure we were called with a network
  if(!is.network(x))
    stop("get.edgeIDs requires an argument of class network.")
  #Do some reality checking
  n<-network.size(x)
  if((v<1)||(v>n))
    return(numeric(0))
  if((!is.null(alter))&&((alter<1)||(alter>n)))
    return(numeric(0))
  #Retrieve the edges
  if(!is.directed(x))
    neighborhood="combined"       #If undirected, out==in==combined
  else
    neighborhood=match.arg(neighborhood)
  #Do the deed
  .Call(getEdgeIDs_R,x,v,alter,neighborhood,na.omit)
}


# Retrieve all edges incident on v, in network x.  Outgoing or incoming
# edges are specified by neighborhood, while na.omit indicates whether
# or not missing edges should be omitted.  The return value is a list of
# edges.
#

#' @rdname get.edges
#' @export get.edges
get.edges<-function(x, v, alter=NULL, neighborhood=c("out","in","combined"), na.omit=TRUE){
  #Check to be sure we were called with a network
  if(!is.network(x))
    stop("get.edges requires an argument of class network.")
  #Do some reality checking
  n<-network.size(x)
  if((v<1)||(v>n))
    return(list())
  if((!is.null(alter))&&((alter<1)||(alter>n)))
    return(list())
  #Retrieve the edges
  if(!is.directed(x))
    neighborhood="combined"       #If undirected, out==in==combined
  else
    neighborhood=match.arg(neighborhood)
  #Do the deed
  .Call(getEdges_R,x,v,alter,neighborhood,na.omit)
}

# get the the edge ids associated with a set of dayds
# as defined by a vector of tails and heads vertex ids
#' @rdname get.edges
#' @export get.dyads.eids
get.dyads.eids<-function(x,tails,heads,neighborhood = c("out", "in", "combined")){
  if(length(tails)!=length(heads)){
    stop('heads and tails vectors must be the same length for get.dyads.eids')
  }
  if (any(heads>network.size(x) | heads<1) | any(tails>network.size(x) | tails<1)){
    stop('invalid vertex id in heads or tails vector')
  }
  neighborhood<-match.arg(neighborhood)
  if (!is.directed(x)){
    neighborhood = "combined"
  }
  lapply(seq_along(tails),function(e){
    eid<-get.edgeIDs(x,v = tails[e],alter=heads[e],neighborhood=neighborhood)
    if(length(eid)>1){
      eid<-NA
      warning('get.dyads.eids found multiple edge ids for dyad ',tails[e],',',heads[e],' NA will be returned')
    }
    eid
  })
}


# Given a network and a set of vertices, return the subgraph induced by those
# vertices (preserving all associated metadata); if given two such sets, 
# return the edge cut (along with the associated vertices and meta-data) as
# a bipartite network.
#


#' Retrieve Induced Subgraphs and Cuts
#' 
#' Given a set of vertex IDs, \code{get.inducedSubgraph} returns the subgraph
#' induced by the specified vertices (i.e., the vertices and all associated
#' edges).  Optionally, passing a second set of alters returns the cut from the
#' first to the second set (i.e., all edges passing between the sets), along
#' with the associated endpoints. Alternatively, passing in a vector of edge
#' ids will induce a subgraph containing the specified edges and their incident
#' vertices.  In all cases, the result is returned as a network object, with
#' all attributes of the selected edges and/or vertices (and any network
#' attributes) preserved.
#' 
#' For \code{get.inducedSubgraph}, \code{v} can be a vector of vertex IDs.  If
#' \code{alter=NULL}, the subgraph induced by these vertices is returned.
#' Calling \code{\%s\%} with a single vector of vertices has an identical effect.
#' 
#' Where \code{alters} is specified, it must be a vector of IDs disjoint with
#' \code{v}.  Where both are given, the edges spanning \code{v} and
#' \code{alters} are returned, along with the vertices in question.
#' (Technically, only the edges really constitute the \dQuote{cut,} but the
#' vertices are included as well.)  The same result can be obtained with the
#' \code{\%s\%} operator by passing a two-element list on the right hand side;
#' the first element is then interpreted as \code{v}, and the second as
#' \code{alters}.
#' 
#' When \code{eid} is specified, the \code{v} and \code{alters} argument will
#' be ignored and the subgraph induced by the specified edges and their
#' incident vertices will be returned.
#' 
#' Any network, vertex, or edge attributes for the selected network elements
#' are retained (although features such as vertex IDs and the network size will
#' typically change).  These are copies of the elements in the original
#' network, which is not altered by this function.
#' 
#' @param x an object of class \code{network}.
#' @param v a vector of vertex IDs, or, for \code{\%s\%}, optionally a list containing two disjoint vectors of vertex IDs (see below).
#'
#' @param alters optionally, a second vector of vertex IDs.  Must be disjoint
#' with \code{v}.
#'
#' @param eid optionally, a numeric vector of valid edge ids in \code{x} that
#' should be retained (cannot be used with \code{v} or \code{alter})
#'
#' @return A \code{\link{network}} object containing the induced subgraph.
#' @author Carter T. Butts \email{buttsc@@uci.edu}
#' @seealso \code{\link{network}}, \code{\link{network.extraction}}
#' @keywords graphs manip
#' @examples
#' 
#' #Load the Drabek et al. EMON data
#' data(emon)
#' 
#' #For the Mt. St. Helens, EMON, several types of organizations are present:
#' type<-emon$MtStHelens %v% "Sponsorship"
#' 
#' #Plot interactions among the state organizations
#' plot(emon$MtStHelens %s% which(type=="State"), displaylabels=TRUE)
#' 
#' #Plot state/federal interactions
#' plot(emon$MtStHelens %s% list(which(type=="State"), 
#'     which(type=="Federal")),  displaylabels=TRUE)
#' 
#' #Plot state interactions with everyone else
#' plot(emon$MtStHelens %s% list(which(type=="State"), 
#'     which(type!="State")), displaylabels=TRUE)
#'     
#' # plot only interactions with frequency of 2
#' subG2<-get.inducedSubgraph(emon$MtStHelens,
#'             eid=which(emon$MtStHelens%e%'Frequency'==2))
#' plot(subG2,edge.label='Frequency')
#' 
#' 
#' @export get.inducedSubgraph
get.inducedSubgraph<-function(x, v, alters=NULL, eid=NULL){
  #Check to be sure we were called with a network
  if(!is.network(x))
    stop("get.inducedSubgraph requires an argument of class network.")
  #Do some reality checking
  n<-network.size(x)
  
  # are we doing this via eids, or v and alters
  if (is.null(eid)){  # do checks for v and alters
    if((length(v)<1)||any(is.na(v))||any(v<1)||any(v>n))
      stop("Illegal vertex selection in get.inducedSubgraph")
    if(!is.null(alters)){
      if((length(alters)<1)||any(is.na(alters))||any(alters<1)||any(alters>n)|| any(alters%in%v))
        stop("Illegal vertex selection (alters) in get.inducedSubgraph")
    }
    if (!is.null(eid)){
      warning('eid argument to get.inducedSubgraph ignored when using v or alter argument')
    }
  } else { # do checks for eids
    if (!is.numeric(eid)){
      stop('eid must be a numeric vector of edge ids')
    }
    if (!missing(v)){
      warning('v argument to get.inducedSubgraph ignored when using eid argument')
    }
    if (!is.null(alters)){
      warning('alters argument to get.inducedSubgraph ignored when using eid argument')
    }
    # check that eids are valid
    if (any(!eid%in%valid.eids(x))){
      stop('eid argument contains non-valid edge ids')
    }
    
  }
  
  #Start by making a copy of our target network (yes, this can be wasteful)
  #TODO: in most cases, probably faster to create a new network and only copy over what is needed
  newNet<-network.copy(x)
  
  if (is.null(eid)){  # using v and alter
    #Now, strip out what is needed, and/or permute in the two-mode case
    if(is.null(alters)){                    #Simple case
      delete.vertices(newNet,(1:n)[-v])           #Get rid of everyone else
    }else{                                  #Really an edge cut, but w/vertices
      nv<-length(v)
      na<-length(alters)
      newids<-sort(c(v,alters))
      newv<-match(v,newids)
      newalt<-match(alters,newids)
      delete.vertices(newNet,(1:n)[-c(v,alters)])  #Get rid of everyone else
      permute.vertexIDs(newNet,c(newv,newalt))    #Put the new vertices first
      #Remove within-group edges
      for(i in 1:nv)
        for(j in (i:nv)[-1]){
          torem<-get.edgeIDs(newNet,i,alter=j,neighborhood="combined",na.omit=FALSE)
          if(length(torem)>0)
            delete.edges(newNet,torem)
        }
      for(i in (nv+1):(nv+na))
        for(j in (i:(nv+na))[-1]){
          torem<-get.edgeIDs(newNet,i,alter=j,neighborhood="combined",na.omit=FALSE)
          if(length(torem)>0)
            delete.edges(newNet,torem)
        }
      newNet%n%"bipartite"<-nv   #Set bipartite attribute
    }
  } else {  # using eids instead of v and alters
    # delete all the edges not in eid
    removeEid<-setdiff(valid.eids(newNet),eid)
    delete.edges(newNet,removeEid)
    # find the set of vertices incident on the remaining edges
    v<-unique(c(unlist(sapply(newNet$mel, "[[", "outl")),unlist(sapply(newNet$mel, "[[", "inl"))))
    removeV<-setdiff(seq_len(network.size(newNet)),v)
    delete.vertices(newNet,removeV)
  }
  #Return the updated object
  newNet
}


# Retrieve a specified network-level attribute from network x.  The attribute
# type depends on the underlying storage mode, and cannot be guaranteed.
#
#' @rdname attribute.methods
#' @export

get.network.attribute <- function(x, ...) {
  UseMethod("get.network.attribute")
}

#' @rdname attribute.methods
#' @export
get.network.attribute.network <- function(x, attrname, unlist=FALSE, ...) {
  if(!has.network.attribute(x, attrname)) {
    warning(
      paste0("there is no attribute ", sQuote(attrname), " in ",
             sQuote(deparse(substitute(x))))
    )
  }
  x <- x$gal[[attrname]]
  if(unlist){unlist(x)}else{x}
}

# Retrieve the neighborhood of v in network x.  Depending on the value of 
# type, the neighborhood in question may be in, out, or the union of the two.
# The return value for the function is a vector containing vertex IDs.
#


#' Obtain the Neighborhood of a Given Vertex
#' 
#' \code{get.neighborhood} returns the IDs of all vertices belonging to the in,
#' out, or combined neighborhoods of \code{v} within network \code{x}.
#' 
#' Note that the combined neighborhood is the union of the in and out
#' neighborhoods -- as such, no vertex will appear twice.
#' 
#' @param x an object of class \code{network}
#' @param v a vertex ID
#' @param type the neighborhood to be computed
#' @param na.omit logical; should missing edges be ignored when obtaining
#' vertex neighborhoods?
#' @return A vector containing the vertex IDs for the chosen neighborhood.
#' @author Carter T. Butts \email{buttsc@@uci.edu}
#' @seealso \code{\link{get.edges}}, \code{\link{is.adjacent}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{http://www.jstatsoft.org/v24/i02/}
#' 
#' Wasserman, S. and Faust, K.  1994.  \emph{Social Network Analysis: Methods
#' and Applications.} Cambridge: Cambridge University Press.
#' @keywords graphs
#' @examples
#' 
#' #Create a network with three edges
#' m<-matrix(0,3,3)
#' m[1,2]<-1; m[2,3]<-1; m[3,1]<-1
#' g<-network(m)
#' 
#' #Examine the neighborhood of vertex 1
#' get.neighborhood(g,1,"out")
#' get.neighborhood(g,1,"in")
#' get.neighborhood(g,1,"combined")
#' 
#' @export get.neighborhood
get.neighborhood<-function(x, v, type=c("out","in","combined"), na.omit=TRUE){
  #Check to be sure we were called with a network
  if(!is.network(x))
    stop("get.neighborhood requires an argument of class network.")
  #Do some reality checking
  n<-network.size(x)
  if((v<1)||(v>n))
    return(numeric(0))
  #Retrieve the edges
  if(!is.directed(x))
    type="combined"       #If undirected, out==in==combined
  else
    type=match.arg(type)
  #Do the deed
  .Call(getNeighborhood_R,x,v,type,na.omit)
}


# Retrieve a specified vertex attribute (indicated by attrname) from network x.
# Where na.omit==TRUE, values for missing vertices are removed; where
# null.na==TRUE, NULL values are converted to NAs.  The return value of this
# function is a list.
# 
#' @rdname attribute.methods
#' @export
get.vertex.attribute <- function(x, ...) {
  UseMethod("get.vertex.attribute")
}

#' @rdname attribute.methods
#' @export
get.vertex.attribute.network <- function(x, attrname, na.omit=FALSE, null.na=TRUE, unlist=TRUE, ...) {
  #Check to see if there's anything to be done
  if(network.size(x)==0){
    return(NULL)
  }
  if(!has.vertex.attribute(x, attrname))
    warning(paste('attribute', attrname,'is not specified for these vertices'))
  #Get the list of attribute values
  va<-lapply(x$val,"[[",attrname)
  #If needed, figure out who's missing
  if(na.omit)
    vna<-unlist(lapply(x$val,"[[","na"))
  else
    vna<-rep(FALSE,length(va))
  #Replace NULL values with NAs, if requested
  if(null.na)
    va[sapply(va,is.null)]<-NA
  #Return the result
  if (na.omit){
   x <- va[!vna]
  } else {
    x<-va
  }
  if(unlist){unlist(x)}else{x}
}


# Return TRUE iff network x has loops.
#
#' Indicator Functions for Network Properties
#' 
#' Various indicators for properties of \code{network} class objects.
#' 
#' These methods are the standard means of assessing the state of a
#' \code{network} object; other methods can (and should) use these routines in
#' governing their own behavior.  As such, improper setting of the associated
#' attributes may result in unpleasantly creative results.  (See the
#' \code{edge.check} argument to \code{\link{add.edges}} for an example of code
#' which makes use of these network properties.)
#' 
#' The functions themselves behave has follows:
#' 
#' \code{has.loops} returns \code{TRUE} iff \code{x} is allowed to contain
#' loops (or loop-like edges, in the hypergraphic case).
#' 
#' \code{is.bipartite} returns \code{TRUE} iff the \code{x} has been explicitly
#' bipartite-coded. Values of \code{bipartite=NULL}, and \code{bipartite=FALSE}
#' will evaluate to \code{FALSE}, numeric values of \code{bipartite>=0} will
#' evaluate to \code{TRUE}. (The value \code{bipartite==0} indicates that it is
#' a bipartite network with a zero-sized first partition.) Note that
#' \code{is.bipartite} refers only to the storage properties of \code{x} and
#' how it should be treated by some algorithms; \code{is.bipartite(x)==FALSE}
#' it does \emph{not} mean that \code{x} cannot admit a bipartition!
#' 
#' \code{is.directed} returns \code{TRUE} iff the edges of \code{x} are to be
#' interpreted as directed.
#' 
#' \code{is.hyper} returns \code{TRUE} iff \code{x} is allowed to contain
#' hypergraphic edges.
#' 
#' \code{is.multiplex} returns \code{TRUE} iff \code{x} is allowed to contain
#' multiplex edges.
#' 
#' @name network.indicators
#'
#' @param x an object of class \code{network}
#' @return \code{TRUE} or \code{FALSE}
#' @author Carter T. Butts \email{buttsc@@uci.edu}
#' @seealso \code{\link{network}}, \code{\link{get.network.attribute}},
#' \code{set.network.attribute}, \code{\link{add.edges}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{http://www.jstatsoft.org/v24/i02/}
#' @keywords classes graphs
#' @examples
#' 
#' g<-network.initialize(5)    #Initialize the network
#' is.bipartite(g)
#' is.directed(g)
#' is.hyper(g)
#' is.multiplex(g)
#' has.loops(g)
#' 
#' @export
has.loops<-function(x){
  if(!is.network(x))
    stop("has.loops requires an argument of class network.")
  else
    get.network.attribute(x,"loops")
}


# Return TRUE iff (vi,vj) in network x.  Where na.omit==TRUE, edges flagged
# as missing are ignored.
#

#' @rdname attribute.methods
#'
#' @details Functions \code{has.vertex.attribute}, \code{has.edge.attribute},
#'   and \code{has.network.attribute} test if attribute \code{attrname} is
#'   present in network \code{x}.
#'
#' @return Functions \code{has.vertex.attribute}, \code{has.edge.attribute}, and
#'   \code{has.network.attribute} return \code{TRUE} if attribute
#'   \code{attrname} is present in network \code{x}, \code{FALSE} otherwise.
#'
#' @export

has.vertex.attribute <- function(x, ...) UseMethod("has.vertex.attribute")

#' @rdname attribute.methods
#' @export
has.vertex.attribute.network <- function(x, attrname) {
  attrname %in% list.vertex.attributes(x)
}

#' @rdname attribute.methods
#' @export
has.edge.attribute <- function(x, ...) UseMethod("has.edge.attribute")

#' @rdname attribute.methods
#' @export
has.edge.attribute.network <- function(x, attrname) {
  attrname %in% list.edge.attributes(x)
}

#' @rdname attribute.methods
#' @export
has.network.attribute <- function(x, ...) UseMethod("has.network.attribute")

#' @rdname attribute.methods
#' @export
has.network.attribute <- function(x, attrname) {
  attrname %in% list.network.attributes(x)
}




#' Determine Whether Two Vertices Are Adjacent
#' 
#' \code{is.adjacent} returns \code{TRUE} iff \code{vi} is adjacent to
#' \code{vj} in \code{x}.  Missing edges may be omitted or not, as per
#' \code{na.omit}.
#' 
#' Vertex \eqn{v} is said to be adjacent to vertex \eqn{v'} within directed
#' network \eqn{G} iff there exists some edge whose tail set contains \eqn{v}
#' and whose head set contains \eqn{v'}.  In the undirected case, head and tail
#' sets are exchangeable, and thus \eqn{v} is adjacent to \eqn{v'} if there
#' exists an edge such that \eqn{v} belongs to one endpoint set and \eqn{v'}
#' belongs to the other.  (In dyadic graphs, these sets are of cardinality 1,
#' but this may not be the case where hyperedges are admitted.)
#' 
#' If an edge which would make \eqn{v} and \eqn{v'} adjacent is marked as
#' missing (via its \code{na} attribute), then the behavior of
#' \code{is.adjacent} depends upon \code{na.omit}.  If \code{na.omit==FALSE}
#' (the default), then the return value is considered to be \code{NA} unless
#' there is also \emph{another} edge from \eqn{v} to \eqn{v'} which is
#' \emph{not} missing (in which case the two are clearly adjacent).  If
#' \code{na.omit==TRUE}, on the other hand the missing edge is simply
#' disregarded in assessing adjacency (i.e., it effectively treated as not
#' present).  It is important not to confuse \dQuote{not present} with
#' \dQuote{missing} in this context: the former indicates that the edge in
#' question does not belong to the network, while the latter indicates that the
#' state of the corresponding edge is regarded as unknown.  By default, all
#' edge states are assumed \dQuote{known} unless otherwise indicated (by
#' setting the edge's \code{na} attribute to \code{TRUE}; see
#' \code{\link{attribute.methods}}).
#' 
#' Adjacency can also be determined via the extraction/replacement operators.
#' See the associated man page for details.
#' 
#' @param x an object of class \code{network}
#' @param vi a vertex ID
#' @param vj a second vertex ID
#' @param na.omit logical; should missing edges be ignored when assessing
#' adjacency?
#' @return A logical, giving the status of the (i,j) edge
#' @note Prior to version 1.4, \code{na.omit} was set to \code{TRUE} by
#' default.
#' @author Carter T. Butts \email{buttsc@@uci.edu}
#' @seealso \code{\link{get.neighborhood}}, \code{\link{network.extraction}},
#' \code{\link{attribute.methods}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{http://www.jstatsoft.org/v24/i02/}
#' 
#' Wasserman, S. and Faust, K.  1994.  \emph{Social Network Analysis: Methods
#' and Applications}.  Cambridge: Cambridge University Press.
#' @keywords graphs
#' @examples
#' 
#' #Create a very simple graph
#' g<-network.initialize(3)
#' add.edge(g,1,2)
#' is.adjacent(g,1,2)  #TRUE
#' is.adjacent(g,2,1)  #FALSE
#' g[1,2]==1           #TRUE
#' g[2,1]==1           #FALSE
#' 
#' @export is.adjacent
is.adjacent<-function(x,vi,vj,na.omit=FALSE){
  if(!is.network(x))
    stop("is.adjacent requires an argument of class network.\n")
  if(length(vi)!=length(vj)){
    vi<-rep(vi,length=max(length(vi),length(vj)))
    vj<-rep(vj,length=max(length(vi),length(vj)))
  }
  #Do the deed
 .Call(isAdjacent_R,x,vi,vj,na.omit)
}


# Return TRUE iff network x is bipartite
#
#' @rdname network.indicators
#' @export
is.bipartite<-function(x){
  if(!is.network(x))
    stop("is.bipartite requires an argument of class network.")
  else
    bip <- get.network.attribute(x,"bipartite")
  if(is.null(bip)){
   return(FALSE)
  } else if (is.logical(bip)){
   return(bip)  
  }else{
   return(bip>=0)
  }
}


# Return TRUE iff network x is directed.
#
#' @rdname network.indicators
#' @export
is.directed<-function(x){
  if(!is.network(x))
    stop("is.directed requires an argument of class network.\n")
  else
    get.network.attribute(x,"directed")
}


# Return TRUE iff network x is hypergraphic.
#
#' @rdname network.indicators
#' @export
is.hyper<-function(x){
  if(!is.network(x))
    stop("is.hyper requires an argument of class network.\n")
  else
    get.network.attribute(x,"hyper")
}


# Return TRUE iff network x is multiplex.
#
#' @rdname network.indicators
#' @export
is.multiplex<-function(x){
  if(!is.network(x))
    stop("is.multiplex requires an argument of class network.\n")
  else
    get.network.attribute(x,"multiple")
}


# Return a network whose edges are the missing edges of x
#

#' @rdname network.naedgecount
#' @name missing.edges
#' @title Identifying and Counting Missing Edges in a Network Object
#' 
#' @description \code{network.naedgecount} returns the number of edges within a
#' \code{network} object which are flagged as missing.  The \code{is.na}
#' network method returns a new network containing the missing edges.
#' 
#' @details The missingness of an edge is controlled by its \code{na} attribute (which
#' is mandatory for all edges); \code{network.naedgecount} returns the number
#' of edges for which \code{na==TRUE}.  The \code{is.na} network method
#' produces a new network object whose edges correspond to the missing
#' (\code{na==TRUE}) edges of the original object, and is thus a covenient
#' method of extracting detailed missingness information on the entire network.
#' The network returned by \code{is.na} is guaranteed to have the same base
#' network attributes (directedness, loopness, hypergraphicity, multiplexity,
#' and bipartite constraint) as the original network object, but no other
#' information is copied; note too that edge IDs are \emph{not} preserved by
#' this process (although adjacency obviously is).  Since the resulting object
#' is a \code{\link{network}}, standard coercion, print/summary, and other
#' methods can be applied to it in the usual fashion.
#' 
#' It should be borne in mind that \dQuote{missingness} in the sense used here
#' reflects the assertion that an edge's presence or absence is unknown,
#' \emph{not} that said edge is known not to be present.  Thus, the \code{na}
#' count for an empty graph is properly 0, since all edges are known to be
#' absent.  Edges can be flagged as missing by setting their \code{na}
#' attribute to \code{TRUE} using \code{\link{set.edge.attribute}}, or by
#' appropriate use of the network assignment operators; see below for an
#' example of the latter.
#' 
#' @param x an object of class \code{network}
#' @param \dots additional arguments, not used
#' @return \code{is.na(x)} returns a network object, and
#' \code{network.naedgecount(x)} returns the number of missing edges.
#' @author Carter T. Butts \email{buttsc@@uci.edu}
#' @seealso \code{\link{network.edgecount}},
#' \code{\link{get.network.attribute}}, \code{is.adjacent}, \code{\link{is.na}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{http://www.jstatsoft.org/v24/i02/}
#' @keywords classes graphs
#' @examples
#' 
#' #Create an empty network with no missing data
#' g<-network.initialize(5)
#' g[,]                        #No edges present....
#' network.naedgecount(g)==0   #Edges not present are not "missing"!
#' 
#' #Now, add some missing edges
#' g[1,,add.edges=TRUE]<-NA    #Establish that 1's ties are unknown
#' g[,]                        #Observe the missing elements
#' is.na(g)                    #Observe in network form
#' network.naedgecount(g)==4   #These elements do count!
#' network.edgecount(is.na(g)) #Same as above
#' 
#' 
#' @export is.na.network
#' @export
is.na.network<-function(x){
  #Create an empty network with the same properties as x
  y<-network.initialize(network.size(x),directed=is.directed(x), hyper=is.hyper(x),loops=has.loops(x),multiple=is.multiplex(x), bipartite=x%n%"bipartite")
  #Add the missing edges of x to y
  y<-.Call(isNANetwork_R,x,y)
  #Return the updated network 
  y
}


# Return TRUE iff x is a network.
#
#' Network Objects
#' 
#' Construct, coerce to, test for and print \code{network} objects.
#' 
#' \code{network} constructs a \code{network} class object from a matrix
#' representation. If the \code{matrix.type} parameter is not specified, it
#' will make a guess as to the intended \code{edgeset.constructors} function to
#' call based on the format of these input matrices. If the class of \code{x}
#' is not a matrix, network construction can be dispatched to other methods.
#' For example, If the \code{ergm} package is loaded, \code{network()} can
#' function as a shorthand for \code{as.network.numeric} with
#' \code{x} as an integer specifying the number of nodes to be created in the
#' random graph.
#' 
#' If the \code{ergm} package is loaded, \code{network} can function as a
#' shorthand for \code{as.network.numeric} if \code{x} is an integer specifying
#' the number of nodes. See the help page for
#' \code{as.network.numeric} in \code{ergm} package for details.
#' 
#' \code{network.copy} creates a new \code{network} object which duplicates its
#' supplied argument.  (Direct assignment with \code{<-} should be used rather
#' than \code{network.copy} in most cases.)
#' 
#' \code{as.network} tries to coerce its argument to a network, using the
#' \code{as.network.matrix} functions if \code{x} is a matrix. (If the argument
#' is already a network object, it is returned as-is and all other arguments
#' are ignored.)
#' 
#' \code{is.network} tests whether its argument is a network (in the sense that
#' it has class \code{network}).
#' 
#' \code{print.network} prints a network object in one of several possible
#' formats.  It also prints the list of global attributes of the network.
#' 
#' \code{summary.network} provides similar information.
#' 
#' @name network
#' 
#' @aliases as.network.network print.summary.network $<-.network <-.network
#' @param x for \code{network}, a matrix giving the network structure in
#' adjacency, incidence, or edgelist form; otherwise, an object of class
#' \code{network}.
#' @param vertex.attr optionally, a list containing vertex attributes.
#' @param vertex.attrnames optionally, a list containing vertex attribute
#' names.
#' @param directed logical; should edges be interpreted as directed?
#' @param hyper logical; are hyperedges allowed?
#' @param loops logical; should loops be allowed?
#' @param multiple logical; are multiplex edges allowed?
#' @param bipartite count; should the network be interpreted as bipartite? If
#' present (i.e., non-NULL, non-FALSE) it is the count of the number of actors
#' in the bipartite network. In this case, the number of nodes is equal to the
#' number of actors plus the number of events (with all actors preceeding all
#' events). The edges are then interpreted as nondirected. Values of
#' bipartite==0 are permited, indicating a bipartite network with zero-sized
#' first partition.
#' @param matrix.type one of \code{"adjacency"}, \code{"edgelist"},
#' \code{"incidence"}. See \code{\link{edgeset.constructors}} for details and
#' optional additional arguments
#' @param object an object of class \code{network}.
#' @param na.omit logical; omit summarization of missing attributes in
#' \code{network}?
#' @param mixingmatrices logical; print the mixing matrices for the discrete
#' attributes?
#' @param print.adj logical; print the network adjacency structure?
#' @param ... additional arguments.
#' @return \code{network}, \code{as.network}, and \code{print.network} all
#' return a network class object; \code{is.network} returns TRUE or FALSE.
#' @note Between versions 0.5 and 1.2, direct assignment of a network object
#' created a pointer to the original object, rather than a copy.  As of version
#' 1.2, direct assignment behaves in the same manner as \code{network.copy}.
#' Direct use of the latter is thus superfluous in most situations, and is
#' discouraged.
#' 
#' Many of the network package functions modify their network object arguments
#' in-place. For example, \code{set.network.attribute(net,"myVal",5)} will have
#' the same effect as \code{net<-set.network.attribute(net,"myVal",5)}.
#' Unfortunately, the current implementation of in-place assignment breaks when
#' the network argument is an element of a list or a named part of another
#' object. So \code{set.network.attribute(myListOfNetworks[[1]],"myVal",5)}
#' will silently fail to modify its network argument, likely leading to
#' incorrect output.
#' @author Carter T. Butts \email{buttsc@@uci.edu} and David Hunter
#' \email{dhunter@@stat.psu.edu}
#' @seealso \code{\link{network.initialize}}, \code{\link{attribute.methods}},
#' \code{\link{as.network.matrix}}, \code{\link{as.matrix.network}},
#' \code{\link{deletion.methods}}, \code{\link{edgeset.constructors}},
#' \code{\link{network.indicators}}, \code{\link{plot.network}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{http://www.jstatsoft.org/v24/i02/}
#' @keywords classes graphs
#' @examples
#' 
#' m <- matrix(rbinom(25,1,.4),5,5)
#' diag(m) <- 0
#' g <- network(m, directed=FALSE)
#' summary(g)
#' 
#' h <- network.copy(g)       #Note: same as h<-g
#' summary(h)
#' 
#' @export
is.network<-function(x){
  inherits(x, "network")
}


# List attributes present on any edge
#
#' @rdname attribute.methods
#' @export
list.edge.attributes <- function(x, ...) {
  UseMethod("list.edge.attributes")
}

#' @rdname attribute.methods
#' @export
list.edge.attributes.network <- function(x, ...) {
  # no edges in the network
  if (network.edgecount(x, na.omit=F) == 0) return(character(0))
  #Accumulate names
  allnam<-sapply(lapply(x$mel[!is.null(x$mel)],"[[","atl"),names)
  #Return the sorted, unique attribute names
  sort(unique(as.vector(unlist(allnam))))
}


# List network-level attributes
#
#' @rdname attribute.methods
#' @export
list.network.attributes <- function(x, ...) {
  UseMethod("list.network.attributes")
}

#' @rdname attribute.methods
#' @export
list.network.attributes.network <- function(x, ...) {
  #Return the attribute names
  sort(names(x$gal))
}


# List attributes present on any vertex
#
#' @rdname attribute.methods
#' @export
list.vertex.attributes <- function(x, ...) {
  UseMethod("list.vertex.attributes")
}

#' @rdname attribute.methods
#' @export
list.vertex.attributes.network <- function(x, ...) {
  if(network.size(x)==0){
    return(NULL)
  }
  #Accumulate names
  allnam<-unlist(sapply(x$val,names))
  #Return the sorted, unique attribute names
  sort(unique(as.vector(allnam)))
}


# Retrieve the number of free dyads (i.e., number of non-missing) of network x.
#


#' @export
network.dyadcount<-function(x, ...) UseMethod("network.dyadcount")

#' Return the Number of (Possibly Directed) Dyads in a Network Object
#' 
#' \code{network.dyadcount} returns the number of possible dyads within a
#' \code{network}, removing those flagged as missing if desired.  If the
#' network is directed, directed dyads are counted accordingly.
#' 
#' The return value \code{network.dyadcount} is equal to the number of dyads,
#' minus the number of \code{NULL} edges (and missing edges, if
#' \code{na.omit==TRUE}).  If \code{x} is directed, the number of directed
#' dyads is returned. If the network allows loops, the number of possible
#' entries on the diagnonal is added.  Allthough the function does not give an
#' error on multiplex networks or hypergraphs, the results probably don't make
#' sense.
#' 
#' @name network.dyadcount
#'
#' @param x an object of class \code{network}
#' @param na.omit logical; omit edges with \code{na==TRUE} from the count?
#' @param \dots possible additional arguments, used by other implementations
#' @return The number of dyads in the network
#' @author Mark S. Handcock \email{handcock@@stat.washington.edu}, skyebend
#' @seealso \code{\link{get.network.attribute}},
#' \code{\link{network.edgecount}}, \code{\link{is.directed}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{http://www.jstatsoft.org/v24/i02/}
#' @keywords classes graphs
#' @examples
#' 
#' #Create a directed network with three edges
#' m<-matrix(0,3,3)
#' m[1,2]<-1; m[2,3]<-1; m[3,1]<-1
#' g<-network(m)
#' network.dyadcount(g)==6                 #Verify the directed dyad count
#' g<-network(m|t(m),directed=FALSE)
#' network.dyadcount(g)==3                         #nC2 in undirected case
#' 
#' @export
network.dyadcount.network<-function(x,na.omit=TRUE,...){
 nodes <- network.size(x)
 if(is.directed(x)){
   if(is.bipartite(x)){ # directed bipartite
     nactor <- get.network.attribute(x,"bipartite")
     nevent <- nodes - nactor
     dyads <- nactor * nevent *2
   }else{ # directed unipartite
    dyads <- nodes * (nodes-1)
    if(has.loops(x)){
      # add in the diagonal
      dyads<-dyads+nodes
    }
   }
 }else{ # undirected
  if(is.bipartite(x)){ # undirected bipartite
   nactor <- get.network.attribute(x,"bipartite")
   nevent <- nodes - nactor
   dyads <- nactor * nevent
  }else{ # undirected unipartite
   dyads <- nodes * (nodes-1)/2
   if(has.loops(x)){
     # add in the diagonal
     dyads<-dyads+nodes
   }
  }
 }
 if(na.omit){
#
#  Adjust for missing
#
  design <- get.network.attribute(x,"design")
  if(!is.null(design)){
   dyads <- dyads - network.edgecount(design)
  }else{
   design <- get.network.attribute(x,"mClist.design")
   if(!is.null(design)){
    dyads <- dyads - design$nedges
   }else{
    dyads <- dyads - network.naedgecount(x)
   }
  }
 }
 dyads
}


#Retrieve the number of edges in network x.
#

#' @export
network.edgecount<-function(x, ...) UseMethod("network.edgecount")

#' Return the Number of Edges in a Network Object
#' 
#' \code{network.edgecount} returns the number of edges within a
#' \code{network}, removing those flagged as missing if desired.
#' 
#' The return value is the number of distinct edges within the network object,
#' including multiplex edges as appropriate.  (So if there are 3 edges from
#' vertex i to vertex j, each contributes to the total edge count.)
#' 
#' The return value \code{network.edgecount} is in the present implementation
#' related to the (required) \code{mnext} network attribute.  \code{mnext} is
#' an internal legacy attribute that currently indicates the index number of
#' the next edge to be added to a network object.  (Do not modify it unless you
#' enjoy unfortunate surprises.)  The number of edges returned by
#' \code{network.edgecount} is equal to \code{x\%n\%"mnext"-1}, minus the number
#' of \code{NULL} edges (and missing edges, if \code{na.omit==TRUE}).  Note
#' that \code{g\%n\%"mnext"-1} cannot, by itself, be counted upon to be an
#' accurate count of the number of edges!  As \code{mnext} is not part of the
#' API (and is not guaranteed to remain), users and developers are urged to use
#' \code{network.edgecount} instead.
#' 
#' @name network.edgecount
#'
#' @param x an object of class \code{network}
#' @param na.omit logical; omit edges with \code{na==TRUE} from the count?
#' @param \dots additional arguments, used by extending functio
#' @return The number of edges
#' @section Warning : \code{network.edgecount} uses the real state of the
#' network object to count edges, not the state it hypothetically should have.
#' Thus, if you add extra edges to a non-multiplex network, directed edges to
#' an undirected network, etc., the actual number of edges in the object will
#' be returned (and not the number you would expect if you relied only on the
#' putative number of possible edges as reflected by the
#' \link{network.indicators}).  Don't create \code{network} objects with
#' contradictory attributes unless you know what you are doing.
#' @author Carter T. Butts \email{buttsc@@uci.edu}
#' @seealso \code{\link{get.network.attribute}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{http://www.jstatsoft.org/v24/i02/}
#' @keywords classes graphs
#' @examples
#' 
#' #Create a network with three edges
#' m<-matrix(0,3,3)
#' m[1,2]<-1; m[2,3]<-1; m[3,1]<-1
#' g<-network(m)
#' network.edgecount(g)==3   #Verify the edgecount
#' 
#' @export
network.edgecount.network<-function(x,na.omit=TRUE,...){
  .Call(networkEdgecount_R,x,na.omit)
}


#Retrieve the number of missing edges in network x
#
#' @rdname network.naedgecount
#' @export
network.naedgecount<-function(x, ...) UseMethod("network.naedgecount")

#' @export
network.naedgecount.network<-function(x, ...){
  na<-get.edge.attribute(x$mel,"na")
  if(is.null(na))
    0
  else
    sum(na)
}


# Retrieve the size (i.e., number of vertices) of network x.
#


#' Return the Size of a Network
#' 
#' \code{network.size} returns the order of its argument (i.e., number of
#' vertices).
#' 
#' \code{network.size(x)} is equivalent to \code{get.network.attribute(x,"n")};
#' the function exists as a convenience.
#' 
#' @param x an object of class \code{network}
#' @param \dots additional arguments, not used
#' @return The network size
#' @author Carter T. Butts \email{buttsc@@uci.edu}
#' @seealso \code{\link{get.network.attribute}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{http://www.jstatsoft.org/v24/i02/}
#' @keywords classes graphs
#' @examples
#' 
#' #Initialize a network
#' g<-network.initialize(7)
#' network.size(g)
#' 
#' @export network.size
network.size<-function(x, ...) UseMethod("network.size")

#' @export
network.size.network<-function(x, ...){
  get.network.attribute(x,"n")
}


# Retrieve the vertex names of network x (if present).
#
#' @rdname attribute.methods
#' @export
network.vertex.names<-function(x){
  if(!is.network(x)){
    stop("network.vertex.names requires an argument of class network.")
  }else{
    if(network.size(x)==0)
      return(NULL)
    if(has.vertex.attribute(x, "vertex.names")) {
      get.vertex.attribute(x, "vertex.names")
    } else {
      as.character(1:network.size(x))
    }
  }
}


# Set the vertex names of network x
#
#' @rdname attribute.methods
#' @export
"network.vertex.names<-"<-function(x,value){
  set.vertex.attribute(x,attrname="vertex.names",value=value)
}


# Permute the internal IDs (ordering) of the vertex set


#' Permute (Relabel) the Vertices Within a Network
#' 
#' \code{permute.vertexIDs} permutes the vertices within a given network in the
#' specified fashion.  Since this occurs internally (at the level of vertex
#' IDs), it is rarely of interest to end-users.
#' 
#' \code{permute.vertexIDs} alters the internal ordering of vertices within a
#' \code{\link{network}}.  For most practical applications, this should not be
#' necessary -- de facto permutation can be accomplished by altering the
#' appropriate vertex attributes.  \code{permute.vertexIDs} is needed for
#' certain other routines (such as \code{\link{delete.vertices}}), where it is
#' used in various arcane and ineffable ways.
#' 
#' @param x an object of class \code{\link{network}}.
#' @param vids a vector of vertex IDs, in the order to which they are to be
#' permuted.
#' @return Invisibly, a pointer to the permuted network.
#' \code{permute.vertexIDs} modifies its argument in place.
#' @author Carter T. Butts \email{buttsc@@uci.edu}
#' @seealso \code{\link{network}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{http://www.jstatsoft.org/v24/i02/}
#' @keywords manip graphs
#' @examples
#' 
#' data(flo)                     #Load the Florentine Families data
#' nflo<-network(flo)                      #Create a network object
#' n<-network.size(nflo)                #Get the number of vertices
#' permute.vertexIDs(nflo,n:1)                #Reverse the vertices
#' all(flo[n:1,n:1]==as.sociomatrix(nflo))          #Should be TRUE
#' 
#' @export permute.vertexIDs
permute.vertexIDs<-function(x,vids){
  #First, check to see that this is a graph object
  if(!is.network(x))
    stop("permute.vertexIDs requires an argument of class network.\n")
  #Sanity check: is this a permutation vector?
  n<-network.size(x)
  if((length(unique(vids))!=n)||any(range(vids)!=c(1,n)))
    stop("Invalid permutation vector in permute.vertexIDs.")
  if(is.bipartite(x)){  #If bipartite, enforce partitioning
    bpc<-get.network.attribute(x,"bipartite")
    if(any(vids[0:bpc]>bpc)||any(vids[(bpc+1):n]<=bpc))
      warning("Performing a cross-mode permutation in permute.vertexIDs.  I hope you know what you're doing....")
  }
  #Return the permuted graph
  xn<-substitute(x)
  x<-.Call(permuteVertexIDs_R,x,vids)
  if(.validLHS(xn,parent.frame())){  #If x not anonymous, set in calling env 
    on.exit(eval.parent(call('<-',xn,x)))
  }
  invisible(x)
}


# Set an edge attribute for network x.
#
# set.edge.attribute<-function(x,attrname,value,e=seq_along(x$mel)){
#   #Check to be sure we were called with a network
#   if(!is.network(x))
#     stop("set.edge.attribute requires an argument of class network.")
#   #Make sure that value is appropriate, coercing if needed
#   if(!is.list(value)){
#     if(!is.vector(value))
#       stop("Inappropriate edge value given in set.edge.attribute.\n")
#     else
#       value<-as.list(rep(value,length=length(e)))
#   }else
#     if(length(value)!=length(e))
#       value<-rep(value,length=length(e))
#   xn<-deparse(substitute(x))
#   ev<-parent.frame()
#   if(length(e)>0){
#     if((min(e)<1)|(max(e)>length(x$mel)))
#       stop("Illegal edge in set.edge.attribute.\n")
#     #Do the deed
#     x<-.Call("setEdgeAttribute_R",x,attrname,value,e, PACKAGE="network")
#     if(exists(xn,envir=ev))          #If x not anonymous, set in calling env
#       on.exit(assign(xn,x,pos=ev))
#     invisible(x)
#   }else
#     invisible(x)
# }
#' @rdname attribute.methods
#' @export
set.edge.attribute <- function(x, attrname, value, e, ...) {
  UseMethod("set.edge.attribute")
}

#' @rdname attribute.methods
#' @export
set.edge.attribute.network <- function(x, attrname, value, e=seq_along(x$mel), ...) {
  # determine if we have to do anything at all
  if(length(e)>0){
    if((min(e)<1)|(max(e)>length(x$mel))){
      stop("Illegal edge in set.edge.attribute.\n")
    }
    xn<-substitute(x)
    # determine if we will be setting single or multiple values
    if(length(attrname)==1){
      #Make sure that value is appropriate, coercing if needed
      if(!is.list(value)){
        if(!is.vector(value)){
          stop("Inappropriate edge value given in set.edge.attribute.\n")
        } else {
          value<-as.list(rep(value,length=length(e)))
        }
      } else {
        if(length(value)!=length(e)) {
          value<-rep(value,length=length(e))
        }
      }
      #Do the deed, call the set single value version
      x<-.Call(setEdgeAttribute_R,x,attrname,value,e)
    } else { # we will be setting multiple values
      if (length(attrname)!=length(value)){
        stop("the 'value' attribute must have an element corresponding to each attribute name in 'attrname' in set.edge.attribute")
      }
      #Make sure that value is appropriate, coercing if needed
      if(!is.list(value)){
        if(!is.vector(value)){
          stop("Inappropriate edge value given in set.edge.attribute.\n")
        } else { # value must be a vector
          # replicate each element of value e times if needed
          value<-lapply(1:length(value),function(n){
            if (length(value[n])<length(e)){
              return(as.list(rep(value[n],length=length(e))))
            } else {
              return(as.list(value[n]))
            }
          })
        }
      } else {
        # replicate each element of value e times if needed
        value<-lapply(1:length(value),function(n){
          if (length(value[[n]])<length(e)){
            return(as.list(rep(value[[n]],length=length(e))))
          } else {
            return(as.list(value[[n]]))
          }
        })
        
      }
      #Do the deed, call the set multiple version
      x<-.Call(setEdgeAttributes_R,x,attrname,value,e)
    }
    if(.validLHS(xn,parent.frame())){  #If x not anonymous, set in calling env 
      on.exit(eval.parent(call('<-',xn,x)))
    }
  }
  invisible(x)
}


# Set an edge value for network x.
#
#' @rdname attribute.methods
#' @export
set.edge.value <- function(x, attrname, value, e, ...) {
  UseMethod("set.edge.value")
}

#' @rdname attribute.methods
#' @export
set.edge.value.network <- function(x, attrname, value, e = seq_along(x$mel), ...) {
  #Check to ensure that this is not a hypergraph
  if(is.hyper(x))
    stop("Hypergraphs not currently supported in set.edge.value.\n")
  # Check edges
  if (length(e)==0) return(invisible(x))
  if((min(e)<1)|(max(e)>length(x$mel)))
    stop("Illegal edge in set.edge.value.\n")
  #Make sure that value is appropriate, coercing if needed
  n<-network.size(x)
  if(!is.matrix(value)){
    if(is.vector(value))
      value<-matrix(rep(value,length=n*n),n,n)
    else
      value<-matrix(value,n,n)
  } else if (min(dim(value)) < n) {
    stop("set.edge.value requires a matrix whose dimension is equal to or larger than the network size")
  }
  #Do the deed
  xn<-substitute(x)
  x<-.Call(setEdgeValue_R,x,attrname,value,e)
  if(.validLHS(xn,parent.frame())){  #If x not anonymous, set in calling env 
    on.exit(eval.parent(call('<-',xn,x)))
  }
  invisible(x)
}


# Set a network-level attribute for network x.
#
#' @rdname attribute.methods
#' @export
set.network.attribute <- function(x, attrname, value, ...) {
  UseMethod("set.network.attribute")
}

#' @rdname attribute.methods
#' @export
set.network.attribute.network <- function(x, attrname, value, ...) {
  #Make sure the values are consistent
  if(length(attrname)==1){
    value<-list(value)
  }else{
    if(is.list(value)){
      value<-rep(value,length=length(attrname))
    }else if(is.vector(value)){
      value<-as.list(rep(value,length=length(attrname)))
    }else
      stop("Non-replicable value with multiple attribute names in set.network.attribute.\n")
  }
  #Do the deed
  xn<-substitute(x)
  x<-.Call(setNetworkAttribute_R,x,attrname,value)
  if(.validLHS(xn,parent.frame())){  #If x not anonymous, set in calling env 
    on.exit(eval.parent(call('<-',xn,x)))
  }
  invisible(x)
}


# Set a vertex attribute for network x.
# This version has been removed so we can test one that can set multiple values at once
# set.vertex.attribute<-function(x,attrname,value,v=seq_len(network.size(x))){
#   #Check to be sure we were called with a network
#   if(!is.network(x))
#     stop("set.vertex.attribute requires an argument of class network.")
#   #Perform some sanity checks
#   if(any((v>network.size(x))|(v<1)))
#     stop("Vertex ID does not correspond to actual vertex in set.vertex.attribute.\n")
#   #Make sure that value is appropriate, coercing if needed
#   if(!is.list(value)){
#     if(!is.vector(value))
#       stop("Inappropriate value given in set.vertex.attribute.\n")
#     else
#       value<-as.list(rep(value,length=length(v)))
#   }else
#     if(length(value)!=length(v))
#       value<-rep(value,length=length(v))
#   #Do the deed
#   xn<-deparse(substitute(x))
#   ev<-parent.frame()
#   x<-.Call("setVertexAttribute_R",x,attrname,value,v, PACKAGE="network")
#   if(exists(xn,envir=ev))          #If x not anonymous, set in calling env
#     on.exit(assign(xn,x,pos=ev))
#   invisible(x)
# }

# valid.eids  returns a list of non-null edge ids for a given network


#' Get the ids of all the edges that are valid in a network
#' 
#' Returns a vector of valid edge ids (corresponding to non-NULL edges) for a
#' network that may have some deleted edges.
#' 
#' The edge ids used in the network package are positional indices on the
#' internal "mel" list. When edges are removed using \code{\link{delete.edges}}
#' \code{NULL} elements are left on the list.  The function \code{valid.eids}
#' returns the ids of all the valid (non-null) edge ids for its \code{network}
#' argument.
#' 
#' @param x a network object, possibly with some deleted edges.
#' @return a vector of integer ids corresponding to the non-null edges in x
#' @note If it is known that x has no deleted edges, \code{seq_along(x$mel)} is
#' a faster way to generate the sequence of possible edge ids.
#' @author skyebend
#' @seealso See also \code{\link{delete.edges}}
#' @examples
#' 
#' net<-network.initialize(100)
#' add.edges(net,1:99,2:100)
#' delete.edges(net,eid=5:95)
#' # get the ids of the non-deleted edges
#' valid.eids(net)
#' 
#' @export valid.eids
valid.eids <-function(x){
  # maybe should omit class test for speed?
  if (!is.network(x)){
    stop("cannot determine non-null edge ids because argument x is not a network object")
  }
  # get the ids of all the non-null elements on the edgelist of x
  return(which(!sapply(x$mel,is.null)))
}

#' @rdname attribute.methods
#' @export
set.vertex.attribute <- function(x, attrname, value, v = seq_len(network.size(x)), ...) {
  UseMethod("set.vertex.attribute")
}

#' @rdname attribute.methods
#' @export
set.vertex.attribute.network <- function(x, attrname, value, v = seq_len(network.size(x)), ...) {
  #Perform some sanity checks
  if(any((v>network.size(x))|(v<1)))
    stop("Vertex ID does not correspond to actual vertex in set.vertex.attribute.\n")
  
  xn<-substitute(x)
  
  #Make sure that value is appropriate, coercing if needed
  if (length(attrname)==1){ # if we are only setting a single attribute use old version
    if(!is.list(value)){
      if(!is.vector(value)){
        stop("Inappropriate value given in set.vertex.attribute.\n")
      } else {
        value<-as.list(rep(value,length=length(v)))
      }
    } else {
      if(length(value)!=length(v)){
        value<-rep(value,length=length(v))
      }
    }
    # call older singular value version
    x<-.Call(setVertexAttribute_R,x,attrname,value,v)
  } else { # setting multiple values
    if (length(value)!=length(attrname)){
      stop("the 'value' attribute must have an element corresponding to each attribute name in 'attrnames' in set.vertex.attribute")
    }
    if(!is.list(value)){
      if(!is.vector(value)){
        stop("Inappropriate value given in set.vertex.attribute.\n")
      } else { # value is a vector
    
        # replicate each element of value v times if needed
        value<-lapply(1:length(value),function(n){
                  if (length(value[n])<length(v)){
                    return(as.list(rep(value[n],length=length(v))))
                  } else {
                    return(as.list(value[n]))
                  }
              })
      }
    } else {  # value is a list
      # replicate each element of value v times if needed
      value<-lapply(1:length(value),function(n){
        if (length(value[[n]])<length(v)){
          return(as.list(rep(value[[n]],length=length(v))))
        } else {
          return(as.list(value[[n]]))
        }
      })
    }
    # call multiple value version
    x<-.Call(setVertexAttributes_R,x,attrname,value,v)
  } # end setting multiple values
  #Do the deed
  
  if(.validLHS(xn,parent.frame())){  #If x not anonymous, set in calling env 
    on.exit(eval.parent(call('<-',xn,x)))
  }
  invisible(x)
}

# valid.eids  returns a list of non-null edge ids for a given network
valid.eids <-function(x){
  # maybe should omit class test for speed?
  if (!is.network(x)){
    stop("cannot determine non-null edge ids because argument x is not a network object")
  }
  # get the ids of all the non-null elements on the edgelist of x
  return(which(!sapply(x$mel,is.null)))
}

