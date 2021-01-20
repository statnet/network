######################################################################
#
# operators.R
#
# Written by Carter T. Butts <buttsc@uci.edu>; portions contributed by
# David Hunter <dhunter@stat.psu.edu> and Mark S. Handcock
# <handcock@u.washington.edu>.
#
# Last Modified 01/28/11
# Licensed under the GNU General Public License version 2 (June, 1991)
# or greater
#
# Part of the R/network package
#
# This file contains various operators which take networks as inputs.
#
# Contents:
#
# "$<-.network"
# "[.network"
# "[<-.network"
# "%e%"
# "%e%<-"
# "%eattr%"
# "%eattr%<-"
# "%n%"
# "%n%<-"
# "%nattr%"
# "%nattr%<-"
# "%s%"
# "%v%"
# "%v%<-"
# "%vattr%"
# "%vattr%<-"
# "+"
# "+.default"
# "+.network"
# "-"
# "-.default"
# "-.network"
# "*"
# "*.default"
# "*.network"
# "!.network"
# "|.network"
# "&.network"
# "%*%.network"
# "%c%"
# "%c%.network"
# networkOperatorSetup
# prod.network
# sum.network
# 
######################################################################

# removed this function because it appears that '<-' is no longer a generic in R, so it was never getting called and the copy was not being made. See ticket #550
#' @export "<-.network"
"<-.network"<-function(x,value){
  .Deprecated("network.copy or '<-' works just fine",msg="The network assignment S3 method '<-.network' has been deprecated because the operator '<-' is no longer an S3 generic in R so the .network version does not appear to be called. If you see this warning, please contact the maintainers to let us know you use this function")
  x<-network.copy(value)
  return(x)
}

# A helper function to check that a particular edgelist can be validly queried or assigned to.
#' @importFrom statnet.common NVL
out_of_bounds <- function(x, el){
  n <- network.size(x)
  bip <- NVL(x%n%"bipartite", FALSE)

  anyNA(el) || any(el<1L) || any(el>n) ||
    (bip && (any((el[,1]<=bip) == (el[,2]<=bip))))
}

# removed so that will dispatch to internal primitive method #642
#"$<-.network"<-function(x,i,value){
#  cl<-oldClass(x)
#  class(x)<-NULL
#  x[[i]]<-value
#  class(x)<-cl
#  return(x)
#}

#' Extraction and Replacement Operators for Network Objects
#' 
#' Various operators which allow extraction or replacement of various
#' components of a \code{network} object.
#' 
#' Indexing for edge extraction operates in a manner analogous to \code{matrix}
#' objects.  Thus, \code{x[,]} selects all vertex pairs, \code{x[1,-5]} selects
#' the pairing of vertex 1 with all vertices except for 5, etc.  Following
#' this, it is acceptable for \code{i} and/or \code{j} to be logical vectors
#' indicating which vertices are to be included.  During assignment, an attempt
#' is made to match the elements of \code{value} to the extracted pairs in an
#' intelligent way; in particular, elements of \code{value} will be replicated
#' if too few are supplied (allowing expressions like \code{x[1,]<-1}).  Where
#' \code{names.eval==NULL}, zero and non-zero values are taken to indicate the
#' presence of absence of edges.  \code{x[2,4]<-6} thus adds a single (2,4)
#' edge to \code{x}, and \code{x[2,4]<-0} removes such an edge (if present).
#' If \code{x} is multiplex, assigning 0 to a vertex pair will eliminate
#' \emph{all} edges on that pair.  Pairs are taken to be directed where
#' \code{is.directed(x)==TRUE}, and undirected where
#' \code{is.directed(x)==FALSE}.
#' 
#' If an edge attribute is specified using \code{names.eval}, then the provided
#' values will be assigned to that attribute.  When assigning values, only
#' extant edges are employed (unless \code{add.edges==TRUE}); in the latter
#' case, any non-zero assignment results in the addition of an edge where
#' currently absent.  If the attribute specified is not present on a given
#' edge, it is added.  Otherwise, any existing value is overwritten.  The
#' \code{\%e\%} operator can also be used to extract/assign edge values; in those
#' roles, it is respectively equivalent to \code{get.edge.value(x,attrname)}
#' and \code{set.edge.value(x,attrname=attrname,value=value)} (if \code{value}
#' is a matrix) and \code{set.edge.attribute(x,attrname=attrname,value=value)}
#' (if \code{value} is anything else). That is, if \code{value} is a matrix,
#' the assignment operator treats it as an adjacency matrix; and if not, it
#' treats it as a vector (recycled as needed) in the internal ordering of edges
#' (i.e., edge IDs), skipping over deleted edges. In no case will attributes be
#' assigned to nonexisted edges.
#' 
#' The \code{\%n\%} and \code{\%v\%} operators serve as front-ends to the network
#' and vertex extraction/assignment functions (respectively).  In the
#' extraction case, \code{x \%n\% attrname} is equivalent to
#' \code{get.network.attribute(x,attrname)}, with \code{x \%v\% attrname}
#' corresponding to \code{get.vertex.attribute(x,attrname)}.  In assignment,
#' the respective equivalences are to
#' \code{set.network.attribute(x,attrname,value)} and
#' \code{set.vertex.attribute(x,attrname,value)}.  Note that the `%%`
#' assignment forms are generally slower than the named versions of the
#' functions beause they will trigger an additional internal copy of the
#' network object.
#' 
#' The \code{\%eattr\%}, \code{\%nattr\%}, and \code{\%vattr\%} operators are
#' equivalent to \code{\%e\%}, \code{\%n\%}, and \code{\%v\%} (respectively).  The
#' short forms are more succinct, but may produce less readable code.
#' 
#' @name network.extraction
#'
#' @param x an object of class \code{network}.
#' @param i,j indices of the vertices with respect to which adjacency is to be
#' tested.  Empty values indicate that all vertices should be employed (see
#' below).
#' @param na.omit logical; should missing edges be omitted (treated as
#' no-adjacency), or should \code{NA}s be returned?  (Default: return \code{NA}
#' on missing.)
#' @param names.eval optionally, the name of an edge attribute to use for
#' assigning edge values.
#' @param add.edges logical; should new edges be added to \code{x} where edges
#' are absent and the appropriate element of \code{value} is non-zero?
#' @param value the value (or set thereof) to be assigned to the selected
#' element of \code{x}.
#' @param attrname the name of a network or vertex attribute (as appropriate).
#' @return The extracted data, or none.
#' @author Carter T. Butts \email{buttsc@@uci.edu}
#' @seealso \code{\link{is.adjacent}}, \code{\link{as.sociomatrix}},
#' \code{\link{attribute.methods}}, \code{\link{add.edges}},
#' \code{\link{network.operators}}, and \code{\link{get.inducedSubgraph}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{http://www.jstatsoft.org/v24/i02/}
#' @keywords graphs manip
#' @examples
#' 
#'   #Create a random graph (inefficiently)
#'   g<-network.initialize(10)
#'   g[,]<-matrix(rbinom(100,1,0.1),10,10)
#'   plot(g)
#'   
#'   #Demonstrate edge addition/deletion
#'   g[,]<-0
#'   g[1,]<-1
#'   g[2:3,6:7]<-1
#'   g[,]
#'   
#'   #Set edge values
#'   g[,,names.eval="boo"]<-5
#'   as.sociomatrix(g,"boo")
#'   #Assign edge values from a vector
#'   g %e% "hoo" <- "wah"
#'   g %e% "hoo"
#'   g %e% "om" <- c("wow","whee")
#'   g %e% "om"
#'   #Assign edge values as a sociomatrix
#'   g %e% "age" <- matrix(1:100, 10, 10)
#'   g %e% "age"
#'   as.sociomatrix(g,"age")
#' 
#'   #Set/retrieve network and vertex attributes
#'   g %n% "blah" <- "Pork!"                 #The other white meat?
#'   g %n% "blah" == "Pork!"                 #TRUE!
#'   g %v% "foo" <- letters[10:1]            #Letter the vertices
#'   g %v% "foo" == letters[10:1]            #All TRUE
#' 
#' @export "[.network"
#' @export
"[.network"<-function(x,i,j,na.omit=FALSE){
  narg<-nargs()+missing(na.omit)
  n<-network.size(x)
  bip <- x%n%"bipartite"
  xnames <- network.vertex.names(x)
  if(missing(i)){              #If missing, use 1:n
    i <- if(is.bipartite(x)) 1:bip else 1:n
  }
  if((narg>3)&&missing(j)){
    j <- if(is.bipartite(x)) (bip+1L):n else 1:n
  }
  if(is.matrix(i)&&(NCOL(i)==1))  #Vectorize if degenerate matrix
    i<-as.vector(i)
  if(is.matrix(i)){    #Still a matrix?
    if(is.logical(i)){                    #Subset w/T/F?
      j<-col(i)[i]
      i<-row(i)[i]
      if(out_of_bounds(x, cbind(i,j))) stop("subscript out of bounds")
      out<-is.adjacent(x,i,j,na.omit=na.omit)
    }else{                                #Were we passed a pair list?
      if(is.character(i))
        i<-apply(i,c(1,2),match,xnames)
      if(out_of_bounds(x, i)) stop("subscript out of bounds")
      out<-is.adjacent(x,i[,1],i[,2], na.omit=na.omit)
    }
  }else if((narg<3)&&missing(j)){   #Here, assume a list of cell numbers
    ir<-1+((i-1)%%n)
    ic<-1+((i-1)%/%n)
    if(out_of_bounds(x, cbind(ir,ic))) stop("subscript out of bounds")
    out<-is.adjacent(x,ir,ic,na.omit=na.omit)
  }else{                      #Otherwise, assume a vector or submatrix
    if(is.character(i))
      i<-match(i,xnames)
    if(is.character(j))
      j<-match(j,xnames)
    i<-(1:n)[i]                 #Piggyback on R's internal tricks
    j<-(1:n)[j]
    if(length(i)==1){
      if(out_of_bounds(x, cbind(i,j))) stop("subscript out of bounds")
      out<-is.adjacent(x,i,j,na.omit=na.omit)
    }else{
      if(length(j)==1){
        if(out_of_bounds(x, cbind(i,j))) stop("subscript out of bounds")
        out<-is.adjacent(x,i,j,na.omit=na.omit)
      }else{
        jrep<-rep(j,rep.int(length(i),length(j)))
        if(length(i)>0)
          irep<-rep(i,times=ceiling(length(jrep)/length(i)))
        if(out_of_bounds(x, cbind(irep,jrep))) stop("subscript out of bounds")
        out<-matrix(is.adjacent(x,irep,jrep,na.omit=na.omit), length(i),length(j))
      }
    }
    if((!is.null(xnames))&&is.matrix(out))
      dimnames(out) <- list(xnames[i],xnames[j])
  }
  out+0                       #Coerce to numeric
}

#' @rdname network.extraction
#' @export "[<-.network"
#' @export
"[<-.network"<-function(x,i,j,names.eval=NULL,add.edges=FALSE,value){
  #For the common special case of x[,] <- 0, delete edges quickly by
  #reconstructing new outedgelists, inedgelists, and edgelists,
  #leaving the old ones to the garbage collector.
  if(missing(i) && missing(j) && is.null(names.eval) && isTRUE(all(value==FALSE))){
    if(length(x$mel)==0 || network.edgecount(x,na.omit=FALSE)==0) return(x) # Nothing to do; note that missing edges are still edges for the purposes of this.
    x$oel <- rep(list(integer(0)), length(x$oel))
    x$iel <- rep(list(integer(0)), length(x$iel))
    x$mel <- list()
    x$gal$mnext <- 1
    return(x)
  }
  #Check for hypergraphicity
  if(is.hyper(x))
    stop("Assignment operator overloading does not currently support hypergraphic networks.");
  #Set up the edge list to change
  narg<-nargs()+missing(names.eval)+missing(add.edges)
  n<-network.size(x)
  xnames <- network.vertex.names(x)
  bip <- x%n%"bipartite"
  if(missing(i)){              #If missing, use 1:n
    i <- if(is.bipartite(x)) 1:bip else 1:n
  }
  if((narg>5)&&missing(j)){
    j <- if(is.bipartite(x)) (bip+1L):n else 1:n
  }
  if(is.matrix(i)&&(NCOL(i)==1))  #Vectorize if degenerate matrix
    i<-as.vector(i)
  if(is.matrix(i)){    #Still a matrix?
    if(is.logical(i)){                    #Subset w/T/F?
      j<-col(i)[i]
      i<-row(i)[i]
      el<-cbind(i,j)
    }else{                                #Were we passed a pair list?
      if(is.character(i))
        i<-apply(i,c(1,2),match,xnames)
      el<-i
    }
  }else if((narg<6)&&missing(j)){  #Here, assume a list of cell numbers
    el<-1+cbind((i-1)%%n,(i-1)%/%n)
  }else{                      #Otherwise, assume a vector or submatrix
    if(is.character(i))
      i<-match(i,xnames)
    if(is.character(j))
      j<-match(j,xnames)
    i<-(1:n)[i]                 #Piggyback on R's internal tricks
    j<-(1:n)[j]
    if(length(i)==1){
      el<-cbind(rep(i,length(j)),j)
    }else{
      if(length(j)==1)
        el<-cbind(i,rep(j,length(i)))
      else{
        jrep<-rep(j,rep.int(length(i),length(j)))
        if(length(i)>0)
          irep<-rep(i,times=ceiling(length(jrep)/length(i)))
        el<-cbind(irep,jrep)
      }
    }
  }

  # Check bounds
  if(out_of_bounds(x, el)) stop("subscript out of bounds")

  #Set up values
  if(is.matrix(value))
    val<-value[cbind(match(el[,1],sort(unique(el[,1]))), match(el[,2],sort(unique(el[,2]))))]
  else
    val<-rep(as.vector(value),length=NROW(el))
  #Perform the changes
  if(is.null(names.eval)){  #If no names given, don't store values
    for (k in seq_along(val)) {
      eid <- get.edgeIDs(x,el[k,1],el[k,2],neighborhood="out", na.omit=FALSE)
      if (!is.na(val[k]) & val[k] == 0) {
        # delete edge
        if (length(eid) > 0) x<-delete.edges(x,eid)
      } else {
        if (length(eid) == 0 & (has.loops(x)|(el[k,1]!=el[k,2]))) {
          # add edge if needed
          x<-add.edges(x,as.list(el[k,1]),as.list(el[k,2]))
          eid <- get.edgeIDs(x,el[k,1],el[k,2],neighborhood="out", na.omit=FALSE)
        }
        if (is.na(val[k])) {
          set.edge.attribute(x,"na",TRUE,eid)   # set to NA
        } else if (val[k] == 1) {
          set.edge.attribute(x,"na",FALSE,eid)   # set to 1
        }
      }
    }
  }else{                   #An attribute name was given, so store values
    epresent<-vector()
    eid<-vector()
    valsl<-list()
    for(k in 1:NROW(el)){
      if(is.adjacent(x,el[k,1],el[k,2],na.omit=FALSE)){  #Collect extant edges
        loceid<-get.edgeIDs(x,el[k,1],el[k,2],neighborhood="out",na.omit=FALSE)
        if(add.edges){    #Need to know if we're adding/removing edges
          if(val[k]==0){     #If 0 and adding/removing, eliminate present edges
            x<-delete.edges(x,loceid)
          }else{             #Otherwise, add as normal
            valsl<-c(valsl,as.list(rep(val[k],length(loceid))))
            eid<-c(eid,loceid)
          }
        }else{
          valsl<-c(valsl,as.list(rep(val[k],length(loceid))))
          eid<-c(eid,loceid)
        }
        epresent[k]<-TRUE
      }else
        epresent[k]<-!is.na(val[k]) && (val[k]==0)   #If zero, skip it; otherwise (including NA), add
    }
    if(sum(epresent)>0)               #Adjust attributes for extant edges
      x<-set.edge.attribute(x,names.eval,valsl,eid)
    if(add.edges&&(sum(!epresent)>0))           #Add new edges, if needed
      x<-add.edges(x,as.list(el[!epresent,1]),as.list(el[!epresent,2]), names.eval=as.list(rep(names.eval,sum(!epresent))),vals.eval=as.list(val[!epresent]))
  }
  #Return the modified graph
  x
}

#' @rdname network.extraction
#' @export
"%e%"<-function(x,attrname){
  get.edge.value(x,attrname=attrname)
}

#' @rdname network.extraction
#' @usage x \%e\% attrname <- value
#' @export
"%e%<-"<-function(x,attrname,value){
  if(is.matrix(value)) set.edge.value(x,attrname=attrname,value=value)
  else set.edge.attribute(x,attrname=attrname,value=value,e=valid.eids(x))
}

#' @rdname network.extraction
#' @export
"%eattr%"<-function(x,attrname){
  x %e% attrname
}

#' @rdname network.extraction
#' @usage x \%eattr\% attrname <- value
#' @export
"%eattr%<-"<-function(x,attrname,value){
  x %e% attrname <- value
}

#' @rdname network.extraction
#' @export
"%n%"<-function(x,attrname){
  get.network.attribute(x,attrname=attrname)
}

#' @rdname network.extraction
#' @usage x \%n\% attrname <- value
#' @export
"%n%<-"<-function(x,attrname,value){
  set.network.attribute(x,attrname=attrname,value=value)
}

#' @rdname network.extraction
#' @export
"%nattr%"<-function(x,attrname){
  x %n% attrname
}

#' @rdname network.extraction
#' @usage x \%nattr\% attrname <- value
#' @export
"%nattr%<-"<-function(x,attrname,value){
  x %n% attrname <- value
}

#' @rdname get.inducedSubgraph
#' @usage x \%s\% v
#' @export
"%s%"<-function(x,v){
  if(is.list(v))
    get.inducedSubgraph(x,v=v[[1]],alters=v[[2]])
  else
    get.inducedSubgraph(x,v=v)
}

#' @rdname network.extraction
#' @export
"%v%"<-function(x,attrname){
  get.vertex.attribute(x,attrname=attrname)
}

#' @rdname network.extraction
#' @usage x \%v\% attrname <- value
#' @export
"%v%<-"<-function(x,attrname,value){
  set.vertex.attribute(x,attrname=attrname,value=value)
}

#' @rdname network.extraction
#' @export
"%vattr%"<-function(x,attrname){
  x %v% attrname
}

#' @rdname network.extraction
#' @usage x \%vattr\% attrname <- value
#' @export
"%vattr%<-"<-function(x,attrname,value){
  x %v% attrname <- value
}

#"+"<-function(e1, e2, ...) UseMethod("+")
#
#"+.default"<-function(e1,e2,...) { (base::"+")(e1,e2) }
#
#"+.network"<-function(e1,e2,attrname=NULL,...){
#  e1<-as.sociomatrix(e1,attrname=attrname)
#  e2<-as.sociomatrix(e2,attrname=attrname)
#  network(e1+e2,ignore.eval=is.null(attrname),names.eval=attrname)
#}
#' Network Operators
#' 
#' These operators allow for algebraic manipulation of relational structures.
#' 
#' In general, the binary network operators function by producing a new network
#' object whose edge structure is based on that of the input networks.  The
#' properties of the new structure depend upon the inputs as follows: \itemize{
#' \item The size of the new network is equal to the size of the input networks
#' (for all operators save \code{\%c\%}), which must themselves be of equal size.
#' Likewise, the \code{bipartite} attributes of the inputs must match, and this
#' is preserved in the output.  \item If either input network allows loops,
#' multiplex edges, or hyperedges, the output acquires this property.  (If both
#' input networks do not allow these features, then the features are disallowed
#' in the output network.)  \item If either input network is directed, the
#' output is directed; if exactly one input network is directed, the undirected
#' input is treated as if it were a directed network in which all edges are
#' reciprocated.  \item Supplemental attributes (including vertex names, but
#' not edgwise missingness) are not transferred to the output.  } The unary
#' operator acts per the above, but with a single input.  Thus, the output
#' network has the same properties as the input, with the exception of
#' supplemental attributes.
#' 
#' The behavior of the composition operator, \code{\%c\%}, is somewhat more
#' complex than the others.  In particular, it will return a bipartite network
#' whenever either input network is bipartite \emph{or} the vertex names of the
#' two input networks do not match (or are missing).  If both inputs are
#' non-bipartite and have identical vertex names, the return value will have
#' the same structure (but with loops).  This behavior corresponds to the
#' interpretation of the composition operator as counting walks on labeled sets
#' of vertices.
#' 
#' Hypergraphs are not yet supported by these routines, but ultimately will be
#' (as suggested by the above).
#' 
#' The specific operations carried out by these operators are generally
#' self-explanatory in the non-multiplex case, but semantics in the latter
#' circumstance bear elaboration.  The following summarizes the behavior of
#' each operator:
#' \describe{
#'   \item{\code{+}}{An \eqn{(i,j)} edge is created in
#' the return graph for every \eqn{(i,j)} edge in each of the input graphs.}
#'   \item{\code{-}}{An \eqn{(i,j)} edge is created in the return graph for
#' every \eqn{(i,j)} edge in the first input that is not matched by an
#' \eqn{(i,j)} edge in the second input; if the second input has more
#' \eqn{(i,j)} edges than the first, no \eqn{(i,j)} edges are created in the
#' return graph.}
#'   \item{\code{*}}{An \eqn{(i,j)} edge is created for every
#' pairing of \eqn{(i,j)} edges in the respective input graphs.}
#'   \item{\code{\%c\%}}{An \eqn{(i,j)} edge is created in the return graph for
#' every edge pair \eqn{(i,k),(k,j)} with the first edge in the first input and
#' the second edge in the second input.}
#'   \item{\code{!}}{An \eqn{(i,j)} edge
#' is created in the return graph for every \eqn{(i,j)} in the input not having
#' an edge.}
#'   \item{\code{|}}{An \eqn{(i,j)} edge is created in the return
#' graph if either input contains an \eqn{(i,j)} edge.}
#'   \item{\code{&}}{An
#' \eqn{(i,j)} edge is created in the return graph if both inputs contain an
#' \eqn{(i,j)} edge.}
#' }
#' Semantics for missing-edge cases follow from the above,
#' under the interpretation that edges with \code{na==TRUE} are viewed as
#' having an unknown state.  Thus, for instance, \code{x*y} with \code{x}
#' having 2 \eqn{(i,j)} non-missing and 1 missing edge and \code{y} having 3
#' respective non-missing and 2 missing edges will yield an output network with
#' 6 non-missing and 9 missing \eqn{(i,j)} edges.
#' 
#' @rdname network-operators
#' @name network.operators
#'
#' @aliases %c%
#' @param e1 an object of class \code{network}.
#' @param e2 another \code{network}.
#' @return The resulting network.
#' @note Currently, there is a naming conflict between the composition operator
#' and the \code{\%c\%} operator in the \code{\link[sna]{sna}} package.  This
#' will be resolved in future releases; for the time being, one can determine
#' which version of \code{\%c\%} is in use by varying which package is loaded
#' first.
#' @author Carter T. Butts \email{buttsc@@uci.edu}
#' @seealso \code{\link{network.extraction}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{http://www.jstatsoft.org/v24/i02/}
#' 
#' Wasserman, S. and Faust, K.  (1994).  \emph{Social Network Analysis: Methods
#' and Applications.} Cambridge: University of Cambridge Press.
#' @keywords math graphs
#' @examples
#' 
#' #Create an in-star
#' m<-matrix(0,6,6)
#' m[2:6,1]<-1
#' g<-network(m)
#' plot(g)
#' 
#' #Compose g with its transpose
#' gcgt<-g %c% (network(t(m)))
#' plot(gcgt)
#' gcgt
#' 
#' #Show the complement of g
#' !g
#' 
#' #Perform various arithmatic and logical operations
#' (g+gcgt)[,] == (g|gcgt)[,]             #All TRUE
#' (g-gcgt)[,] == (g&(!(gcgt)))[,]
#' (g*gcgt)[,] == (g&gcgt)[,]
#' @export "+.network"
#' @export
"+.network"<-function(e1,e2){
  #Set things up
  outinf<-networkOperatorSetup(x=e1,y=e2)
  #Select edges to add; semantics are "adding" edges, which is like union
  #in the non-multigraph case, but actually results in accumulating edge copies
  #in for multiplex graphs.
  out<-outinf$net
  if(is.hyper(out)){            #Hypergraph; for now, return an apology
    stop("Elementwise operations on hypergraphs not yet supported.") 
  }else{                        #Dyadic network
    out<-outinf$net
    #For boolean addition, take the union of edge sets
    el<-rbind(outinf$elx,outinf$ely)
    elna<-rbind(outinf$elnax,outinf$elnay)
    if(!is.multiplex(out)){           #If not multiplex, remove duplicates
      el<-unique(el)
      elna<-unique(elna)
      if(NROW(el)>0&&NROW(elna)>0){
        n<-network.size(out)
        elnum<-(el[,1]-1)+n*(el[,2]-1)
        elnanum<-(elna[,1]-1)+n*(elna[,2]-1)
        elna<-elna[!(elnanum%in%elnum),,drop=FALSE] #For union, NA loses
      }
    }
    if(NROW(el)>0)                             #Add non-missing edges
      add.edges(out,tail=el[,1],head=el[,2])
    if(NROW(elna)>0)                           #Add missing edges
      add.edges(out,tail=elna[,1],head=elna[,2], names.eval=replicate(NROW(elna),list("na")), vals.eval=replicate(NROW(elna),list(list(na=TRUE))))
  }
  #Return the resulting network
  out
}


#"-"<-function(e1, e2, ...) UseMethod("-")
#
#"-.default"<-function(e1,e2,...) { (base::"-")(e1,e2) }
#
#' @rdname network-operators
#' @export "-.network"
#' @export
"-.network"<-function(e1,e2){
  #Set things up
  outinf<-networkOperatorSetup(x=e1,y=e2)
  #Semantics here are "edge subtraction"; this is like "and not" for the
  #non-multiplex case, but in the latter we can think of it as subtracting
  #copies of edges (so if there were 5 copies of (i,j) in e1 and 2 copies in 
  #e2, we would be left with 3 copies).  Note that this means that NAs are
  #asymmetric: an edge in e2 will first cancel a "sure" edge, and then an
  #NA edge when the sure ones are exhausted.  NA edges in e2 don't cancel
  #sure edges in e1, but they render them unsure (i.e., NA).  NAs in e2
  #have no effect on remaining NAs in e1 (unsure vs unsure), nor on 0s.
  out<-outinf$net
  if(is.hyper(out)){            #Hypergraph; for now, return an apology
    stop("Elementwise operations on hypergraphs not yet supported.") 
  }else{                        #Dyadic network
    out<-outinf$net
    #For boolean subtraction, want edges in e1 that are not in e2
    el<-outinf$elx
    elna<-outinf$elnax
    if(!is.multiplex(out)){         #If not multiplex, cancellation is absolute
      n<-network.size(out)
      elnum<-(el[,1]-1)+n*(el[,2]-1)
      elnanum<-(elna[,1]-1)+n*(elna[,2]-1)
      elynum<-(outinf$ely[,1]-1)+n*(outinf$ely[,2]-1)
      elynanum<-(outinf$elnay[,1]-1)+n*(outinf$elnay[,2]-1)
      #For every edge or NA edge in x, kill it if in ely
      sel<-!(elnum%in%elynum)
      el<-el[sel,,drop=FALSE]
      elnum<-elnum[sel]
      sel<-!(elnanum%in%elynum)
      elna<-elna[sel,,drop=FALSE]
      elnanum<-elnanum[sel]
      #Now, for the remaining edges from x, set to NA if in elyna
      sel<-!(elnum%in%elynanum)
      elna<-rbind(elna,el[!sel,,drop=FALSE])
      el<-el[sel,,drop=FALSE]
      #Clean up any non-uniqueness (recall that el, elna started unique)
      elna<-unique(elna)
    }else{                          #If multiplex, cancellation is 1:1
      n<-network.size(out)
      elnum<-(el[,1]-1)+n*(el[,2]-1)
      elnanum<-(elna[,1]-1)+n*(elna[,2]-1)
      elynum<-(outinf$ely[,1]-1)+n*(outinf$ely[,2]-1)
      elynanum<-(outinf$elnay[,1]-1)+n*(outinf$elnay[,2]-1)
      #Every edge in ely kills one copy of the corresponding edge in el
      i<-1
      while((NROW(el)>0)&&(i<=length(elynum))){
        j<-match(elynum[i],elnum)
        if(is.na(j)){                #No match; increment i
          i<-i+1
        }else{                       #Match!  Cancel both and don't increment
          el<-el[-j,,drop=FALSE]
          elnum<-elnum[-j]
          elynum<-elynum[-i]
        }
      }
      #Every remaining ely kills one copy of the corresponding edge in elna
      i<-1
      while((NROW(elna)>0)&&(i<=length(elynum))){
        j<-match(elynum[i],elnanum)
        if(is.na(j)){                #No match; increment i
          i<-i+1
        }else{                       #Match!  Cancel both and don't increment
          elna<-elna[-j,,drop=FALSE]
          elnanum<-elnanum[-j]
          elynum<-elynum[-i]
        }
      }
      #Every elnay converts one corresponding el to elna
      i<-1
      while((NROW(el)>0)&&(i<=length(elynanum))){
        j<-match(elynanum[i],elnum)
        if(is.na(j)){                #No match; increment i
          i<-i+1
        }else{                       #Match!  Cancel both and don't increment
          elna<-rbind(elna,el[j,,drop=FALSE])
          el<-el[-j,,drop=FALSE]
          elnum<-elnum[-j]
          elynanum<-elynanum[-i]
        }
      }
     }
    if(NROW(el)>0)                             #Add non-missing edges
      add.edges(out,tail=el[,1],head=el[,2])
    if(NROW(elna)>0)                           #Add missing edges
      add.edges(out,tail=elna[,1],head=elna[,2], names.eval=replicate(NROW(elna),list("na")), vals.eval=replicate(NROW(elna),list(list(na=TRUE))))
  }
  #Return the resulting network
  out
}


#"*"<-function(e1, e2, ...) UseMethod("*")
#
#"*.default"<-function(e1,e2,...) { (base::"*")(e1,e2) }
#
#' @rdname network-operators
#' @export "*.network"
#' @export
"*.network"<-function(e1,e2){
  #Set things up
  outinf<-networkOperatorSetup(x=e1,y=e2)
  #Multiplication semantics here are like "and" in the non-multiplex case,
  #but in the multiplex case we assume that the number of edges is itself
  #multplied.  Multiplication is treated by pairing, so the number of sure
  #edges is sure(e1)*sure(e2), and the number of NA edges is
  #sure(e1)*NA(e2) + NA(e1)*sure(e2) + NA(e1)*NA(e2), where sure and NA are
  #here counts of the (i,j) edge that are non-missing or missing
  #(respectively).
  out<-outinf$net
  if(is.hyper(out)){            #Hypergraph; for now, return an apology
    stop("Elementwise operations on hypergraphs not yet supported.") 
  }else{                        #Dyadic network
    out<-outinf$net
    n<-network.size(out)
    el<-matrix(nrow=0,ncol=2)
    elna<-matrix(nrow=0,ncol=2)
    if(is.multiplex(out)){        #Multiplex case: add edge for every pair
      allpairs<-unique(rbind(outinf$elx,outinf$elnax,outinf$ely,outinf$elnay))
      allnum<-(allpairs[,1]-1)+n*(allpairs[,2]-1)
      elxnum<-(outinf$elx[,1]-1)+n*(outinf$elx[,2]-1)
      elxnanum<-(outinf$elnax[,1]-1)+n*(outinf$elnax[,2]-1)
      elynum<-(outinf$ely[,1]-1)+n*(outinf$ely[,2]-1)
      elynanum<-(outinf$elnay[,1]-1)+n*(outinf$elnay[,2]-1)
      allxcnt<-sapply(allnum,function(z,w){sum(z==w)},w=elxnum)
      allxnacnt<-sapply(allnum,function(z,w){sum(z==w)},w=elxnanum)
      allycnt<-sapply(allnum,function(z,w){sum(z==w)},w=elynum)
      allynacnt<-sapply(allnum,function(z,w){sum(z==w)},w=elynanum)
      el<-allpairs[rep(1:length(allnum),times=allxcnt*allycnt),,drop=FALSE]
      elna<-allpairs[rep(1:length(allnum),times=allxcnt*allynacnt+ allxnacnt*allycnt+allxnacnt*allynacnt),,drop=FALSE]
    }else{                       #Non-multiplex case: "and"
      elx<-unique(outinf$elx)
      elnax<-unique(outinf$elnax)
      ely<-unique(outinf$ely)
      elnay<-unique(outinf$elnay)
      elxnum<-(elx[,1]-1)+n*(elx[,2]-1)
      elxnanum<-(elnax[,1]-1)+n*(elnax[,2]-1)
      sel<-elxnanum%in%elxnum                    #Override NA with edges w/in x
      if(sum(sel)>0){
        elnax<-elnax[!sel,,drop=FALSE]
        elxnanum<-elxnanum[!sel,,drop=FALSE]
      }
      elynum<-(ely[,1]-1)+n*(ely[,2]-1)
      elynanum<-(elnay[,1]-1)+n*(elnay[,2]-1)
      sel<-elynanum%in%elynum                    #Override NA with edges w/in y
      if(sum(sel)>0){
        elnay<-elnay[!sel,,drop=FALSE]
        elynanum<-elynanum[!sel,,drop=FALSE]
      }
      #Check for matches across the "sure" edges
      ematch<-match(elxnum,elynum)
      el<-rbind(el,elx[!is.na(ematch),,drop=FALSE])
      elx<-elx[is.na(ematch),,drop=FALSE]              #Remove the matched cases
      elxnum<-elxnum[is.na(ematch)]
      if(length(ematch[!is.na(ematch)])>0){
        ely<-ely[-ematch[!is.na(ematch)],,drop=FALSE]
        elynum<-elynum[-ematch[!is.na(ematch)]]
      }
      #Match sure xs with unsure ys
      if(length(elxnum)*length(elynanum)>0){
        ematch<-match(elxnum,elynanum)
        elna<-rbind(elna,elx[!is.na(ematch),,drop=FALSE])
        elx<-elx[is.na(ematch),,drop=FALSE]            #Remove the matched cases
        elxnum<-elxnum[is.na(ematch)]
        if(length(ematch[!is.na(ematch)])>0){
          elnay<-elnay[-ematch[!is.na(ematch)],,drop=FALSE]
          elynanum<-elynanum[-ematch[!is.na(ematch)]]
        }
      }
      #Match sure ys with unsure xs
      if(length(elynum)*length(elxnanum)>0){
        ematch<-match(elynum,elxnanum)
        elna<-rbind(elna,ely[!is.na(ematch),,drop=FALSE])
        ely<-ely[is.na(ematch),,drop=FALSE]            #Remove the matched cases
        elynum<-elynum[is.na(ematch)]
        if(length(ematch[!is.na(ematch)])>0){
          elnax<-elnax[-ematch[!is.na(ematch)],,drop=FALSE]
          elxnanum<-elxnanum[-ematch[!is.na(ematch)]]
        }
      }
      #Match unsure xs with unsure ys
      if(length(elxnanum)*length(elynanum)>0){
        ematch<-match(elxnanum,elynanum)
        elna<-rbind(elna,elnax[!is.na(ematch),,drop=FALSE])
      }
    }
    if(NROW(el)>0)                             #Add non-missing edges
      add.edges(out,tail=el[,1],head=el[,2])
    if(NROW(elna)>0)                           #Add missing edges
      add.edges(out,tail=elna[,1],head=elna[,2], names.eval=replicate(NROW(elna),list("na")), vals.eval=replicate(NROW(elna),list(list(na=TRUE))))
  }
  #Return the resulting network
  out
}

#' @rdname network-operators
#' @export "!.network"
#' @export
"!.network"<-function(e1){
  #Set things up
  outinf<-networkOperatorSetup(x=e1)
  #Select edges to add; semantics are "not" which means that one takes the
  #non-multiplex complement of edges.  Any sure edge implies 0, an NA edge
  #without a sure edge implies NA, no sure or NA edge implies 1.
  out<-outinf$net
  if(is.hyper(out)){            #Hypergraph; for now, return an apology
    stop("Elementwise operations on hypergraphs not yet supported.") 
  }else{                        #Dyadic network
    out<-outinf$net
    n<-network.size(out)
    #Start with the complete graph, and cut things away
    el<-cbind(rep(1:n,each=n),rep(1:n,n))
    if(!is.directed(out))         #Needs to match order in networkOperatorSetup
      el<-el[el[,1]<=el[,2],]
    if(!has.loops(out))
      el<-el[el[,1]!=el[,2],]
    elnum<-(el[,1]-1)+n*(el[,2]-1)
    elna<-matrix(nrow=0,ncol=2)
    #Remove all sure edges
    elx<-unique(outinf$elx)
    elxnum<-(elx[,1]-1)+n*(elx[,2]-1)
    ematch<-match(elxnum,elnum)
    if(length(ematch[!is.na(ematch)])>0){
      el<-el[-ematch[!is.na(ematch)],,drop=FALSE]
      elnum<-elnum[-ematch[!is.na(ematch)]]
    }
    #Convert all unsure edges to NAs
    elnax<-unique(outinf$elnax)
    elxnanum<-(elnax[,1]-1)+n*(elnax[,2]-1)
    ematch<-match(elxnanum,elnum)
    if(length(ematch[!is.na(ematch)])>0){
      elna<-el[ematch[!is.na(ematch)],,drop=FALSE]
      el<-el[-ematch[!is.na(ematch)],,drop=FALSE]
    }
    if(NROW(el)>0)                             #Add non-missing edges
      add.edges(out,tail=el[,1],head=el[,2])
    if(NROW(elna)>0)                           #Add missing edges
      add.edges(out,tail=elna[,1],head=elna[,2], names.eval=replicate(NROW(elna),list("na")), vals.eval=replicate(NROW(elna),list(list(na=TRUE))))
  }
  #Return the resulting network
  out
}

#' @rdname network-operators
#' @export "|.network"
#' @export
"|.network"<-function(e1,e2){
  #Set things up
  outinf<-networkOperatorSetup(x=e1,y=e2)
  #Select edges to add; semantics are "or," which means that one takes the
  #non-multiplex union of edges (like the non-multiplex case of the +
  #operator).  Here, a sure edge in either input graph will override an NA,
  #and an NA will override a zero.
  out<-outinf$net
  if(is.hyper(out)){            #Hypergraph; for now, return an apology
    stop("Elementwise operations on hypergraphs not yet supported.") 
  }else{                        #Dyadic network
    out<-outinf$net
    #For boolean addition, take the union of edge sets
    el<-rbind(outinf$elx,outinf$ely)
    elna<-rbind(outinf$elnax,outinf$elnay)
    el<-unique(el)
    elna<-unique(elna)
    if(NROW(el)>0&&NROW(elna)>0){
      n<-network.size(out)
      elnum<-(el[,1]-1)+n*(el[,2]-1)
      elnanum<-(elna[,1]-1)+n*(elna[,2]-1)
      elna<-elna[!(elnanum%in%elnum),,drop=FALSE] #For union, NA loses
    }
    if(NROW(el)>0)                             #Add non-missing edges
      add.edges(out,tail=el[,1],head=el[,2])
    if(NROW(elna)>0)                           #Add missing edges
      add.edges(out,tail=elna[,1],head=elna[,2], names.eval=replicate(NROW(elna),list("na")), vals.eval=replicate(NROW(elna),list(list(na=TRUE))))
  }
  #Return the resulting network
  out
}

#' @rdname network-operators
#' @export "&.network"
#' @export
"&.network"<-function(e1,e2){
  #Set things up
  outinf<-networkOperatorSetup(x=e1,y=e2)
  #Select edges to add; semantics are "and," which means that one places an
  #(i,j) edge if there exists a sure (i,j) edge in both e1 and e2.  If there
  #is not a sure edge in each but there is at least an unsure edge in each,
  #then we place an NA in the (i,j) slot.  Otherwise, we leave it empty.  This
  #is just like boolean "and" for non-multiplex graphs, but is not quite the
  #same in the multiplex case.
  out<-outinf$net
  if(is.hyper(out)){            #Hypergraph; for now, return an apology
    stop("Elementwise operations on hypergraphs not yet supported.") 
  }else{                        #Dyadic network
    out<-outinf$net
    n<-network.size(out)
    el<-matrix(nrow=0,ncol=2)
    elna<-matrix(nrow=0,ncol=2)
    elx<-unique(outinf$elx)
    elnax<-unique(outinf$elnax)
    ely<-unique(outinf$ely)
    elnay<-unique(outinf$elnay)
    elxnum<-(elx[,1]-1)+n*(elx[,2]-1)
    elxnanum<-(elnax[,1]-1)+n*(elnax[,2]-1)
    sel<-elxnanum%in%elxnum                    #Override NA with edges w/in x
    if(sum(sel)>0){
      elnax<-elnax[!sel,,drop=FALSE]
      elxnanum<-elxnanum[!sel,,drop=FALSE]
    }
    elynum<-(ely[,1]-1)+n*(ely[,2]-1)
    elynanum<-(elnay[,1]-1)+n*(elnay[,2]-1)
    sel<-elynanum%in%elynum                    #Override NA with edges w/in y
    if(sum(sel)>0){
      elnay<-elnay[!sel,,drop=FALSE]
      elynanum<-elynanum[!sel,,drop=FALSE]
    }
    #Check for matches across the "sure" edges
    ematch<-match(elxnum,elynum)
    el<-rbind(el,elx[!is.na(ematch),,drop=FALSE])
    elx<-elx[is.na(ematch),,drop=FALSE]              #Remove the matched cases
    elxnum<-elxnum[is.na(ematch)]
    if(length(ematch[!is.na(ematch)])>0){
      ely<-ely[-ematch[!is.na(ematch)],,drop=FALSE]
      elynum<-elynum[-ematch[!is.na(ematch)]]
    }
    #Match sure xs with unsure ys
    if(length(elxnum)*length(elynanum)>0){
      ematch<-match(elxnum,elynanum)
      elna<-rbind(elna,elx[!is.na(ematch),,drop=FALSE])
      elx<-elx[is.na(ematch),,drop=FALSE]            #Remove the matched cases
      elxnum<-elxnum[is.na(ematch)]
      if(length(ematch[!is.na(ematch)])>0){
        elnay<-elnay[-ematch[!is.na(ematch)],,drop=FALSE]
        elynanum<-elynanum[-ematch[!is.na(ematch)]]
      }
    }
    #Match sure ys with unsure xs
    if(length(elynum)*length(elxnanum)>0){
      ematch<-match(elynum,elxnanum)
      elna<-rbind(elna,ely[!is.na(ematch),,drop=FALSE])
      ely<-ely[is.na(ematch),,drop=FALSE]            #Remove the matched cases
      elynum<-elynum[is.na(ematch)]
      if(length(ematch[!is.na(ematch)])>0){
        elnax<-elnax[-ematch[!is.na(ematch)],,drop=FALSE]
        elxnanum<-elxnanum[-ematch[!is.na(ematch)]]
      }
    }
    #Match unsure xs with unsure ys
    if(length(elxnanum)*length(elynanum)>0){
      ematch<-match(elxnanum,elynanum)
      elna<-rbind(elna,elnax[!is.na(ematch),,drop=FALSE])
    }
    if(NROW(el)>0)                             #Add non-missing edges
      add.edges(out,tail=el[,1],head=el[,2])
    if(NROW(elna)>0)                           #Add missing edges
      add.edges(out,tail=elna[,1],head=elna[,2], names.eval=replicate(NROW(elna),list("na")), vals.eval=replicate(NROW(elna),list(list(na=TRUE))))
  }
  #Return the resulting network
  out
}


# --------------------------- %c% -------------------------------
# conditionally create this method, as it may allready have 
# been created and loaded by sna package

if (!exists('%c%')){

#' @export "%c%"
"%c%"<-function(e1,e2){
  UseMethod("%c%",e1)
}

}

#' @rdname network-operators
#' @export "%c%.network"
#' @export
"%c%.network"<-function(e1,e2){
  #Set things up
  net1<-networkOperatorSetup(x=e1)
  net2<-networkOperatorSetup(x=e2)
  if(is.bipartite(net1$net)){          #Find in/out set sizes for e1
    insz1<-net1$net%n%"bipartite"
    outsz1<-net1$net%n%"n"-net1$net%n%"bipartite"
  }else{
    insz1<-net1$net%n%"n"
    outsz1<-net1$net%n%"n"
  }
  if(is.bipartite(net2$net)){          #Find in/out set sizes for e2
    insz2<-net2$net%n%"bipartite"
    outsz2<-net2$net%n%"n"-net2$net%n%"bipartite"
  }else{
    insz2<-net2$net%n%"n"
    outsz2<-net2$net%n%"n"
  }
  if(outsz1!=insz2)
    stop("Non-conformable relations in %c%.  Cannot compose.")
  if(is.hyper(net1$net)||is.hyper(net2$net))  #Hypergraph; for now, stop
    stop("Elementwise operations on hypergraphs not yet supported.")
  #Test for vertex name matching (governs whether we treat as bipartite)
  if(is.network(e1))
    vnam1<-network.vertex.names(e1)
  else if(!is.null(attr(e1,"vnames")))
    vnam1<-attr(e1,"vnames")
  else if(is.matrix(e1)||is.data.frame(e1)||is.array(e1))
    vnam1<-row.names(e1)
  else
    vnam1<-NULL
  if(is.network(e2))
    vnam2<-network.vertex.names(e2)
  else if(!is.null(attr(e2,"vnames")))
    vnam2<-attr(e2,"vnames")
  else if(is.matrix(e2)||is.data.frame(e2)||is.array(e2))
    vnam2<-row.names(e2)
  else
    vnam2<-NULL
  if((!is.null(vnam1))&&(!is.null(vnam2))&&(length(vnam1)==length(vnam2)) &&all(vnam1==vnam2))
    vnammatch<-TRUE
  else
    vnammatch<-FALSE
  #Decide on bipartite representation and create graph
  if((!is.bipartite(net1$net))&&(!is.bipartite(net2$net))&&vnammatch)
    out<-network.initialize(insz1, directed=is.directed(net1$net)|is.directed(net2$net), loops=TRUE,multiple=is.multiplex(net1$net)|is.multiplex(net2$net))
  else
    out<-network.initialize(insz1+outsz2,bipartite=insz1, directed=is.directed(net1$net)|is.directed(net2$net),multiple=is.multiplex(net1$net)|is.multiplex(net2$net))
  #Accumulate edges (yeah, could be made more efficient -- cope with it)
  el<-matrix(nrow=0,ncol=2)
  elna<-matrix(nrow=0,ncol=2)
  bip1<-net1$net%n%"bipartite"
  bip2<-net2$net%n%"bipartite"
  if(!is.directed(net1$net)){  #Double the edges if undirected
    net1$elx<-rbind(net1$elx,net1$elx[net1$elx[,1]!=net1$elx[,2],2:1])
    net1$elnax<-rbind(net1$elnax,net1$elnax[net1$elnax[,1]!=net1$elnax[,2],2:1])
  }
  if(!is.directed(net2$net)){  #Double the edges if undirected
    net2$elx<-rbind(net2$elx,net2$elx[net2$elx[,1]!=net2$elx[,2],2:1])
    net2$elnax<-rbind(net2$elnax,net2$elnax[net2$elnax[,1]!=net2$elnax[,2],2:1])
  }
  if(NROW(net1$elx)>0){
    for(i in 1:NROW(net1$elx)){
      sel<-net2$elx[net2$elx[,1]==(net1$elx[i,2]-bip1),2]-bip2
      if(length(sel)>0)
        el<-rbind(el,cbind(rep(net1$elx[i,1],length(sel)),sel+insz1))
    }
  }
  if(NROW(net1$elnax)>0){
    for(i in 1:NROW(net1$elnax)){
      sel<-net2$elnax[net2$elnax[,1]==(net1$elnax[i,2]-bip1),2]-bip2
      if(length(sel)>0)
        elna<-rbind(elna,cbind(rep(net1$elnax[i,1],length(sel)),sel+insz1))
    }
  }
  if(!is.bipartite(out)){     #If not bipartite, remove the insz1 offset
    if(NROW(el)>0)
      el[,2]<-el[,2]-insz1
    if(NROW(elna)>0)
      elna[,2]<-elna[,2]-insz1
  }
  if(!is.multiplex(out)){     #If necessary, consolidate edges
    if(NROW(el)>1)
      el<-unique(el)
    if(NROW(elna)>1){
      elna<-unique(elna)
    }
    if(NROW(elna)>0&&NROW(el)>0){
      sel<-rep(TRUE,NROW(elna))
      for(i in 1:NROW(elna)){
        if(any((el[,1]==elna[i,1])&(el[,2]==elna[i,2])))
          sel[i]<-FALSE
      }
      elna<-elna[sel,]
    }
  }
  #Add the edges
  if(NROW(el)>0)                             #Add non-missing edges
    add.edges(out,tail=el[,1],head=el[,2])
  if(NROW(elna)>0)                           #Add missing edges
    add.edges(out,tail=elna[,1],head=elna[,2], names.eval=replicate(NROW(elna),list("na")), vals.eval=replicate(NROW(elna),list(list(na=TRUE))))
  #Return the resulting network
  out
}


#Given one or two input networks, return the information needed to generate
#output for binary or unary operations.  The return value for this function is
#a list with elements:
# net: the output network (empty, but with attributes set)
# elx: the edgelist for the first network (non-missing)
# elnax: the list of missing edges for the first network
# ely: in the binary case, the edgelist for the second network (non-missing)
# elnay: in the binary case, the list of missing edges for the second network
#' @rdname network-internal
networkOperatorSetup<-function(x,y=NULL){
  #Determine what attributes the output should have
  if(is.network(x)){
    nx<-network.size(x)     #Get size, directedness, multiplexity, bipartition
    dx<-is.directed(x)
    mx<-is.multiplex(x)
    hx<-is.hyper(x)
    lx<-has.loops(x)
    bx<-x%n%"bipartite"
    if(is.null(bx))
      bx<-FALSE
  }else{                     #If not a network object, resort to adj form
    x<-as.sociomatrix(x)
    if(NROW(x)!=NCOL(x)){    #Bipartite matrix
      nx<-NROW(x)+NCOL(x)
      dx<-FALSE
      mx<-FALSE
      hx<-FALSE
      lx<-FALSE
      bx<-NROW(x)
    }else{
      nx<-NROW(x)
      dx<-TRUE
      mx<-FALSE
      hx<-FALSE
      lx<-any(diag(x)!=0,na.rm=TRUE)
      bx<-FALSE
    }
  }
  if(is.null(y)){                 #If y is null, setup for unary operator
    n<-nx
    d<-dx
    m<-mx
    h<-hx
    b<-bx
    l<-lx
    x<-x
  }else{                          #Binary case
    if(is.network(y)){
      ny<-network.size(y)     #Get size, directedness, multiplexity, bipartition
      dy<-is.directed(y)
      my<-is.multiplex(y)
      hy<-is.hyper(y)
      ly<-has.loops(y)
      by<-y%n%"bipartite"
      if(is.null(by))
        by<-FALSE
    }else{                     #If not a network object, resort to adj form
      y<-as.sociomatrix(y)
      if(NROW(y)!=NCOL(y)){    #Bipartite matrix
        ny<-NROW(y)+NCOL(y)
        dy<-FALSE
        my<-FALSE
        hy<-FALSE
        ly<-FALSE
        by<-NROW(y)
      }else{
        ny<-NROW(y)
        dy<-TRUE
        my<-FALSE
        hy<-FALSE
        ly<-any(diag(y)!=0,na.rm=TRUE)
        by<-FALSE
      }
    }
    if(nx!=ny)                     #Make sure that our networks are conformable
      stop("Non-conformable networks (must have same numbers of vertices for elementwise operations).")
    if(bx!=by)
      stop("Non-conformable networks (must have same bipartite status for elementwise operations).")
    n<-nx                         #Output size=input size
    b<-bx                         #Output bipartition=input bipartition
    d<-dx|dy                      #Output directed if either input directed
    l<-lx|ly                      #Output has loops if either input does
    h<-hx|hy                      #Output hypergraphic if either input is
    m<-mx|my                      #Output multiplex if either input is
  }
  #Create the empty network object that will ultimately receive the edges
  net<-network.initialize(n=n,directed=d,hyper=h,loops=l,multiple=m,bipartite=b)
  #Create the edge lists; what the operator does with 'em isn't our problem
  if(h){                                           #Hypergraph
    stop("Elementwise operations not yet supported on hypergraphs.")
  }else{                                           #Dyadic network
    #Get the raw edge information
    if(is.network(x)){
      elx<-as.matrix(x,matrix.type="edgelist")
      elnax<-as.matrix(is.na(x),matrix.type="edgelist")
      if(d&(!dx)){      #Need to add two-way edges; BTW, can't have (!d)&dx...
        elx<-rbind(elx,elx[elx[,2]!=elx[,1],2:1,drop=FALSE])
        elnax<-rbind(elnax,elnax[,2:1])
      } else if (!dx){ # need to enforce edge ordering i<j for comparison
        # replace all rows where i<j with j,i
        elx[elx[,1]>elx[,2],]<-elx[elx[,1]>elx[,2],c(2,1)]
      }
    }else{
      elx<-which(x!=0,arr.ind=TRUE)
      elnax<-which(is.na(x),arr.ind=TRUE)
      if(!d){   #Sociomatrix already has two-way edges, so might need to remove
        elx<-elx[elx[,1]>=elx[,2],,drop=FALSE]
        elnax<-elnax[elnax[,1]>=elnax[,2],,drop=FALSE]
      } 
    }
    if(!is.null(y)){
      if(is.network(y)){
        ely<-as.matrix(y,matrix.type="edgelist")
        elnay<-as.matrix(is.na(y),matrix.type="edgelist")
        if(d&(!dy)){      #Need to add two-way edges; BTW, can't have (!d)&dy...
          ely<-rbind(ely,ely[ely[,2]!=ely[,1],2:1,drop=FALSE])
          elnay<-rbind(elnay,elnay[,2:1])
        } else if (!dy){ # need to enforce edge ordering i<j for comparison
          # replace all rows where i<j with j,i
          ely[ely[,1]>ely[,2],]<-ely[ely[,1]>ely[,2],c(2,1)]
        }
      }else{
        ely<-which(y!=0,arr.ind=TRUE)
        elnay<-which(is.na(y),arr.ind=TRUE)
        if(!d){  #Sociomatrix already has two-way edges, so might need to remove
          ely<-ely[ely[,1]>=ely[,2],,drop=FALSE]
          elnay<-elnay[elnay[,1]>=elnay[,2],d,rop=FALSE]
        }
      }
    }
    if(!l){        #Pre-emptively remove loops, as needed
      elx<-elx[elx[,1]!=elx[,2],,drop=FALSE]
      elnax<-elnax[elnax[,1]!=elnax[,2],,drop=FALSE]
      if(!is.null(y)){
        ely<-ely[ely[,1]!=ely[,2],,drop=FALSE]
        elnay<-elnay[elnay[,1]!=elnay[,2],,drop=FALSE]
      }
    }
    if(!m){        #Pre-emptively remove multiplex edges, as needed
      elx<-unique(elx)
      elnax<-unique(elnax)
      if(!is.null(y)){
        ely<-unique(ely)
        elnay<-unique(elnay)
      }
    }
  }
  #Return everything
  if(is.null(y))
    list(net=net,elx=elx,elnax=elnax)
  else
    list(net=net,elx=elx,elnax=elnax,ely=ely,elnay=elnay)
}




#' Combine Networks by Edge Value Multiplication
#' 
#' Given a series of networks, \code{prod.network} attempts to form a new
#' network by multiplication of edges.  If a non-null \code{attrname} is given,
#' the corresponding edge attribute is used to determine and store edge values.
#' 
#' The network product method attempts to combine its arguments by edgewise
#' multiplication (\emph{not} composition) of their respective adjacency
#' matrices; thus, this method is only applicable for networks whose adjacency
#' coercion is well-behaved.  Multiplication is effectively boolean unless
#' \code{attrname} is specified, in which case this is used to assess edge
#' values -- net values of 0 will result in removal of the underlying edge.
#' 
#' Other network attributes in the return value are carried over from the first
#' element in the list, so some persistence is possible (unlike the
#' multiplication operator).  Note that it is sometimes possible to
#' \dQuote{multiply} networks and raw adjacency matrices using this routine (if
#' all dimensions are correct), but more exotic combinations may result in
#' regrettably exciting behavior.
#' 
#' @param \dots one or more \code{network} objects.
#' @param attrname the name of an edge attribute to use when assessing edge
#' values, if desired.
#' @param na.rm logical; should edges with missing data be ignored?
#' @return A \code{\link{network}} object.
#' @author Carter T. Butts \email{buttsc@@uci.edu}
#' @seealso \code{\link{network.operators}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{http://www.jstatsoft.org/v24/i02/}
#' @keywords arith graphs
#' @examples
#' 
#' #Create some networks
#' g<-network.initialize(5)
#' h<-network.initialize(5)
#' i<-network.initialize(5)
#' g[1:3,,names.eval="marsupial",add.edges=TRUE]<-1
#' h[1:2,,names.eval="marsupial",add.edges=TRUE]<-2
#' i[1,,names.eval="marsupial",add.edges=TRUE]<-3
#' 
#' #Combine by addition
#' pouch<-prod(g,h,i,attrname="marsupial")
#' pouch[,]                                   #Edge values in the pouch?
#' as.sociomatrix(pouch,attrname="marsupial")     #Recover the marsupial
#' 
#' @export prod.network
#' @export
prod.network<-function(..., attrname=NULL, na.rm=FALSE){
  inargs<-list(...)
  y<-inargs[[1]]
  for(i in (1:length(inargs))[-1]){
    x<-as.sociomatrix(inargs[[i]],attrname=attrname)
    if(na.rm)
      x[is.na(x)]<-0
    ym<-as.sociomatrix(y,attrname=attrname)
    if(na.rm)
      ym[is.na(ym)]<-0
    y[,,names.eval=attrname,add.edges=TRUE]<-x*ym
  }
  y
}




#' Combine Networks by Edge Value Addition
#' 
#' Given a series of networks, \code{sum.network} attempts to form a new
#' network by accumulation of edges.  If a non-null \code{attrname} is given,
#' the corresponding edge attribute is used to determine and store edge values.
#' 
#' The network summation method attempts to combine its arguments by addition
#' of their respective adjacency matrices; thus, this method is only applicable
#' for networks whose adjacency coercion is well-behaved.  Addition is
#' effectively boolean unless \code{attrname} is specified, in which case this
#' is used to assess edge values -- net values of 0 will result in removal of
#' the underlying edge.
#' 
#' Other network attributes in the return value are carried over from the first
#' element in the list, so some persistence is possible (unlike the addition
#' operator).  Note that it is sometimes possible to \dQuote{add} networks and
#' raw adjacency matrices using this routine (if all dimensions are correct),
#' but more exotic combinations may result in regrettably exciting behavior.
#' 
#' @param \dots one or more \code{network} objects.
#' @param attrname the name of an edge attribute to use when assessing edge
#' values, if desired.
#' @param na.rm logical; should edges with missing data be ignored?
#' @return A \code{\link{network}} object.
#' @author Carter T. Butts \email{buttsc@@uci.edu}
#' @seealso \code{\link{network.operators}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{http://www.jstatsoft.org/v24/i02/}
#' @keywords arith graphs
#' @examples
#' 
#' #Create some networks
#' g<-network.initialize(5)
#' h<-network.initialize(5)
#' i<-network.initialize(5)
#' g[1,,names.eval="marsupial",add.edges=TRUE]<-1
#' h[1:2,,names.eval="marsupial",add.edges=TRUE]<-2
#' i[1:3,,names.eval="marsupial",add.edges=TRUE]<-3
#' 
#' #Combine by addition
#' pouch<-sum(g,h,i,attrname="marsupial")
#' pouch[,]                                   #Edge values in the pouch?
#' as.sociomatrix(pouch,attrname="marsupial")     #Recover the marsupial
#' 
#' @export sum.network
#' @export
sum.network<-function(..., attrname=NULL, na.rm=FALSE){
  inargs<-list(...)
  y<-inargs[[1]]
  for(i in (1:length(inargs))[-1]){
    x<-as.sociomatrix(inargs[[i]],attrname=attrname)
    if(na.rm)
      x[is.na(x)]<-0
    ym<-as.sociomatrix(y,attrname=attrname)
    if(na.rm)
      ym[is.na(ym)]<-0
    y[,,names.eval=attrname,add.edges=TRUE]<-x+ym
  }
  y
}
