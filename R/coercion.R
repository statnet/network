######################################################################
#
# coercion.R
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
# This file contains various routines for coercion to/from network
# class objects.
#
# Contents:
#
#   as.matrix.network
#   as.matrix.network.adjacency
#   as.matrix.network.edgelist
#   as.matrix.network.incidence
#   as.network
#   as.network.default
#   as.network.network
#   as.network.matrix
#   as.sociomatrix
#
######################################################################


# Method for general coercion of network class objects into matrices.
# Matrix type is indicated by the eponymous argument; note that some
# types may not be supported for certain networks.  Where
# attrname!=NULL, an edge attribute of name attrname is used to supply
# edge values.  Otherwise, edges are assumed to be unvalued.
#


#' Coerce a Network Object to Matrix or Table Form
#' 
#' The \code{as.matrix} methods attempt to coerce their input to a matrix in
#' adjacency, incidence, or edgelist form.  Edge values (from a stored
#' attribute) may be used if present. \code{\link[tibble:as_tibble]{as_tibble}}
#' coerces into an edgelist in \code{\link{tibble}} (a type of
#' \code{\link{data.frame}}) form; this can be especially useful if extrecting
#' a character-type edge attribute.
#' 
#' If no matrix type is specified, \code{\link{which.matrix.type}} will be used
#' to make an educated guess based on the shape of \code{x}.  Where edge values
#' are not specified, a dichotomous matrix will be assumed.
#' 
#' Edgelists returned by the \code{as.matrix} methods are by default in a
#' slightly different form from the \code{sna} edgelist standard, but do
#' contain the \code{sna} extended matrix attributes (see
#' \code{\link{as.network.matrix}}).  They should typically be compatible with
#' \code{sna} library functions.  To ensure compatibility, the
#' \code{as.sna.edgelist} argument can be set (which returns an exact
#' \code{sna} edgelist). The \code{\link{as.edgelist}} function also returns a
#' similar edgelist matrix but with an enforced sorting.
#' 
#' For the \code{as.matrix} methods, if the \code{attrname} attribute is used
#' to include a charcter attribute, the resulting edgelist matrix will be
#' character rather than numeric. The \code{as_tibble} methods never coerce.
#' 
#' Note that adjacency matrices may also be obtained using the extraction
#' operator.  See the relevant man page for details. Also note that which
#' attributes get returned by the \code{as_tibble} method by default depends on
#' \code{unit}: by default no edge attributes are returned but all vertex
#' attributes are.
#' 
#' @param x an object of class \code{network}
#' @param matrix.type one of \code{"adjacency"}, \code{"incidence"},
#' \code{"edgelist"}, or \code{NULL}
#' @param attrname optionally, the name of an edge attribute to use for edge
#' values
#' @param attrnames optionally, either a character vector of the names of edge
#' attributes to use for edge values, or a numerical or logical vector to use
#' as indices for selecting them from \code{\link{list.edge.attributes}(x)} or
#' \code{\link{list.vertex.attributes}(x)} (depending on \code{unit}); passing
#' \code{TRUE} therefore returns all edge attributes as columns
#' @param expand.bipartite logical; if \code{x} is bipartite, should we return
#' the full adjacency matrix (rather than the abbreviated, two-mode form)?
#' @param as.sna.edgelist logical; should the edgelist be returned in sna
#' edglist form?
#' @param na.rm logical; should missing edges/vertices be included in the
#' edgelist formats? Ignored if \code{as.sna.edgelist=TRUE}.
#' @param unit whether a \code{\link{tibble}} of edge or vertex attributes
#' should be returned.
#' @param ...  additional arguments.
#' @return For \code{as.matrix} methods, an adjacency, incidence, or edgelist
#' matrix. For the \code{as_tibble} method, a \code{tibble} whose first two
#' columns are \code{.head} and \code{.tail}, whose third column \code{.eid} is
#' the edge ID, and whose subsequent columns are the requested edge attributes.
#' @author Carter T. Butts \email{buttsc@@uci.edu} and David Hunter
#' \email{dhunter@@stat.psu.edu}
#' @seealso \code{\link{which.matrix.type}}, \code{\link{network}},
#' \code{\link{network.extraction}},\code{\link{as.edgelist}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{http://www.jstatsoft.org/v24/i02/}
#' @keywords classes graphs
#' @examples
#' 
#' # Create a random network
#' m <- matrix(rbinom(25,4,0.159),5,5) # 50% density
#' diag(m) <- 0
#' g <- network(m, ignore.eval=FALSE, names.eval="a") # With values
#' g %e% "ac" <- letters[g %e% "a"]
#' 
#' # Coerce to matrix form
#' # No attributes:
#' as.matrix(g,matrix.type="adjacency")
#' as.matrix(g,matrix.type="incidence")
#' as.matrix(g,matrix.type="edgelist")
#' # Attributes:
#' as.matrix(g,matrix.type="adjacency",attrname="a")
#' as.matrix(g,matrix.type="incidence",attrname="a")
#' as.matrix(g,matrix.type="edgelist",attrname="a")
#' as.matrix(g,matrix.type="edgelist",attrname="ac")
#' 
#' # Coerce to a tibble:
#' library(tibble)
#' as_tibble(g)
#' as_tibble(g, attrnames=c("a","ac"))
#' as_tibble(g, attrnames=TRUE)
#' # Get vertex attributes instead:
#' as_tibble(g, unit = "vertices")
#' 
#' # Missing data handling:
#' g[1,2] <- NA
#' as.matrix(g,matrix.type="adjacency") # NA in the corresponding cell
#' as.matrix(g,matrix.type="edgelist", na.rm=TRUE) # (1,2) excluded
#' as.matrix(g,matrix.type="edgelist", na.rm=FALSE) # (1,2) included
#' as_tibble(g, attrnames="na", na.rm=FALSE) # Which edges are marked missing?
#' 
#' # Can also use the extraction operator
#' g[,]                            # Get entire adjacency matrix
#' g[1:2,3:5]                      # Obtain a submatrix
#' 
#' @export as.matrix.network
#' @export
as.matrix.network<-function(x,matrix.type=NULL,attrname=NULL,...){
  #Get the matrix type
  if(is.null(matrix.type))
    matrix.type<-"adjacency"
  else
    matrix.type<-match.arg(matrix.type,c("adjacency","incidence","edgelist"))
  #Dispatch as needed
  switch(matrix.type,
    adjacency=as.matrix.network.adjacency(x=x,attrname=attrname,...),
    incidence=as.matrix.network.incidence(x=x,attrname=attrname,...),
    edgelist=as.matrix.network.edgelist(x=x,attrname=attrname,...)
  )
}


# Coerce a network object to an adjacency matrix (where possible).  If
# provided, attrname is used to identify an attribute to use for edge
# values.
#
#' @rdname as.matrix.network
#' @usage \method{as.matrix.network}{adjacency}(x, attrname=NULL, 
#'    expand.bipartite = FALSE, ...)
#' @export as.matrix.network.adjacency
#' @rawNamespace S3method(as.matrix.network,adjacency)
as.matrix.network.adjacency<-function(x,attrname=NULL,expand.bipartite=FALSE,...){
  #Check to make sure this is a supported network type
  if(is.hyper(x))
    stop("Hypergraphs not currently supported in as.matrix.network.adjacency.  Exiting.\n")
  if(is.multiplex(x))
    stop("Multigraphs not currently supported in as.matrix.network.adjacency.  Exiting.\n")
  #Generate the adjacency matrix 
  m<-matrix(0,nrow=network.size(x),ncol=network.size(x))
  if(network.size(x)==0)
    return(m)
  tl<-unlist(sapply(x$mel,"[[","outl")) #Can unlist b/c no hyperedges
  hl<-unlist(sapply(x$mel,"[[","inl"))
  nal<-as.logical(get.edge.attribute(x$mel,"na",unlist=TRUE))
  if(!is.null(attrname)){
    val<-unlist(get.edge.attribute(x$mel,attrname,unlist=FALSE))
    if(is.null(val)){
     warning(paste("There is no edge attribute named", attrname))
     val<-rep(1,length(tl))
    }
  }else{
    val<-rep(1,length(tl))
  }
  if(length(hl[!nal])>0){
    m[tl[!nal]+(hl[!nal]-1)*network.size(x)]<-val[!nal]
  }
  if(length(hl[ nal])>0){
   m[tl[ nal]+(hl[ nal]-1)*network.size(x)]<-NA
  }
  #If undirected, symmetrize
  if(!is.directed(x)){
# changed by MSH to allow non binary values
#   m<-pmax(m,t(m))
    sel<-m
    sel[is.na(m)]<-1
    m[sel==0] <- t(m)[sel==0]
  }
  #Set row/colnames to vertex names
  xnames <- network.vertex.names(x)
  dimnames(m) <- list(xnames, xnames)
  #If bipartite and !expand.bipartite, return in two-mode form
  if(is.bipartite(x)&(!expand.bipartite)){
    nactors <- get.network.attribute(x, "bipartite")
    nevents <- network.size(x) - nactors
    m <- m[0:nactors, nactors+(1:nevents)]
  }
  #Return the result
  m
}


# Coerce a network object to an edgelist matrix.  If provided, attrname is 
# used to identify an attribute to use for edge values.  Setting as.sna.edgelist
# results in output in the sna edgelist format (including missing edge handling)
# and is used by the sna package for coercion.
#
#' @rdname as.matrix.network
#' @usage \method{as.matrix.network}{edgelist}(x, attrname=NULL, 
#'    as.sna.edgelist = FALSE, na.rm = TRUE, ...)
#' @export as.matrix.network.edgelist
#' @rawNamespace S3method(as.matrix.network,edgelist)
as.matrix.network.edgelist<-function(x,attrname=NULL,as.sna.edgelist=FALSE,na.rm=TRUE,...){
  #Check to make sure this is a supported network type
  if(is.hyper(x))
    stop("Hypergraphs not currently supported in as.matrix.network.edgelist.  Exiting.\n")
  #Find the missing edges
  nal<-as.logical(get.edge.attribute(x$mel,"na"))
  #Generate the edgelist matrix
  m<-cbind(unlist(sapply(x$mel,"[[","outl")), unlist(sapply(x$mel,"[[","inl")))
  #Add edge values, if needed
  if(!is.null(attrname))
    m<-cbind(m,get.edge.attribute(x$mel,attrname,na.omit=FALSE,null.na=TRUE,deleted.edges.omit=TRUE))
  else if(as.sna.edgelist)
    m<-cbind(m,rep(1,NROW(m)))
  #Set additional attributes and return the result
  if(as.sna.edgelist && nrow(m) > 0) # check that there are actually edges
    m[nal,3]<-NA
  else if(na.rm) m<-m[!nal,,drop=FALSE]
  
  if(length(m)==0)
    m<-matrix(numeric(0),ncol=2+as.sna.edgelist+!is.null(attrname))
  else if((!is.directed(x))&&as.sna.edgelist){    #sna uses directed form
    m<-rbind(m,m[m[,2]!=m[,1],c(2:1,3)])
  }
  attr(m,"n")<-network.size(x)
  attr(m,"vnames")<-network.vertex.names(x)
  if(is.bipartite(x))
    attr(m,"bipartite")<-x%n%"bipartite"
  m
}

# Coerce a network object to an edgelist tibble.  If provided, attrnames is 
# used to identify a list of attributes to use for edge values.
#
#' @rdname as.matrix.network
#' @param store.eid whether the edge ID should be stored in the third column (`.eid`).
#' @export
as_tibble.network<-function(x,attrnames=(match.arg(unit)=="vertices"),na.rm=TRUE,..., unit=c("edges", "vertices"), store.eid=FALSE){
  unit <- match.arg(unit)
  if(unit=="edges"){

    #Find the missing edges
    nal<-as.logical(get.edge.attribute(x$mel,"na"))

    #Generate the edgelist matrix
    tails <- lapply(x$mel,`[[`,"outl")
    heads <- lapply(x$mel,`[[`,"inl")
    m <- list(
      .tail = if(is.hyper(x)) tails else as.integer(unlist(tails)),
      .head = if(is.hyper(x)) heads else as.integer(unlist(heads))
    )
    if(store.eid) m$.eid <- which(as.logical(sapply(tails, length)) | as.logical(sapply(heads, length)))

    #Add edge values, if needed
    # If logical or numeric, use as index; na.omit() is needed to handle
    # a pathological case where list.edge.attributes(x) is empty but
    # attrnames=TRUE.
    if(is.logical(attrnames) || is.numeric(attrnames)) attrnames <- na.omit(list.edge.attributes(x)[attrnames])
    a <- attrnames %>%
      lapply(get.edge.attribute, x=x$mel, unlist=FALSE, na.omit=FALSE,null.na=TRUE,deleted.edges.omit=TRUE) %>% # Obtain a list of edge attribute values.
      lapply(function(l) if(length(lens <- unique(lengths(l))) == 1L && lens==1L) unlist(l, recursive=FALSE) else l) %>% # Iff all values of an edge attribut ehave the same length *and* that length is 1, convert to vector. (FIXME: why didn't I just compare all lengths to 1L?)
      set_names(attrnames)
    m <- c(m, a)

  }else{ # "vertices" is the only other possibility at this time

    #Find the missing vertices
    nal<-as.logical(get.vertex.attribute(x,"na"))

    if(is.logical(attrnames) || is.numeric(attrnames)) attrnames <- na.omit(list.vertex.attributes(x)[attrnames])
    a <- attrnames %>%
      lapply(get.vertex.attribute, x=x, unlist=FALSE, na.omit=FALSE,null.na=TRUE) %>% # Obtain a list of edge attribute values.
      lapply(function(l) if(length(lens <- unique(lengths(l))) == 1L && lens==1L) unlist(l, recursive=FALSE) else l) %>% # Iff all values of a vertex attribut ehave the same length *and* that length is 1, convert to vector. (FIXME: why didn't I just compare all lengths to 1L?)
      set_names(attrnames)
    m <- a

  }

  m <- as_tibble(m)
  if(na.rm) m <- m[!nal,]

  attr(m,"n")<-network.size(x)
  attr(m,"vnames")<-network.vertex.names(x)
  if(is.bipartite(x))
    attr(m,"bipartite")<-x%n%"bipartite"
  m
}

#' @rdname as.matrix.network
#' @rawNamespace S3method(as.tibble,network)
as.tibble.network <- as_tibble.network

# Coerce a network object to an incidence matrix (where possible).  If
# provided, attrname is used to identify an attribute to use for edge
# values.
#
#' @rdname as.matrix.network
#' @usage \method{as.matrix.network}{incidence}(x, attrname=NULL, ...)
#' @export as.matrix.network.incidence
#' @rawNamespace S3method(as.matrix.network,incidence)
as.matrix.network.incidence<-function(x,attrname=NULL,...){
  #Perform preprocessing
  n<-network.size(x)
  nulledge<-sapply(x$mel,is.null)
  inl<-lapply(x$mel,"[[","inl")[!nulledge]
  outl<-lapply(x$mel,"[[","outl")[!nulledge]
  if(!is.null(attrname))
    evals<-unlist(get.edge.attribute(x$mel,attrname))[!nulledge]
  else
    evals<-rep(1,length(x$mel))[!nulledge]
  ena<-as.logical(get.edge.attribute(x$mel,"na"))[!nulledge]
  #If called with an empty graph, return a degenerate matrix
  if(length(ena)==0)
    return(matrix(numeric(0),nrow=n))
  #Generate the incidence matrix
  dir<-is.directed(x)
  f<-function(a,m,k){y<-rep(0,m); y[a]<-k; y}
  im<-sapply(inl,f,n,1)+sapply(outl,f,n,ifelse(dir,-1,1))
  if(!dir)
    im<-pmin(im,1)
  im<-sweep(im,2,evals,"*")              #Fill in edge values
  im[(sapply(ena,rep,n)*(im!=0))>0]<-NA      #Add NAs, if needed
  #Return the result
  im
}

#' @rdname network
#' @export
as.network<-function(x,...)
  UseMethod("as.network")

#' @name as.network.matrix
#'
#' @title Coercion from Matrices to Network Objects
#' 
#' @description \code{as.network.matrix} attempts to coerce its first argument to an object
#' of class \code{network}.
#' 
#' @details Depending on \code{matrix.type}, one of three edgeset constructor methods
#' will be employed to read the input matrix (see
#' \code{\link{edgeset.constructors}}).  If \code{matrix.type==NULL},
#' \code{\link{which.matrix.type}} will be used to guess the appropriate matrix
#' type.
#' 
#' The coercion methods will recognize and attempt to utilize the \code{sna}
#' extended matrix attributes where feasible.  These are as follows: \itemize{
#' \item\code{"n"}: taken to indicate number of vertices in the network.
#' \item\code{"bipartite"}: taken to indicate the network's \code{bipartite}
#' attribute, where present.  \item\code{"vnames"}: taken to contain vertex
#' names, where present.  } These attributes are generally used with edgelists,
#' and indeed data in \code{sna} edgelist format should be transparently
#' converted in most cases.  Where the extended matrix attributes are in
#' conflict with the actual contents of \code{x}, results are no guaranteed
#' (but the latter will usually override the former). For an edge list, the
#' number of nodes in a network is determined by the number of unique nodes
#' specified. If there are isolate nodes not in the edge list, the "n"
#' attribute needs to be set. See example below.
#' 
#' @param x a matrix containing an adjacency structure
#' @param matrix.type one of \code{"adjacency"}, \code{"edgelist"},
#' \code{"incidence"}, or \code{NULL}
#' @param directed logical; should edges be interpreted as directed?
#' @param hyper logical; are hyperedges allowed?
#' @param loops logical; should loops be allowed?
#' @param multiple logical; are multiplex edges allowed?
#' @param bipartite count; should the network be interpreted as bipartite? If
#' present (i.e., non-NULL) it is the count of the number of actors in the
#' bipartite network. In this case, the number of nodes is equal to the number
#' of actors plus the number of events (with all actors preceding all events).
#' The edges are then interpreted as nondirected.
#' @param ignore.eval logical; ignore edge values?
#' @param names.eval optionally, the name of the attribute in which edge values
#' should be stored
#' @param na.rm logical; ignore missing entries when constructing the network?
#' @param edge.check logical; perform consistency checks on new edges?
#' @param ... additional arguments
#' @return An object of class \code{network}
#' @author Carter T. Butts \email{buttsc@@uci.edu} and David Hunter
#' \email{dhunter@@stat.psu.edu}
#' @seealso \code{\link{edgeset.constructors}}, \code{\link{network}},
#' \code{\link{which.matrix.type}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{http://www.jstatsoft.org/v24/i02/}
#' @keywords classes graphs
#' @examples
#' 
#' #Draw a random matrix
#' m<-matrix(rbinom(25,1,0.5),5)
#' diag(m)<-0
#' 
#' #Coerce to network form
#' g<-as.network.matrix(m,matrix.type="adjacency")
#' 
#' # edge list example. Only 4 nodes in the edge list.
#' m = matrix(c(1,2, 2,3, 3,4), byrow = TRUE, nrow=3)
#' attr(m, 'n') = 7
#' as.network(m, matrix.type='edgelist')
#' 
#' @export as.network.default
#' @export
as.network.default<-function(x,...)
  as.network.matrix(x,...)

#' @export as.network.network
#' @export
as.network.network<-function(x,...)
  x


#
# MSH modified for bipartite
#

#' @rdname as.network.matrix
#' @export as.network.matrix
#' @export
as.network.matrix<-function(x, matrix.type=NULL,
        directed=TRUE, hyper=FALSE, loops=FALSE, multiple=FALSE,
        bipartite=FALSE,
        ignore.eval=TRUE, names.eval=NULL, na.rm=FALSE, edge.check=FALSE, ...){
  #Before doing anything else, pull any attributes from the matrix that we
  #might need....
  nattr<-attr(x,"n")             #Currently, only using sna edgelist attributes
  battr<-attr(x,"bipartite")
  vattr<-attr(x,"vnames")
  #Convert logicals to numeric form
  if(is.logical(x)){x <- 1*x}
  #Get the matrix type
  if(is.null(matrix.type))
    matrix.type<-which.matrix.type(x)
  else
    matrix.type<-match.arg(matrix.type,c("adjacency","incidence","edgelist",
                                         "bipartite"))
  if(is.logical(bipartite)&&bipartite)
    matrix.type<-"bipartite"
  #Patch adj->bipartite case
  if((bipartite>0)&&(matrix.type=="adjacency")&&(NROW(x)==bipartite))  
    matrix.type<-"bipartite"
  
  # Add names if available
  unames <- NULL
  if(matrix.type=="edgelist"){
    if(dim(x)[2]>2)
      vals<-x[,-(1:2),drop=FALSE]
    else
      vals<-NULL
    if(is.character(x<-as.matrix(x[,1:2,drop=FALSE]))){
      unames <- sort(unique(as.vector(x)))
      x <- cbind(match(x[,1],unames),match(x[,2],unames))
    }
    if(!is.null(vals)){
      x<-cbind(x,vals)
      
      if (is.null(colnames(vals))){
        colnames(x)<-NULL  #R creates these, and they are annoying later
      } else {
        # leave colnames for vals intact so they can be used for edge attributes
        colnames(x)<-c(NA,NA,colnames(vals))
      }
    }
  }
  if(matrix.type=="adjacency" && !is.null(colnames(x))){
    unames <- colnames(x)
  }
  if(matrix.type=="bipartite"){
   directed <- FALSE
   bipartite <- dim(x)[1]
   unames <- 1:sum(dim(x))
   if(!is.null(rownames(x))){
     unames[1:(dim(x)[1])] <- rownames(x)
   }
   if(!is.null(colnames(x))){
     unames[(dim(x)[1])+(1:(dim(x)[2]))] <- colnames(x)
   }
  }
  if(!is.null(vattr))                        #If given names, use 'em
    unames<-vattr
  #Initialize the network object
  n<-switch(matrix.type,	#Extract n based on matrix type
    adjacency=dim(x)[1],
    incidence=dim(x)[1],
    bipartite=sum(dim(x)),
    edgelist=max(x[,1:2]),
  )
  if(is.numeric(nattr))                      #If given n, use it
    n<-nattr
  if(is.numeric(battr))                      #If given bipartite info, use it
    bipartite<-battr
  
  # if we are going to build an adjacency matrix and it doesn't match the nattr, give an error, because otherwise will crash
  # this may happen if a square edgelist with attribute information is passed in
  if (is.numeric(nattr) & matrix.type=='adjacency'){
    if (nattr != ncol(x)){
      stop('the dimensions of the matrix argument (',nrow(x),' by ', ncol(x),') do not match the network size indicated by the attached n attribute (',nattr,'), perhaps matrix.type argument is not correct')
    }
  }
  
  g<-network.initialize(n,directed=directed, hyper=hyper, loops=loops, multiple=multiple,bipartite=bipartite)
  #Call the specific coercion routine, depending on matrix type
  g<-switch(matrix.type,
    adjacency=network.adjacency(x,g,
     ignore.eval,names.eval,na.rm,edge.check),
    incidence=network.incidence(x,g,
     ignore.eval,names.eval,na.rm,edge.check),
    bipartite=network.bipartite(x,g,
     ignore.eval,names.eval,na.rm,edge.check),
    edgelist=network.edgelist(x,g, 
     ignore.eval,names.eval,na.rm,edge.check)
  )

  if(!is.null(unames)){
   g <- set.vertex.attribute(g,"vertex.names", unames)
  }
  #Return the result
  g
}


#Force the input into sociomatrix form.  This is a shortcut to 
#as.matrix.network.adjacency, which ensures that a raw matrix is
#passed through as-is.


#' Coerce One or More Networks to Sociomatrix Form
#' 
#' \code{as.sociomatrix} takes adjacency matrices, adjacency arrays,
#' \code{\link{network}} objects, or lists thereof, and returns one or more
#' sociomatrices (adjacency matrices) as appropriate.  This routine provides a
#' useful input-agnostic front-end to functions which process adjacency
#' matrices.
#' 
#' \code{as.sociomatrix} provides a more general means of coercing input into
#' adjacency matrix form than \code{\link{as.matrix.network}}. In particular,
#' \code{as.sociomatrix} will attempt to coerce all input networks into the
#' appropriate form, and return the resulting matrices in a regularized manner.
#' If \code{simplify==TRUE}, \code{as.sociomatrix} attempts to return the
#' matrices as a single adjacency array.  If the input networks are of variable
#' size, or if \code{simplify==FALSE}, the networks in question are returned as
#' a list of matrices.  In any event, a single input network is always returned
#' as a lone matrix.
#' 
#' If \code{attrname} is given, the specified edge attribute is used to extract
#' edge values from any \code{\link{network}} objects contained in \code{x}.
#' Note that the same attribute will be used for all networks; if no attribute
#' is specified, the standard dichotomous default will be used instead.
#' 
#' @param x an adjacency matrix, array, \code{\link{network}} object, or list
#' thereof.
#' @param attrname optionally, the name of a network attribute to use for
#' extracting edge values (if \code{x} is a \code{\link{network}} object).
#' @param simplify logical; should \code{as.sociomatrix} attempt to combine its
#' inputs into an adjacency array (\code{TRUE}), or return them as separate
#' list elements (\code{FALSE})?
#' @param expand.bipartite logical; if \code{x} is bipartite, should we return
#' the full adjacency matrix (rather than the abbreviated, two-mode form)?
#' @param ...  additional arguments for the coercion routine.
#' @return One or more adjacency matrices.  If all matrices are of the same
#' dimension and \code{simplify==TRUE}, the matrices are joined into a single
#' array; otherwise, the return value is a list of single adjacency matrices.
#' @author Carter T. Butts \email{buttsc@@uci.edu}
#' @seealso \code{\link{as.matrix.network}}, \code{\link{network}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{http://www.jstatsoft.org/v24/i02/}
#' @keywords graphs manip
#' @examples
#' 
#' #Generate an adjacency array
#' g<-array(rbinom(100,1,0.5),dim=c(4,5,5))
#' 
#' #Generate a network object
#' net<-network(matrix(rbinom(36,1,0.5),6,6))
#' 
#' #Coerce to adjacency matrix form using as.sociomatrix
#' as.sociomatrix(g,simplify=TRUE)   #Returns as-is
#' as.sociomatrix(g,simplify=FALSE)  #Returns as list
#' as.sociomatrix(net)               #Coerces to matrix
#' as.sociomatrix(list(net,g))       #Returns as list of matrices
#' 
#' @export as.sociomatrix
as.sociomatrix<-function(x, attrname=NULL, simplify=TRUE, expand.bipartite=FALSE, ...){
  if(is.network(x)){ #If network, coerce to adjacency matrix
    g<-as.matrix.network.adjacency(x,attrname=attrname, expand.bipartite=expand.bipartite,...)
  }else if(is.matrix(x)||is.array(x)){ #If an array/matrix, use as-is
    g<-x
  }else if(is.list(x)){  #If a list, recurse on list elements
    g<-lapply(x,as.sociomatrix,attrname=attrname,simplify=simplify, expand.bipartite=expand.bipartite,...)
  }else{
    stop("as.sociomatrix input must be an adjacency matrix/array, network, or list.")
  }
  #Convert into the appropriate return format
  if(is.list(g)){   #Collapse if needed
    if(length(g)==1){
      g<-g[[1]]
      if((!simplify)&&(length(dim(g))==3)){  #Coerce to a list of matrices?
        out<-list()
        for(i in 1:dim(g)[1])
          out[[i]]<-g[i,,]
      }else{
        out<-g
      }
    }else{
      #Coerce to array form?
      if(simplify){
        dims<-sapply(g,dim)
        if(is.list(dims)){      #Dims must not be of equal length
          mats<-sapply(dims,length)
          mats[mats==1]<-0
          mats[mats==2]<-1
          mats[mats==3]<-sapply(dims[mats==3],"[[",1)
          mats<-cumsum(mats)
          dims<-sapply(dims,"[",2)
        }else{                  #Dims are of equal length
          if(NROW(dims)==3)      #Determine number of matrices per entry
            mats<-cumsum(dims[1,])
          else
            mats<-1:NCOL(dims)
          dims<-dims[2,]         #Get ncols
        }
        if((!any(is.null(dims)))&&(length(unique(dims))==1)&&(all(mats>0))){
          out<-array(dim=c(mats[length(mats)],dims[1],dims[1]))
          for(i in 1:length(mats))
            out[(c(0,mats)[i]+1):(mats[i]),,]<-g[[i]]
        }else
          out<-g
      }else
        out<-g
    }
  }else{
    if((!simplify)&&(length(dim(g))==3)){  #Coerce to a list of matrices?
      out<-list()
      for(i in 1:dim(g)[1])
        out[[i]]<-g[i,,]
    }else
      out<-g
  }
  #Return the result
  out
}
