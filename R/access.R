######################################################################
#
# access.R
#
# Written by Carter T. Butts <buttsc@uci.edu>; portions contributed by
# David Hunter <dhunter@stat.psu.edu> and Mark S. Handcock
# <handcock@u.washington.edu>.
#
# Last Modified 01/27/11
# Licensed under the GNU General Public License version 2 (June, 1991)
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
#
######################################################################


#Add a single edge to a network object.
#
add.edge<-function(x, tail, head, names.eval=NULL, vals.eval=NULL, edge.check=FALSE, ...){
  #Check to be sure we were called with a network
  if(!is.network(x))
    stop("add.edge requires an argument of class network.")
  #Do the deed
  xn<-deparse(substitute(x))
  ev<-parent.frame()
  x$RIsTooLazy<-NULL    #Defeat R's pathological evaluation rules
  x<-.Call("addEdge_R",x,tail,head,names.eval,vals.eval,edge.check, PACKAGE="network")
  if(exists(xn,envir=ev))            #If x not anonymous, set in calling env
    on.exit(assign(xn,x,pos=ev))
  invisible(x)
}


# Add multiple edges to network x.  Tail must be a list, each element of
# which is the tail set for a given edge (ditto for head).  If edge values
# are provided, they must be given similarly as lists of lists.
#
add.edges<-function(x, tail, head, names.eval=NULL, vals.eval=NULL, ...){
  #Check to be sure we were called with a network
  if(!is.network(x))
    stop("add.edges requires an argument of class network.")
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
    stop("head, tail, and value lists passed to add.edges must be of the same length!\n")
  edge.check<-list(...)$edge.check
  if(is.null(edge.check))
    edge.check<-FALSE
  #Pass the inputs to the C side
  xn<-deparse(substitute(x))
  ev<-parent.frame()
  x$RIsTooLazy<-NULL    #Defeat R's pathological evaluation rules
  x<-.Call("addEdges_R",x,tail,head,names.eval,vals.eval,edge.check, PACKAGE="network")
  if(exists(xn,envir=ev))            #If x not anonymous, set in calling env
    on.exit(assign(xn,x,pos=ev))
  invisible(x)
}


# Add nv vertices to network x.  Vertex attributes (in addition to those which
# are required) are to be provided in vattr; vattr must be a list containing
# nv elements, each of which is equal to the desired val[i] entry.
#
add.vertices<-function(x, nv, vattr=NULL, last.mode=TRUE){
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
  xn<-deparse(substitute(x))
  ev<-parent.frame()
  if(nv>0){
    x$RIsTooLazy<-NULL    #Defeat R's pathological evaluation rules
    if(last.mode||(!is.bipartite(x))){
      x<-.Call("addVertices_R",x,nv,vattr, PACKAGE="network")
      if(exists(xn,envir=ev))           #If x not anonymous, set in calling env
        on.exit(assign(xn,x,pos=ev))
      invisible(x)
    }else{
      .Call("addVertices_R",x,nv,vattr, PACKAGE="network")
      newid<-c(1:(x%n%"bipartite"),network.size(x),
        ((x%n%"bipartite"+1)):(network.size(x)-1))
      permute.vertexIDs(x,vids=newid)
      x%n%"bipartite"<-x%n%"bipartite"+1
      if(exists(xn,envir=ev))           #If x not anonymous, set in calling env
        on.exit(assign(xn,x,pos=ev))
      invisible(x)
    }
  }else
    invisible(x)
}


# Remove all instances of the specified attribute(s) from the edge set
#
delete.edge.attribute<-function(x,attrname){
  #Check to be sure we were called with a network
  if(!is.network(x))
    stop("delete.edge.attribute requires an argument of class network.")
  #Remove the edges
  xn<-deparse(substitute(x))
  ev<-parent.frame()
  x$RIsTooLazy<-NULL    #Defeat R's pathological evaluation rules
  x<-.Call("deleteEdgeAttribute_R",x,attrname, PACKAGE="network")
  if(exists(xn,envir=ev))          #If x not anonymous, set in calling env
    on.exit(assign(xn,x,pos=ev))
  invisible(x)
}
 
 
# Remove specified edges from the network.
#
delete.edges<-function(x,eid){
  #Check to be sure we were called with a network
  if(!is.network(x))
    stop("delete.edges requires an argument of class network.")
  xn<-deparse(substitute(x))
  ev<-parent.frame()
  if(length(eid)>0){
    #Perform a sanity check
    if((min(eid)<1)|(max(eid)>length(x$mel)))
      stop("Illegal edge in delete.edges.\n")
    #Remove the edges
    x$RIsTooLazy<-NULL    #Defeat R's pathological evaluation rules
    x<-.Call("deleteEdges_R",x,eid, PACKAGE="network")
    if(exists(xn,envir=ev))          #If x not anonymous, set in calling env
      on.exit(assign(xn,x,pos=ev))
    invisible(x)
  }else
    invisible(x)
}


# Remove the specified network-level attribute(s)
#
delete.network.attribute<-function(x,attrname){
  #Check to be sure we were called with a network
  if(!is.network(x))
    stop("delete.network.attribute requires an argument of class network.")
  #Remove the edges
  xn<-deparse(substitute(x))
  ev<-parent.frame()
  x$RIsTooLazy<-NULL    #Defeat R's pathological evaluation rules
  x<-.Call("deleteNetworkAttribute_R",x,attrname, PACKAGE="network")
  if(exists(xn,envir=ev))          #If x not anonymous, set in calling env
    on.exit(assign(xn,x,pos=ev))
  invisible(x)
}


# Remove all instances of the specified attribute(s) from the vertex set
#
delete.vertex.attribute<-function(x,attrname){
  #Set the attribute to NULL, thereby deleting it.
  set.vertex.attribute(x,attrname,NULL)
}
delete.vertex.attribute<-function(x,attrname){
  #Check to be sure we were called with a network
  if(!is.network(x))
    stop("delete.vertex.attribute requires an argument of class network.")
  #Remove the attribute
  xn<-deparse(substitute(x))
  ev<-parent.frame()
  x$RIsTooLazy<-NULL    #Defeat R's pathological evaluation rules
  x<-.Call("deleteVertexAttribute_R",x,attrname, PACKAGE="network")
  if(exists(xn,envir=ev))          #If x not anonymous, set in calling env
    on.exit(assign(xn,x,pos=ev))
  invisible(x)
}


# Remove specified vertices (and associated edges) from the network.
#
delete.vertices<-function(x,vid){
  #Check to be sure we were called with a network
  if(!is.network(x))
    stop("delete.vertices requires an argument of class network.")
  #Remove any vids which are out of bounds
  vid<-vid[(vid>0)&(vid<=network.size(x))]
  #Do the deed, if still needed
  xn<-deparse(substitute(x))
  ev<-parent.frame()
  if(length(vid)>0){
    x$RIsTooLazy<-NULL    #Defeat R's pathological evaluation rules
    if(is.bipartite(x)){  #If bipartite, might need to adjust mode 1 count
      m1v<-get.network.attribute(x,"bipartite")  #How many mode 1 verts?
      set.network.attribute(x,"bipartite",m1v-sum(vid<=m1v))
    }
    x<-.Call("deleteVertices_R",x,vid, PACKAGE="network")
    if(exists(xn,envir=ev))
      on.exit(assign(xn,x,pos=ev))
    invisible(x)
  }else
    invisible(x)
}


# Retrieve a specified edge attribute from edge list el.  The attribute
# is returned as a list, regardless of type.
#
get.edge.attribute<-function(el, attrname, unlist=TRUE){
  x <- lapply(lapply(el,"[[","atl"),"[[",attrname)
  if(unlist){unlist(x)}else{x}
}


# Retrieve a specified edge attribute from all edges in x.  The attribute
# is returned as a list, regardless of type.
#
get.edge.value<-function(x, attrname, unlist=TRUE){
  y <- lapply(lapply(x$mel,"[[","atl"),"[[",attrname)
  if(unlist){unlist(y)}else{y}
}


# Retrieve the ID numbers for all edges incident on v, in network x.  
# Outgoing or incoming edges are specified by neighborhood, while na.omit 
# indicates whether or not missing edges should be omitted.  The return value
# is a vector of edge IDs.
#
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
  .Call("getEdgeIDs_R",x,v,alter,neighborhood,na.omit, PACKAGE="network")
}


# Retrieve all edges incident on v, in network x.  Outgoing or incoming
# edges are specified by neighborhood, while na.omit indicates whether
# or not missing edges should be omitted.  The return value is a list of
# edges.
#
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
  .Call("getEdges_R",x,v,alter,neighborhood,na.omit, PACKAGE="network")
}


# Given a network and a set of vertices, return the subgraph induced by those
# vertices (preserving all associated metadata); if given two such sets, 
# return the edge cut (along with the associated vertices and meta-data) as
# a bipartite network.
#
get.inducedSubgraph<-function(x, v, alters=NULL){
  #Check to be sure we were called with a network
  if(!is.network(x))
    stop("get.inducedSubgraph requires an argument of class network.")
  #Do some reality checking
  n<-network.size(x)
  if((length(v)<1)||any(is.na(v))||any(v<1)||any(v>n))
    stop("Illegal vertex selection in get.inducedSubgraph")
  if(!is.null(alters)){
    if((length(alters)<1)||any(is.na(alters))||any(alters<1)||any(alters>n)|| any(alters%in%v))
      stop("Illegal vertex selection (alters) in get.inducedSubgraph")
  }
  #Start by making a copy of our target network (yes, this can be wasteful)
  new<-network.copy(x)
  #Now, strip out what is needed, and/or permute in the two-mode case
  if(is.null(alters)){                    #Simple case
    delete.vertices(new,(1:n)[-v])           #Get rid of everyone else
  }else{                                  #Really an edge cut, but w/vertices
    nv<-length(v)
    na<-length(alters)
    newids<-sort(c(v,alters))
    newv<-match(v,newids)
    newalt<-match(alters,newids)
    delete.vertices(new,(1:n)[-c(v,alters)])  #Get rid of everyone else
    permute.vertexIDs(new,c(newv,newalt))    #Put the new vertices first
    #Remove within-group edges
    for(i in 1:nv)
      for(j in (i:nv)[-1]){
        torem<-get.edgeIDs(new,i,alter=j,neighborhood="combined",na.omit=FALSE)
        if(length(torem)>0)
          delete.edges(new,torem)
      }
    for(i in (nv+1):(nv+na))
      for(j in (i:(nv+na))[-1]){
        torem<-get.edgeIDs(new,i,alter=j,neighborhood="combined",na.omit=FALSE)
        if(length(torem)>0)
          delete.edges(new,torem)
      }
    new%n%"bipartite"<-nv   #Set bipartite attribute
  }
  #Return the updated object
  new
}


# Retrieve a specified network-level attribute from network x.  The attribute
# type depends on the underlying storage mode, and cannot be guaranteed.
#
get.network.attribute<-function(x,attrname,unlist=FALSE){
  x <- x$gal[[attrname]]
  if(unlist){unlist(x)}else{x}
}


# Retrieve the neighborhood of v in network x.  Depending on the value of 
# type, the neighborhood in question may be in, out, or the union of the two.
# The return value for the function is a vector containing vertex IDs.
#
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
  .Call("getNeighborhood_R",x,v,type,na.omit, PACKAGE="network")
}


# Retrieve a specified vertex attribute (indicated by attrname) from network x.
# Where na.omit==TRUE, values for missing vertices are removed; where
# null.na==TRUE, NULL values are converted to NAs.  The return value of this
# function is a list.
# 
get.vertex.attribute<-function(x,attrname,na.omit=FALSE,null.na=TRUE,
                               unlist=TRUE){
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
  x <- va[!vna]
  if(unlist){unlist(x)}else{x}
}


# Return TRUE iff network x has loops.
#
has.loops<-function(x){
  if(!is.network(x))
    stop("has.loops requires an argument of class network.")
  else
    get.network.attribute(x,"loops")
}


# Return TRUE iff (vi,vj) in network x.  Where na.omit==TRUE, edges flagged
# as missing are ignored.
#
is.adjacent<-function(x,vi,vj,na.omit=FALSE){
  if(!is.network(x))
    stop("is.adjacent requires an argument of class network.\n")
  if(length(vi)!=length(vj)){
    vi<-rep(vi,length=max(length(vi),length(vj)))
    vj<-rep(vj,length=max(length(vi),length(vj)))
  }
  #Do the deed
 .Call("isAdjacent_R",x,vi,vj,na.omit, PACKAGE="network")
}


# Return TRUE iff network x is bipartite
#
is.bipartite<-function(x){
  if(!is.network(x))
    stop("is.bipartite requires an argument of class network.")
  else
    bip <- get.network.attribute(x,"bipartite")
  if(is.null(bip)){
   FALSE
  }else{
   bip>0
  }
}


# Return TRUE iff network x is directed.
#
is.directed<-function(x){
  if(!is.network(x))
    stop("is.directed requires an argument of class network.\n")
  else
    get.network.attribute(x,"directed")
}


# Return TRUE iff network x is hypergraphic.
#
is.hyper<-function(x){
  if(!is.network(x))
    stop("is.hyper requires an argument of class network.\n")
  else
    get.network.attribute(x,"hyper")
}


# Return TRUE iff network x is multiplex.
#
is.multiplex<-function(x){
  if(!is.network(x))
    stop("is.multiplex requires an argument of class network.\n")
  else
    get.network.attribute(x,"multiple")
}


# Return a network whose edges are the missing edges of x
#
is.na.network<-function(x){
  #Create an empty network with the same properties as x
  y<-network.initialize(network.size(x),directed=is.directed(x), hyper=is.hyper(x),loops=has.loops(x),multiple=is.multiplex(x), bipartite=x%n%"bipartite")
  #Add the missing edges of x to y
  .Call("isNANetwork_R",x,y,PACKAGE="network")
  #Return the updated network 
  y
}


# Return TRUE iff x is a network.
#
is.network<-function(x){
  inherits(x, "network")
}


# List attributes present on any edge
#
list.edge.attributes<-function(x){
  #First, check to see that this is a graph object
  if(!is.network(x))
    stop("list.network.attributes requires an argument of class network.\n")
  #Accumulate names
  allnam<-sapply(lapply(x$mel[!is.null(x$mel)],"[[","atl"),names)
  #Return the sorted, unique attribute names
  sort(unique(as.vector(unlist(allnam))))
}


# List network-level attributes
#
list.network.attributes<-function(x){
  #First, check to see that this is a graph object
  if(!is.network(x))
    stop("list.network.attributes requires an argument of class network.\n")
  #Return the attribute names
  sort(names(x$gal))
}


# List attributes present on any vertex
#
list.vertex.attributes<-function(x){
  #First, check to see that this is a graph object
  if(!is.network(x))
    stop("list.network.attributes requires an argument of class network.\n")
  #Accumulate names
  allnam<-unlist(sapply(x$val,names))
  #Return the sorted, unique attribute names
  sort(unique(as.vector(allnam)))
}


# Retrieve the number of free dyads (i.e., number of non-missing) of network x.
#
network.dyadcount<-function(x,na.omit=TRUE){
 if(!is.network(x))
   stop("network.dyadcount requires an argument of class network.")

 nodes <- network.size(x)
 if(is.directed(x)){
    dyads <- nodes * (nodes-1)
 }else{
  if(is.bipartite(x)){
   nactor <- get.network.attribute(x,"bipartite")
   nevent <- nodes - nactor
   dyads <- nactor * nevent
  }else{
   dyads <- nodes * (nodes-1)/2
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
network.edgecount<-function(x,na.omit=TRUE){
  #First, check to see that this is a graph object
  if(!is.network(x))
    stop("network.edgecount requires an argument of class network.\n")
  #Return the edge count
  .Call("networkEdgecount_R",x,na.omit, PACKAGE="network")
}


#Retrieve the number of missing edges in network x
#
network.naedgecount<-function(x){
  na<-get.edge.attribute(x$mel,"na")
  if(is.null(na))
    0
  else
    sum(na)
}


# Retrieve the size (i.e., number of vertices) of network x.
#
network.size<-function(x){
  if(!is.network(x))
    stop("network.size requires an argument of class network.\n")
  else
    get.network.attribute(x,"n")
}


# Retrieve the vertex names of network x (if present).
#
network.vertex.names<-function(x){
  if(!is.network(x)){
    stop("network.vertex.names requires an argument of class network.")
  }else{
    vnames <- get.vertex.attribute(x,"vertex.names")
    if(is.null(vnames)  | all(is.na(vnames)) ){
      paste(1:network.size(x))
    }else{
      vnames
    }
  }
}


# Set the vertex names of network x
#
"network.vertex.names<-"<-function(x,value){
  set.vertex.attribute(x,attrname="vertex.names",value=value)
}


# Permute the internal IDs (ordering) of the vertex set
permute.vertexIDs<-function(x,vids){
  #First, check to see that this is a graph object
  if(!is.network(x))
    stop("permute.vertexIDs requires an argument of class network.\n")
  #Sanity check: is this a permutation vector?
  n<-network.size(x)
  if((length(unique(vids))!=n)||(range(vids)!=c(1,n)))
    stop("Invalid permutation vector in permute.vertexIDs.")
  if(is.bipartite(x)){  #If bipartite, enforce partitioning
    bpc<-get.network.attribute(x,"bipartite")
    if(any(vids[1:bpc]>bpc)||(vids[(bpc+1):n]<=bpc))
      warning("Performing a cross-mode permutation in permute.vertexIDs.  I hope you know what you're doing....")
  }
  #Return the permuted graph
  xn<-deparse(substitute(x))
  ev<-parent.frame()
  x$RIsTooLazy<-NULL    #Defeat R's pathological evaluation rules
  x<-.Call("permuteVertexIDs_R",x,vids, PACKAGE="network")
  if(exists(xn,envir=ev))          #If x not anonymous, set in calling env
    on.exit(assign(xn,x,pos=ev))
  invisible(x)
}


# Set an edge attribute for network x.
#
set.edge.attribute<-function(x,attrname,value,e=1:length(x$mel)){
  #Check to be sure we were called with a network
  if(!is.network(x))
    stop("set.edge.attribute requires an argument of class network.")
  #Make sure that value is appropriate, coercing if needed
  if(!is.list(value)){
    if(!is.vector(value))
      stop("Inappropriate edge value given in set.edge.attribute.\n")
    else
      value<-as.list(rep(value,length=length(e)))
  }else
    if(length(value)!=length(e))
      value<-rep(value,length=length(e))
  xn<-deparse(substitute(x))
  ev<-parent.frame()
  if(length(e)>0){
    if((min(e)<1)|(max(e)>length(x$mel)))
      stop("Illegal edge in set.edge.attribute.\n")
    #Do the deed
    x$RIsTooLazy<-NULL    #Defeat R's pathological evaluation rules
    x<-.Call("setEdgeAttribute_R",x,attrname,value,e, PACKAGE="network")
    if(exists(xn,envir=ev))          #If x not anonymous, set in calling env
      on.exit(assign(xn,x,pos=ev))
    invisible(x)
  }else
    invisible(x)
}


# Set an edge value for network x.
#
set.edge.value<-function(x,attrname,value,e=1:length(x$mel)){
  #Check to be sure we were called with a network
  if(!is.network(x))
    stop("set.edge.value requires an argument of class network.\n")
  #Check to ensure that this is not a hypergraph
  if(is.hyper(x))
    stop("Hypergraphs not currently supported in set.edge.value.\n")
  #Make sure that value is appropriate, coercing if needed
  n<-network.size(x)
  if(!is.matrix(value)){
    if(is.vector(value))
      value<-matrix(rep(value,length=n*n),n,n)
    else
      value<-matrix(value,n,n)
  }
  #Perform additional sanity checks
  if((min(e)<1)|(max(e)>length(x$mel)))
    stop("Illegal edge in set.edge.value.\n")
  #Do the deed
  xn<-deparse(substitute(x))
  ev<-parent.frame()
  x$RIsTooLazy<-NULL    #Defeat R's pathological evaluation rules
  x<-.Call("setEdgeValue_R",x,attrname,value,e, PACKAGE="network")
  if(exists(xn,envir=ev))          #If x not anonymous, set in calling env
    on.exit(assign(xn,x,pos=ev))
  invisible(x)
}


# Set a network-level attribute for network x.
#
set.network.attribute<-function(x,attrname,value){
  x$gal[[attrname]]<-value
  x
}
set.network.attribute<-function(x,attrname,value){
  #Check to be sure we were called with a network
  if(!is.network(x))
    stop("set.network.attribute requires an argument of class network.")
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
  xn<-deparse(substitute(x))
  ev<-parent.frame()
  x$RIsTooLazy<-NULL    #Defeat R's pathological evaluation rules
  x<-.Call("setNetworkAttribute_R",x,attrname,value,PACKAGE="network")
  if(exists(xn,envir=ev))          #If x not anonymous, set in calling env
    on.exit(assign(xn,x,pos=ev))
  invisible(x)
}


# Set a vertex attribute for network x.
#
set.vertex.attribute<-function(x,attrname,value,v=1:network.size(x)){
  #Check to be sure we were called with a network
  if(!is.network(x))
    stop("set.vertex.attribute requires an argument of class network.")
  #Perform some sanity checks
  if(any((v>network.size(x))|(v<1)))
    stop("Vertex ID does not correspond to actual vertex in set.vertex.attribute.\n")
  #Make sure that value is appropriate, coercing if needed
  if(!is.list(value)){
    if(!is.vector(value))
      stop("Inappropriate value given in set.vertex.attribute.\n")
    else
      value<-as.list(rep(value,length=length(v)))
  }else
    if(length(value)!=length(v))
      value<-rep(value,length=length(v))
  #Do the deed
  xn<-deparse(substitute(x))
  ev<-parent.frame()
  x$RIsTooLazy<-NULL    #Defeat R's pathological evaluation rules
  x<-.Call("setVertexAttribute_R",x,attrname,value,v, PACKAGE="network")
  if(exists(xn,envir=ev))          #If x not anonymous, set in calling env
    on.exit(assign(xn,x,pos=ev))
  invisible(x)
}

