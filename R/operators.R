######################################################################
#
# operators.R
#
# Written by Carter T. Butts <buttsc@uci.edu>; portions contributed by
# David Hunter <dhunter@stat.psu.edu> and Mark S. Handcock
# <handcock@u.washington.edu>.
#
# Last Modified 10/19/06
# Licensed under the GNU General Public License version 2 (June, 1991)
#
# Part of the R/network package
#
# This file contains various operators which take networks as inputs.
#
# Contents:
#
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
# "%v%"
# "%v%<-"
# "%vattr%"
# "%vattr%<-"
# "+.network"
# "-.network"
# "*.network"
# "!.network"
# "|.network"
# "&.network"
# "%*%.network"
# "%c%"
# "%c%.network"
# 
######################################################################


"[.network"<-function(x,i,j,na.omit=TRUE){
  n<-network.size(x)
  xnames <- network.vertex.names(x)
  if(missing(i))              #If missing, use 1:n
    i<-1:n
  if(missing(j))
    j<-1:n
  if(is.matrix(i)&&(min(dim(i))==1))  #Vectorize if degenerate matrix
    i<-as.vector(i)
  if(is.matrix(i)){    #Still a matrix?
    if(is.logical(i)){                    #Subset w/T/F?
      j<-col(i)[i]
      i<-row(i)[i]
      out<-is.adjacent(x,i,j,na.omit=na.omit)
    }else{                                #Were we passed a pair list?
      if(is.character(i))
        i<-apply(i,c(1,2),match,xnames)
      out<-is.adjacent(x,i[,1],i[,2], na.omit=na.omit)
    }
  }else{                      #Otherwise, assume a vector or submatrix
    if(is.character(i))
      i<-match(i,xnames)
    if(is.character(j))
      j<-match(j,xnames)
    i<-(1:n)[i]                 #Piggyback on R's internal tricks
    j<-(1:n)[j]
    if(length(i)==1){
      out<-is.adjacent(x,i,j,na.omit=na.omit)
    }else{
      if(length(j)==1){
        out<-is.adjacent(x,i,j,na.omit=na.omit)
      }else{
        jrep<-rep(j,rep.int(length(i),length(j)))
        if(length(i)>0)
          irep<-rep(i,times=ceiling(length(jrep)/length(i)))
        out<-matrix(is.adjacent(x,irep,jrep,na.omit=na.omit), length(i),length(j))
      }
    }
    if((!is.null(xnames))&&is.matrix(out))
      dimnames(out) <- list(xnames[i],xnames[j])
  }
  out+0                       #Coerce to numeric
}


"[<-.network"<-function(x,i,j,names.eval=NULL,add.edges=FALSE,value){
  #Check for hypergraphicity
  if(is.hyper(x))
    stop("Assignment operator overloading does not currently support hypergraphic networks.");
  #Set up the edge list to change
  n<-network.size(x)
  xnames <- network.vertex.names(x)
  if(missing(i))              #If missing, use 1:n
    i<-1:n
  if(missing(j))
    j<-1:n
  if(is.matrix(i)&&(min(dim(i))==1))  #Vectorize if degenerate matrix
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
  #Set up values
  if(is.matrix(value))
    val<-value[cbind(match(el[,1],sort(unique(el[,1]))), match(el[,2],sort(unique(el[,2]))))]
  else
    val<-rep(as.vector(value),length=NROW(el))
  #Perform the changes
  if(is.null(names.eval)){  #If no names given, don't store values
    toadd<-(val!=0)&(has.loops(x)|(el[,1]!=el[,2]))
    torem<-val==0
    if(sum(toadd)>0){           #Check for already extant edges
      toadd[toadd][is.adjacent(x,el[toadd,1],el[toadd,2])]<-FALSE
      if(sum(toadd)>0)           #Add edges, if still needed
        x<-add.edges(x,as.list(el[toadd,1]),as.list(el[toadd,2]))
    }
    if(sum(torem)>0){          #Delete edges, if needed
      eid<-vector()
      for(k in (1:length(torem))[torem]){
        eid<-c(eid,get.edgeIDs(x,el[k,1],el[k,2],neighborhood="out", na.omit=FALSE))
      }
      x<-delete.edges(x,eid)
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
        epresent[k]<-(val[k]==0)   #If zero, skip it; otherwise, add
    }
    if(sum(epresent)>0)               #Adjust attributes for extant edges
      x<-set.edge.attribute(x,names.eval,valsl,eid)
    if(add.edges&&(sum(!epresent)>0))           #Add new edges, if needed
      x<-add.edges(x,as.list(el[!epresent,1]),as.list(el[!epresent,2]), names.eval=as.list(rep(names.eval,sum(!epresent))),vals.eval=as.list(val[!epresent]))
  }
  #Return the modified graph
  x
}


"%e%"<-function(x,attrname){
  get.edge.value(x,attrname=attrname)
}


"%e%<-"<-function(x,attrname,value){
  set.edge.value(x,attrname=attrname,value=value)
}


"%eattr%"<-function(x,attrname){
  x %e% attrname
}


"%eattr%<-"<-function(x,attrname,value){
  x %e% attrname <- value
}


"%n%"<-function(x,attrname){
  get.network.attribute(x,attrname=attrname)
}


"%n%<-"<-function(x,attrname,value){
  set.network.attribute(x,attrname=attrname,value=value)
}


"%nattr%"<-function(x,attrname){
  x %n% attrname
}


"%nattr%<-"<-function(x,attrname,value){
  x %n% attrname <- value
}


"%v%"<-function(x,attrname){
  get.vertex.attribute(x,attrname=attrname)
}


"%v%<-"<-function(x,attrname,value){
  set.vertex.attribute(x,attrname=attrname,value=value)
}


"%vattr%"<-function(x,attrname){
  x %v% attrname
}


"%vattr%<-"<-function(x,attrname,value){
  x %v% attrname <- value
}


"+.network"<-function(x,y,attrname=NULL){
  x<-as.sociomatrix(x,attrname=attrname)
  y<-as.sociomatrix(y,attrname=attrname)
  network(x+y,ignore.eval=is.null(attrname),names.eval=attrname)
}


"-.network"<-function(x,y,attrname=NULL){
  x<-as.sociomatrix(x,attrname=attrname)
  y<-as.sociomatrix(y,attrname=attrname)
  network(x-y,ignore.eval=is.null(attrname),names.eval=attrname)
}


"*.network"<-function(x,y,attrname=NULL){
  x<-as.sociomatrix(x,attrname=attrname)
  y<-as.sociomatrix(y,attrname=attrname)
  network(x*y,ignore.eval=is.null(attrname),names.eval=attrname)
}


"!.network"<-function(x){
  y<-network.copy(x)
  y[,]<-!(y[,])
  y
}


"|.network"<-function(x,y){
  network((x[,])|(y[,]))
}


"&.network"<-function(x,y){
  network((x[,])&(y[,]))
}


"%c%"<-function(x,y){
  UseMethod("%c%",x)
}


"%c%.network"<-function(x,y){
  #Convert to adjacency form
  x<-as.sociomatrix(x)
  y<-as.sociomatrix(y)
  #Check for conformability
  if(dim(x)[2]!=dim(y)[1])
    stop("Non-conformable relations in %c%.  Cannot compose.")
  #Obtain the composed graph
  network(round((x%*%y)>0),loops=TRUE)
}

