######################################################################
#
# operators.R
#
# Written by Carter T. Butts <buttsc@uci.edu>; portions contributed by
# David Hunter <dhunter@stat.psu.edu> and Mark S. Handcock
# <handcock@u.washington.edu>.
#
# Last Modified 03/25/08
# Licensed under the GNU General Public License version 2 (June, 1991)
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
# prod.network
# sum.network
# 
######################################################################

"<-.network"<-function(x,i,value){
  cl<-oldClass(x)
  class(x)<-NULL
  x[[i]]<-network.copy(value)
  class(x)<-cl
  return(x)
}


"$<-.network"<-function(x,i,value){
  cl<-oldClass(x)
  class(x)<-NULL
  x[[i]]<-value
  class(x)<-cl
  return(x)
}


"[.network"<-function(x,i,j,na.omit=FALSE){
  narg<-nargs()+missing(na.omit)
  n<-network.size(x)
  xnames <- network.vertex.names(x)
  if(missing(i))              #If missing, use 1:n
    i<-1:n
  if((narg>3)&&missing(j))
    j<-1:n
  if(is.matrix(i)&&(NCOL(i)==1))  #Vectorize if degenerate matrix
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
  }else if((narg<3)&&missing(j)){   #Here, assume a list of cell numbers
    ir<-1+((i-1)%%n)
    ic<-1+((i-1)%/%n)
    out<-is.adjacent(x,ir,ic,na.omit=na.omit)
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
  narg<-nargs()+missing(names.eval)+missing(add.edges)
  n<-network.size(x)
  xnames <- network.vertex.names(x)
  if(missing(i))              #If missing, use 1:n
    i<-1:n
  if((narg>5)&&missing(j))
    j<-1:n
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
  #Set up values
  if(is.matrix(value))
    val<-value[cbind(match(el[,1],sort(unique(el[,1]))), match(el[,2],sort(unique(el[,2]))))]
  else
    val<-rep(as.vector(value),length=NROW(el))
  #Perform the changes
  if(is.null(names.eval)){  #If no names given, don't store values
    valna<-is.na(val)           #Pull out missing edges
    val[valna]<-1               #Treat missing as "present" for our purposes
    toadd<-(val!=0)&(has.loops(x)|(el[,1]!=el[,2]))
    torem<-val==0
    if(sum(toadd)>0){           #Check for already extant edges
      toadd[toadd][is.adjacent(x,el[toadd,1],el[toadd,2])]<-FALSE
      if(sum(toadd)>0)          #Add edges, if still needed
        x<-add.edges(x,as.list(el[toadd,1]),as.list(el[toadd,2]))
      if(sum(valna)>0){         #Mark all relevant edges as missing
        eid<-vector()
        for(k in (1:length(valna))[valna]){
          eid<-c(eid,get.edgeIDs(x,el[k,1],el[k,2],neighborhood="out", na.omit=FALSE))
        }
        set.edge.attribute(x,"na",TRUE,eid)
      }
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

#"+"<-function(e1, e2, ...) UseMethod("+")
#
#"+.default"<-function(e1,e2,...) { (base::"+")(e1,e2) }
#
#"+.network"<-function(e1,e2,attrname=NULL,...){
#  e1<-as.sociomatrix(e1,attrname=attrname)
#  e2<-as.sociomatrix(e2,attrname=attrname)
#  network(e1+e2,ignore.eval=is.null(attrname),names.eval=attrname)
#}
"+.network"<-function(e1,e2){
  e1<-as.sociomatrix(e1)
  e2<-as.sociomatrix(e2)
  network(e1+e2)
}


#"-"<-function(e1, e2, ...) UseMethod("-")
#
#"-.default"<-function(e1,e2,...) { (base::"-")(e1,e2) }
#
"-.network"<-function(e1,e2){
  e1<-as.sociomatrix(e1)
  e2<-as.sociomatrix(e2)
  network(e1-e2)
}


#"*"<-function(e1, e2, ...) UseMethod("*")
#
#"*.default"<-function(e1,e2,...) { (base::"*")(e1,e2) }
#
"*.network"<-function(e1,e2){
  e1<-as.sociomatrix(e1)
  e2<-as.sociomatrix(e2)
  network(e1*e2)
}


"!.network"<-function(e1){
  y<-e1
  y[,]<-!(y[,])
  y
}


"|.network"<-function(e1,e2){
  network((e1[,])|(e2[,]))
}


"&.network"<-function(e1,e2){
  network((e1[,])&(e2[,]))
}


"%c%"<-function(e1,e2){
  UseMethod("%c%",e1)
}


"%c%.network"<-function(e1,e2){
  #Convert to adjacency form
  e1<-as.sociomatrix(e1)
  e2<-as.sociomatrix(e2)
  #Check for conformability
  if(dim(e1)[2]!=dim(e2)[1])
    stop("Non-conformable relations in %c%.  Cannot compose.")
  #Obtain the composed graph
  network(round((e1%*%e2)>0),loops=TRUE)
}


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
