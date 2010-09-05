######################################################################
#
# misc.R
#
# Written by Carter T. Butts <buttsc@uci.edu>; portions contributed by
# David Hunter <dhunter@stat.psu.edu> and Mark S. Handcock
# <handcock@u.washington.edu>.
#
# Last Modified 07/23/08
# Licensed under the GNU General Public License version 2 (June, 1991)
#
# Part of the R/network package
#
# This file contains various network routines which don't fit anywhere
# else (generally, utilities and the like).
#
# Contents:
#
#   as.color
#   network.density
#   is.color
#   is.discrete
#   is.discrete.character
#   is.discrete.numeric
#   which.matrix.type
#
######################################################################


#Given a vector of non-colors, try to coerce them into some reasonable
#color format.  This may not work well, but what the hell....
as.color<-function(x){
  #Numeric rule: if integer leave as-is, otherwise convert to grayscale
  if(is.numeric(x)){
    if(any(x!=round(x),na.rm=TRUE)){
      return(gray((x-min(x))/(max(x)-min(x))))
    }else
      return(x)
  }
  #Factor rule: categorical colorings
  if(is.factor(x)){
    return(match(levels(x)[x],levels(x)))
  }
  #Character rule: if colors, retain as colors; else categorical
  if(is.character(x)){
    if(all(is.color(x)))
      return(x)
    else{
      return(match(x,sort(unique(x))))
    }
  }
}


# Return the density of the given network.  (This probably won't stay in
# this package....
#
network.density<-function(x,na.omit=TRUE,discount.bipartite=FALSE){
  if(is.multiplex(x))
    warning("Network is multiplex - no general way to define density.  Returning value for a non-multiplex network (hope that's what you wanted).\n")
  ec<-network.edgecount(x,na.omit=na.omit)
  n<-network.size(x)
  bip<-x%n%"bipartite"
  if(is.hyper(x)){
    if((bip>0)&&(discount.bipartite)){
      pe<-choose(bip,1:bip)*choose(n-bip,1:(n-bip))*(1+is.directed(x))
    }else{
      if(has.loops(x))
        pe<-sum(choose(n,1:n))^(1+is.directed(x))
      else
        pe<-sum(choose(n,1:n))/(1+!is.directed(x))
    }
  }else{
    if((bip>0)&&(discount.bipartite)){
      pe<-bip*(n-bip)*(1+is.directed(x))
    }else{
      pe<-n*(n-1)/(1+!is.directed(x))+(has.loops(x)*network.size(x))
    }
  }  
  ec/pe
}


#Returns TRUE if x is a character in a known color format
is.color<-function(x){
  xic<-rep(FALSE,length(x))         #Assume not a color by default
  xic[is.na(x)]<-NA                 #Missing counts as missing
  xc<-sapply(x,is.character)        #Must be a character string
  #For characters, must be a named color or a #RRGGBB/#RRGGBBAA sequence
  xic[xc]<-(x[xc]%in%colors())| ((nchar(x[xc])%in%c(7,9))&(substr(x[xc],1,1)=="#"))
  #Return the result
  xic
}


is.discrete.numeric<-function(x){
 (is.numeric(x)|is.logical(x)) && mean(duplicated(x)) > 0.8
}


is.discrete.character<-function(x){
 (is.character(x)|is.logical(x)) && mean(duplicated(x)) > 0.8
}


is.discrete<-function(x){
 (is.numeric(x)|is.logical(x)|is.character(x)) && mean(duplicated(x)) > 0.8
}


which.matrix.type<-function(x)
{
    if (!is.network(x)) {
        if (is.character(x<-as.matrix(x))){ 
          if (diff(dim(x))==0)
            out<-"adjacency"
          else if (dim(x)[2]==2)
            out<-"edgelist"
          else
            out<-"bipartite"
        }else if (!is.numeric(x))  
            out<-NA
        else if (diff(dim(x))==0)  
            out<-"adjacency"
        else if (max(abs(x))==1 && max(abs(x-as.integer(x)))==0)
            out<-"bipartite"
        else if (max(abs(x-as.integer(x))[,1:2])==0 && min(x[,1:2])>0)
            out<-"edgelist"
        else
            out<-NA
    }
    else {  # Very ad-hoc criteria for choosing; choice can be overridden.
	if (is.hyper(x))
            out<-"incidence"
        else if ((n<-x$gal$n)<14 || x$gal$mnext>n*n/2)
            out<-"adjacency"
        else
            out<-"edgelist"
    }
    out
}
