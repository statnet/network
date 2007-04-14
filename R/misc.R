######################################################################
#
# misc.R
#
# Written by Carter T. Butts <buttsc@uci.edu>; portions contributed by
# David Hunter <dhunter@stat.psu.edu> and Mark S. Handcock
# <handcock@u.washington.edu>.
#
# Last Modified 4/10/06
# Licensed under the GNU General Public License version 2 (June, 1991)
#
# Part of the R/network package
#
# This file contains various network routines which don't fit anywhere
# else (generally, utilities and the like).
#
# Contents:
#
#   network.density
#   is.discrete
#   is.discrete.character
#   is.discrete.numeric
#   which.matrix.type
#
######################################################################


# Return the density of the given network.  (This probably won't stay in
# this package....
#
network.density<-function(x){
  ec<-network.edgecount(x,na.omit=TRUE)
  n<-network.size(x)
  if(is.hyper(x)){
    pe<-sum(choose(n,1:n))^(1+is.directed(x))
  }else{
    pe<-n*(n-1)/(1+!is.directed(x))+(has.loops(x)*network.size(x))
  }  
  ec/pe
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
        else if (max(abs(x),na.rm=TRUE)==1 && max(abs(x-as.integer(x)),na.rm=TRUE)==0)
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
