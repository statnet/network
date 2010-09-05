######################################################################
#
# layout.R
#
# Written by Carter T. Butts <buttsc@uci.edu>; portions contributed by
# David Hunter <dhunter@stat.psu.edu> and Mark S. Handcock
# <handcock@u.washington.edu>.
#
# Last Modified 2/06/05
# Licensed under the GNU General Public License version 2 (June, 1991)
#
# Part of the R/network package
#
# This file contains various routines related to vertex layouts (for
# graph drawing).  These are currently ported directly from the sna
# package for R (Carter T. Butts <buttsc@uci.edu>).
#
# Contents:
#
#   network.layout.circle
#   network.layout.fruchtermanreingold
#   network.layout.kamadakawaii
#
######################################################################


#Place vertices in a circular layout (for plot.network)
network.layout.circle<-function(d,layout.par){ 
  n<-dim(d)[1]
  cbind(sin(2*pi*((0:(n-1))/n)),cos(2*pi*((0:(n-1))/n)))
}


#Fruchterman-Reingold layout routine for plot.network
network.layout.fruchtermanreingold<-function(d,layout.par){
  #Provide default settings
  n<-dim(d)[1]
  if(is.null(layout.par$niter))
    niter<-500
  else
    niter<-layout.par$niter
  if(is.null(layout.par$max.delta))
    max.delta<-n
  else
    max.delta<-layout.par$max.delta
  if(is.null(layout.par$area))
    area<-n^2
  else
    area<-layout.par$area
  if(is.null(layout.par$cool.exp))
    cool.exp<-3
  else
    cool.exp<-layout.par$cool.exp
  if(is.null(layout.par$repulse.rad))
    repulse.rad<-area*n
  else
    repulse.rad<-layout.par$repulse.rad
  if(is.null(layout.par$seed.coord)){
    tempa<-sample((0:(n-1))/n) #Set initial positions randomly on the circle
    x<-n/(2*pi)*sin(2*pi*tempa)
    y<-n/(2*pi)*cos(2*pi*tempa)
  }else{
    x<-layout.par$seed.coord[,1]
    y<-layout.par$seed.coord[,2]
  }
  #Symmetrize the network, just in case
  d<-d|t(d)  
  #Perform the layout calculation
  layout<-.C("network_layout_fruchtermanreingold_R", as.integer(d), as.double(n), as.integer(niter), as.double(max.delta), as.double(area), as.double(cool.exp), as.double(repulse.rad), x=as.double(x), y=as.double(y), PACKAGE="network")
  #Return the result
  cbind(layout$x,layout$y)
}


#Kamada-Kawai layout function for plot.network
network.layout.kamadakawai<-function(d,layout.par){
  n<-NROW(d)
  if(is.null(layout.par$niter)){
    niter<-1000
  }else
    niter<-layout.par$niter
  if(is.null(layout.par$sigma)){
    sigma<-n/4
  }else
    sigma<-layout.par$sigma
  if(is.null(layout.par$initemp)){
    initemp<-10
  }else
    initemp<-layout.par$initemp
  if(is.null(layout.par$coolexp)){
    coolexp<-0.99
  }else
    coolexp<-layout.par$coolexp
  if(is.null(layout.par$kkconst)){
    kkconst<-n^2
  }else
    kkconst<-layout.par$kkconst
  if(is.null(layout.par$elen)){
    elen<-geodist(symmetrize(d),inf.replace=sqrt(n))$gdist
  }else
    elen<-layout.par$elen
  if(is.null(layout.par$seed.coord)){
    x<-rnorm(n,0,n/4)
    y<-rnorm(n,0,n/4)
  }else{
    x<-layout.par$seed.coord[,1]
    y<-layout.par$seed.coord[,2]
  }
  #Obtain locations
  pos<-.C("network_layout_kamadakawai_R",as.integer(d),as.double(n), as.integer(niter),as.double(elen),as.double(initemp),as.double(coolexp), as.double(kkconst),as.double(sigma),x=as.double(x),y=as.double(y), PACKAGE="network")
  #Return to x,y coords
  cbind(pos$x,pos$y)
}

