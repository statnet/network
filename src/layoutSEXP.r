######################################################################
#
# layoutSEXP.R
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

#Fruchterman-Reingold layout routine for plot.network
network.layout.fruchtermanreingold<-function(nw,layout.par){
  #Provide default settings
  n<-network.size(nw)
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
  #d<-d|t(d)  
  #Perform the layout calculation
  param <- c(max.delta,area,cool.exp,repulse.rad)
  loc <- c(x,y)
  layout <- .Call("network_layout_fg_R",nw,niter,param,loc, PACKAGE="network")
  #Return the result
  matrix(layout, ncol=2)
}
