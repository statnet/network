######################################################################
#
# layout.R
#
# Written by Carter T. Butts <buttsc@uci.edu>; portions contributed by
# David Hunter <dhunter@stat.psu.edu> and Mark S. Handcock
# <handcock@u.washington.edu>.
#
# Last Modified 06/06/21
# Licensed under the GNU General Public License version 2 (June, 1991)
# or greater
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
#' Vertex Layout Functions for plot.network
#' 
#' Various functions which generate vertex layouts for the
#' \code{\link{plot.network}} visualization routine.
#' 
#' Vertex layouts for network visualization pose a difficult problem -- there
#' is no single, ``good'' layout algorithm, and many different approaches may
#' be valuable under different circumstances.  With this in mind,
#' \code{\link{plot.network}} allows for the use of arbitrary vertex layout
#' algorithms via the \code{network.layout.*} family of routines.  When called,
#' \code{\link{plot.network}} searches for a \code{network.layout} function
#' whose fourth name matches its \code{mode} argument (see
#' \code{\link{plot.network}} help for more information); this function is then
#' used to generate the layout for the resulting plot.  In addition to the
#' routines documented here, users may add their own layout functions as
#' needed.  The requirements for a \code{network.layout} function are as
#' follows:
#' \enumerate{
#' \item the first argument, \code{nw}, must be a network object;
#' \item the second argument, \code{layout.par}, must be a list of parameters
#'   (or \code{NULL}, if no parameters are specified); and
#' \item the return value must be a real matrix of dimension \code{c(2,network.size(nw))},
#'   whose rows contain the vertex coordinates.
#' }
#' Other than this, anything goes.  (In particular, note that \code{layout.par}
#' could be used to pass additional matrices or other information, if needed.
#' Alternately, it is possible to make layout methods that respond to
#' covariates on the network object, which are maintained intact by
#' plot.network.)
#' 
#' The \code{network.layout} functions currently supplied by default are as
#' follows (with \code{n==network.size(nw)}):
#' \describe{ 
#' \item{circle}{ This function places vertices uniformly in a circle; it takes no arguments.}
#' \item{fruchtermanreingold}{ This function generates a layout using a variant of Fruchterman and Reingold's force-directed placement algorithm.  It takes the following arguments:
#' \describe{
#' \item{layout.par$niter}{ This argument controls the number of iterations to be employed.  Larger values take longer, but will provide a more refined layout.  (Defaults to 500.) }
#' \item{layout.par$max.delta}{ Sets the maximum change in position for any given iteration.  (Defaults to \code{n}.)}
#' \item{layout.par$area}{ Sets the "area" parameter for the F-R algorithm. (Defaults to \code{n^2}.)}
#' \item{layout.par$cool.exp}{ Sets the cooling exponent for the annealer. (Defaults to 3.)}
#' \item{layout.par$repulse.rad}{ Determines the radius at which vertex-vertex repulsion cancels out attraction of adjacent vertices. (Defaults to \code{area*log(n)}.)} 
#' \item{layout.par$ncell}{ To speed calculations on large graphs, the plot region is divided at each iteration into \code{ncell} by \code{ncell} \dQuote{cells}, which are used to define neighborhoods for force calculation.  Moderate numbers of cells result in fastest performance; too few cells (down to 1, which produces \dQuote{pure} F-R results) can yield odd layouts, while too many will result in long layout times.  (Defaults to \code{n^0.4}.)} 
#' \item{layout.par$cell.jitter}{ Jitter factor (in units of cell width) used in assigning vertices to cells. Small values may generate \dQuote{grid-like} anomalies for graphs with many isolates.  (Defaults to \code{0.5}.)}
#' \item{layout.par$cell.pointpointrad}{ Squared \dQuote{radius} (in units of cells) such that exact point interaction calculations are used for all vertices belonging to any two cells less than or equal to this distance apart.  Higher values approximate the true F-R solution, but increase computational cost.  (Defaults to \code{0}.)} 
#' \item{layout.par$cell.pointcellrad}{ Squared \dQuote{radius} (in units of cells) such that approximate point/cell interaction calculations are used for all vertices belonging to any two cells less than or equal to this distance apart (and not within the point/point radius). Higher values provide somewhat better approximations to the true F-R solution at slightly increased computational cost.  (Defaults to \code{18}.)}
#' \item{layout.par$cell.cellcellrad}{ Squared \dQuote{radius} (in units of cells) such that approximate cell/cell interaction calculations are used for all vertices belonging to any two cells less than or equal to this distance apart (and not within the point/point or point/cell radii). Higher values provide somewhat better approximations to the true F-R solution at slightly increased computational cost.  Note that cells beyond this radius (if any) do not interact, save through edge attraction. (Defaults to \code{ncell^2}.)}
#' \item{layout.par$seed.coord}{ A two-column matrix of initial vertex coordinates.  (Defaults to a random circular layout.) }
#' }
#' }
#' \item{kamadakawai}{ This function generates a vertex layout using a version of the Kamada-Kawai force-directed placement algorithm.  It takes the following arguments:
#' \describe{
#' \item{layout.par$niter}{ This argument controls the number of iterations to be employed.  (Defaults to 1000.) }
#' \item{layout.par$sigma}{ Sets the base standard deviation of position change proposals.  (Defaults to \code{n/4}.)}
#' \item{layout.par$initemp}{ Sets the initial "temperature" for the annealing algorithm. (Defaults to 10.)}
#' \item{layout.par$cool.exp}{ Sets the cooling exponent for the annealer. (Defaults to 0.99.)} 
#' \item{layout.par$kkconst}{ Sets the Kamada-Kawai vertex attraction constant.  (Defaults to \code{n)^2}.)}
#' \item{layout.par$elen}{ Provides the matrix of interpoint distances to be approximated.  (Defaults to the geodesic distances of \code{nw} after symmetrizing, capped at \code{sqrt(n)}.)}
#' \item{layout.par$seed.coord}{ A two-column matrix of initial vertex coordinates.  (Defaults to a gaussian layout.) } 
#' } 
#' } 
#' }
#'  
#' @name network.layout
#'
#' @param nw a network object, as passed by \code{\link{plot.network}}.
#' @param layout.par a list of parameters.
#' @return A matrix whose rows contain the x,y coordinates of the vertices of
#' \code{d}.
#' @note The \code{network.layout} routines shown here are adapted directly
#' from the \code{\link[sna]{gplot.layout}} routines of the \code{sna} package.
#' @author Carter T. Butts \email{buttsc@@uci.edu}
#' @seealso \code{\link{plot.network}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \doi{10.18637/jss.v024.i02}
#' 
#' Fruchterman, T.M.J. and Reingold, E.M.  (1991).  \dQuote{Graph Drawing by
#' Force-directed Placement.} \emph{Software - Practice and Experience,}
#' 21(11):1129-1164.
#' 
#' Kamada, T. and Kawai, S.  (1989). \dQuote{An Algorithm for Drawing General
#' Undirected Graphs.} \emph{Information Processing Letters,} 31(1):7-15.
#' @keywords graphs dplot
#' @export
network.layout.circle<-function(nw,layout.par){ 
  n<-network.size(nw)
  cbind(sin(2*pi*((0:(n-1))/n)),cos(2*pi*((0:(n-1))/n)))
}


#Fruchterman-Reingold layout routine for plot.network
#' @rdname network.layout
#' @export
network.layout.fruchtermanreingold<-function(nw,layout.par){
  #Provide default settings
  n<-network.size(nw)
  d<-as.matrix.network(nw,matrix.type="edgelist")[,1:2,drop=FALSE]
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
    repulse.rad<-area*log(n)
  else
    repulse.rad<-layout.par$repulse.rad
  if(is.null(layout.par$ncell))
    ncell<-ceiling(n^0.4)
  else
    ncell<-layout.par$ncell
  if(is.null(layout.par$cell.jitter))
    cell.jitter<-0.5
  else
    cell.jitter<-layout.par$cell.jitter
  if(is.null(layout.par$cell.pointpointrad))
    cell.pointpointrad<-0
  else
    cell.pointpointrad<-layout.par$cell.pointpointrad
  if(is.null(layout.par$cell.pointcellrad))
    cell.pointcellrad<-18
  else
    cell.pointcellrad<-layout.par$cell.pointcellrad
  if(is.null(layout.par$cellcellcellrad))
    cell.cellcellrad<-ncell^2
  else
    cell.cellcellrad<-layout.par$cell.cellcellrad
  if(is.null(layout.par$seed.coord)){
    tempa<-sample((0:(n-1))/n) #Set initial positions randomly on the circle
    x<-n/(2*pi)*sin(2*pi*tempa)
    y<-n/(2*pi)*cos(2*pi*tempa)
  }else{
    x<-layout.par$seed.coord[,1]
    y<-layout.par$seed.coord[,2]
  }
  #Symmetrize the network, just in case
  d<-unique(rbind(d,d[,2:1]))  
  #Perform the layout calculation
  layout<-.C("network_layout_fruchtermanreingold_R", as.double(d), as.double(n), as.double(NROW(d)), as.integer(niter), as.double(max.delta), as.double(area), as.double(cool.exp), as.double(repulse.rad), as.integer(ncell), as.double(cell.jitter), as.double(cell.pointpointrad), as.double(cell.pointcellrad), as.double(cell.cellcellrad), x=as.double(x), y=as.double(y), PACKAGE="network")
  #Return the result
  cbind(layout$x,layout$y)
}


#Kamada-Kawai layout function for plot.network
#' @rdname network.layout
#' @export
network.layout.kamadakawai<-function(nw,layout.par){
  n<-network.size(nw)
  d<-as.sociomatrix(nw)
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
    # these functions require that the SNA package be installed
    elen<-sna::geodist(sna::symmetrize(d),inf.replace=sqrt(n),count.paths = FALSE,predecessors = FALSE)$gdist
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

