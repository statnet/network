######################################################################
#
# plot.R
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
# This file contains various routines related to network visualization.
#
# Contents:
#
#   network.arrow
#   network.loop
#   network.vertex
#   plot.network
#   plot.network.default
#
######################################################################


#Introduce a function to make coordinates for a single polygon
make.arrow.poly.coords<-function(x0,y0,x1,y1,ahangle,ahlen,swid,toff,hoff,ahead, curve,csteps){ 
  slen<-sqrt((x0-x1)^2+(y0-y1)^2)  #Find the total length
  if(curve==0){         #Straight edges
    if(ahead){    
      coord<-rbind(                    #Produce a "generic" version w/head
        c(-swid/2,toff),
        c(-swid/2,slen-0.5*ahlen-hoff),
        c(-ahlen*sin(ahangle),slen-ahlen*cos(ahangle)-hoff),
        c(0,slen-hoff),
        c(ahlen*sin(ahangle),slen-ahlen*cos(ahangle)-hoff),
        c(swid/2,slen-0.5*ahlen-hoff),
        c(swid/2,toff),
        c(NA,NA)
      )
    }else{
      coord<-rbind(                    #Produce a "generic" version w/out head
        c(-swid/2,toff),
        c(-swid/2,slen-hoff),
        c(swid/2,slen-hoff),
        c(swid/2,toff),
        c(NA,NA)
      )
    }
  }else{             #Curved edges
    if(ahead){    
      inc<-(0:csteps)/csteps
      coord<-rbind(
        cbind(-curve*(1-(2*(inc-0.5))^2)-swid/2-sqrt(2)/2*(toff+inc*(hoff-toff)), inc*(slen-sqrt(2)/2*(hoff+toff)-ahlen*0.5)+sqrt(2)/2*toff),
        c(ahlen*sin(-ahangle-pi/16)-sqrt(2)/2*hoff, slen-ahlen*cos(-ahangle-pi/16)-sqrt(2)/2*hoff),
        c(-sqrt(2)/2*hoff,slen-sqrt(2)/2*hoff),
        c(ahlen*sin(ahangle-pi/16)-sqrt(2)/2*hoff, slen-ahlen*cos(ahangle-pi/16)-sqrt(2)/2*hoff),
        cbind(-curve*(1-(2*(rev(inc)-0.5))^2)+swid/2-sqrt(2)/2*(toff+rev(inc)*(hoff-toff)), rev(inc)*(slen-sqrt(2)/2*(hoff+toff)-ahlen*0.5)+sqrt(2)/2*toff),
        c(NA,NA)
      )
    }else{
      inc<-(0:csteps)/csteps
      coord<-rbind(
        cbind(-curve*(1-(2*(inc-0.5))^2)-swid/2-sqrt(2)/2*(toff+inc*(hoff-toff)), inc*(slen-sqrt(2)/2*(hoff+toff))+sqrt(2)/2*toff),
        cbind(-curve*(1-(2*(rev(inc)-0.5))^2)+swid/2-sqrt(2)/2*(toff+rev(inc)*(hoff-toff)), rev(inc)*(slen-sqrt(2)/2*(hoff+toff))+sqrt(2)/2*toff),
        c(NA,NA)
      )
    }
  }
  theta<-atan2(y1-y0,x1-x0)-pi/2     #Rotate about origin
  rmat<-rbind(c(cos(theta),sin(theta)),c(-sin(theta),cos(theta)))
  coord<-coord%*%rmat
  coord[,1]<-coord[,1]+x0            #Translate to (x0,y0)
  coord[,2]<-coord[,2]+y0
  coord
}

#Custom arrow-drawing method for plot.network


#' Add Arrows or Segments to a Plot
#' 
#' \code{network.arrow} draws a segment or arrow between two pairs of points;
#' unlike \code{\link{arrows}} or \code{\link{segments}}, the new plot element
#' is drawn as a polygon.
#' 
#' \code{network.arrow} provides a useful extension of \code{\link{segments}}
#' and \code{\link{arrows}} when fine control is needed over the resulting
#' display.  (The results also look better.)  Note that edge curvature is
#' quadratic, with \code{curve} providing the maximum horizontal deviation of
#' the edge (left-handed).  Head/tail offsets are used to adjust the end/start
#' points of an edge, relative to the baseline coordinates; these are useful
#' for functions like \code{\link{plot.network}}, which need to draw edges
#' incident to vertices of varying radii.
#' 
#' @param x0 A vector of x coordinates for points of origin
#' @param y0 A vector of y coordinates for points of origin
#' @param x1 A vector of x coordinates for destination points
#' @param y1 A vector of y coordinates for destination points
#' @param length Arrowhead length, in current plotting units
#' @param angle Arrowhead angle (in degrees)
#' @param width Width for arrow body, in current plotting units (can be a
#' vector)
#' @param col Arrow body color (can be a vector)
#' @param border Arrow border color (can be a vector)
#' @param lty Arrow border line type (can be a vector)
#' @param offset.head Offset for destination point (can be a vector)
#' @param offset.tail Offset for origin point (can be a vector)
#' @param arrowhead Boolean; should arrowheads be used?  (Can be a vector))
#' @param curve Degree of edge curvature (if any), in current plotting units
#' (can be a vector)
#' @param edge.steps For curved edges, the number of steps to use in
#' approximating the curve (can be a vector)
#' @param \dots Additional arguments to \code{\link{polygon}}
#' @return None.
#' @note \code{network.arrow} is a direct adaptation of
#' \code{\link[sna]{gplot.arrow}} from the \code{sna} package.
#' @author Carter T. Butts \email{buttsc@@uci.edu}
#' @seealso \code{\link{plot.network}}, \code{\link{network.loop}},
#' \code{\link{polygon}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{https://www.jstatsoft.org/v24/i02/}
#' @keywords aplot graphs
#' @examples
#' 
#'   #Plot two points
#'   plot(1:2,1:2)
#'   
#'   #Add an edge
#'   network.arrow(1,1,2,2,width=0.01,col="red",border="black")
#' 
#' @export network.arrow
network.arrow<-function(x0,y0,x1,y1,length=0.1,angle=20,width=0.01,col=1,border=1,lty=1,offset.head=0,offset.tail=0,arrowhead=TRUE,curve=0,edge.steps=50,...){
  if(length(x0)==0)   #Leave if there's nothing to do
    return()

  #"Stretch" the arguments
  n<-length(x0)
  angle<-rep(angle,length.out=n)/360*2*pi
  length<-rep(length,length.out=n)
  width<-rep(width,length.out=n)
  col<-rep(col,length.out=n)
  border<-rep(border,length.out=n)
  lty<-rep(lty,length.out=n)
  arrowhead<-rep(arrowhead,length.out=n)
  offset.head<-rep(offset.head,length.out=n)
  offset.tail<-rep(offset.tail,length.out=n)
  curve<-rep(curve,length.out=n)
  edge.steps<-rep(edge.steps,length.out=n)
  #Obtain coordinates
  coord<-vector()
  for(i in 1:n)  
    coord<-rbind(coord,make.arrow.poly.coords(x0[i],y0[i],x1[i],y1[i],angle[i],length[i], width[i],offset.tail[i],offset.head[i],arrowhead[i],curve[i],edge.steps[i]))
  coord<-coord[-NROW(coord),]
  #Draw polygons.  
  # the coord matrix has some NA rows, which will break it into multiple polygons
  polygon(coord,col=col,border=border,lty=lty,...)
}

#Introduce a function to make coordinates for a single polygon
make.loop.poly.coords<-function(x0,y0,xctr,yctr,ahangle,ahlen,swid,off,rad,ahead,edge.steps){
  #Determine the center of the plot
  xoff <- x0-xctr
  yoff <- y0-yctr
  roff <- sqrt(xoff^2+yoff^2)
  x0hat <- xoff/roff
  y0hat <- yoff/roff
  r0.vertex <- off
  r0.loop <- rad
  x0.loop <- x0hat*r0.loop
  y0.loop <- y0hat*r0.loop
  ang <- (((0:edge.steps)/edge.steps)*(1-(2*r0.vertex+0.5*ahlen*ahead)/ (2*pi*r0.loop))+r0.vertex/(2*pi*r0.loop))*2*pi+atan2(-yoff,-xoff)
  ang2 <- ((1-(2*r0.vertex)/(2*pi*r0.loop))+r0.vertex/(2*pi*r0.loop))*2*pi+ atan2(-yoff,-xoff)
  if(ahead){
    x0.arrow <- x0.loop+(r0.loop+swid/2)*cos(ang2)
    y0.arrow <- y0.loop+(r0.loop+swid/2)*sin(ang2)
    coord<-rbind(
      cbind(x0.loop+(r0.loop+swid/2)*cos(ang), 
            y0.loop+(r0.loop+swid/2)*sin(ang)),
      cbind(x0.arrow+ahlen*cos(ang2-pi/2),
            y0.arrow+ahlen*sin(ang2-pi/2)),
      cbind(x0.arrow,y0.arrow),
      cbind(x0.arrow+ahlen*cos(-2*ahangle+ang2-pi/2),
            y0.arrow+ahlen*sin(-2*ahangle+ang2-pi/2)),
      cbind(x0.loop+(r0.loop-swid/2)*cos(rev(ang)),
            y0.loop+(r0.loop-swid/2)*sin(rev(ang))),
      c(NA,NA)
    )
  }else{
    coord<-rbind(
      cbind(x0.loop+(r0.loop+swid/2)*cos(ang),
            y0.loop+(r0.loop+swid/2)*sin(ang)),
      cbind(x0.loop+(r0.loop-swid/2)*cos(rev(ang)),
            y0.loop+(r0.loop-swid/2)*sin(rev(ang))),
      c(NA,NA)
    )
  }
  coord[,1]<-coord[,1]+x0            #Translate to (x0,y0)
  coord[,2]<-coord[,2]+y0
  coord
}

#Custom loop-drawing method for plot.network


#' Add Loops to a Plot
#' 
#' \code{network.loop} draws a "loop" at a specified location; this is used to
#' designate self-ties in \code{\link{plot.network}}.
#' 
#' \code{network.loop} is the companion to \code{\link{network.arrow}}; like
#' the latter, plot elements produced by \code{network.loop} are drawn using
#' \code{\link{polygon}}, and as such are scaled based on the current plotting
#' device.  By default, loops are drawn so as to encompass a circular region of
#' radius \code{radius}, whose center is \code{offset} units from \code{x0,y0}
#' and at maximum distance from \code{xctr,yctr}.  This is useful for functions
#' like \code{\link{plot.network}}, which need to draw loops incident to
#' vertices of varying radii.
#' 
#' @param x0 a vector of x coordinates for points of origin.
#' @param y0 a vector of y coordinates for points of origin.
#' @param length arrowhead length, in current plotting units.
#' @param angle arrowhead angle (in degrees).
#' @param width width for loop body, in current plotting units (can be a
#' vector).
#' @param col loop body color (can be a vector).
#' @param border loop border color (can be a vector).
#' @param lty loop border line type (can be a vector).
#' @param offset offset for origin point (can be a vector).
#' @param edge.steps number of steps to use in approximating curves.
#' @param radius loop radius (can be a vector).
#' @param arrowhead boolean; should arrowheads be used?  (Can be a vector.)
#' @param xctr x coordinate for the central location away from which loops
#' should be oriented.
#' @param yctr y coordinate for the central location away from which loops
#' should be oriented.
#' @param \dots additional arguments to \code{\link{polygon}}.
#' @return None.
#' @note \code{network.loop} is a direct adaptation of
#' \code{\link[sna]{gplot.loop}}, from the \code{sna} package.
#' @author Carter T. Butts \email{buttsc@@uci.edu}
#' @seealso \code{\link{network.arrow}}, \code{\link{plot.network}},
#' \code{\link{polygon}}
#' @keywords aplot graphs
#' @examples
#' 
#' #Plot a few polygons with loops
#' plot(0,0,type="n",xlim=c(-2,2),ylim=c(-2,2),asp=1)
#' network.loop(c(0,0),c(1,-1),col=c(3,2),width=0.05,length=0.4,
#'   offset=sqrt(2)/4,angle=20,radius=0.5,edge.steps=50,arrowhead=TRUE)
#' polygon(c(0.25,-0.25,-0.25,0.25,NA,0.25,-0.25,-0.25,0.25), 
#'     c(1.25,1.25,0.75,0.75,NA,-1.25,-1.25,-0.75,-0.75),col=c(2,3))
#' 
#' 
#' @export network.loop
network.loop<-function(x0,y0,length=0.1,angle=10,width=0.01,col=1,border=1,lty=1,offset=0,edge.steps=10,radius=1,arrowhead=TRUE,xctr=0,yctr=0,...){
  if(length(x0)==0)   #Leave if there's nothing to do
    return()

  #"Stretch" the arguments
  n<-length(x0)
  angle<-rep(angle,length.out=n)/360*2*pi
  length<-rep(length,length.out=n)
  width<-rep(width,length.out=n)
  col<-rep(col,length.out=n)
  border<-rep(border,length.out=n)
  lty<-rep(lty,length.out=n)
  rad<-rep(radius,length.out=n)
  arrowhead<-rep(arrowhead,length.out=n)
  offset<-rep(offset,length.out=n)
  #Obtain coordinates
  coord<-vector()
  for(i in 1:n)  
    coord<-rbind(coord,make.loop.poly.coords(x0[i],y0[i],xctr,yctr,angle[i],length[i], width[i],offset[i],rad[i],arrowhead[i],edge.steps))
  coord<-coord[-NROW(coord),]
  #Draw polygons
  polygon(coord,col=col,border=border,lty=lty,...)
}

#Introduce a function to make coordinates for a single vertex polygon
# this version just uses the raw radius, so triangles appear half the size of circles
old.make.vertex.poly.coords<-function(x,y,r,s,rot){
  ang<-(1:s)/s*2*pi+rot*2*pi/360
  rbind(cbind(x+r*cos(ang),y+r*sin(ang)),c(NA,NA))  
}

#Introduce a function to make coordinates for a single vertex polygon
# all polygons produced will have equal area
make.vertex.poly.coords<-function(x,y,r,s,rot){
  # trap some edge cases
  if(is.na(s) || s<2){
    return(rbind(c(x,y),c(NA,NA))) # return a single point
  } else {
    #scale r (circumradius) to make area equal
    area<-pi*r^2  # target area based desired r as radius of circle
    # solve for new r as polygon radius that would match the area of the circle
    r<-sqrt(2*area / (s*sin(2*pi/s)))
    ang<-(1:s)/s*2*pi+rot*2*pi/360
    return(rbind(cbind(x+r*cos(ang),y+r*sin(ang)),c(NA,NA)))
  }
}

#Routine to plot vertices, using polygons


#' Add Vertices to a Plot
#' 
#' \code{network.vertex} adds one or more vertices (drawn using
#' \code{\link{polygon}}) to a plot.
#' 
#' \code{network.vertex} draws regular polygons of specified radius and number
#' of sides, at the given coordinates.  This is useful for routines such as
#' \code{\link{plot.network}}, which use such shapes to depict vertices.
#' 
#' @param x a vector of x coordinates.
#' @param y a vector of y coordinates.
#' @param radius a vector of vertex radii.
#' @param sides a vector containing the number of sides to draw for each
#' vertex.
#' @param border a vector of vertex border colors.
#' @param col a vector of vertex interior colors.
#' @param lty a vector of vertex border line types.
#' @param rot a vector of vertex rotation angles (in degrees).
#' @param lwd a vector of vertex border line widths.
#' @param \dots Additional arguments to \code{\link{polygon}}
#' @return None
#' @note \code{network.vertex} is a direct adaptation of
#' \code{\link[sna]{gplot.vertex}} from the \code{sna} package.
#' @author Carter T. Butts \email{buttsc@@uci.edu}
#' @seealso \code{\link{plot.network}}, \code{\link{polygon}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{https://www.jstatsoft.org/v24/i02/}
#' @keywords aplot graphs
#' @examples
#' 
#' 
#' #Open a plot window, and place some vertices
#' plot(0,0,type="n",xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),asp=1)
#' network.vertex(cos((1:10)/10*2*pi),sin((1:10)/10*2*pi),col=1:10,
#'     sides=3:12,radius=0.1)
#' 
#' 
#' @export network.vertex
network.vertex<-function(x,y,radius=1,sides=4,border=1,col=2,lty=NULL,rot=0,lwd=1,...){
  
  #Prep the vars
  n<-length(x)
  radius<-rep(radius,length.out=n)
  sides<-rep(sides,length.out=n)
  border<-rep(border,length.out=n)
  col<-rep(col,length.out=n)
  lty<-rep(lty,length.out=n)
  rot<-rep(rot,length.out=n)
  lwd<-rep(lwd,length.out=n)
  #Obtain the coordinates
  coord<-vector()
  for(i in 1:length(x)) {
    coord<-make.vertex.poly.coords(x[i],y[i],radius[i],sides[i],rot[i])
    polygon(coord,border=border[i],col=col[i],lty=lty[i],lwd=lwd[i], ...)
  }
  #Plot the polygons
  
}

# draw a label for a network edge


#' Plots a label corresponding to an edge in a network plot.
#' 
#' Draws a text labels on (or adjacent to) the line segments connecting
#' vertices on a network plot.
#' 
#' Called internally by \code{\link{plot.network}} when \code{edge.label}
#' parameter is used. For directed, non-curved edges, the labels are shifted
#' towards the tail of the edge. Labels for curved edges are not shifted
#' because opposite-direction edges curve the opposite way.  Makes a crude
#' attempt to shift labels to either side of line, and to draw the edge labels
#' for self-loops near the vertex. No attempt is made to avoid overlap between
#' vertex and edge labels.
#' 
#' @param px0 vector of x coordinates of tail vertex of the edge
#' @param py0 vector of y coordinates of tail vertex of the edge
#' @param px1 vector of x coordinates of head vertex of the edge
#' @param py1 vector of y coordinate of head vertex of the edge
#' @param label vector strings giving labels to be drawn for edge edge
#' @param directed logical: is the underlying network directed? If FALSE,
#' labels will be drawn in the middle of the line segment, otherwise in the
#' first 3rd so that the labels for edges pointing in the opposite direction
#' will not overlap.
#' @param loops logical: if true, assuming the labels to be drawn belong to
#' loop-type edges and render appropriately
#' @param cex numeric vector giving the text expansion factor for each label
#' @param curve numeric vector controling the extent of edge curvature (0 =
#' straight line edges)
#' @param \dots additional arguments to be passed to \code{\link{text}}
#' @return no value is returned but text will be rendered on the active plot
#' @author skyebend
#' @export network.edgelabel
network.edgelabel<-function(px0,py0,px1,py1,label,directed,loops=FALSE,cex,curve=0,...){
  curve<-rep(curve,length(label))
  posl<-rep(0,length(label))
  offsets<-rep(0.1,length(label))
    if (loops){  # loops version 
      # assume coordinates are the first pair
      # math is hard.  For now just draw label near the vertex
      lpx<-px0
      lpy<-py0
      # compute crude offset so that label doesn't land on vertex
      # todo, this doesn't work well on all edge orientations
      posl<-rep(0,length(label))
      posl[(px0>px1) & (py0>py1)]<-4
      posl[(px0<=px1) & (py0<=py1)]<-2
      posl[(px0>px1) & (py0<=py1)]<-1
      posl[(px0<=px1) & (py0>py1)]<-3
      offsets<-rep(0.5,length(label))
      
    } else {  # either curved or straight line
      if (all(curve==0)){  # straight line non-curved version
        if (directed){
          # draw labels off center of line so won't overlap
          lpx<-px0+((px1-px0)/3)
          lpy<-py0+((py1-py0)/3)
        } else {
          # draw labels on center of line
          lpx<-px0+((px1-px0)/2)
          lpy<-py0+((py1-py0)/2)
          # assumes that line is straight
        }
        
    } else { # curved edge case
      coords<-sapply(seq_len(length(label)),function(p){
        make.arrow.poly.coords(px0[p],py0[p],px1[p],py1[p],ahangle = 0,ahlen=0,swid = 0,toff = 0,hoff=0,ahead = 0,curve=curve[p],csteps=2)[2,] # pick a point returned from the middle of the curve
      })
      lpx<-coords[1,]
      lpy<-coords[2,]
      # this should 
    }
    # compute crude offset so that label doesn't land on line
    # todo, this doesn't work well on all edge orientations
    posl[(px0>px1) & (py0>py1)]<-1
    posl[(px0<=px1) & (py0<=py1)]<-3
    posl[(px0>px1) & (py0<=py1)]<-2
    posl[(px0<=px1) & (py0>py1)]<-4
    
  }
   # debug coord location
    text(lpx,lpy,labels=label,cex=cex,pos=posl,offset=offsets,...)
}


#Generic plot.network method. 

#' Two-Dimensional Visualization for Network Objects
#' 
#' \code{plot.network} produces a simple two-dimensional plot of network
#' \code{x}, using optional attribute \code{attrname} to set edge values.  A
#' variety of options are available to control vertex placement, display
#' details, color, etc.
#' 
#' \code{plot.network} is the standard visualization tool for the
#' \code{network} class.  By means of clever selection of display parameters, a
#' fair amount of display flexibility can be obtained.  Vertex layout -- if not
#' specified directly using \code{coord} -- is determined via one of the
#' various available algorithms.  These should be specified via the \code{mode}
#' argument; see \code{\link{network.layout}} for a full list.  User-supplied
#' layout functions are also possible -- see the aforementioned man page for
#' details.
#' 
#' Note that where \code{is.hyper(x)==TRUE}, the network is converted to
#' bipartite adjacency form prior to computing coordinates.  If
#' \code{interactive==TRUE}, then the user may modify the initial network
#' layout by selecting an individual vertex and then clicking on the location
#' to which this vertex is to be moved; this process may be repeated until the
#' layout is satisfactory.
#' 
#' @rdname plot.network
#' @name plot.network.default
#'
#' @param x an object of class \code{network}.
#' @param attrname an optional edge attribute, to be used to set edge values.
#' @param label a vector of vertex labels, if desired; defaults to the vertex
#' labels returned by \code{\link{network.vertex.names}}. If \code{label} has
#' one element and it matches with a vertex attribute name, the value of the
#' attribute will be used. Note that labels may be set but hidden by the
#' \code{displaylabels} argument.
#' @param coord user-specified vertex coordinates, in an network.size(x)x2
#' matrix.  Where this is specified, it will override the \code{mode} setting.
#' @param jitter boolean; should the output be jittered?
#' @param thresh real number indicating the lower threshold for tie values.
#' Only ties of value >\code{thresh} are displayed.  By default,
#' \code{thresh}=0.
#' @param usearrows boolean; should arrows (rather than line segments) be used
#' to indicate edges?
#' @param mode the vertex placement algorithm; this must correspond to a
#' \code{\link{network.layout}} function.
#' @param displayisolates boolean; should isolates be displayed?
#' @param interactive boolean; should interactive adjustment of vertex
#' placement be attempted?
#' @param xlab x axis label.
#' @param ylab y axis label.
#' @param xlim the x limits (min, max) of the plot.
#' @param ylim the y limits of the plot.
#' @param pad amount to pad the plotting range; useful if labels are being
#' clipped.
#' @param label.pad amount to pad label boxes (if \code{boxed.labels==TRUE}),
#' in character size units.
#' @param displaylabels boolean; should vertex labels be displayed?
#' @param boxed.labels boolean; place vertex labels within boxes?
#' @param label.pos position at which labels should be placed, relative to
#' vertices.  \code{0} results in labels which are placed away from the center
#' of the plotting region; \code{1}, \code{2}, \code{3}, and \code{4} result in
#' labels being placed below, to the left of, above, and to the right of
#' vertices (respectively); and \code{label.pos>=5} results in labels which are
#' plotted with no offset (i.e., at the vertex positions).
#' @param label.bg background color for label boxes (if
#' \code{boxed.labels==TRUE}); may be a vector, if boxes are to be of different
#' colors.
#' @param vertex.sides number of polygon sides for vertices; may be given as a
#' vector or a vertex attribute name, if vertices are to be of different types.
#' As of v1.12, radius of polygons are scaled so that all shapes have equal
#' area
#' @param vertex.rot angle of rotation for vertices (in degrees); may be given
#' as a vector or a vertex attribute name, if vertices are to be rotated
#' differently.
#' @param vertex.lwd line width of vertex borders; may be given as a vector or
#' a vertex attribute name, if vertex borders are to have different line
#' widths.
#' @param arrowhead.cex expansion factor for edge arrowheads.
#' @param label.cex character expansion factor for label text.
#' @param loop.cex expansion factor for loops; may be given as a vector or a
#' vertex attribute name, if loops are to be of different sizes.
#' @param vertex.cex expansion factor for vertices; may be given as a vector or
#' a vertex attribute name, if vertices are to be of different sizes.
#' @param edge.col color for edges; may be given as a vector, adjacency matrix,
#' or edge attribute name, if edges are to be of different colors.
#' @param label.col color for vertex labels; may be given as a vector or a
#' vertex attribute name, if labels are to be of different colors.
#' @param vertex.col color for vertices; may be given as a vector or a vertex
#' attribute name, if vertices are to be of different colors.
#' @param label.border label border colors (if \code{boxed.labels==TRUE}); may
#' be given as a vector, if label boxes are to have different colors.
#' @param vertex.border border color for vertices; may be given as a vector or
#' a vertex attribute name, if vertex borders are to be of different colors.
#' @param edge.lty line type for edge borders; may be given as a vector,
#' adjacency matrix, or edge attribute name, if edge borders are to have
#' different line types.
#' @param label.lty line type for label boxes (if \code{boxed.labels==TRUE});
#' may be given as a vector, if label boxes are to have different line types.
#' @param vertex.lty line type for vertex borders; may be given as a vector or
#' a vertex attribute name, if vertex borders are to have different line types.
#' @param edge.lwd line width scale for edges; if set greater than 0, edge
#' widths are scaled by \code{edge.lwd*dat}.  May be given as a vector,
#' adjacency matrix, or edge attribute name, if edges are to have different
#' line widths.
#' @param edge.label if non-\code{NULL}, labels for edges will be drawn. May be
#' given as a vector, adjacency matrix, or edge attribute name, if edges are to
#' have different labels. A single value of \code{TRUE} will use edge ids as
#' labels. NOTE: currently doesn't work for curved edges.
#' @param edge.label.cex character expansion factor for edge label text; may be
#' given as a vector or a edge attribute name, if edge labels are to have
#' different sizes.
#' @param edge.label.col color for edge labels; may be given as a vector or a
#' edge attribute name, if labels are to be of different colors.
#' @param label.lwd line width for label boxes (if \code{boxed.labels==TRUE});
#' may be given as a vector, if label boxes are to have different line widths.
#' @param edge.len if \code{uselen==TRUE}, curved edge lengths are scaled by
#' \code{edge.len}.
#' @param edge.curve if \code{usecurve==TRUE}, the extent of edge curvature is
#' controlled by \code{edge.curv}.  May be given as a fixed value, vector,
#' adjacency matrix, or edge attribute name, if edges are to have different
#' levels of curvature.
#' @param edge.steps for curved edges (excluding loops), the number of line
#' segments to use for the curve approximation.
#' @param loop.steps for loops, the number of line segments to use for the
#' curve approximation.
#' @param object.scale base length for plotting objects, as a fraction of the
#' linear scale of the plotting region. Defaults to 0.01.
#' @param uselen boolean; should we use \code{edge.len} to rescale edge
#' lengths?
#' @param usecurve boolean; should we use \code{edge.curve}?
#' @param suppress.axes boolean; suppress plotting of axes?
#' @param vertices.last boolean; plot vertices after plotting edges?
#' @param new boolean; create a new plot?  If \code{new==FALSE}, vertices and
#' edges will be added to the existing plot.
#' @param layout.par parameters to the \code{\link{network.layout}} function
#' specified in \code{mode}.
#' @param \dots additional arguments to \code{\link{plot}}.
#' @return A two-column matrix containing the vertex positions as x,y
#' coordinates
#' @note \code{plot.network} is adapted (with minor modifications) from the
#' \code{\link[sna]{gplot}} function of the \code{sna} library (authors: Carter
#' T. Butts and Alex Montgomery); eventually, these two packages will be
#' integrated.
#' @author Carter T. Butts \email{buttsc@@uci.edu}
#' @seealso \code{\link{network}}, \code{\link{network.arrow}},
#' \code{\link{network.loop}}, \code{\link{network.vertex}}
#' @references Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
#' Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{https://www.jstatsoft.org/v24/i02/}
#' 
#' Wasserman, S., and Faust, K.  (1994).  \emph{Social Network Analysis:
#' Methods and Applications.} Cambridge: Cambridge University Press.
#' @keywords hplot graphs
#' @examples
#' 
#' #Construct a sparse graph
#' m<-matrix(rbinom(100,1,1.5/9),10)
#' diag(m)<-0
#' g<-network(m)
#' 
#' #Plot the graph
#' plot(g)
#' 
#' #Load Padgett's marriage data
#' data(flo)
#' nflo<-network(flo)
#' #Display the network, indicating degree and flagging the Medicis
#' plot(nflo, vertex.cex=apply(flo,2,sum)+1, usearrows=FALSE,
#'     vertex.sides=3+apply(flo,2,sum),
#'     vertex.col=2+(network.vertex.names(nflo)=="Medici"))
#' @export plot.network
#' @export
plot.network <- function(x, ...){
  plot.network.default(x, ...)
}


#Two-dimensional network visualization; this was originally a direct port of the gplot
#routine from sna (Carter T. Butts <buttsc@uci.edu>)


#' @rdname plot.network
#' @usage \method{plot.network}{default}(x, attrname = NULL, 
#'    label = network.vertex.names(x), coord = NULL, jitter = TRUE,
#'    thresh = 0, usearrows = TRUE, mode = "fruchtermanreingold", 
#'    displayisolates = TRUE, interactive = FALSE, xlab = NULL, 
#'    ylab = NULL, xlim = NULL, ylim = NULL, pad = 0.2, label.pad = 0.5,
#'    displaylabels = !missing(label), boxed.labels = FALSE, label.pos = 0,
#'    label.bg = "white", vertex.sides = 50, vertex.rot = 0, vertex.lwd=1,
#'    arrowhead.cex = 1, label.cex = 1, loop.cex = 1, vertex.cex = 1,
#'    edge.col = 1, label.col = 1, vertex.col = 2, label.border = 1,
#'    vertex.border = 1, edge.lty = 1, label.lty = NULL, vertex.lty = 1,
#'    edge.lwd = 0, edge.label = NULL, edge.label.cex = 1,
#'    edge.label.col = 1, label.lwd = par("lwd"), edge.len = 0.5, 
#'    edge.curve = 0.1, edge.steps = 50, loop.steps = 20, 
#'    object.scale = 0.01, uselen = FALSE, usecurve = FALSE,
#'    suppress.axes = TRUE, vertices.last = TRUE, new = TRUE, 
#'    layout.par = NULL, \dots)
#' @export plot.network.default
#' @rawNamespace S3method(plot.network,default)
plot.network.default<-function(x,
attrname=NULL,
label=network.vertex.names(x),
coord=NULL,
jitter=TRUE,
thresh=0,
usearrows=TRUE,
mode="fruchtermanreingold",
displayisolates=TRUE,
interactive=FALSE,
xlab=NULL,
ylab=NULL,
xlim=NULL,
ylim=NULL,
pad=0.2,
label.pad=0.5,
displaylabels=!missing(label),
boxed.labels=FALSE,
label.pos=0,
label.bg="white",
vertex.sides=50,
vertex.rot=0,
vertex.lwd=1,
arrowhead.cex=1,
label.cex=1,
loop.cex=1,
vertex.cex=1,
edge.col=1,
label.col=1,
vertex.col=2,
label.border=1,
vertex.border=1,
edge.lty=1,
label.lty=NULL,
vertex.lty=1,
edge.lwd=0,
edge.label=NULL,
edge.label.cex=1,
edge.label.col=1,                               
label.lwd=par("lwd"),
edge.len=0.5,
edge.curve=0.1,
edge.steps=50,
loop.steps=20,
object.scale=0.01,
uselen=FALSE,
usecurve=FALSE,
suppress.axes=TRUE,
vertices.last=TRUE,
new=TRUE,
layout.par=NULL,
...){
   #Check to see that things make sense
   if(!is.network(x))
     stop("plot.network requires a network object.")
   if(network.size(x)==0)
     stop("plot.network called on a network of order zero - nothing to plot.")
   #Turn the annoying locator bell off, and remove recursion limit
   bellstate<-options()$locatorBell
   expstate<-options()$expressions
   on.exit(options(locatorBell=bellstate,expressions=expstate))
   options(locatorBell=FALSE,expressions=500000)
   #Create a useful interval inclusion operator
   "%iin%"<-function(x,int) (x>=int[1])&(x<=int[2])
   #Extract the network to be displayed
   if(is.hyper(x)){    #Is this a hypergraph?  If so, use two-mode form.
     #Create a new graph to store the two-mode structure
     xh<-network.initialize(network.size(x)+sum(!sapply(x$mel, is.null)), 
       directed=is.directed(x))
     #Port attributes, in case we need them
     for(i in list.vertex.attributes(x)){
       set.vertex.attribute(xh,attrname=i,
       value=get.vertex.attribute(x,attrname=i,null.na=FALSE,unlist=FALSE),
       v=1:network.size(x))
     }
     for(i in list.network.attributes(x)){
       if(!(i%in%c("bipartite","directed","hyper","loops","mnext","multiple",
          "n")))
         set.network.attribute(xh,attrname=i,
           value=get.network.attribute(x,attrname=i,unlist=FALSE))
     }
     #Now, import the edges
     cnt<-1
     for(i in 1:length(x$mel)){  #Not a safe way to do this, long-term
       if(!is.null(x$mel[[i]])){
         for(j in x$mel[[i]]$outl){
           if(!is.adjacent(xh,j,network.size(x)+cnt))
             add.edge(xh,j,network.size(x)+cnt,names.eval=names(x$mel[[i]]$atl),
               vals.eval=x$mel[[i]]$atl)
         }
         for(j in x$mel[[i]]$inl){
           if(!is.adjacent(xh,network.size(x)+cnt,j)){
             add.edge(xh,network.size(x)+cnt,j,names.eval=names(x$mel[[i]]$atl),
               vals.eval=x$mel[[i]]$atl)
           }
         }
         cnt<-cnt+1                    #Increment the edge counter
       }
     }
     cnt<-cnt-1
     if(length(label)==network.size(x))  #Fix labels, if needed
       label<-c(label,paste("e",1:cnt,sep=""))
     xh%v%"vertex.names"<-c(x%v%"vertex.names",paste("e",1:cnt,sep=""))
     x<-xh
     n<-network.size(x)
     d<-as.matrix.network(x,matrix.type="edgelist",attrname=attrname)
     if(!is.directed(x))
       usearrows<-FALSE
   }else if(is.bipartite(x)){
     n<-network.size(x)
     d<-as.matrix.network(x,matrix.type="edgelist",attrname=attrname)
     usearrows<-FALSE
   }else{
     n<-network.size(x)
     d<-as.matrix.network(x,matrix.type="edgelist",attrname=attrname)
     if(!is.directed(x))
       usearrows<-FALSE
   }
   #Make sure that edge values are in place, matrix has right shape, etc.
   if(NCOL(d)==2){
     if(NROW(d)==0)
       d<-matrix(nrow=0,ncol=3)
     else
       d<-cbind(d,rep(1,NROW(d)))
   }
   diag<-has.loops(x)         #Check for existence of loops
   #Replace NAs with 0s
   d[is.na(d)]<-0
   #Determine which edges should be used when plotting
   edgetouse<-d[,3]>thresh
   d<-d[edgetouse,,drop=FALSE]
   #Save original matrix, which we may use below
   d.raw<-d
   #Determine coordinate placement
   if(!is.null(coord)){      #If the user has specified coords, override all other considerations
     cx<-coord[,1]
     cy<-coord[,2]
   }else{   #Otherwise, use the specified layout function
     layout.fun<-try(match.fun(paste("network.layout.",mode,sep="")), silent=TRUE)
     if(inherits(layout.fun,"try-error"))
       stop("Error in plot.network.default: no layout function for mode ",mode)
     temp<-layout.fun(x,layout.par)
     cx<-temp[,1]
     cy<-temp[,2]
   }
   #Jitter the coordinates if need be
   if(jitter){
      cx<-jitter(cx)
      cy<-jitter(cy)
   }
   #Which nodes should we use?
   use<-displayisolates|(((sapply(x$iel,length)+sapply(x$oel,length))>0))   
   #Deal with axis labels
   if(is.null(xlab))
     xlab=""
   if(is.null(ylab))
     ylab=""
   #Set limits for plotting region
   if(is.null(xlim))
     xlim<-c(min(cx[use])-pad,max(cx[use])+pad)  #Save x, y limits
   if(is.null(ylim))
     ylim<-c(min(cy[use])-pad,max(cy[use])+pad)
   xrng<-diff(xlim)          #Force scale to be symmetric
   yrng<-diff(ylim)
   xctr<-(xlim[2]+xlim[1])/2 #Get center of plotting region
   yctr<-(ylim[2]+ylim[1])/2
   if(xrng<yrng)
     xlim<-c(xctr-yrng/2,xctr+yrng/2)
   else
     ylim<-c(yctr-xrng/2,yctr+xrng/2)
   baserad<-min(diff(xlim),diff(ylim))*object.scale  #Extract "base radius"
   #Create the base plot, if needed
   if(new){  #If new==FALSE, we add to the existing plot; else create a new one
     plot(0,0,xlim=xlim,ylim=ylim,type="n",xlab=xlab,ylab=ylab,asp=1, axes=!suppress.axes,...)
   }
   # force lazy evaluation of display labels arg before we change value of labels
   displaylabels<-displaylabels
   #Fill out vertex vectors; assume we're using attributes if chars used
   # this is done with the plotArgs.network so we can standarize it
   label <-plotArgs.network(x,'label',label)
   vertex.cex <- plotArgs.network(x,'vertex.cex',vertex.cex)
   vertex.radius <-rep(baserad*vertex.cex,length.out=n)   #Create vertex radii
   vertex.sides <- plotArgs.network(x,'vertex.sides',vertex.sides)
   vertex.border <- plotArgs.network(x,'vertex.border',vertex.border)
   vertex.col <- plotArgs.network(x,'vertex.col',vertex.col)
   vertex.lty <- plotArgs.network(x,'vertex.lty',vertex.lty)
   vertex.rot <- plotArgs.network(x,'vertex.rot',vertex.rot)
   vertex.lwd <- plotArgs.network(x,'vertex.lwd',vertex.lwd)
   loop.cex <- plotArgs.network(x,'loop.cex',loop.cex)
   label.col <- plotArgs.network(x,'label.col',label.col)
   label.border<-plotArgs.network(x,'label.border',label.border)
   label.bg <- plotArgs.network(x,'label.bg',label.bg)
   #Plot vertices now, if desired
   if(!vertices.last)
     network.vertex(cx[use],cy[use],radius=vertex.radius[use], sides=vertex.sides[use],col=vertex.col[use],border=vertex.border[use],lty=vertex.lty[use],rot=vertex.rot[use], lwd=vertex.lwd[use])
   #Generate the edges and their attributes
   # TODO: initialize to full length, or sapply code below
   # don't append in loop, no wonder is slow. 
   nDrawEdges<-NROW(d)
   px0<-numeric(nDrawEdges)   #Create position vectors (tail, head)
   py0<-numeric(nDrawEdges)
   px1<-numeric(nDrawEdges)
   py1<-numeric(nDrawEdges)
   e.lwd<-numeric(nDrawEdges) #Create edge attribute vectors
   e.curv<-numeric(nDrawEdges)
   e.type<-numeric(nDrawEdges)
   e.col<-character(nDrawEdges)
   e.hoff<-numeric(nDrawEdges) #Offset radii for heads
   e.toff<-numeric(nDrawEdges) #Offset radii for tails
   e.diag<-logical(nDrawEdges) #Indicator for self-ties
   e.rad<-numeric(nDrawEdges)  #Edge radius (only used for loops)
   if(NROW(d)>0){
     #Edge color
     edge.col<-plotArgs.network(x,'edge.col',edge.col,d=d)
     #Edge line type
     edge.lty<-plotArgs.network(x,'edge.lty',edge.lty,d=d)
     #Edge line width
     edge.lwd<-plotArgs.network(x,'edge.lwd',edge.lwd,d=d)
     #Edge curve
     # TODO: can't move this into prepare plot args becaue it also sets the e.curve.as.mult
     #       but I think it could be refactored to use the d[] array as the other edge functions do
     if(!is.null(edge.curve)){
       if(length(dim(edge.curve))==2){
         edge.curve<-edge.curve[d[,1:2]]
         e.curv.as.mult<-FALSE
       }else{ 
         if(length(edge.curve)==1)
           e.curv.as.mult<-TRUE
         else
           e.curv.as.mult<-FALSE
         edge.curve<-rep(edge.curve,length.out=NROW(d))
       }
     }else if(is.character(edge.curve)&&(length(edge.curve)==1)){
       temp<-edge.curve
       edge.curve<-(x%e%edge.curve)[edgetouse]
       if(all(is.na(edge.curve)))
         stop("Attribute '",temp,"' had illegal missing values for edge.curve or was not present in plot.network.default.")
       e.curv.as.mult<-FALSE
     }else{
       edge.curve<-rep(0,length.out=NROW(d))
       e.curv.as.mult<-FALSE
     }
     # only evaluate edge label stuff if we will draw label
     if(!is.null(edge.label)){
       #Edge label
        edge.label<-plotArgs.network(x,'edge.label',edge.label,d=d)
       
       #Edge label color
       edge.label.col<-plotArgs.network(x,'edge.label.col',edge.label.col,d=d)
       #Edge label cex
       edge.label.cex<-plotArgs.network(x,'edge.label.cex',edge.label.cex,d=d)
     } # end edge label setup block
     
     #Proceed with edge setup
     dist<-((cx[d[,1]]-cx[d[,2]])^2+(cy[d[,1]]-cy[d[,2]])^2)^0.5  #Get the inter-point distances for curves
     tl<-d.raw*dist   #Get rescaled edge lengths
     tl.max<-max(tl)  #Get maximum edge length
     for(i in 1:NROW(d)){
       if(use[d[i,1]]&&use[d[i,2]]){  #Plot edges for displayed vertices (wait,doesn't 'use' track isolates, which don't have edges anyway?)
         px0[i]<-as.double(cx[d[i,1]])  #Store endpoint coordinates
         py0[i]<-as.double(cy[d[i,1]])
         px1[i]<-as.double(cx[d[i,2]])
         py1[i]<-as.double(cy[d[i,2]])
         e.toff[i]<-vertex.radius[d[i,1]] #Store endpoint offsets
         e.hoff[i]<-vertex.radius[d[i,2]]
         e.col[i]<-edge.col[i]   #Store other edge attributes
         e.type[i]<-edge.lty[i]
         e.lwd[i]<-edge.lwd[i]
         e.diag[i]<-d[i,1]==d[i,2]  #Is this a loop?
         e.rad[i]<-vertex.radius[d[i,1]]*loop.cex[d[i,1]]
         if(uselen){   #Should we base curvature on interpoint distances?
           if(tl[i]>0){ 
             e.len<-dist[i]*tl.max/tl[i]
             e.curv[i]<-edge.len*sqrt((e.len/2)^2-(dist[i]/2)^2)
           }else{
             e.curv[i]<-0
           }
         }else{        #Otherwise, use prespecified edge.curve
           if(e.curv.as.mult)    #If it's a scalar, multiply by edge str
             e.curv[i]<-edge.curve[i]*d.raw[i]
           else
             e.curv[i]<-edge.curve[i]
         }
       }
     } 
   }# end edges block
   #Plot loops for the diagonals, if diag==TRUE, rotating wrt center of mass
   if(diag&&(length(px0)>0)&&sum(e.diag>0)){  #Are there any loops present?
     network.loop(as.vector(px0)[e.diag],as.vector(py0)[e.diag], length=1.5*baserad*arrowhead.cex,angle=25,width=e.lwd[e.diag]*baserad/10,col=e.col[e.diag],border=e.col[e.diag],lty=e.type[e.diag],offset=e.hoff[e.diag],edge.steps=loop.steps,radius=e.rad[e.diag],arrowhead=usearrows,xctr=mean(cx[use]),yctr=mean(cy[use]))
     if(!is.null(edge.label)){
       network.edgelabel(px0,py0,0,0,edge.label[e.diag],directed=is.directed(x),cex=edge.label.cex[e.diag],col=edge.label.col[e.diag],loops=TRUE)
     }
     
   }
   #Plot standard (i.e., non-loop) edges
   if(length(px0)>0){  #If edges are present, remove loops from consideration
     px0<-px0[!e.diag] 
     py0<-py0[!e.diag]
     px1<-px1[!e.diag]
     py1<-py1[!e.diag]
     e.curv<-e.curv[!e.diag]
     e.lwd<-e.lwd[!e.diag]
     e.type<-e.type[!e.diag]
     e.col<-e.col[!e.diag]
     e.hoff<-e.hoff[!e.diag]
     e.toff<-e.toff[!e.diag]
     e.rad<-e.rad[!e.diag]
   }
   if(!usecurve&!uselen){   #Straight-line edge case
     if(length(px0)>0){
       network.arrow(as.vector(px0),as.vector(py0),as.vector(px1), as.vector(py1),length=2*baserad*arrowhead.cex,angle=20,col=e.col,border=e.col,lty=e.type,width=e.lwd*baserad/10,offset.head=e.hoff,offset.tail=e.toff,arrowhead=usearrows)
       if(!is.null(edge.label)){
         network.edgelabel(px0,py0,px1,py1,edge.label[!e.diag],directed=is.directed(x),cex=edge.label.cex[!e.diag],col=edge.label.col[!e.diag])
       }
     }
   }else{   #Curved edge case
     if(length(px0)>0){
       network.arrow(as.vector(px0),as.vector(py0),as.vector(px1), as.vector(py1),length=2*baserad*arrowhead.cex,angle=20,col=e.col,border=e.col,lty=e.type,width=e.lwd*baserad/10,offset.head=e.hoff,offset.tail=e.toff,arrowhead=usearrows,curve=e.curv,edge.steps=edge.steps)
       if(!is.null(edge.label)){
         network.edgelabel(px0,py0,px1,py1,edge.label[!e.diag],directed=is.directed(x),cex=edge.label.cex[!e.diag],col=edge.label.col[!e.diag],curve=e.curv)
       }
     }
   }
   
   #Plot vertices now, if we haven't already done so
   if(vertices.last)
     network.vertex(cx[use],cy[use],radius=vertex.radius[use], sides=vertex.sides[use],col=vertex.col[use],border=vertex.border[use],lty=vertex.lty[use],rot=vertex.rot[use], lwd=vertex.lwd[use])
   #Plot vertex labels, if needed
   if(displaylabels&(!all(label==""))&(!all(use==FALSE))){
     if (label.pos==0){
       xhat <- yhat <- rhat <- rep(0,n) 
       #Set up xoff yoff and roff when we get odd vertices
       xoff <- cx[use]-mean(cx[use])
       yoff <- cy[use]-mean(cy[use])
       roff <- sqrt(xoff^2+yoff^2)
       #Loop through vertices
       for (i in (1:n)[use]){
         #Find all in and out ties that aren't loops
         ij <- unique(c(d[d[,2]==i&d[,1]!=i,1],d[d[,1]==i&d[,2]!=i,2]))
         ij.n <- length(ij)
         if (ij.n>0) {
           #Loop through all ties and add each vector to label direction
           for (j in ij){
             dx <- cx[i]-cx[j]
             dy <- cy[i]-cy[j]
             dr <- sqrt(dx^2+dy^2)
             xhat[i] <- xhat[i]+dx/dr
             yhat[i] <- yhat[i]+dy/dr
           }
           
           #Take the average of all the ties
           xhat[i] <- xhat[i]/ij.n
           yhat[i] <- yhat[i]/ij.n
           rhat[i] <- sqrt(xhat[i]^2+yhat[i]^2)
           if (!is.nan(rhat[i]) && rhat[i]!=0) { # watch out for NaN when vertices have same position
             # normalize direction vector
             xhat[i] <- xhat[i]/rhat[i]
             yhat[i] <- yhat[i]/rhat[i]
           } else { #if no direction, make xhat and yhat away from center
             xhat[i] <- xoff[i]/roff[i]
             yhat[i] <- yoff[i]/roff[i]
           }
         } else { #if no ties, make xhat and yhat away from center
           xhat[i] <- xoff[i]/roff[i]
           yhat[i] <- yoff[i]/roff[i]
         }
         if ( is.nan(xhat[i]) || xhat[i]==0 ) xhat[i] <- .01 #jitter to avoid labels on points
         if (is.nan(yhat[i]) || yhat[i]==0 ) yhat[i] <- .01
       }
       xhat <- xhat[use]
       yhat <- yhat[use]
     } else if (label.pos<5) {
       xhat <- switch(label.pos,0,-1,0,1)
       yhat <- switch(label.pos,-1,0,1,0)
     } else if (label.pos==6) {
       xoff <- cx[use]-mean(cx[use])
       yoff <- cy[use]-mean(cy[use])
       roff <- sqrt(xoff^2+yoff^2)
       xhat <- xoff/roff
       yhat <- yoff/roff
     } else {
       xhat <- 0
       yhat <- 0
     }
     os<-par()$cxy*mean(label.cex,na.rm = TRUE) # don't think this is actually used?
     lw<-strwidth(label[use],cex=label.cex)/2
     lh<-strheight(label[use],cex=label.cex)/2
     if(boxed.labels){
       rect(cx[use]+xhat*vertex.radius[use]-(lh*label.pad+lw)*((xhat<0)*2+ (xhat==0)*1),
         cy[use]+yhat*vertex.radius[use]-(lh*label.pad+lh)*((yhat<0)*2+ (yhat==0)*1),
         cx[use]+xhat*vertex.radius[use]+(lh*label.pad+lw)*((xhat>0)*2+ (xhat==0)*1),
         cy[use]+yhat*vertex.radius[use]+(lh*label.pad+lh)*((yhat>0)*2+ (yhat==0)*1),
         col=label.bg,border=label.border,lty=label.lty,lwd=label.lwd)
     }
     text(cx[use]+xhat*vertex.radius[use]+(lh*label.pad+lw)*((xhat>0)-(xhat<0)),
          cy[use]+yhat*vertex.radius[use]+(lh*label.pad+lh)*((yhat>0)-(yhat<0)),
          label[use],cex=label.cex,col=label.col,offset=0)         
   }
   #If interactive, allow the user to mess with things
   if(interactive&&((length(cx)>0)&&(!all(use==FALSE)))){
     #Set up the text offset increment
     os<-c(0.2,0.4)*par()$cxy
     #Get the location for text messages, and write to the screen
     textloc<-c(min(cx[use])-pad,max(cy[use])+pad)
     tm<-"Select a vertex to move, or click \"Finished\" to end."
     tmh<-strheight(tm)
     tmw<-strwidth(tm)
     text(textloc[1],textloc[2],tm,adj=c(0,0.5)) #Print the initial instruction
     fm<-"Finished"
     finx<-c(textloc[1],textloc[1]+strwidth(fm))
     finy<-c(textloc[2]-3*tmh-strheight(fm)/2,textloc[2]-3*tmh+strheight(fm)/2)
     finbx<-finx+c(-os[1],os[1])
     finby<-finy+c(-os[2],os[2])
     rect(finbx[1],finby[1],finbx[2],finby[2],col="white")
     text(finx[1],mean(finy),fm,adj=c(0,0.5))
     #Get the click location
     clickpos<-unlist(locator(1))
     #If the click is in the "finished" box, end our little game.  Otherwise,
     #relocate a vertex and redraw.
     if((clickpos[1]%iin%finbx)&&(clickpos[2]%iin%finby)){
       cl<-match.call()                #Get the args of the current function
       cl$interactive<-FALSE           #Turn off interactivity
       cl$coord<-cbind(cx,cy)          #Set the coordinates
       cl$x<-x                         #"Fix" the data array
       return(eval.parent(cl))     #Execute the function and return
     }else{
       #Figure out which vertex was selected
       clickdis<-sqrt((clickpos[1]-cx[use])^2+(clickpos[2]-cy[use])^2)
       selvert<-match(min(clickdis),clickdis)
       #Create usable labels, if the current ones aren't
       if(all(label==""))
         label<-1:n
       #Clear out the old message, and write a new one
       rect(textloc[1],textloc[2]-tmh/2,textloc[1]+tmw,textloc[2]+tmh/2, border="white",col="white")
       tm<-"Where should I move this vertex?"
       tmh<-strheight(tm)
       tmw<-strwidth(tm)
       text(textloc[1],textloc[2],tm,adj=c(0,0.5))
       fm<-paste("Vertex",label[use][selvert],"selected")
       finx<-c(textloc[1],textloc[1]+strwidth(fm))
       finy<-c(textloc[2]-3*tmh-strheight(fm)/2,textloc[2]-3*tmh+ strheight(fm)/2)
       finbx<-finx+c(-os[1],os[1])
       finby<-finy+c(-os[2],os[2])
       rect(finbx[1],finby[1],finbx[2],finby[2],col="white")
       text(finx[1],mean(finy),fm,adj=c(0,0.5))
       #Get the destination for the new vertex
       clickpos<-unlist(locator(1))
       #Set the coordinates accordingly
       cx[use][selvert]<-clickpos[1]
       cy[use][selvert]<-clickpos[2]
       #Iterate (leaving interactivity on)
       cl<-match.call()                #Get the args of the current function
       cl$coord<-cbind(cx,cy)          #Set the coordinates
       cl$x<-x                         #"Fix" the data array
       return(eval.parent(cl))     #Execute the function and return
     }
   }
   #Return the vertex positions, should they be needed
   invisible(cbind(cx,cy))
}

# moving all of the plot argument checking and expansion into a single function
# so that it will be acessible from other plot-related tools (like ndtv)
# argName = character named of argument to be checked/expaneded
# argValue = value passed in by user, to be processed/expanded
# d is an edgelist matrix of edge values optionally used by some edge attribute functions
# edgetouse the set of edge ids to be used (in case some edges are not being shown)



#' Expand and transform attributes of networks to values appropriate for
#' aguments to plot.network
#' 
#' This is primairly an internal function called by \code{plot.network} or by
#' external packages such as \code{ndtv} that want to prepare
#' \code{plot.network} graphic arguments in a standardized way.
#' 
#' Given a network object, the name of graphic parameter argument to
#' \code{plot.network} and value, it will if necessary transform the value, or
#' extract it from the network, according to the description in
#' \code{\link{plot.network}}. For some attributes, if the value is the name of
#' a vertex or edge attribute, the appropriate values will be extracted from
#' the network before transformation.
#' 
#' @rdname preparePlotArgs
#' @name plotArgs.network
#'
#' @param x a \code{network} object which is going to be plotted
#' @param argName character, the name of \code{plot.network} graphic parameter
#' @param argValue value for the graphic paramter named in \code{argName} which
#' to be transformed/prepared.  For many attributes, if this is a single
#' character vector it will be assumed to be the name of a vertex or edge
#' attribute to be extracted and transformed
#' @param d is an edgelist matrix of edge values optionally used by some edge
#' attribute functions
#' @param edgetouse numeric vector giving set of edge ids to be used (in case
#' some edges are not being shown) required by some attributes
#' @return returns a vector with length corresponding to the number of vertices
#' or edges (depending on the paramter type) giving the appropriately prepared
#' values for the parameter type.  If the values or specified attribute can not
#' be processed correctly, and Error may occur.
#' @author skyebend@@uw.edu
#' @seealso See also \code{\link{plot.network}}
#' @examples
#' 
#'   net<-network.initialize(3)
#'   set.vertex.attribute(net,'color',c('red','green','blue'))
#'   set.vertex.attribute(net,'charm',1:3)
#'   # replicate a single colorname value
#'   plotArgs.network(net,'vertex.col','purple')
#'   # map the 'color' attribute to color
#'   plotArgs.network(net,'vertex.col','color')
#'   # similarly for a numeric attribute ...
#'   plotArgs.network(net,'vertex.cex',12)
#'   plotArgs.network(net,'vertex.cex','charm')
#' 
#' @export plotArgs.network
plotArgs.network<-function(x,argName, argValue,d=NULL,edgetouse=NULL){
  n<-network.size(x)
  # count the number of edges 
  # not sure if nrow d is every differnt, than network edgecount, but just being safe
  if(!is.null(d)){
    nE<-NROW(d)
  } else {
    nE<-network.edgecount(x)
  }
  if(is.null(edgetouse)){
    edgetouse<-seq_len(nE) # use all the edges
  }
  # if d exists, it may need to be subset to the number of edges
  if (!is.null(d)){
    d<-d[edgetouse,,drop=FALSE]
  }
  
  # assign the value to a local variable with the appropriate name
  assign(argName,argValue)
  #Fill out vertex vectors; assume we're using attributes if chars used
  # TODO: only one of the code blocks below should execute, set up as a switch?
  switch(argName,
      # ----- vertex labels ---------------------------
      label=if(is.character(label)&(length(label)==1)){
        temp<-label
        if(temp%in%list.vertex.attributes(x)){
          label <- rep(get.vertex.attribute(x,temp),length.out=n)
          if(all(is.na(label))){
            stop("Attribute '",temp,"' had illegal missing values for label or was not present in plot.network.default.")
          }
        } else { # didn't match with a vertex attribute, assume we are supposed to replicate it
          label <- rep(label,length.out=n)
        }
      }else{
        label <- rep(as.character(label),length.out=n)
      }
      ,
      # ------ vertex sizes (vertex.cex) --------------------
      vertex.cex=if(is.character(vertex.cex)&(length(vertex.cex)==1)){
        temp<-vertex.cex
        vertex.cex <- rep(get.vertex.attribute(x,vertex.cex),length.out=n)
        if(all(is.na(vertex.cex)))
          stop("Attribute '",temp,"' had illegal missing values for vertex.cex or was not present in plot.network.default.")
      }else
        vertex.cex <- rep(vertex.cex,length.out=n)
      ,
      # ------ vertex sides (number of sides for polygon) ---------
      vertex.sides=if(is.character(vertex.sides)&&(length(vertex.sides==1))){
        temp<-vertex.sides
        vertex.sides <- rep(get.vertex.attribute(x,vertex.sides),length.out=n)
        if(all(is.na(vertex.sides)))
          stop("Attribute '",temp,"' had illegal missing values for vertex.sides or was not present in plot.network.default.")
      }else
        vertex.sides <- rep(vertex.sides,length.out=n)
      ,
      # --------- vertex border  --------------------
      vertex.border=if(is.character(vertex.border)&&(length(vertex.border)==1)){
        temp<-vertex.border
        vertex.border <- rep(get.vertex.attribute(x,vertex.border),length.out=n)
        if(all(is.na(vertex.border)))
          vertex.border <- rep(temp,length.out=n) #Assume it was a color word
        else{
          if(!all(is.color(vertex.border),na.rm=TRUE))
            vertex.border<-as.color(vertex.border)
        }
      }else
        vertex.border <- rep(vertex.border,length.out=n)
      ,
      # -------- vertex color ------------------------
      vertex.col=if(is.character(vertex.col)&&(length(vertex.col)==1)){
        temp<-vertex.col
        vertex.col <- rep(get.vertex.attribute(x,vertex.col),length.out=n)
        if(all(is.na(vertex.col)))
          vertex.col <- rep(temp,length.out=n) #Assume it was a color word
        else{
          if(!all(is.color(vertex.col),na.rm=TRUE))
            vertex.col<-as.color(vertex.col)
        }
      }else
        vertex.col <- rep(vertex.col,length.out=n)
      ,
      # ------- vertex line type (vertex.lty) --------------------
      vertex.lty=if(is.character(vertex.lty)&&(length(vertex.lty)==1)){
        temp<-vertex.lty
        vertex.lty <- rep(get.vertex.attribute(x,vertex.lty),length.out=n)
        if(all(is.na(vertex.lty)))
          stop("Attribute '",temp,"' had illegal missing values for vertex.col or was not present in plot.network.default.")
      }else
        vertex.lty <- rep(vertex.lty,length.out=n)
      ,
      # ------- vertex rotation --------------------------------------
      vertex.rot=if(is.character(vertex.rot)&&(length(vertex.rot)==1)){
        temp<-vertex.rot
        vertex.rot <- rep(get.vertex.attribute(x,vertex.rot),length.out=n)
        if(all(is.na(vertex.rot)))
          stop("Attribute '",temp,"' had illegal missing values for vertex.rot or was not present in plot.network.default.")
      }else
        vertex.rot <- rep(vertex.rot,length.out=n)
      ,
      # -------- vertex line width --------------------------
      vertex.lwd=if(is.character(vertex.lwd)&&(length(vertex.lwd)==1)){
        temp<-vertex.lwd
        vertex.lwd <- rep(get.vertex.attribute(x,vertex.lwd),length.out=n)
        if(all(is.na(vertex.lwd)))
          stop("Attribute '",temp,"' had illegal missing values for vertex.lwd or was not present in plot.network.default.")
      }else
        vertex.lwd <- rep(vertex.lwd,length.out=n)
      ,
      # -------- vertex self-loop size -----------------------
      loop.cex=if(is.character(loop.cex)&&(length(loop.cex)==1)){
        temp<-loop.cex
        loop.cex <- rep(get.vertex.attribute(x,loop.cex),length.out=n)
        if(all(is.na(loop.cex)))
          stop("Attribute ",temp," had illegal missing values for loop.cex or was not present in plot.network.default.")
      }else
        loop.cex <- rep(loop.cex,length.out=n)
      ,
      # ---------  vertex label color -----------------------------
      label.col=if(is.character(label.col)&&(length(label.col)==1)){
        temp<-label.col
        label.col <- rep(get.vertex.attribute(x,label.col),length.out=n)
        if(all(is.na(label.col)))
          label.col <- rep(temp,length.out=n) #Assume it was a color word
        else{
          if(!all(is.color(label.col),na.rm=TRUE))
            label.col<-as.color(label.col)
        }
      }else
        label.col <- rep(label.col,length.out=n)
      ,
      # -------- vertex label border ------------------------------
      label.border=if(is.character(label.border)&&(length(label.border)==1)){
        temp<-label.border
        label.border <- rep(get.vertex.attribute(x,label.border),length.out=n)
        if(all(is.na(label.border)))
          label.border <- rep(temp,length.out=n) #Assume it was a color word
        else{
          if(!all(is.color(label.border),na.rm=TRUE))
            label.border<-as.color(label.border)
        }
      }else{
        label.border <- rep(label.border,length.out=n)
      }
      ,
      # ------- vertex label border background color ----------------
      label.bg=if(is.character(label.bg)&&(length(label.bg)==1)){
        temp<-label.bg
        label.bg <- rep(get.vertex.attribute(x,label.bg),length.out=n)
        if(all(is.na(label.bg)))
          label.bg <- rep(temp,length.out=n) #Assume it was a color word
        else{
          if(!all(is.color(label.bg),na.rm=TRUE))
            label.bg<-as.color(label.bg)
        }
      }else{
        label.bg <- rep(label.bg,length.out=n)
      }
      ,
      # ------ Edge color---------
      edge.col=if(length(dim(edge.col))==2)   #Coerce edge.col/edge.lty to vector form
        edge.col<-edge.col[d[,1:2]]
      else if(is.character(edge.col)&&(length(edge.col)==1)){
        temp<-edge.col
        edge.col<-x%e%edge.col
        if(!is.null(edge.col)){
          edge.col<-edge.col[edgetouse]
          if(!all(is.color(edge.col),na.rm=TRUE))
            edge.col<-as.color(edge.col)
        }else{
          edge.col<-rep(temp,length.out=nE)  #Assume it was a color word
        }
      }else{
        edge.col<-rep(edge.col,length.out=nE)
      }
      ,
      # ----------- Edge line type ------------------
      edge.lty=if(length(dim(edge.lty))==2){
        edge.lty<-edge.lty[d[,1:2]]
      }else if(is.character(edge.lty)&&(length(edge.lty)==1)){
        temp<-edge.lty
        edge.lty<-(x%e%edge.lty)[edgetouse]
        if(all(is.na(edge.lty)))
          stop("Attribute '",temp,"' had illegal missing values for edge.lty or was not present in plot.network.default.")
      }else{
        edge.lty<-rep(edge.lty,length.out=nE)
      }
      , 
      # ----------- Edge line width ------
      edge.lwd=if(length(dim(edge.lwd))==2){
        edge.lwd<-edge.lwd[d[,1:2]]  # what is going on here? aren't these the incident vertices? # for later matrix lookup?
      }else if(is.character(edge.lwd)&&(length(edge.lwd)==1)){
        temp<-edge.lwd
        edge.lwd<-(x%e%edge.lwd)[edgetouse]
        if(all(is.na(edge.lwd))){
          stop("Attribute '",temp,"' had illegal missing values for edge.lwd or was not present in plot.network.default.")
        }
      }else{ 
        if(length(edge.lwd)==1){ # if lwd has only one element..
          if(edge.lwd>0){  # ... and that element > 0 ,use it as a scale factor for the edge values in d
                           # .. unless d is missing
            if (!is.null(d)){
              edge.lwd<-edge.lwd*d[,3]
            } else {
              # d is missing, so just replicate
            } 
              edge.lwd<-rep(edge.lwd,length.out=nE)
          }else{  # edge is zero or less, so set it to 1
            edge.lwd<-rep(1,length.out=nE)
          }
        } else { # just replacte for the number of edges
          edge.lwd<-rep(edge.lwd,length.out=nE)
        }
      }
      ,
      
      # ----------- Edge curve---------------
      edge.curve=if(!is.null(edge.curve)){
        if(length(dim(edge.curve))==2){
          edge.curve<-edge.curve[d[,1:2]]
          e.curv.as.mult<-FALSE
        }else{ 
          if(length(edge.curve)==1){
            e.curv.as.mult<-TRUE
          }else{
            e.curv.as.mult<-FALSE
          }
          edge.curve<-rep(edge.curve,length.out=nE)
        }
      }else if(is.character(edge.curve)&&(length(edge.curve)==1)){
        temp<-edge.curve
        edge.curve<-(x%e%edge.curve)[edgetouse]
        if(all(is.na(edge.curve))){
          stop("Attribute '",temp,"' had illegal missing values for edge.curve or was not present in plot.network.default.")
        }
        e.curv.as.mult<-FALSE
      }else{
        edge.curve<-rep(0,length.out=nE)
        e.curv.as.mult<-FALSE
      }
      ,
      # -------- edge label  ----------------------
      edge.label=if(length(dim(edge.label))==2){   #Coerce edge.label to vector form
        edge.label<-edge.label[d[,1:2]]
      }else if(is.character(edge.label)&&(length(edge.label)==1)){
        temp<-edge.label
        edge.label<-x%e%edge.label
        if(!is.null(edge.label)){
          edge.label<-edge.label[edgetouse]
        }else
          edge.label<-rep(temp,length.out=nE)  #Assume it was a value to replicate
      }else if(is.logical(edge.label)&&(length(edge.label)==1)) {
        if (edge.label){
          # default to edge ids.
          edge.label<-valid.eids(x)[edgetouse]
        } else {
          # don't draw edge labels if set to FALSE
          edge.label<-NULL
        }
      }else{   
        # do nothing and hope for the best!
        edge.label<-rep(edge.label,length.out=nE)
      }
      ,
      # ------ edge label color --------------------
      #Edge  label color
      edge.label.col=if(length(dim(edge.label.col))==2){   #Coerce edge.label.col
        edge.label.col<-edge.label.col[d[,1:2]]
      } else if(is.character(edge.label.col)&&(length(edge.label.col)==1)){
        temp<-edge.label.col
        edge.label.col<-x%e%edge.label.col
        if(!is.null(edge.label.col)){
          edge.label.col<-edge.label.col[edgetouse]
          if(!all(is.color(edge.label.col),na.rm=TRUE))
            edge.label.col<-as.color(edge.label.col)
        }else
          edge.label.col<-rep(temp,length.out=nE)  #Assume it was a color word
      }else{
        edge.label.col<-rep(edge.label.col,length.out=nE)
      }
      ,
      # ------- edge.label.cex  --------------------
      #Edge label cex
      edge.label.cex=if(length(dim(edge.label.cex))==2)
        edge.label.cex<-edge.label.cex[d[,1:2]]
      else if(is.character(edge.label.cex)&&(length(edge.label.cex)==1)){
        temp<-edge.label.cex
        edge.label.cex<-(x%e%edge.label.cex)[edgetouse]
        if(all(is.na(edge.label.cex)))
          stop("Attribute '",temp,"' had illegal missing values for edge.label.cex or was not present in plot.network.default.")
      }else{
        edge.label.cex<-rep(edge.label.cex,length.out=nE)
      }
      # case in which none of the argument names match up
      # stop('argument "',argName,'"" does not match with any of the plot.network arguments')
      # can't error out, because this function will be called with non-network args, so just
      # return the value passed in
      
  ) # end switch block
  # now return the checked / expanded value
  return(get(argName))
}
