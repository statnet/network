######################################################################
#
# plot.R
#
# Written by Carter T. Butts <buttsc@uci.edu>; portions contributed by
# David Hunter <dhunter@stat.psu.edu> and Mark S. Handcock
# <handcock@u.washington.edu>.
#
# Last Modified 9/14/06
# Licensed under the GNU General Public License version 2 (June, 1991)
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


#Custom arrow-drawing method for plot.network
network.arrow<-function(x0,y0,x1,y1,length=0.1,angle=20,width=0.01,col=1,border=1,lty=1,offset.head=0,offset.tail=0,arrowhead=TRUE,curve=0,edge.steps=50,...){
  if(length(x0)==0)   #Leave if there's nothing to do
    return;
  #Introduce a function to make coordinates for a single polygon
  make.coords<-function(x0,y0,x1,y1,ahangle,ahlen,swid,toff,hoff,ahead, curve,csteps){ 
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
  #"Stretch" the arguments
  n<-length(x0)
  angle<-rep(angle,length=n)/360*2*pi
  length<-rep(length,length=n)
  width<-rep(width,length=n)
  col<-rep(col,length=n)
  border<-rep(border,length=n)
  lty<-rep(lty,length=n)
  arrowhead<-rep(arrowhead,length=n)
  offset.head<-rep(offset.head,length=n)
  offset.tail<-rep(offset.tail,length=n)
  curve<-rep(curve,length=n)
  edge.steps<-rep(edge.steps,length=n)
  #Obtain coordinates
  coord<-vector()
  for(i in 1:n)  
    coord<-rbind(coord,make.coords(x0[i],y0[i],x1[i],y1[i],angle[i],length[i], width[i],offset.tail[i],offset.head[i],arrowhead[i],curve[i],edge.steps[i]))
  coord<-coord[-NROW(coord),]
  #Draw polygons
  polygon(coord,col=col,border=border,lty=lty,...)
}


#Custom loop-drawing method for plot.network
network.loop<-function(x0,y0,length=0.1,angle=10,width=0.01,col=1,border=1,lty=1,offset=0,edge.steps=10,radius=1,arrowhead=TRUE,xctr=0,yctr=0,...){
  if(length(x0)==0)   #Leave if there's nothing to do
    return;
  #Introduce a function to make coordinates for a single polygon
  make.coords<-function(x0,y0,xctr,yctr,ahangle,ahlen,swid,off,rad,ahead){
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
  #"Stretch" the arguments
  n<-length(x0)
  angle<-rep(angle,length=n)/360*2*pi
  length<-rep(length,length=n)
  width<-rep(width,length=n)
  col<-rep(col,length=n)
  border<-rep(border,length=n)
  lty<-rep(lty,length=n)
  rad<-rep(radius,length=n)
  arrowhead<-rep(arrowhead,length=n)
  offset<-rep(offset,length=n)
  #Obtain coordinates
  coord<-vector()
  for(i in 1:n)  
    coord<-rbind(coord,make.coords(x0[i],y0[i],xctr,yctr,angle[i],length[i], width[i],offset[i],rad[i],arrowhead[i]))
  coord<-coord[-NROW(coord),]
  #Draw polygons
  polygon(coord,col=col,border=border,lty=lty,...)
}


#Routine to plot vertices, using polygons
network.vertex<-function(x,y,radius=1,sides=4,border=1,col=2,lty=NULL,rot=0,...){
  #Introduce a function to make coordinates for a single polygon
  make.coords<-function(x,y,r,s,rot){
    ang<-(1:s)/s*2*pi+rot*2*pi/360
    rbind(cbind(x+r*cos(ang),y+r*sin(ang)),c(NA,NA))  
  }
  #Prep the vars
  n<-length(x)
  radius<-rep(radius,length=n)
  sides<-rep(sides,length=n)
  border<-rep(border,length=n)
  col<-rep(col,length=n)
  lty<-rep(lty,length=n)
  rot<-rep(rot,length=n)
  #Obtain the coordinates
  coord<-vector()
  for(i in 1:length(x))
    coord<-rbind(coord,make.coords(x[i],y[i],radius[i],sides[i],rot[i]))
  #Plot the polygons
  polygon(coord,border=border,col=col,lty=lty,...)
}


#Generic plot.network method.  May need to change....
plot.network <- function(x, ...){
 if("statnet"  %in% .packages() |
    "statnetval"  %in% .packages()
    ){
  plot.network.ergm(x, ...)
 }else{
  plot.network.default(x, ...)
 }         
}


#Two-dimensional network visualization; this is a direct port of the gplot
#routine from sna (Carter T. Butts <buttsc@uci.edu>)
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
displaylabels=FALSE,
boxed.labels=TRUE,
label.pos=0,
label.bg="white",
vertex.sides=8,
vertex.rot=0,
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
   #Turn the annoying locator bell off, and remove recursion limit
   bellstate<-options()$locatorBell
   expstate<-options()$expression
   on.exit(options(locatorBell=bellstate,expression=expstate))
   options(locatorBell=FALSE,expression=Inf)
   #Create a useful interval inclusion operator
   "%iin%"<-function(x,int) (x>=int[1])&(x<=int[2])
   #Extract the network to be displayed
   if(is.hyper(x)){    #Is this a hypergraph?  If so, use two-mode form.
     d<-as.matrix.network(x,matrix.type="incidence",attrname=attrname)
     n<-sum(dim(d))
     temp<-matrix(0,nrow=n,ncol=n)
     if(is.directed(x)){  #If directed, depict as such.
       temp[1:dim(d)[1],(dim(d)[1]+1):n]<-abs(pmin(d,0))     #Tail set
       temp[(dim(d)[1]+1):n,1:dim(d)[1]]<-t(abs(pmax(d,0)))  #Head set
       d<-temp
     }else{
       temp[1:dim(d)[1],(dim(d)[1]+1):n]<-d
       temp[lower.tri(temp)]<-t(temp)[lower.tri(temp)]
       d<-temp
       usearrows<-FALSE   #Don't use labels for undirected graphs
     }
     if(length(label)==network.size(x))  #Fix labels, if needed
       label<-c(label,paste("e",1:(n-network.size(x)),sep=""))
   }else if(is.bipartite(x)){
     n<-network.size(x)
     edgelist<-as.matrix.network(x,matrix.type="edgelist")
     nedge <- nrow(edgelist)
     usearrows<-FALSE
   }else{
     n<-network.size(x)
     edgelist<-as.matrix.network(x,matrix.type="edgelist")
     nedge <- nrow(edgelist)
     if(!is.directed(x))
       usearrows<-FALSE
   }
   diag<-has.loops(x)         #Check for existence of loops
   #Dichotomize d
   if(!is.null(attrname)){
    d<-matrix(as.numeric(d>thresh),n,n)
   }
   #Determine coordinate placement
   if(!is.null(coord)){      #If the user has specified coords, override all other considerations
      cx<-coord[,1]
      cy<-coord[,2]
   }else{   #Otherwise, use the specified layout function
     layout.fun<-try(match.fun(paste("network.layout.",mode,sep="")), silent=TRUE)
     if(class(layout.fun)=="try-error")
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
   use<-displayisolates|!is.isolated(x)
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
   #Fill out vertex vectors; assume we're using attributes if chars used
   if(is.character(vertex.cex)){
     vertex.cex <- rep(get.vertex.attribute(x,vertex.cex),length=n)
     if(any(is.na(vertex.cex)))
       stop("Attribute",vertex.cex,"had illegal missing values or was not present in plot.graph.default.")
   }else
     vertex.cex <- rep(vertex.cex,length=n)
   vertex.radius<-rep(baserad*vertex.cex,length=n)   #Create vertex radii
   if(is.character(vertex.sides)){
     vertex.sides <- rep(get.vertex.attribute(x,vertex.sides),length=n)
     if(any(is.na(vertex.sides)))
       stop("Attribute",vertex.sides,"had illegal missing values or was not present in plot.graph.default.")
   }else
     vertex.sides <- rep(vertex.sides,length=n)
   if(is.character(vertex.border)){
     vertex.border <- rep(get.vertex.attribute(x,vertex.border),length=n)
     if(any(is.na(vertex.border)))
       stop("Attribute",vertex.border,"had illegal missing values or was not present in plot.graph.default.")
   }else
     vertex.border <- rep(vertex.border,length=n)
   if(is.character(vertex.col)&&(length(vertex.col)==1)){
     temp<-vertex.col
     vertex.col <- rep(get.vertex.attribute(x,vertex.col),length=n)
     if(any(is.na(vertex.col)))
       vertex.col <- rep(temp,length=n) #Assume it was a color word
   }else
     vertex.col <- rep(vertex.col,length=n)
   if(is.character(vertex.lty)){
     vertex.lty <- rep(get.vertex.attribute(x,vertex.lty),length=n)
     if(any(is.na(vertex.lty)))
       stop("Attribute",vertex.lty,"had illegal missing values or was not present in plot.graph.default.")
   }else
     vertex.lty <- rep(vertex.lty,length=n)
   if(is.character(vertex.rot)){
     vertex.rot <- rep(get.vertex.attribute(x,vertex.rot),length=n)
     if(any(is.na(vertex.rot)))
       stop("Attribute",vertex.rot,"had illegal missing values or was not present in plot.graph.default.")
   }else
     vertex.rot <- rep(vertex.rot,length=n)
   if(is.character(loop.cex)){
     loop.cex <- rep(get.vertex.attribute(x,loop.cex),length=n)
     if(any(is.na(loop.cex)))
       stop("Attribute",loop.cex,"had illegal missing values or was not present in plot.graph.default.")
   }else
     loop.cex <- rep(loop.cex,length=n)
   #Plot vertices now, if desired
   if(!vertices.last)
     network.vertex(cx[use],cy[use],radius=vertex.radius[use], sides=vertex.sides[use],col=vertex.col[use],border=vertex.border[use],lty=vertex.lty[use],rot=vertex.rot[use])
   #Generate the edges and their attributes
   px0<-vector()   #Create position vectors (tail, head)
   py0<-vector()
   px1<-vector()
   py1<-vector()
   e.lwd<-vector() #Create edge attribute vectors
   e.curv<-vector()
   e.type<-vector()
   e.col<-vector()
   e.hoff<-vector() #Offset radii for heads
   e.toff<-vector() #Offset radii for tails
   e.diag<-vector() #Indicator for self-ties
   e.rad<-vector()  #Edge radius (only used for loops)
   #Obtain the correct edge properties (possibly as attributes)
   if(is.character(edge.col)&&(length(edge.col)==1)){
     temp<-edge.col
     edge.col <- as.matrix.network.adjacency(x,attrname=edge.col)
     if(any(is.na(edge.col)))
       edge.col<-temp  #Assume things were OK, and put it back
   }
   if(is.character(edge.lty)){
     edge.lty <- as.matrix.network.adjacency(x,attrname=edge.lty)
     if(any(is.na(edge.lty)))
       stop("Attribute",edge.lty,"had illegal missing values or was not present in plot.graph.default.")
   }
   if(is.character(edge.lwd)){
     edge.lwd <- as.matrix.network.adjacency(x,attrname=edge.lwd)
     if(any(is.na(edge.lwd)))
       stop("Attribute",edge.lwd,"had illegal missing values or was not present in plot.graph.default.")
   }
   if(is.character(edge.curve)){
     edge.curve <- as.matrix.network.adjacency(x,attrname=edge.curve)
     if(any(is.na(edge.curve)))
       stop("Attribute",edge.curve,"had illegal missing values or was not present in plot.graph.default.")
   }
   #Coerce edge properties to appropriate forms
   if(!is.array(edge.col))   #Coerce edge.col/lty/lwd/curve to array form
     edge.col<-rep(edge.col,length.out=nedge)
   if(!is.array(edge.lty))
     edge.lty<-rep(edge.lty,length.out=nedge)
   if(!is.array(edge.lwd)){
    if(edge.lwd>0)
     edge.lwd<-rep(edge.lwd,length.out=nedge)
    else
     edge.lwd<-rep(1,length.out=nedge)
   }
   if(!is.array(edge.curve)){
    if(!is.null(edge.curve)) #If it's a scalar, multiply by edge str
      edge.curve<-rep(edge.curve,length.out=nedge)
    else
      edge.curve<-rep(0,length.out=nedge)
   }
   cloc<-cbind(cx,cy) #Get the inter-point distances for curves
#  dist<-as.matrix(dist(cbind(cx,cy))) #Get the inter-point distances for curves
#  tl<-d.raw*dist   #Get rescaled edge lengths
#  tl.max<-max(tl)  #Get maximum edge length   
   distij <- array(cloc[as.vector(edgelist),],dim=c(n,2,2))
   distij <- sqrt(apply((distij[,1,]-distij[,2,])^2,1,sum))
   tl.max<-max(distij)  #Get maximum edge length   
   for(k in 1:nrow(edgelist)){    #Plot edges for displayed vertices
     i <- edgelist[k,1]
     j <- edgelist[k,2]
     if(use[i] && use[j]){ #Perform for actually existing edges
         px0<-c(px0,as.real(cx[i]))  #Store endpoint coordinates
         py0<-c(py0,as.real(cy[i]))
         px1<-c(px1,as.real(cx[j]))
         py1<-c(py1,as.real(cy[j]))
         e.toff<-c(e.toff,vertex.radius[i]) #Store endpoint offsets
         e.hoff<-c(e.hoff,vertex.radius[j])
         e.col<-c(e.col,edge.col[k])    #Store other edge attributes
         e.type<-c(e.type,edge.lty[k])
         e.lwd<-c(e.lwd,edge.lwd[k])
         e.diag<-c(e.diag,i==j)  #Is this a loop?
         e.rad<-c(e.rad,vertex.radius[i]*loop.cex[i])
         if(uselen){   #Should we base curvature on interpoint distances?
           if(distij[k]>0){ 
#            e.len<-distij[k]*tl.max/tl[i,j] MSH
             e.len<-tl.max
             e.curv<-c(e.curv,edge.len*sqrt((e.len/2)^2-(distij[k]/2)^2))
           }else{      
             e.curv<-c(e.curv,0)   
           }
         }else{        #Otherwise, use prespecified edge.curve
           e.curv<-c(e.curv,edge.curve[k])
         }
       }
   }
   #Plot loops for the diagonals, if diag==TRUE, rotating wrt center of mass
   if(diag&&(length(px0)>0)&&sum(e.diag>0)){  #Are there any loops present?
     network.loop(as.vector(px0)[e.diag],as.vector(py0)[e.diag], length=1.5*baserad*arrowhead.cex,angle=25,width=e.lwd[e.diag]*baserad/10,col=e.col[e.diag],border=e.col[e.diag],lty=e.type[e.diag],offset=e.hoff[e.diag],edge.steps=loop.steps,radius=e.rad[e.diag],arrowhead=usearrows,xctr=mean(cx[use]),yctr=mean(cy[use]))
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
     if(length(px0)>0)
       network.arrow(as.vector(px0),as.vector(py0),as.vector(px1), as.vector(py1),length=2*baserad*arrowhead.cex,angle=20,col=e.col,border=e.col, lty=e.type,width=e.lwd*baserad/10,offset.head=e.hoff,offset.tail=e.toff, arrowhead=usearrows)
   }else{   #Curved edge case
     if(length(px0)>0){
       network.arrow(as.vector(px0),as.vector(py0),as.vector(px1), as.vector(py1),length=2*baserad*arrowhead.cex,angle=20,col=e.col,border=e.col, lty=e.type,width=e.lwd*baserad/10,offset.head=e.hoff,offset.tail=e.toff, arrowhead=usearrows,curve=e.curv,edge.steps=edge.steps)
     }
   }
   #Plot vertices now, if we haven't already done so
   if(vertices.last)
     network.vertex(cx[use],cy[use],radius=vertex.radius[use], sides=vertex.sides[use],col=vertex.col[use],border=vertex.border[use],lty=vertex.lty[use],rot=vertex.rot[use])
   #Plot vertex labels, if needed
   if(displaylabels&(!all(label==""))&(!all(use==FALSE))){
     if (label.pos==0){
       xoff <- cx[use]-mean(cx[use])
       yoff <- cy[use]-mean(cy[use])
       roff <- sqrt(xoff^2+yoff^2)
       xhat <- xoff/roff
       yhat <- yoff/roff
     } else if (label.pos<5) {
       xhat <- switch(label.pos,0,-1,0,1)
       yhat <- switch(label.pos,-1,0,1,0)
     } else {
       xhat <- 0
       yhat <- 0
     }
     os<-par()$cxy*label.cex
     lw<-strwidth(label[use],cex=label.cex)/2
     lh<-strheight(label[use],cex=label.cex)/2
     if(boxed.labels){
       rect(cx[use]-lw*(1+label.pad)+xhat*(lw*(1+label.pad+0.2)+ vertex.radius[use]),
            cy[use]-lh*(1+label.pad)+yhat*(lh*(1+label.pad+0.2)+ vertex.radius[use]),
            cx[use]+lw*(1+label.pad)+xhat*(lw*(1+label.pad+0.2)+ vertex.radius[use]),
            cy[use]+lh*(1+label.pad)+yhat*(lh*(1+label.pad+0.2)+ vertex.radius[use]),
            col=label.bg,border=label.border,lty=label.lty,lwd=label.lwd)
     }
     text(cx[use]+xhat*(lw*(1+label.pad+0.2)+vertex.radius[use]),
          cy[use]+yhat*(lh*(1+label.pad+0.2)+vertex.radius[use]),
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
       return(eval(cl))     #Execute the function and return
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
       return(eval(cl))     #Execute the function and return
     }
   }
   #Return the vertex positions, should they be needed
   invisible(cbind(cx,cy))
}
