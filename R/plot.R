######################################################################
#
# plot.R
#
# Written by Carter T. Butts <buttsc@uci.edu>; portions contributed by
# David Hunter <dhunter@stat.psu.edu> and Mark S. Handcock
# <handcock@u.washington.edu>.
#
# Last Modified 02/26/13
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
network.vertex<-function(x,y,radius=1,sides=4,border=1,col=2,lty=NULL,rot=0,lwd=1,...){
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
  lwd<-rep(lwd,length=n)
  #Obtain the coordinates
  coord<-vector()
  for(i in 1:length(x)) {
    coord<-make.coords(x[i],y[i],radius[i],sides[i],rot[i])
    polygon(coord,border=border[i],col=col[i],lty=lty[i],lwd=lwd[i], ...)
  }
  #Plot the polygons
  
}

# draw a label for a network edge
network.edgelabel<-function(px0,py0,px1,py1,label,directed,loops=FALSE,cex,...){
  posl<-rep(0,length(label))
  offsets<-rep(0.1,length(label))
    if (loops){
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
      
    } else {
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
      # compute crude offset so that label doesn't land on line
      # todo, this doesn't work well on all edge orientations
      posl[(px0>px1) & (py0>py1)]<-1
      posl[(px0<=px1) & (py0<=py1)]<-3
      posl[(px0>px1) & (py0<=py1)]<-2
      posl[(px0<=px1) & (py0>py1)]<-4
      
  } # end non-loop case. 
    text(lpx,lpy,labels=label,cex=cex,pos=posl,offset=offsets,...)
}


#Generic plot.network method. 
plot.network <- function(x, ...){
  plot.network.default(x, ...)
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
   expstate<-options()$expression
   on.exit(options(locatorBell=bellstate,expression=expstate))
   options(locatorBell=FALSE,expression=Inf)
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
   #Fill out vertex vectors; assume we're using attributes if chars used
   if(is.character(vertex.cex)&&(length(vertex.cex==1))){
     temp<-vertex.cex
     vertex.cex <- rep(get.vertex.attribute(x,vertex.cex),length=n)
     if(all(is.na(vertex.cex)))
       stop("Attribute '",temp,"' had illegal missing values for vertex.cex or was not present in plot.network.default.")
   }else
     vertex.cex <- rep(vertex.cex,length=n)
   vertex.radius<-rep(baserad*vertex.cex,length=n)   #Create vertex radii
   if(is.character(vertex.sides)&&(length(vertex.sides==1))){
     temp<-vertex.sides
     vertex.sides <- rep(get.vertex.attribute(x,vertex.sides),length=n)
     if(all(is.na(vertex.sides)))
       stop("Attribute '",temp,"' had illegal missing values for vertex.sides or was not present in plot.network.default.")
   }else
     vertex.sides <- rep(vertex.sides,length=n)
   if(is.character(vertex.border)&&(length(vertex.border)==1)){
     temp<-vertex.border
     vertex.border <- rep(get.vertex.attribute(x,vertex.border),length=n)
     if(all(is.na(vertex.border)))
       vertex.border <- rep(temp,length=n) #Assume it was a color word
     else{
       if(!all(is.color(vertex.border),na.rm=TRUE))
         vertex.border<-as.color(vertex.border)
     }
   }else
     vertex.border <- rep(vertex.border,length=n)
   if(is.character(vertex.col)&&(length(vertex.col)==1)){
     temp<-vertex.col
     vertex.col <- rep(get.vertex.attribute(x,vertex.col),length=n)
     if(all(is.na(vertex.col)))
       vertex.col <- rep(temp,length=n) #Assume it was a color word
     else{
       if(!all(is.color(vertex.col),na.rm=TRUE))
         vertex.col<-as.color(vertex.col)
     }
   }else
     vertex.col <- rep(vertex.col,length=n)
   if(is.character(vertex.lty)&&(length(vertex.lty)==1)){
     temp<-vertex.lty
     vertex.lty <- rep(get.vertex.attribute(x,vertex.lty),length=n)
     if(all(is.na(vertex.lty)))
       stop("Attribute '",temp,"' had illegal missing values for vertex.col or was not present in plot.network.default.")
   }else
     vertex.lty <- rep(vertex.lty,length=n)
   if(is.character(vertex.rot)&&(length(vertex.rot)==1)){
     temp<-vertex.rot
     vertex.rot <- rep(get.vertex.attribute(x,vertex.rot),length=n)
     if(all(is.na(vertex.rot)))
       stop("Attribute '",temp,"' had illegal missing values for vertex.rot or was not present in plot.network.default.")
   }else
     vertex.rot <- rep(vertex.rot,length=n)
   
   if(is.character(vertex.lwd)&&(length(vertex.lwd)==1)){
     temp<-vertex.lwd
     vertex.lwd <- rep(get.vertex.attribute(x,vertex.lwd),length=n)
     if(all(is.na(vertex.lwd)))
       stop("Attribute '",temp,"' had illegal missing values for vertex.lwd or was not present in plot.network.default.")
   }else
     vertex.lwd <- rep(vertex.lwd,length=n)
   
   if(is.character(loop.cex)&&(length(loop.cex)==1)){
     temp<-loop.cex
     loop.cex <- rep(get.vertex.attribute(x,loop.cex),length=n)
     if(all(is.na(loop.cex)))
       stop("Attribute ",temp," had illegal missing values for loop.cex or was not present in plot.network.default.")
   }else
     loop.cex <- rep(loop.cex,length=n)
   if(is.character(label.col)&&(length(label.col)==1)){
     temp<-label.col
     label.col <- rep(get.vertex.attribute(x,label.col),length=n)
     if(all(is.na(label.col)))
       label.col <- rep(temp,length=n) #Assume it was a color word
     else{
       if(!all(is.color(label.col),na.rm=TRUE))
         label.col<-as.color(label.col)
     }
   }else
     label.col <- rep(label.col,length=n)
   if(is.character(label.border)&&(length(label.border)==1)){
     temp<-label.border
     label.border <- rep(get.vertex.attribute(x,label.border),length=n)
     if(all(is.na(label.border)))
       label.border <- rep(temp,length=n) #Assume it was a color word
     else{
       if(!all(is.color(label.border),na.rm=TRUE))
         label.border<-as.color(label.border)
     }
   }else
     label.border <- rep(label.border,length=n)
   if(is.character(label.bg)&&(length(label.bg)==1)){
     temp<-label.bg
     label.bg <- rep(get.vertex.attribute(x,label.bg),length=n)
     if(all(is.na(label.bg)))
       label.bg <- rep(temp,length=n) #Assume it was a color word
     else{
       if(!all(is.color(label.bg),na.rm=TRUE))
         label.bg<-as.color(label.bg)
     }
   }else
     label.bg <- rep(label.bg,length=n)
   #Plot vertices now, if desired
   if(!vertices.last)
     network.vertex(cx[use],cy[use],radius=vertex.radius[use], sides=vertex.sides[use],col=vertex.col[use],border=vertex.border[use],lty=vertex.lty[use],rot=vertex.rot[use], lwd=vertex.lwd[use])
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
   if(NROW(d)>0){
     #Edge color
     if(length(dim(edge.col))==2)   #Coerce edge.col/edge.lty to vector form
       edge.col<-edge.col[d[,1:2]]
     else if(is.character(edge.col)&&(length(edge.col)==1)){
       temp<-edge.col
       edge.col<-x%e%edge.col
       if(!is.null(edge.col)){
         edge.col<-edge.col[edgetouse]
         if(!all(is.color(edge.col),na.rm=TRUE))
           edge.col<-as.color(edge.col)
       }else
         edge.col<-rep(temp,length=NROW(d))  #Assume it was a color word
     }else
       edge.col<-rep(edge.col,length=NROW(d))
     #Edge line type
     if(length(dim(edge.lty))==2)
       edge.lty<-edge.lty[d[,1:2]]
     else if(is.character(edge.lty)&&(length(edge.lty)==1)){
       temp<-edge.lty
       edge.lty<-(x%e%edge.lty)[edgetouse]
       if(all(is.na(edge.lty)))
         stop("Attribute '",temp,"' had illegal missing values for edge.lty or was not present in plot.network.default.")
     }else
       edge.lty<-rep(edge.lty,length=NROW(d))
     #Edge line width
     if(length(dim(edge.lwd))==2){
       edge.lwd<-edge.lwd[d[,1:2]]
       e.lwd.as.mult<-FALSE
     }else if(is.character(edge.lwd)&&(length(edge.lwd)==1)){
       temp<-edge.lwd
       edge.lwd<-(x%e%edge.lwd)[edgetouse]
       if(all(is.na(edge.lwd)))
         stop("Attribute '",temp,"' had illegal missing values for edge.lwd or was not present in plot.network.default.")
       e.lwd.as.mult<-FALSE
     }else{ 
       if(length(edge.lwd)==1)
         e.lwd.as.mult<-TRUE
       else
         e.lwd.as.mult<-FALSE
       edge.lwd<-rep(edge.lwd,length=NROW(d))
     }
     #Edge curve
     if(!is.null(edge.curve)){
       if(length(dim(edge.curve))==2){
         edge.curve<-edge.curve[d[,1:2]]
         e.curv.as.mult<-FALSE
       }else{ 
         if(length(edge.curve)==1)
           e.curv.as.mult<-TRUE
         else
           e.curv.as.mult<-FALSE
         edge.curve<-rep(edge.curve,length=NROW(d))
       }
     }else if(is.character(edge.curve)&&(length(edge.curve)==1)){
       temp<-edge.curve
       edge.curve<-(x%e%edge.curve)[edgetouse]
       if(all(is.na(edge.curve)))
         stop("Attribute '",temp,"' had illegal missing values for edge.curve or was not present in plot.network.default.")
       e.curv.as.mult<-FALSE
     }else{
       edge.curve<-rep(0,length=NROW(d))
       e.curv.as.mult<-FALSE
     }
     # only evaluate edge label stuff if we will draw label
     if(!is.null(edge.label)){
       #Edge label
       if(length(dim(edge.label))==2){   #Coerce edge.label to vector form
         edge.label<-edge.label[d[,1:2]]
       }else if(is.character(edge.label)&&(length(edge.label)==1)){
         temp<-edge.label
         edge.label<-x%e%edge.label
         if(!is.null(edge.label)){
           edge.label<-edge.label[edgetouse]
         }else
           edge.label<-rep(temp,length=NROW(d))  #Assume it was a value to replicate
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
         edge.label<-rep(edge.label,length=NROW(d))
       }
       #Edge label color
       if(length(dim(edge.col))==2)   #Coerce edge.label.col
         edge.label.col<-edge.label.col[d[,1:2]]
       else if(is.character(edge.label.col)&&(length(edge.label.col)==1)){
         temp<-edge.label.col
         edge.label.col<-x%e%edge.label.col
         if(!is.null(edge.label.col)){
           edge.label.col<-edge.label.col[edgetouse]
           if(!all(is.color(edge.label.col),na.rm=TRUE))
             edge.label.col<-as.color(edge.label.col)
         }else
           edge.label.col<-rep(temp,length=NROW(d))  #Assume it was a color word
       }else
         edge.label.col<-rep(edge.label.col,length=NROW(d))
       #Edge label cex
       if(length(dim(edge.label.cex))==2)
         edge.label.cex<-edge.label.cex[d[,1:2]]
       else if(is.character(edge.label.cex)&&(length(edge.label.cex)==1)){
         temp<-edge.label.cex
         edge.label.cex<-(x%e%edge.label.cex)[edgetouse]
         if(all(is.na(edge.label.cex)))
           stop("Attribute '",temp,"' had illegal missing values for edge.label.cex or was not present in plot.network.default.")
       }else
         edge.label.cex<-rep(edge.label.cex,length=NROW(d))
     } # end edge label block
     
     #Proceed with edge setup
     dist<-((cx[d[,1]]-cx[d[,2]])^2+(cy[d[,1]]-cy[d[,2]])^2)^0.5  #Get the inter-point distances for curves
     tl<-d.raw*dist   #Get rescaled edge lengths
     tl.max<-max(tl)  #Get maximum edge length
     for(i in 1:NROW(d))
       if(use[d[i,1]]&&use[d[i,2]]){  #Plot edges for displayed vertices
         px0<-c(px0,as.double(cx[d[i,1]]))  #Store endpoint coordinates
         py0<-c(py0,as.double(cy[d[i,1]]))
         px1<-c(px1,as.double(cx[d[i,2]]))
         py1<-c(py1,as.double(cy[d[i,2]]))
         e.toff<-c(e.toff,vertex.radius[d[i,1]]) #Store endpoint offsets
         e.hoff<-c(e.hoff,vertex.radius[d[i,2]])
         e.col<-c(e.col,edge.col[i])    #Store other edge attributes
         e.type<-c(e.type,edge.lty[i])
         if(edge.lwd[i]>0){
           if(e.lwd.as.mult)
             e.lwd<-c(e.lwd,edge.lwd[i]*d.raw[i,3])
           else
             e.lwd<-c(e.lwd,edge.lwd[i])
         }else
           e.lwd<-c(e.lwd,1)
         e.diag<-c(e.diag,d[i,1]==d[i,2])  #Is this a loop?
         e.rad<-c(e.rad,vertex.radius[d[i,1]]*loop.cex[d[i,1]])
         if(uselen){   #Should we base curvature on interpoint distances?
           if(tl[i]>0){ 
             e.len<-dist[i]*tl.max/tl[i]
             e.curv<-c(e.curv,edge.len*sqrt((e.len/2)^2-(dist[i]/2)^2))
           }else{
             e.curv<-c(e.curv,0)   
           }
         }else{        #Otherwise, use prespecified edge.curve
           if(e.curv.as.mult)    #If it's a scalar, multiply by edge str
             e.curv<-c(e.curv,edge.curve[i]*d.raw[i])
           else
             e.curv<-c(e.curv,edge.curve[i])
         }
       }
     }
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
     if(length(px0)>0)
       network.arrow(as.vector(px0),as.vector(py0),as.vector(px1), as.vector(py1),length=2*baserad*arrowhead.cex,angle=20,col=e.col,border=e.col,lty=e.type,width=e.lwd*baserad/10,offset.head=e.hoff,offset.tail=e.toff,arrowhead=usearrows)
     if(!is.null(edge.label)){
       network.edgelabel(px0,py0,px1,py1,edge.label[!e.diag],directed=is.directed(x),cex=edge.label.cex[!e.diag],col=edge.label.col[!e.diag])
     }
   }else{   #Curved edge case
     if(length(px0)>0){
       network.arrow(as.vector(px0),as.vector(py0),as.vector(px1), as.vector(py1),length=2*baserad*arrowhead.cex,angle=20,col=e.col,border=e.col,lty=e.type,width=e.lwd*baserad/10,offset.head=e.hoff,offset.tail=e.toff,arrowhead=usearrows,curve=e.curv,edge.steps=edge.steps)
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
           if (rhat[i]!=0) { # normalize direction vector
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
         if (xhat[i]==0 | is.nan(xhat[i])) xhat[i] <- .01 #jitter to avoid labels on points
         if (yhat[i]==0 | is.nan(yhat[i])) yhat[i] <- .01
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
     os<-par()$cxy*label.cex
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
