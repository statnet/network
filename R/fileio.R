######################################################################
#
# fileio.R
#
# Written by Carter T. Butts <buttsc@uci.edu>; portions contributed by
# David Hunter <dhunter@stat.psu.edu> and Mark S. Handcock
# <handcock@u.washington.edu>.
#
# Last Modified 9/05/10
# Licensed under the GNU General Public License version 2 (June, 1991)
#
# Part of the R/network package
#
# This file contains various routines related to reading/writing network
# objects from external files.
#
# Contents:
#
# read.paj
# read.paj.simplify
# readAndVectorizeLine
# switchArcDirection
#
######################################################################


#Read an input file in Pajek format
read.paj <- function(file,verbose=FALSE,debug=FALSE,
                    edge.name=NULL, simplify=FALSE)
 {

   fileNameParts0 <- strsplit(file,"/")[[1]]
   fileNameParts1 <- strsplit(fileNameParts0[length(fileNameParts0)],"\\.")[[1]]
   fileName <- paste(fileNameParts1[1:length(fileNameParts1)-1],collapse=".")
   fileExt <- fileNameParts1[length(fileNameParts1)]  #should be "net" or "paj"
   isURL <- regexpr("http",file)>0

   if (is.character(file)) {
       file <- file(file, "rt")
       on.exit(close(file))
   }
   if (!inherits(file, "connection"))
       stop("argument 'file' must be a character string or connection")
   if (!isOpen(file)) {
       open(file, "rt")
       on.exit(close(file))
   }


   nnetworks <- 0               #in paramter list of  projectizeNetworks()
   network.names <- NULL        #in paramter list of  projectizeNetworks()

   vertex <- NULL               #in paramter list of  projectizeNetworks()
   nvertex <- 0                 #in paramter list of  projectizeNetworks()
   network.title <- fileName    #"network"#in paramter list of  projectizeNetworks()

   partition <- NULL
   names.partition <- NULL

   vector <- NULL               #in paramter list of  projectizeNetworks()
   colnames.vector <- NULL      #in paramter list of  projectizeNetworks()

   projects <- NULL             #in paramter list of  projectizeNetworks()
   nprojects <- 0               #in paramter list of  projectizeNetworks()
   names.projects <- NULL       #in paramter list of  projectizeNetworks()


   nextline <- TRUE

   line <- " "
   is2mode <- FALSE #for two-mode data


   nevents <- 0   #for two-mode data
   nactors <- 0   #for two-mode data

#
  while(!inherits(line,"try-error")){
   while(any(grep("^%", line)) | nextline){
     if(debug) print("nextline called... new loop started")
     options(show.error.messages=FALSE)
     line <- try(readLines(file, 1, ok = FALSE))
     options(show.error.messages=TRUE)
     if(!inherits(line,"try-error") & length(line)>0){
      line <- strsplit(line, " ")[[1]]
      line <- line[line!=""]
     }
     nextline <- FALSE
   }
   nextline <- TRUE
#       if(verbose) warning(paste("afterbeingWhileLoop",line))


#
#   Network specification
#
   if(any(grep("\\*Network", line, ignore.case = TRUE))){

     network.title <- paste(line[-1],collapse=" ")
     previousDyads <- NULL  #used for arc+edge specified networks...   reset to null for every new network.. not really necessary here

 if(is.null(network.title)) network.title <- network.name

 if(debug) print(paste("nnetworks=",nnetworks))
 if(debug) print(paste("network.names=",network.names))
 if(debug) print(paste("vertex null?",is.null(vertex)))
 if(debug) print(paste("network.title=",network.title))
 if(debug) print(paste("vector null?",is.null(vector)))
 if(debug) print(paste("colnames.vector=",colnames.vector))
 if(debug) print(paste("projects null?",is.null(projects)))
 if(debug) print(paste("nprojects=",nprojects))
 if(debug) print(paste("names.projects=",names.projects))

 if(verbose) print(paste("number of networks",nnetworks))  #dschruth added

   if(nnetworks > 0){
     colnames(vector) <- colnames.vector
     colnames(vertex) <- c("vertex.numbers","vertex.names","cen1","cen2")[1:ncol(vertex)]
     networks <- vector("list",length=nnetworks)
     for(i in seq(along=network.names)){
       if(verbose) print(paste("working along network names",network.names))
       temp <- get(network.names[i])
       if(!is.null(vertex)){   #Changed "vector" to "vertex"
          ##Start new addition from Alex Montgomery (Thanks AHM!)
          if (nrow(as.data.frame(vertex)) == network.size(temp)) {
              temp <- set.vertex.attribute(temp, "vertex.names",
                as.character(vertex[as.numeric(vertex[,1]),2]))
              if (ncol(vertex)>2) { # number of columns > 2 -> vertex has attributes
                vert.attr.nam <- c("na","vertex.names","x","y","z") #assume first three are coords (true?)
                if (ncol(vertex)>5) vert.attr.nam <- c(vert.attr.nam,6:ncol(vertex)) #temp names for rest
                for (vert.attr.i in 3:ncol(vertex)){
                  v <- vertex[,vert.attr.i]
                  if (is.factor(v)){ # if it's a factor, then
                    vert.attr.nam.tmp <- levels(v)[1] # see if the first factor is an attribute name
                    if (vert.attr.nam.tmp=="") vert.attr.nam.tmp <- levels(v)[2] # in case of missing data
                    if (nlevels(v)<=2&!is.na(match(vert.attr.nam.tmp, # check for match if # factors <=2
                                 c("s_size","x_fact","y_fact","phi","r","q",
                                   "ic","bc","bw","lc","la","lr",
                                   "lphi","fos","font")))) { #from pajekman.pdf v1.2.3 p.69-70
                      vert.attr.nam[vert.attr.i+1] <- vert.attr.nam.tmp #if match, name the next column
                    } else { #if not, set the attribute, converting to character (networks incompat w/factors)
                      temp <- set.vertex.attribute(temp,vert.attr.nam[vert.attr.i],
                                                   as.character(vertex[as.numeric(vertex[,1]),vert.attr.i]))
                    }
                  } else { #not a factor, set the attribute and don't convert to character
                    temp <- set.vertex.attribute(temp,vert.attr.nam[vert.attr.i],
                                                 vertex[as.numeric(vertex[,1]),vert.attr.i])
                  }
                }
              }
             if (debug)
                print("set vertex names to matrix")
          }
          ##End AHM addition
##Original code from Dave Schruth:
##          if(nrow(as.data.frame(vector))== network.size(temp)) {#should i be doing this? why don't these numbers match all time
##            temp <- set.vertex.attribute(temp, vector.name , value=as.matrix(vector))
##                   #set.vertex.attribute(x   , attrname    , value,      v=1:network.size(x))
##            if(debug) print("set vector to network")
##          }else{
##            warning(paste("vectorLength (",nrow(as.data.frame(vector)),") != number of nodes (",temp$gal$n,"), vertex attribute not set",sep=""))
##           #dschruth added... crashing on Scotland.paj vector length != numOfEdges (http://vlado.fmf.uni-lj.si/pub/networks/data/esna/scotland.htm)
##          }
       }
       if(!is.null(network.title)){
         temp <- set.network.attribute(temp, "title", network.title)
         if(debug) print("set network title")
       }else{
         warning("null network title")
       }
       if(nrow(as.data.frame(vertex))== network.size(temp)){ #should i be doing this? why don't these numbers match all time
         temp <- set.vertex.attribute(temp,"vertex.names",as.character(vertex[as.numeric(vertex[,1]),2]))
         if(debug) print("set vertex names to matrix")
       }
       networks[[i]] <- temp
       if (debug) print("added  network to list of networks")
     }
     names(networks) <- network.names
     if(nnetworks > 1){
       networks <- list(formula = ~1, networks = networks,
                      stats = numeric(0),coef=0)
       class(networks) <- "network.series"
     }else{
       networks  <- networks[[1]]
       class(networks) <- "network"
#        networks <- set.network.attribute(networks, "title",)
     }
     names.projects <- c(names.projects, network.title)
     nprojects <- nprojects+1
     projects <- c(projects,list(networks))
   }




     network.names <- NULL
     nnetworks <- 0
     vector <- NULL
     colnames.vector <- NULL
     nextline <- TRUE
   }

#
#   vertices specification
#
   if(any(grep("\\*Vertices", line, ignore.case = TRUE))){

     previousDyads <- NULL  #used for arc+edge specified networks.... reset to null for every new network.. might be sufficient here
     nvertex <- as.numeric(line[2])

     if(!is.na(line[3])){                                        #dschruth added for two-mode
       is2mode <- TRUE                    #used in matrix below  #dschruth added for two-mode
       nactors <- as.numeric(line[3])     #used for error check  #dschruth added for two-mode
       nevents <- nvertex-nactors         #used for error check  #dschruth added for two-mode
     }                                                           #dschruth added for two-mode
     if(!isURL)
     preReadTablePosition <- seek(file,where=NA)

#     if(network.title =="SanJuanSur_deathmessage.net")  #read.third paragraph in details of documentation of read table about how it determines the number of columns in the first 5 lines...
#       vertex <- read.table(file,skip=-1,nrows=nvertex,col.names=1:8,comment.char="%",fill=TRUE,as.is=FALSE)  #dschruth added 'comment.char="%"' and 'fill=TRUE'
#     else
       vertex <- read.table(file,skip=-1,nrows=nvertex,              comment.char="%",fill=TRUE,as.is=FALSE)  #dschruth added 'comment.char="%"' and 'fill=TRUE'
       if(ncol(vertex)==1){ vertex <- cbind(1:nrow(vertex),vertex)}


     #need to check to see if we are reading in more rows than there actually are (some edges are implied)
     edgelistPosition <-  grep("\\*(arcs|edges|matrix)",as.matrix(vertex),ignore.case=TRUE)

     if(any(edgelistPosition)){
       if(verbose) print("vertex list has missing entries or n was mis-specified, re-reading it...")
       if(isURL) stop("Resize of abbreviated vertex list via seek is not possible with URLs.  Try downloading file and loading locally")
      nVertexRows <- edgelistPosition-1
       dummyNotUsed <- seek(file,where=preReadTablePosition)  #reset the file position back to before the table was read
       vertex <- read.table(file,skip=-1,nrows=nVertexRows,comment.char="%",fill=TRUE,as.is=FALSE)  #dschruth added 'comment.char="%"' and 'fill=TRUE'
       if(ncol(vertex)==1){ vertex <- cbind(1:nrow(vertex),vertex)}
     }
     if(nvertex!=nrow(vertex)){
      if(verbose) print(paste("vertex list (length=",nrow(vertex),") is being re-sized to conform with specified network size (n=",nvertex,")",sep=""))
      colnames(vertex)[1:2] <- c("vn","name")
       vertex <- merge(data.frame(vn=1:nvertex),vertex,all.x=TRUE,all.y=FALSE,by.y="vn") #fill in the holes with NA names
     }
     if(verbose) print("vertex list set")

   }
#
#   partition specification
#
   if(any(grep("\\*Partition", line, ignore.case = TRUE))){

    partition.name <- as.character(paste(line[-1],collapse="."))
    names.partition <- c(names.partition,partition.name)
    line <- readAndVectorizeLine(file)

    while(any(grep("^%", line))){
      line <- readAndVectorizeLine(file)

    }
    nvertex <- as.numeric(line[2])
    if(is.null(partition)){
      partition <- read.table(file,skip=0,nrows=nvertex)
    }else{
      partition <- c(partition,
        read.table(file,skip=0,nrows=nvertex))
    }
    if(verbose) print("partition found and set")

   }
#
#   Vector specification
#
   if(any(grep("\\*Vector", line, ignore.case = TRUE))){

    vector.name <- as.character(paste(line[-1],collapse="."))
    colnames.vector <- c(colnames.vector,vector.name)
    line <- readAndVectorizeLine(file)

    while(any(grep("^%", line))){
      line <- readAndVectorizeLine(file)

    }
    nvertex <- as.numeric(line[2])
    if(is.null(vector)){
      vector <- read.table(file,skip=0,nrows=nvertex)
    }else{
      vector <- data.frame(vector,
        read.table(file,skip=0,nrows=nvertex))
    }
    if(verbose) print("vector found and set")

   }


#
#   arcs specification
#
   arcsLinePresent <- edgesLinePresent <- FALSE
   arcsLinePresent <- any(grep("\\*Arcs", line, ignore.case = TRUE))#;print(paste("arcs?",arcsLinePresent))
   edgesLinePresent <- any(grep("\\*Edges", line, ignore.case = TRUE))#;print(paste("edges?",edgesLinePresent))
   # print(network.name)

   if(arcsLinePresent | edgesLinePresent){
     if(debug) print(paste("arc or edge lines present"))#,line)

     if(missing(edge.name)){
      if(length(line)>1){
       network.name <- strsplit(paste(line[3:length(line)],collapse="."),'\"')[[1]][2]  #dschruth added collapse to allow for multi work network names
      }else{
       network.name <- paste(network.title,nnetworks+1,sep="")
       #network.name <- network.title  #old way
      }
     }else{
       network.name <- edge.name
     }

     dyadList <- list() #dschruth changed (was NULL)
     listIndex <- 1     #dschruth added

     line <- readAndVectorizeLine(file)

     while(any(grep("^%", line))){
       line <- readAndVectorizeLine(file)
     }
     while(!any(grep("\\*[a-zA-Z]", line)) & length(line)>0){  #dschruth changed \\*  to \\*[a-zA-Z] to allow for time asterisks
       dyadList[[listIndex]] <- as.numeric(gsub("Newline","",line))        # dyads <- rbind(dyads, as.numeric(line[1:3]))  # this is the old way
       line <- readAndVectorizeLine(file)
       listIndex <- listIndex+1

     }     
     if(verbose) print(paste("length of dyad list",length(dyadList)))
     nextline <- FALSE                                            #dschruth added
     if(length(dyadList)>0){                                   #dschruth added

      ###    deal with the possible Ragged Array [RA] dyad list .. see  Lederberg.net  ###

       RAlengths <- unlist(lapply(dyadList,length))
       maxRAwidth <- max(RAlengths)


       dyadsHaveAttributes <- any(is.na(unlist(dyadList))) #  handling  edge attributes (NAs introduced by coersion)
       if(dyadsHaveAttributes) warning(paste("don't worry about these",length(dyadList),"warnings,the dyads have attributes and were NA'ed during as.numeric() call. \n the actual dyad matrix width is only 2 "))

       if(maxRAwidth > 4 & !dyadsHaveAttributes){# #needs to be 4 because of normal edgelist can have sender reciever weight and time
         if(verbose)print("stacking ragged dyad array")
         dyads0 <- unlist(lapply(dyadList, function(x)  c(x, rep(NA, maxRAwidth - length(x)))))
         dyads1 <- data.frame(matrix(dyads0,nrow=length(dyadList),ncol=maxRAwidth,byrow=TRUE))

         colnames(dyads1) <- c("sender","receiver",paste("r",seq(3,maxRAwidth),sep=""))

         dyads2 <- reshape(dyads1,idvar="senderNo",ids=row.names(dyads1),direction="long",
                           times=names(dyads1)[-1],timevar="receiverNo",
                           varying=list(names(dyads1)[-1]))

         dyads <- as.matrix(dyads2[!is.na(dyads2$receiver),c("sender","receiver")])

         if(verbose)print("finished stacking ragged dyad array")
       }else{
         if(debug) print("unlisting dyad list to matrix")
         dyads <- matrix(unlist(lapply(dyadList,function(x) x[1:3])),length(dyadList),3,byrow=TRUE)
       }
      ### done dealing with RA possiblity ###  all written by dschruth

    #  dyads <- as.numeric(dyads)
       if(debug) print(paste("isnull previous dyads?: ",is.null(previousDyads)))

       if(is.null(previousDyads)){ #first time through (always an arc list?)
         nnetworks <- nnetworks + 1
         network.names <- c(network.names, network.name)
         previousDyads <- dyads
         directed <- arcsLinePresent
       }else{ #second time through (always an edge list?)
         if(verbose) print(paste("previous dyads exist!!   symmetrizing edges and combining with arcs"))
         if(arcsLinePresent){
           previousDyads.flipped <- switchArcDirection(previousDyads)
           dyads <- rbind(previousDyads,previousDyads.flipped,dyads)
         }else{
           dyads.flipped <- switchArcDirection(dyads)
           dyads <- rbind(dyads,dyads.flipped,previousDyads)
         }
         directed <- TRUE #arcsLinePresent <- TRUE
         previousDyads <- NULL
       }

       if((max(dyads[,1:2]) > nvertex) | nrow(dyads)==1){  # nrow(dyads)==1 is for C95.net
         if(verbose) print("edge end out of range, skipping network creation")
         if(verbose) print("first dyad list (arcs?), is too short to be a full network, skipping to next dyad list (edges?)")
       }else{
         if(is2mode){
           temp <- network.initialize(n=nvertex, directed=directed,
                                      bipartite=nactors)
         }else{
           temp <- network.initialize(n=nvertex, directed=directed)
         }
         add.edges(temp,tail=dyads[,1],head=dyads[,2])
#          temp <- network(x=dyads[,1:2],directed=directed)#arcsLinePresent)#dschruth added
#         temp <- set.edge.value(temp,"FALSE",NULL) #dschruth is this necessary??   should i comment out?
#         temp <- set.edge.value(temp,"NULL",NULL)  #dschruth is this necessary??   should i comment out?
         if(dim(dyads)[2]>2){  #only try to set the edge value if there is a third column
           temp <- set.edge.attribute(temp,network.names[nnetworks], dyads[,3])
           if(verbose) print("edge value created from edge/arc list")
         }
         assign(network.names[nnetworks], temp)
         rm(temp)
         if(verbose) print("network created from edge/arc list")
#        if(arcsLinePresent) nextline <- TRUE    #{ print(" 'arcs' line followed by dyads present... skip past the current 'edges' line");}
       }                                                           #dschruth added
     }


   }

#
#   matrix specification
#
   if(any(grep("\\*Matrix", line, ignore.case = TRUE))){
     if(verbose) print("found matrix")

     if(length(line)>1){
       network.name <- strsplit(line[3],'\"')[[1]][2]
     }else{
       network.name <- paste("network",nnetworks+1,sep="")
     }
     nnetworks <- nnetworks + 1
     network.names <- c(network.names, network.name)
     temp0 <- as.matrix(read.table(file,skip=0,nrows=nvertex,as.is=TRUE))
     lastColNum <- ncol(temp0)
     if(all(apply(temp0[,-lastColNum],1,sum)==temp0[,lastColNum])){
       if(verbose) print("removing final marginal sum column of matrix")
       temp0 <- temp0[,-lastColNum]
     }
     if(verbose) print(paste("dim1",dim(temp0)[1],"na",nactors,"dim2",dim(temp0)[2],"ne",nevents)) #checking
     if(is2mode & (dim(temp0)[1]!=nactors | dim(temp0)[2]!=nevents)){
       warning("error!, dimensions do not match bipartite specifications")
     }else{
       temp <- network(x=temp0,bipartite=is2mode)               #dschruth added "bipartate=is2mode" for two-mode
#       temp <- set.edge.attribute(temp,network.names[nnetworks],        dyads[,3])
               if(verbose) print("network created from matrix")
     }
     assign(network.names[nnetworks], temp)
     rm(temp)
   }
 }#end while loop


 if(is.null(network.title)) network.title <- network.name

 if(debug) print(paste("nnetworks=",nnetworks))
 if(debug) print(paste("network.names=",network.names))
 if(debug) print(paste("vertex null?",is.null(vertex)))
 if(debug) print(paste("network.title=",network.title))
 if(debug) print(paste("vector null?",is.null(vector)))
 if(debug) print(paste("colnames.vector=",colnames.vector))
 if(debug) print(paste("projects null?",is.null(projects)))
 if(debug) print(paste("nprojects=",nprojects))
 if(debug) print(paste("names.projects=",names.projects))

 if(verbose) print(paste("number of networks",nnetworks))  #dschruth added

   if(nnetworks > 0){
     colnames(vector) <- colnames.vector
     colnames(vertex) <- c("vertex.numbers","vertex.names","cen1","cen2")[1:ncol(vertex)]
     networks <- vector("list",length=nnetworks)
     for(i in seq(along=network.names)){
       if(verbose) print(paste("working along network names",network.names))
       temp <- get(network.names[i])
       if(!is.null(vertex)){  #Changed "vector" to "vertex"
          ##Start new addition from Alex Montgomery (Thanks AHM!)
          if (nrow(as.data.frame(vertex)) == network.size(temp)) {
              temp <- set.vertex.attribute(temp, "vertex.names",
                as.character(vertex[as.numeric(vertex[,1]),2]))
              if (ncol(vertex)>2) { # number of columns > 2 -> vertex has attributes
                vert.attr.nam <- c("na","vertex.names","x","y","z") #assume first three are coords (true?)
                if (ncol(vertex)>5) vert.attr.nam <- c(vert.attr.nam,6:ncol(vertex)) #temp names for rest
                for (vert.attr.i in 3:ncol(vertex)){
                  v <- vertex[,vert.attr.i]
                  if (is.factor(v)){ # if it's a factor, then
                    vert.attr.nam.tmp <- levels(v)[1] # see if the first factor is an attribute name
                    if (vert.attr.nam.tmp=="") vert.attr.nam.tmp <- levels(v)[2] # in case of missing data
                    if (nlevels(v)<=2&!is.na(match(vert.attr.nam.tmp, # check for match if # factors <=2
                                 c("s_size","x_fact","y_fact","phi","r","q",
                                   "ic","bc","bw","lc","la","lr",
                                   "lphi","fos","font")))) { #from pajekman.pdf v1.2.3 p.69-70
                      vert.attr.nam[vert.attr.i+1] <- vert.attr.nam.tmp #if match, name the next column
                    } else { #if not, set the attribute, converting to character (networks incompat w/factors)
                      temp <- set.vertex.attribute(temp,vert.attr.nam[vert.attr.i],
                                                   as.character(vertex[as.numeric(vertex[,1]),vert.attr.i]))
                    }
                  } else { #not a factor, set the attribute and don't convert to character
                    temp <- set.vertex.attribute(temp,vert.attr.nam[vert.attr.i],
                                                 vertex[as.numeric(vertex[,1]),vert.attr.i])
                  }
                }
              }
             if (debug)
                print("set vertex names to matrix")
          }
          ##End AHM addition
##Original by Dave Schruth:
##          if(nrow(as.data.frame(vector))== network.size(temp)) {#should i be doing this? why don't these numbers match all time
##            temp <- set.vertex.attribute(temp, vector.name , value=as.data.frame(vector))
##                   #set.vertex.attribute(x   , attrname    , value,      v=1:network.size(x))
##            if(debug) print("set vector to network")
##          }else{
##            warning(paste("vectorLength (",nrow(as.data.frame(vector)),") != number of nodes (",temp$gal$n,"), vertex attribute not set",sep=""))
##           #dschruth added... crashing on Scotland.paj vector length != numOfEdges (http://vlado.fmf.uni-lj.si/pub/networks/data/esna/scotland.htm)
##          }
       }
       if(!is.null(network.title)){
         temp <- set.network.attribute(temp, "title", network.title)
         if(debug) print("set network title")
       }else{
         warning("null network title")
       }
       if(nrow(as.data.frame(vertex))== network.size(temp)){ #should i be doing this? why don't these numbers match all time
         temp <- set.vertex.attribute(temp,"vertex.names",as.character(vertex[as.numeric(vertex[,1]),2]))
         if(debug) print("set vertex names to matrix")
       }

       networks[[i]] <- temp
       if (debug) print("added  network to list of networks")
     }
     names(networks) <- network.names
     if(nnetworks > 1){
       networks <- list(formula = ~1, networks = networks,
                      stats = numeric(0),coef=0)
       class(networks) <- "network.series"
     }else{
       networks  <- networks[[1]]
       class(networks) <- "network"
#        networks <- set.network.attribute(networks, "title",)
     }
     names.projects <- c(names.projects, network.title)
     nprojects <- nprojects+1
     projects <- c(projects,list(networks))
   }



   if(!is.null(projects)) #dschruth added
     names(projects) <- names.projects
   if(is.null(partition)){
     if(verbose) print(paste("number of projects",nprojects))  #dschruth added
     if(nprojects==1)
       projects <- projects[[1]]
     if(nnetworks>1){
       class(projects) <- "network.series"
     }
   }else{
     names(partition) <- names.partition
     projects <- list(networks=projects, partitions=partition)
   } #end ifelse
#
#   Simplify
#
   if(is.logical(simplify)){
    if(simplify){
     simplify <- fileName
    }else{
     return(projects)
    }
   }
   read.paj.simplify(x=projects,file=simplify,verbose=verbose)
 } #end read.paj



read.paj.old <- function(file,verbose=FALSE,debug=FALSE,
                     edge.name=NULL, simplify=FALSE) 
  {

    fileNameParts0 <- strsplit(file,"/")[[1]]
    fileNameParts1 <- strsplit(fileNameParts0[length(fileNameParts0)],"\\.")[[1]]
    fileName <- paste(fileNameParts1[1:length(fileNameParts1)-1],collapse=".")
    fileExt <- fileNameParts1[length(fileNameParts1)]  #should be "net" or "paj"
    isURL <- regexpr("http",file)>0
   
    if (is.character(file)) {
        file <- file(file, "rt")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) 
        stop("argument 'file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "rt")
        on.exit(close(file))
    }
    

    nnetworks <- 0               #in paramter list of  projectizeNetworks()
    network.names <- NULL        #in paramter list of  projectizeNetworks()
    
    vertex <- NULL               #in paramter list of  projectizeNetworks()
    nvertex <- 0                 #in paramter list of  projectizeNetworks()
    network.title <- fileName    #"network"#in paramter list of  projectizeNetworks()

    partition <- NULL
    names.partition <- NULL
    
    vector <- NULL               #in paramter list of  projectizeNetworks()
    colnames.vector <- NULL      #in paramter list of  projectizeNetworks()
   
    projects <- NULL             #in paramter list of  projectizeNetworks()
    nprojects <- 0               #in paramter list of  projectizeNetworks()
    names.projects <- NULL       #in paramter list of  projectizeNetworks()

    
    nextline <- TRUE
 
    line <- " "
    is2mode <- FALSE #for two-mode data

   
    nevents <- 0   #for two-mode data
    nactors <- 0   #for two-mode data
    
#
   while(!inherits(line,"try-error")){
    while(any(grep("^%", line)) | nextline){
      if(debug) print("nextline called... new loop started")
      options(show.error.messages=FALSE)
      line <- try(readLines(file, 1, ok = FALSE))
      options(show.error.messages=TRUE)
      if(!inherits(line,"try-error") & length(line)>0){
       line <- strsplit(line, " ")[[1]]
       line <- line[line!=""]
      }
      nextline <- FALSE
    }
    nextline <- TRUE
#       if(verbose) warning(paste("afterbeingWhileLoop",line))

    
#
#   Network specification
#
    if(any(grep("\\*Network", line, ignore.case = TRUE))){
     
      network.title <- paste(line[-1],collapse=" ")
      previousDyads <- NULL  #used for arc+edge specified networks...   reset to null for every new network.. not really necessary here

  if(is.null(network.title)) network.title <- network.name
  
  if(debug) print(paste("nnetworks=",nnetworks))
  if(debug) print(paste("network.names=",network.names))
  if(debug) print(paste("vertex null?",is.null(vertex)))
  if(debug) print(paste("network.title=",network.title))
  if(debug) print(paste("vector null?",is.null(vector)))
  if(debug) print(paste("colnames.vector=",colnames.vector))
  if(debug) print(paste("projects null?",is.null(projects)))
  if(debug) print(paste("nprojects=",nprojects))
  if(debug) print(paste("names.projects=",names.projects))
  
  if(verbose) print(paste("number of networks",nnetworks))  #dschruth added
    
    if(nnetworks > 0){
      colnames(vector) <- colnames.vector
      colnames(vertex) <- c("vertex.numbers","vertex.names","cen1","cen2")[1:ncol(vertex)]
      networks <- vector("list",length=nnetworks)
      for(i in seq(along=network.names)){
        if(verbose) print(paste("working along network names",network.names))        
        temp <- get(network.names[i])
        if(!is.null(vertex)){   #Changed "vector" to "vertex"
           ##Start new addition from Alex Montgomery (Thanks AHM!)
           if (nrow(as.data.frame(vertex)) == network.size(temp)) {
               temp <- set.vertex.attribute(temp, "vertex.names",
                 as.character(vertex[as.numeric(vertex[,1]),2]))
               if (ncol(vertex)>2) { # number of columns > 2 -> vertex has attributes
                 vert.attr.nam <- c("na","vertex.names","x","y","z") #assume first three are coords (true?)
                 if (ncol(vertex)>5) vert.attr.nam <- c(vert.attr.nam,6:ncol(vertex)) #temp names for rest
                 for (vert.attr.i in 3:ncol(vertex)){
                   v <- vertex[,vert.attr.i]
                   if (is.factor(v)){ # if it's a factor, then
                     vert.attr.nam.tmp <- levels(v)[1] # see if the first factor is an attribute name
                     if (vert.attr.nam.tmp=="") vert.attr.nam.tmp <- levels(v)[2] # in case of missing data
                     if (nlevels(v)<=2&!is.na(match(vert.attr.nam.tmp, # check for match if # factors <=2
                                  c("s_size","x_fact","y_fact","phi","r","q",
                                    "ic","bc","bw","lc","la","lr",
                                    "lphi","fos","font")))) { #from pajekman.pdf v1.2.3 p.69-70
                       vert.attr.nam[vert.attr.i+1] <- vert.attr.nam.tmp #if match, name the next column
                     } else { #if not, set the attribute, converting to character (networks incompat w/factors)
                       temp <- set.vertex.attribute(temp,vert.attr.nam[vert.attr.i],
                                                    as.character(vertex[as.numeric(vertex[,1]),vert.attr.i]))
                     }
                   } else { #not a factor, set the attribute and don't convert to character
                     temp <- set.vertex.attribute(temp,vert.attr.nam[vert.attr.i],
                                                  vertex[as.numeric(vertex[,1]),vert.attr.i])
                   }
                 }
               }
              if (debug)
                 print("set vertex names to matrix")
           }
           ##End AHM addition
##Original code from Dave Schruth:
##          if(nrow(as.data.frame(vector))== network.size(temp)) {#should i be doing this? why don't these numbers match all time
##            temp <- set.vertex.attribute(temp, vector.name , value=as.matrix(vector))
##                   #set.vertex.attribute(x   , attrname    , value,      v=1:network.size(x))
##            if(debug) print("set vector to network")
##          }else{
##            warning(paste("vectorLength (",nrow(as.data.frame(vector)),") != number of nodes (",temp$gal$n,"), vertex attribute not set",sep=""))
##           #dschruth added... crashing on Scotland.paj vector length != numOfEdges (http://vlado.fmf.uni-lj.si/pub/networks/data/esna/scotland.htm)
##          }
        }
        if(!is.null(network.title)){
          temp <- set.network.attribute(temp, "title", network.title)
          if(debug) print("set network title")
        }else{
          warning("null network title")
        }
        if(nrow(as.data.frame(vertex))== network.size(temp)){ #should i be doing this? why don't these numbers match all time
          temp <- set.vertex.attribute(temp,"vertex.names",as.character(vertex[as.numeric(vertex[,1]),2]))
          if(debug) print("set vertex names to matrix")
        }
        networks[[i]] <- temp
        if (debug) print("added  network to list of networks")
      }
      names(networks) <- network.names
      if(nnetworks > 1){
        networks <- list(formula = ~1, networks = networks,
                       stats = numeric(0),coef=0)
        class(networks) <- "network.series"
      }else{
        networks  <- networks[[1]]
        class(networks) <- "network"
#        networks <- set.network.attribute(networks, "title",)
      }
      names.projects <- c(names.projects, network.title)
      nprojects <- nprojects+1
      projects <- c(projects,list(networks))
    }
    
    


      
      network.names <- NULL
      nnetworks <- 0
      vector <- NULL
      colnames.vector <- NULL
      nextline <- TRUE
    }
    
#
#   vertices specification
#
    if(any(grep("\\*Vertices", line, ignore.case = TRUE))){
      
      previousDyads <- NULL  #used for arc+edge specified networks.... reset to null for every new network.. might be sufficient here
      nvertex <- as.numeric(line[2])

      if(!is.na(line[3])){                                        #dschruth added for two-mode
        is2mode <- TRUE                    #used in matrix below  #dschruth added for two-mode
        nactors <- as.numeric(line[3])     #used for error check  #dschruth added for two-mode
        nevents <- nvertex-nactors         #used for error check  #dschruth added for two-mode
      }                                                           #dschruth added for two-mode
      if(!isURL)
      preReadTablePosition <- seek(file,where=NA)

#     if(network.title =="SanJuanSur_deathmessage.net")  #read.third paragraph in details of documentation of read table about how it determines the number of columns in the first 5 lines... 
#       vertex <- read.table(file,skip=-1,nrows=nvertex,col.names=1:8,comment.char="%",fill=TRUE,as.is=FALSE)  #dschruth added 'comment.char="%"' and 'fill=TRUE'        
#     else
        vertex <- read.table(file,skip=-1,nrows=nvertex,              comment.char="%",fill=TRUE,as.is=FALSE)  #dschruth added 'comment.char="%"' and 'fill=TRUE'
        if(ncol(vertex)==1){ vertex <- cbind(1:nrow(vertex),vertex)}


      #need to check to see if we are reading in more rows than there actually are (some edges are implied)
      edgelistPosition <-  grep("\\*(arcs|edges|matrix)",as.matrix(vertex),ignore.case=TRUE)

      if(any(edgelistPosition)){
        if(verbose) print("vertex list has missing entries or n was mis-specified, re-reading it...")
        if(isURL) stop("Resize of abbreviated vertex list via seek is not possible with URLs.  Try downloading file and loading locally")
       nVertexRows <- edgelistPosition-1
        dummyNotUsed <- seek(file,where=preReadTablePosition)  #reset the file position back to before the table was read
        vertex <- read.table(file,skip=-1,nrows=nVertexRows,comment.char="%",fill=TRUE,as.is=FALSE)  #dschruth added 'comment.char="%"' and 'fill=TRUE' 
        if(ncol(vertex)==1){ vertex <- cbind(1:nrow(vertex),vertex)}
      }      
      if(nvertex!=nrow(vertex)){
       if(verbose) print(paste("vertex list (length=",nrow(vertex),") is being re-sized to conform with specified network size (n=",nvertex,")",sep=""))
       colnames(vertex)[1:2] <- c("vn","name")
        vertex <- merge(data.frame(vn=1:nvertex),vertex,all.x=TRUE,all.y=FALSE,by.y="vn") #fill in the holes with NA names      
      }
      if(verbose) print("vertex list set")
      
    }
#
#   partition specification
#
    if(any(grep("\\*Partition", line, ignore.case = TRUE))){
     
     partition.name <- as.character(paste(line[-1],collapse="."))
     names.partition <- c(names.partition,partition.name)
     line <- readAndVectorizeLine(file)
  
     while(any(grep("^%", line))){
       line <- readAndVectorizeLine(file)
    
     }
     nvertex <- as.numeric(line[2])
     if(is.null(partition)){
       partition <- read.table(file,skip=0,nrows=nvertex)
     }else{
       partition <- c(partition,
         read.table(file,skip=0,nrows=nvertex))
     }
     if(verbose) print("partition found and set")          

    }
#
#   Vector specification
#
    if(any(grep("\\*Vector", line, ignore.case = TRUE))){
     
     vector.name <- as.character(paste(line[-1],collapse="."))
     colnames.vector <- c(colnames.vector,vector.name)
     line <- readAndVectorizeLine(file)
   
     while(any(grep("^%", line))){
       line <- readAndVectorizeLine(file)
   
     }
     nvertex <- as.numeric(line[2])
     if(is.null(vector)){
       vector <- read.table(file,skip=0,nrows=nvertex)
     }else{
       vector <- data.frame(vector,
         read.table(file,skip=0,nrows=nvertex))
     }
     if(verbose) print("vector found and set")     

    }

    
#
#   arcs specification
#
    arcsLinePresent <- edgesLinePresent <- FALSE
    arcsLinePresent <- any(grep("\\*Arcs", line, ignore.case = TRUE))#;print(paste("arcs?",arcsLinePresent))
    edgesLinePresent <- any(grep("\\*Edges", line, ignore.case = TRUE))#;print(paste("edges?",edgesLinePresent))
    # print(network.name)
   
    if(arcsLinePresent | edgesLinePresent){
      if(debug) print(paste("arc or edge lines present"))#,line)   
      
      if(missing(edge.name)){
       if(length(line)>1){
        network.name <- strsplit(paste(line[3:length(line)],collapse="."),'\"')[[1]][2]  #dschruth added collapse to allow for multi work network names
       }else{
        network.name <- paste(network.title,nnetworks+1,sep="")
        #network.name <- network.title  #old way
       }
      }else{
        network.name <- edge.name
      }
      
      dyadList <- list() #dschruth changed (was NULL)
      listIndex <- 1     #dschruth added
      
      line <- readAndVectorizeLine(file)
      
      while(any(grep("^%", line))){
        line <- readAndVectorizeLine(file)
      }
      while(!any(grep("\\*[a-zA-Z]", line)) & length(line)>0){  #dschruth changed \\*  to \\*[a-zA-Z] to allow for time asterisks
        dyadList[[listIndex]] <- as.numeric(gsub("Newline","",line))        # dyads <- rbind(dyads, as.numeric(line[1:3]))  # this is the old way
        line <- readAndVectorizeLine(file)       
        listIndex <- listIndex+1
        
      }
      if(verbose) print(paste("length of dyad list",length(dyadList)))
      nextline <- FALSE                                            #dschruth added
      if(length(dyadList)>0){                                   #dschruth added
 
       ###    deal with the possible Ragged Array [RA] dyad list .. see  Lederberg.net  ###
        
        RAlengths <- unlist(lapply(dyadList,length))
        maxRAwidth <- max(RAlengths)
        
       
        dyadsHaveAttributes <- any(is.na(unlist(dyadList))) #  handling  edge attributes (NAs introduced by coersion)
        if(dyadsHaveAttributes) warning(paste("don't worry about these",length(dyadList),"warnings,the dyads have attributes and were NA'ed during as.numeric() call. \n the actual dyad matrix width is only 2 "))
        
        if(maxRAwidth > 4 & !dyadsHaveAttributes){# #needs to be 4 because of normal edgelist can have sender reciever weight and time
          if(verbose)print("stacking ragged dyad array")         
          dyads0 <- unlist(lapply(dyadList, function(x)  c(x, rep(NA, maxRAwidth - length(x)))))
          dyads1 <- data.frame(matrix(dyads0,nrow=length(dyadList),ncol=maxRAwidth,byrow=TRUE))
          
          colnames(dyads1) <- c("sender","receiver",paste("r",seq(3,maxRAwidth),sep=""))
          
          dyads2 <- reshape(dyads1,idvar="senderNo",ids=row.names(dyads1),direction="long",
                            times=names(dyads1)[-1],timevar="receiverNo",
                            varying=list(names(dyads1)[-1]))
          
          dyads <- as.matrix(dyads2[!is.na(dyads2$receiver),c("sender","receiver")])
          
          if(verbose)print("finished stacking ragged dyad array")         
        }else{
          if(debug) print("unlisting dyad list to matrix")
          dyads <- matrix(unlist(lapply(dyadList,function(x) x[1:3])),length(dyadList),3,byrow=TRUE)
        }
       ### done dealing with RA possiblity ###  all written by dschruth

     #  dyads <- as.numeric(dyads)
        if(debug) print(paste("isnull previous dyads?: ",is.null(previousDyads)))
        
        if(is.null(previousDyads)){ #first time through (always an arc list?)
          nnetworks <- nnetworks + 1                     
          network.names <- c(network.names, network.name)
          previousDyads <- dyads
          directed <- arcsLinePresent
        }else{ #second time through (always an edge list?)
          if(verbose) print(paste("previous dyads exist!!   symmetrizing edges and combining with arcs"))
          if(arcsLinePresent){
            previousDyads.flipped <- switchArcDirection(previousDyads)
            dyads <- rbind(previousDyads,previousDyads.flipped,dyads)
          }else{
            dyads.flipped <- switchArcDirection(dyads)
            dyads <- rbind(dyads,dyads.flipped,previousDyads)
          }
          directed <- TRUE #arcsLinePresent <- TRUE
          previousDyads <- NULL
        }
        
        if((max(dyads[,1:2]) > nvertex) | nrow(dyads)==1){  # nrow(dyads)==1 is for C95.net  
          if(verbose) print("edge end out of range, skipping network creation")
          if(verbose) print("first dyad list (arcs?), is too short to be a full network, skipping to next dyad list (edges?)")
        }else{
          temp <- network.initialize(n=nvertex, directed=directed)
          add.edges(temp,tail=dyads[,1],head=dyads[,2])
#          temp <- network(x=dyads[,1:2],directed=directed)#arcsLinePresent)#dschruth added
#         temp <- set.edge.value(temp,"FALSE",NULL) #dschruth is this necessary??   should i comment out?
#         temp <- set.edge.value(temp,"NULL",NULL)  #dschruth is this necessary??   should i comment out?
          if(dim(dyads)[2]>2){  #only try to set the edge value if there is a third column
            temp <- set.edge.attribute(temp,network.names[nnetworks], dyads[,3])
            if(verbose) print("edge value created from edge/arc list")
          }
          assign(network.names[nnetworks], temp)
          rm(temp)
          if(verbose) print("network created from edge/arc list")
#        if(arcsLinePresent) nextline <- TRUE    #{ print(" 'arcs' line followed by dyads present... skip past the current 'edges' line");}
        }                                                           #dschruth added
      }

      
    }
    
#
#   matrix specification
#
    if(any(grep("\\*Matrix", line, ignore.case = TRUE))){
      if(verbose) print("found matrix")
      
      if(length(line)>1){
        network.name <- strsplit(line[3],'\"')[[1]][2]
      }else{
        network.name <- paste("network",nnetworks+1,sep="")
      }
      nnetworks <- nnetworks + 1
      network.names <- c(network.names, network.name)
      temp0 <- as.matrix(read.table(file,skip=0,nrows=nvertex,as.is=TRUE))
      lastColNum <- ncol(temp0)
      if(all(apply(temp0[,-lastColNum],1,sum)==temp0[,lastColNum])){
        if(verbose) print("removing final marginal sum column of matrix")
        temp0 <- temp0[,-lastColNum]
      }
      if(verbose) print(paste("dim1",dim(temp0)[1],"na",nactors,"dim2",dim(temp0)[2],"ne",nevents)) #checking
      if(is2mode & (dim(temp0)[1]!=nactors | dim(temp0)[2]!=nevents)){
        warning("error!, dimensions do not match bipartite specifications")
      }else{
        temp <- network(x=temp0,bipartite=is2mode)               #dschruth added "bipartate=is2mode" for two-mode
#       temp <- set.edge.attribute(temp,network.names[nnetworks],        dyads[,3])
                if(verbose) print("network created from matrix")
      }
      assign(network.names[nnetworks], temp)
      rm(temp)
    }
  }#end while loop


  if(is.null(network.title)) network.title <- network.name
  
  if(debug) print(paste("nnetworks=",nnetworks))
  if(debug) print(paste("network.names=",network.names))
  if(debug) print(paste("vertex null?",is.null(vertex)))
  if(debug) print(paste("network.title=",network.title))
  if(debug) print(paste("vector null?",is.null(vector)))
  if(debug) print(paste("colnames.vector=",colnames.vector))
  if(debug) print(paste("projects null?",is.null(projects)))
  if(debug) print(paste("nprojects=",nprojects))
  if(debug) print(paste("names.projects=",names.projects))
  
  if(verbose) print(paste("number of networks",nnetworks))  #dschruth added
    
    if(nnetworks > 0){
      colnames(vector) <- colnames.vector
      colnames(vertex) <- c("vertex.numbers","vertex.names","cen1","cen2")[1:ncol(vertex)]
      networks <- vector("list",length=nnetworks)
      for(i in seq(along=network.names)){
        if(verbose) print(paste("working along network names",network.names))        
        temp <- get(network.names[i])
        if(!is.null(vertex)){  #Changed "vector" to "vertex"
           ##Start new addition from Alex Montgomery (Thanks AHM!)
           if (nrow(as.data.frame(vertex)) == network.size(temp)) {
               temp <- set.vertex.attribute(temp, "vertex.names",
                 as.character(vertex[as.numeric(vertex[,1]),2]))
               if (ncol(vertex)>2) { # number of columns > 2 -> vertex has attributes
                 vert.attr.nam <- c("na","vertex.names","x","y","z") #assume first three are coords (true?)
                 if (ncol(vertex)>5) vert.attr.nam <- c(vert.attr.nam,6:ncol(vertex)) #temp names for rest
                 for (vert.attr.i in 3:ncol(vertex)){
                   v <- vertex[,vert.attr.i]
                   if (is.factor(v)){ # if it's a factor, then
                     vert.attr.nam.tmp <- levels(v)[1] # see if the first factor is an attribute name
                     if (vert.attr.nam.tmp=="") vert.attr.nam.tmp <- levels(v)[2] # in case of missing data
                     if (nlevels(v)<=2&!is.na(match(vert.attr.nam.tmp, # check for match if # factors <=2
                                  c("s_size","x_fact","y_fact","phi","r","q",
                                    "ic","bc","bw","lc","la","lr",
                                    "lphi","fos","font")))) { #from pajekman.pdf v1.2.3 p.69-70
                       vert.attr.nam[vert.attr.i+1] <- vert.attr.nam.tmp #if match, name the next column
                     } else { #if not, set the attribute, converting to character (networks incompat w/factors)
                       temp <- set.vertex.attribute(temp,vert.attr.nam[vert.attr.i],
                                                    as.character(vertex[as.numeric(vertex[,1]),vert.attr.i]))
                     }
                   } else { #not a factor, set the attribute and don't convert to character
                     temp <- set.vertex.attribute(temp,vert.attr.nam[vert.attr.i],
                                                  vertex[as.numeric(vertex[,1]),vert.attr.i])
                   }
                 }
               }
              if (debug)
                 print("set vertex names to matrix")
           }
           ##End AHM addition
##Original by Dave Schruth:
##          if(nrow(as.data.frame(vector))== network.size(temp)) {#should i be doing this? why don't these numbers match all time
##            temp <- set.vertex.attribute(temp, vector.name , value=as.data.frame(vector))
##                   #set.vertex.attribute(x   , attrname    , value,      v=1:network.size(x))
##            if(debug) print("set vector to network")
##          }else{
##            warning(paste("vectorLength (",nrow(as.data.frame(vector)),") != number of nodes (",temp$gal$n,"), vertex attribute not set",sep=""))
##           #dschruth added... crashing on Scotland.paj vector length != numOfEdges (http://vlado.fmf.uni-lj.si/pub/networks/data/esna/scotland.htm)
##          }
        }
        if(!is.null(network.title)){
          temp <- set.network.attribute(temp, "title", network.title)
          if(debug) print("set network title")
        }else{
          warning("null network title")
        }
        if(nrow(as.data.frame(vertex))== network.size(temp)){ #should i be doing this? why don't these numbers match all time
          temp <- set.vertex.attribute(temp,"vertex.names",as.character(vertex[as.numeric(vertex[,1]),2]))
          if(debug) print("set vertex names to matrix")
        }

        networks[[i]] <- temp
        if (debug) print("added  network to list of networks")
      }
      names(networks) <- network.names
      if(nnetworks > 1){
        networks <- list(formula = ~1, networks = networks,
                       stats = numeric(0),coef=0)
        class(networks) <- "network.series"
      }else{
        networks  <- networks[[1]]
        class(networks) <- "network"
#        networks <- set.network.attribute(networks, "title",)
      }
      names.projects <- c(names.projects, network.title)
      nprojects <- nprojects+1
      projects <- c(projects,list(networks))
    }
    
    
    
    if(!is.null(projects)) #dschruth added
      names(projects) <- names.projects
    if(is.null(partition)){  
      if(verbose) print(paste("number of projects",nprojects))  #dschruth added
      if(nprojects==1)
        projects <- projects[[1]]
      if(nnetworks>1){
        class(projects) <- "network.series"
      }
    }else{
      names(partition) <- names.partition
      projects <- list(networks=projects, partitions=partition)
    } #end ifelse
#
#   Simplify
#
    if(is.logical(simplify)){
     if(simplify){
      simplify <- fileName
     }else{
      return(projects)
     }
    }
    read.paj.simplify(x=projects,file=simplify,verbose=verbose)
  } #end read.paj


readAndVectorizeLine <- function(file){
  line <- readLines(file, 1, ok = TRUE)
  if(!inherits(line,"try-error") & length(line)>0){
    line <- strsplit(line," ")[[1]]
    line <- line[line!=""]
  }
  line
}

########## but multirelational ############ only ~200  nodes 
#GulfLDays.net 
#GulfLMonths.net
#GulfLDow.net 
#gulfAllDays.net     #GulfADays.zip
#gulfAllMonths.net   #GulfAMonths.zip
#LevantDays.net 
#LevantMonths.net
#BalkanDays.net 
#BalkanMonths.net 

#arcs and edges both present   search for " #these have both arc and edge lines " or "URL has a net file"
#Graph drawing competition page (GD)
#C95,C95,B98,A99,C99,A99m


#things to do:
#handle ragged array .net files like "CSphd.net"     DONE!!
#handel two mode networks                            DONE!!
#handle mix of edges and arcs                        DONE!!
#handle multirelational pajek files

#issue with read.table and number.cols and fill...SanJuanSur_deathmessage.net has one row with 8 all the rest (including the first 5 have 5)









#read.paj() test links
#../test/Scotland.paj  ../test/Scotland.net  from http://vlado.fmf.uni-lj.si/pub/networks/data/esna/
#
#http://vlado.fmf.uni-lj.si/pub/networks/data/GD/gd95/A95.net
#http://vlado.fmf.uni-lj.si/pub/networks/data/GD/gd96/A96.net
#http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/bkfrat.paj


read.paj.simplify <- function(x,file,verbose=FALSE) 
  {
   classx <- class(x)
   if(classx=="network"){
    cat(paste(file," is a single network object.\n",sep=""))
    assign(file,x)
    save(list=file,
         file=paste(file,".RData",sep=""))
    cat(paste("network save as a 'network' object in ",file,".RData.\n",sep=""))
    return(x)
   }
   if(classx=="network.series"){
    nnets <- length(x$networks)
    cat(paste(file," is a set of ",nnets," networks on the same set of nodes.\n",sep=""))
    cat(paste("The network names are:\n  ",
     paste(names(x$networks),collapse="\n  "),"\n",sep=""))
    cnames <- names(x$networks)
    if(length(cnames) == 1){
     assign(cnames,x$networks[[1]])
     save(list=cnames,
          file=paste(file,".RData",sep=""))
     cat(paste("network simplified to a network object.\n",sep=""))
     cat(paste("network save as a 'network' object in ",file,".RData.\n",sep=""))
     return(x$networks[[1]])
    }else{
     assign(file,x)
     save(list=file,
          file=paste(file,".RData",sep=""))
     cat(paste("network save as a 'network.series' object in ",file,".RData.\n",sep=""))
     return(x)
    }
   }
   if(classx=="list"){
    ncollects <- length(x$networks)
    nnets <- length(x$networks)
    npart <- length(x$partitions)
    cnames <- names(x$networks)
    if(length(cnames) > 1){
     cat(paste(file," is a set of ",ncollects," collections of networks\n",
      "as well as Pajek 'partiton' information.\n",sep=""))
     cat(paste("The collection names are:\n  ",
      paste(cnames,collapse="\n  "),"\n",sep=""))
     for(i in seq(along=cnames)){
      thisnet <- x$networks[[i]]
      classthisnet <- class(thisnet)
      if(classthisnet=="network.series" & length(thisnet$networks)==1){
       thisnet <- thisnet$networks[[1]]
       classthisnet <- class(thisnet)
      }
      if(classthisnet=="network"){
       cat(paste("The collection ",cnames[i]," is a single network object.\n",
        sep=""))
      }else{
       cat(paste("The collection ",cnames[i], " is a set of networks on the same nodes.\n",sep=""))
        cat(paste("The network names are:\n  ",
         paste(names(thisnet$networks),collapse="\n  "),"\n",sep=""))
      }
     }
     cat(paste("There are ",npart," partitions on the networks.\n",sep=""))
     cat(paste("The partition names are:\n  ",
      paste(names(x$partitions),collapse="\n  "),"\n",sep=""))
     cat(paste(".RData file unchanged.\n",sep=""))
    }else{
     thisnet <- x$networks[[1]]
     classthisnet <- class(thisnet)
     if(classthisnet=="network"){
      cat(paste(file," is a single network object called ", cnames,"\n",
       "as well as Pajek 'partiton' information.\n",sep=""))
       cat(paste("There are ",npart," partitions on the networks.\n",sep=""))
       cat(paste("The partition names are:\n  ",
        paste(names(x$partitions),collapse="\n  "),"\n",sep=""))
     }else{
      cat(paste(file," is a collection of networks called ", cnames,"\n",
       "as well as Pajek 'partiton' information.\n",sep=""))
       cat(paste("The network names are:\n  ",
        paste(names(thisnet$networks),collapse="\n  "),"\n",sep=""))
       cat(paste("There are ",npart," partitions on the networks.\n",sep=""))
       cat(paste("The partition names are:\n  ",
        paste(names(x$partitions),collapse="\n  "),"\n",sep=""))
     }
     assign(cnames,x$networks[[1]])
     assign(paste(cnames,"partitions",sep="."),x$partitions)
     save(list=c(cnames, paste(cnames,"partitions",sep=".")),
          file=paste(file,".RData",sep=""))
     if(class(x$networks[[1]])=="network"){
      cat(paste("network simplified to a 'network' object plus partition.\n",sep=""))
      cat(paste("network save as a 'network' object and a separate partition list in ",file,".RData.\n",sep=""))
     }else{
      cat(paste("network simplified to a 'network.series' object plus partition.\n",sep=""))
      cat(paste("network save as a 'network.series' object and a separate partition list in ",file,".RData.\n",sep=""))
     }
    }
   }
   return(x)
} 

switchArcDirection <- function(edgelist){
edgelist[,1:2] <- edgelist[,2:1]
edgelist
}
