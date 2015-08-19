######################################################################
#
# fileio.R
#
# Written by Carter T. Butts <buttsc@uci.edu>; portions contributed by
# David Hunter <dhunter@stat.psu.edu> and Mark S. Handcock
# <handcock@u.washington.edu>.
#
# Last Modified 10/04/10
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
# some details at http://vlado.fmf.uni-lj.si/pub/networks/pajek/doc/pajekman.pdf p. 73

# generally this steps through the file until it finds markers for specific sub sections
# when it sees one ('*Vertices*') it drops into a sub-loop that keeps advancing the file read
# however, note that the overall loop may run multiple times in order to correctly detect all of the pieces in the file

# things are made more complicated becaue there can be multiple *Edges or *Arcs definitions in a network
# when it is a "mutliple network" (multiplex) http://vlado.fmf.uni-lj.si/pub/networks/doc/ECPR/08/ECPR01.pdf slide 21
# TODO: not sure if multiplex is set appropriately for this case

# Also, attributes can be have 'default' values (the previous record) if not explicitly set on each row

# TODO: need an argument to indicate if multiple sets of relations on the same vertex set should be returned
# as a multiplex network or a list of networks.

read.paj <- function(file,verbose=FALSE,debug=FALSE,
                    edge.name=NULL, simplify=FALSE)
 {
   # process filename
   fileNameParts0 <- strsplit(file,"/")[[1]]
   # split again to try to get file extension
   fileNameParts1 <- strsplit(fileNameParts0[length(fileNameParts0)],"\\.")[[1]]
   # filename may not have extension
   if(length(fileNameParts1)>1){
     fileName <- paste(fileNameParts1[1:length(fileNameParts1)-1],collapse=".")
     fileExt <- fileNameParts1[length(fileNameParts1)]  #should be "net" or "paj" (but never used ?)
   } else {
     fileName<-fileNameParts1
     fileExt<-""
   }
   isURL <- regexpr("http",file)>0
   
  # open connection (if it is not one already)
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

   # initialize state tracking variables
   lineNumber<-0              # input line number parsed for debugging
   nnetworks <- 0               # number of networks (edge types) in current *Network block
   network.names <- NULL        # names of networks (edge types) in current *Network block

   vertex <- NULL               # has the vertex block been found?
   nvertex <- 0                 # number of vertices in currently processing network
   network.title <- fileName    # default name for network is filename

   partition <- NULL            # partitions, if found
   names.partition <- NULL      # names of partitions, if found

   vector <- NULL               # vectors, if found
   colnames.vector <- NULL      # names of vectors if found

   projects <- list()             # projects if found (each set of related networks is a 'project')
   nprojects <- 0               # number of projects found
   names.projects <- NULL       # names of projects if found. 

   nextline <- TRUE             # control flag to indicate if should proceede to next line
   line <- " "                  # usually tokens corresponding to line being red
   previousArcs<-NULL
   previousEdges<-NULL
   is2mode <- FALSE             # flag indicating if currently processing biparite network  
   nevents <- 0                 # for two-mode data, size of first mode
   nactors <- 0                 # for two-mode data, size of second mode
   isDynamic<-FALSE             # flag indicating if currently processing dynamic network
   multiplex<-FALSE             # flag indicating if currently processing multiplex network

# begin file parsing
  while(!inherits(line,"try-error")){
   while(any(grep("^%", line)) | nextline){
     if(debug) print(paste("new parsing loop started at line",lineNumber))
     options(show.error.messages=FALSE)
     # read the next line with error messages disabled
     line <- try(readLines(file, 1, ok = FALSE))
     options(show.error.messages=TRUE)
     # If the line was not an error, tokenize using space as seperator
     if(!inherits(line,"try-error") & length(line)>0){
      line <- strsplit(line, " ")[[1]]
      line <- line[line!=""]
      lineNumber<-lineNumber+1
     }
     nextline <- FALSE # there was an error (probably end of file) so don't parse anymore
   }
   nextline <- TRUE
#       if(verbose) warning(paste("afterbeingWhileLoop",line))


#
#   ---- Network parsing -------
#   Search for lines begining with *Network within the .paj file
# not all files will include a *Network heading (usually only .paj)
# it indicates that all the following sections (vertices, partitions, etc) belong to that network
   if(any(grep("\\*Network", line, ignore.case = TRUE))){
     if (verbose) print(paste('parsing *Network block at line',lineNumber))


     if(debug){
       print(paste("    nnetworks=",nnetworks))
       print(paste("    network.names=",network.names))
       print(paste("    vertex null?",is.null(vertex)))
       print(paste("    network.title=",network.title))
       print(paste("    vector null?",is.null(vector)))
       print(paste("    colnames.vector=",colnames.vector))
       print(paste("    names.projects=",names(projects)))
     }

      if(verbose) print(paste("number of networks",nnetworks))  #dschruth added
       # we are about to start a new network, so need to run the post-processing
       # code on the previously parsed network (if there is one)
     if(nnetworks > 0 ){
       if(debug) print("assembleing networks into 'project'")
       # grab all the named networks from the environment
         # and put 'em in a list
         networksData<-lapply(network.names,function(netName){get(netName)})
       # TODO: delete networks from environment to clear up space?
       # take the various objects that have been parsed from the .paj file and assemble
       # them into a network object (or list of network objects, a 'project'), doing some appropriate conversion
         projects <- postProcessProject( network.title,
                                         vector, 
                                         colnames.vector, 
                                         vertex,  # data for building vertices,
                                         nnetworks, # number of networks found,
                                         network.names,  # names of networks found
                                         networksData,
                                         projects,
                                         verbose
         )
     } else { # networks have not been created, but need to check if only vertices have been found and empty network needed
       if(!is.null(vertex)){
         # need to initialize a network here to deal with the case where no arcs/edge in the file
         # Note that without the arcs/edge, we have no way to know if network was supposed to be directed or multiplex
         networksData<-list( network.initialize(n=nvertex, bipartite=nactors))
         projects <- postProcessProject( network.title,
                                         vector, 
                                         colnames.vector, 
                                         vertex,  # data for building vertices,
                                         nnetworks, # number of networks found,
                                         network.names=network.title,  # names of networks found
                                         networksData,
                                         projects,
                                         verbose)
       }
     }

   #  since we are starting a new network, reset all of the network level info, directed, 2mode, etc
     network.title <-NULL
     network.names <- NULL
     vertex<-NULL
     nvertex<-0
     nnetworks <- 0
     vector <- NULL
     colnames.vector <- NULL
     nextline <- TRUE
     arcsLinePresent<-FALSE
     edgesLinePresent<-FALSE
     previousArcs<-NULL
     previousEdges<-NULL
     is2mode <- FALSE #for two-mode data
     nevents <- 0   #for two-mode data
     nactors <- 0   #for two-mode data
     isDynamic<-FALSE
     multiplex<-FALSE

     # now parse the new network title
     network.title <- paste(line[-1],collapse=" ")



    if(is.null(network.title)){ 
      network.title <- network.name # this seems wrong, should be file name?
      warning('no name found for network, using "',network.name,'"')
    }

   }  # END NETWORK PARSING BLOCK

#
#   vertices specification
#   search for lines beignning with *Vertices
#   and then read in the number of lines equal to the expected number of vertices
   if(any(grep("\\*Vertices", line, ignore.case = TRUE))){
     if (verbose) print(paste('parsing *Vertices block at line',lineNumber))
     previousArcs <- NULL  #used for arc+edge specified networks.... reset to null for every new network.. might be sufficient here
     previousEdges<-NULL
     nvertex <- as.numeric(line[2])  # parse the number of vertices
     #nnetworks <- nnetworks + 1  # if we found vertices, we must have a network
     # give the network a default name (may be overwritten later)
     network.name <- paste(network.title,sep="")

     if(!is.na(line[3])){                                        #dschruth added for two-mode
       is2mode <- TRUE                    #used in matrix below  #dschruth added for two-mode
       nactors <- as.numeric(line[3])     #used for error check  #dschruth added for two-mode
       nevents <- nvertex-nactors         #used for error check  #dschruth added for two-mode
     }                                                           #dschruth added for two-mode
     if(!isURL){
       # cache the table position in the input file in case we need to jump pack here later
       preReadTablePosition <- seek(file,where=NA)
     }

#     if(network.title =="SanJuanSur_deathmessage.net")  #read.third paragraph in details of documentation of read table about how it determines the number of columns in the first 5 lines...
#       vertex <- read.table(file,skip=-1,nrows=nvertex,col.names=1:8,comment.char="%",fill=TRUE,as.is=FALSE)  #dschruth added 'comment.char="%"' and 'fill=TRUE'
#     else
       # read it as table
       # NOTE: rows may omit values ()
       vertex <- read.table(file,skip=-1,nrows=nvertex,  comment.char="%",fill=TRUE,as.is=FALSE,row.names=NULL)
       
       if(ncol(vertex)==1){ vertex <- cbind(1:nrow(vertex),vertex)}


     #need to check to see if we are reading in more vertex rows than there actually are (some edges are implied)
     edgelistPosition <-  grep("\\*(arcs|edges|matrix)",as.matrix(vertex),ignore.case=TRUE)

     if(any(edgelistPosition)){
       if(verbose){
         print("vertex list has missing entries or n was mis-specified, re-reading it...")
       } else {
         warning('vertex list has missing entries or n was mis-specified, re-reading it...')
       }
       if(isURL) stop("Resize of abbreviated vertex list via seek is not possible with URLs.  Try downloading file and loading locally")
      nVertexRows <- edgelistPosition-1
       dummyNotUsed <- seek(file,where=preReadTablePosition)  #reset the file position back to before the table was read
       vertex <- read.table(file,skip=-1,nrows=nVertexRows,comment.char="%",fill=TRUE,as.is=FALSE,)  #dschruth added 'comment.char="%"' and 'fill=TRUE'
       if(ncol(vertex)==1){ vertex <- cbind(1:nrow(vertex),vertex)}
     }
     if(nvertex!=nrow(vertex)){
      if(verbose){ 
        print(paste("vertex list (length=",nrow(vertex),") is being re-sized to conform with specified network size (n=",nvertex,")",sep=""))
      }
      colnames(vertex)[1:2] <- c("vn","name")
       vertex <- merge(data.frame(vn=1:nvertex),vertex,all.x=TRUE,all.y=FALSE,by.y="vn") #fill in the holes with NA names
     }
     # increment the debugging line counter
     lineNumber<-lineNumber+nvertex

     if(verbose) print(paste("  found",nvertex,'vertices'))


   }  # end vertices parsing block
#
#   partition specification  (vertex level attribute)
#
   if(any(grep("\\*Partition", line, ignore.case = TRUE))){
     if (verbose) print(paste('parsing *Partition block at line',lineNumber))

    partition.name <- as.character(paste(line[-1],collapse="."))
    names.partition <- c(names.partition,partition.name)
    line <- readAndVectorizeLine(file)
    lineNumber<-lineNumber+1  # update debugging line number
    
    # skip comments
    while(any(grep("^%", line))){
      line <- readAndVectorizeLine(file)
      lineNumber<-lineNumber+1  # update debugging line number
    }
    nvertex <- as.numeric(line[2])
    if(is.null(partition)){
      partition <- read.table(file,skip=0,nrows=nvertex)
      lineNumber<-lineNumber+nvertex  # update debugging line number
    }else{
      partition <- c(partition,
        read.table(file,skip=0,nrows=nvertex))
      lineNumber<-lineNumber+nvertex  # update debugging line number
    }
    if(verbose) print("partition found and set")
    # TODO: why is partition not attached as vertex attribute?
   }
#
#   ----- Vector specification  (vetex-level attribute) -----
#
   if(any(grep("\\*Vector", line, ignore.case = TRUE))){
     if (verbose) print(paste('parsing *Vector block at line',lineNumber))
    vector.name <- as.character(paste(line[-1],collapse="."))
    colnames.vector <- c(colnames.vector,vector.name)
    line <- readAndVectorizeLine(file)
    lineNumber<-lineNumber+1  # update debugging line number
    
    # skip comments
    while(any(grep("^%", line))){
      line <- readAndVectorizeLine(file)
      lineNumber<-lineNumber+1  # update debugging line number
    }
    nvertex <- as.numeric(line[2])
    if(is.null(vector)){
      vector <- read.table(file,skip=0,nrows=nvertex)
      lineNumber<-lineNumber+nvertex  # update debugging line number
    }else{
      vector <- data.frame(vector,
        read.table(file,skip=0,nrows=nvertex))
       lineNumber<-lineNumber+nvertex  # update debugging line number
    }
    if(verbose) print("vector found and set")

   }


#
#   ----- arcs / edges specification --------
#

   arcsLinePresent<-any(grep("\\*Arcs$", line, ignore.case = TRUE))
   edgesLinePresent<-any(grep("\\*Edges$", line, ignore.case = TRUE))
   
  

   if(arcsLinePresent | edgesLinePresent){

     if(arcsLinePresent){
      if(verbose) print(paste("parsing *Arcs block at line",lineNumber))
      # if we had already parsed an arcs block, and we just found another one, better clear the old
      if(!is.null(previousArcs)){
        previousArcs<-NULL
      }
     } else {
       if(verbose) print(paste("parsing *Edges block at line",lineNumber))
       # if we had already parsed an edges block, and we just found another one, better clear the old
       if(!is.null(previousEdges)){
         previousEdges<-NULL
       }
     }
     

     if(missing(edge.name)){
      if(length(line)>1){  # this *Arcs / *Edges block is definding a named 'network' of relationships
       network.name <- strsplit(paste(line[3:length(line)],collapse="."),'\"')[[1]][2]  #dschruth added collapse to allow for multi work network names
       #Note: don't increment the number of networks found until later, because this is executed for both arcs and edges block
      }else{
       # append an index to the network name (to be used as edge attribute) only if we've seen multiple networks
       network.name <- paste(network.title,ifelse(nnetworks>0,nnetworks,''),sep="")
       #network.name <- network.title  #old way
      }
     }else{
       # define the network name as the edge name passed in by user
       # TODO: seems like if user passes in edge.name, multirelational edges will not be parsed correctly 
       # because they will be given the same name
       network.name <- edge.name
     }

     dyadList <- list() #dschruth changed (was NULL)
     listIndex <- 1     #dschruth added

     line <- readAndVectorizeLine(file)
     lineNumber<-lineNumber+1  # update debugging line number
     
     # skip comments / blank lines
     while(any(grep("^%", line))){
       line <- readAndVectorizeLine(file)  
       lineNumber<-lineNumber+1  # update debugging line number
     }
     # keep reading lines until reaching the end of the block
     while(!any(grep("\\*[a-zA-Z]", line)) & length(line)>0){  #dschruth changed \\*  to \\*[a-zA-Z] to allow for time asterisks
       # check line length for parse problems
       # should be fromId,toId, weight
       # if there are not 3, matrix reform will go bad later on
       if(length(line)<2){
         stop("Arc/Edge record on line ",lineNumber," does not appear to have the required 2 elements:'",paste(line,collapse=' '),"'")
       }
       dyadList[[listIndex]] <- gsub("Newline","",line)   # replace any newlines
       line <- readAndVectorizeLine(file)
       lineNumber<-lineNumber+1  # update debugging line number
       listIndex <- listIndex+1
     }     
     if(verbose) print(paste("  length of dyad list",length(dyadList)))
     nextline <- FALSE    
     # check if we found any dyads
     if(length(dyadList)>0){  
      ###    deal with the possible Ragged Array [RA] dyad list .. see  Lederberg.net  ###
       #TODO: I think this was for dealing with *arcslist / *edgelist, move to seperate section or do detection directly
       RAlengths <- unlist(lapply(dyadList,length))
       maxRAwidth <- max(RAlengths)
       
       # if there are 4 elements, the 4th should be time?
       if (min(RAlengths) > 3){
         # look for time info in the form of brackets
         timesList<-lapply(dyadList,function(row){
           grep('^\\[.+\\]$',row,value=TRUE)
         })
         # check if any of them have times
         if(any(sapply(timesList,length)>0)){
           isDynamic<-TRUE
         }
         # TODO: implement dynamics as per issue #780
         if(isDynamic){
           warning('arcs/edges include timing information. network dynamics not currently supported by this parser so will be added as plain edge attribute.')
         }
       }
       

      # TODO: this is an ugly error-prone way to check if there are attributes, need to fix
       dyadsHaveAttributes <- any(is.na(as.numeric(unlist(dyadList)))) #  handling  edge attributes (NAs introduced by coersion)
       if(dyadsHaveAttributes){
        warning(paste("don't worry about these",length(dyadList),"warnings,the dyads have attributes and were NA'ed during as.numeric() call. \n the actual dyad matrix width is only 2 "))
       }

       if(maxRAwidth > 4 & !dyadsHaveAttributes){# #needs to be 4 because of normal edgelist can have sender reciever weight and time
         if(verbose)print(" stacking ragged dyad array ")
         dyads0 <- unlist(lapply(dyadList, function(x)  c(x, rep(NA, maxRAwidth - length(x)))))
         dyads1 <- data.frame(matrix(dyads0,nrow=length(dyadList),ncol=maxRAwidth,byrow=TRUE))

         colnames(dyads1) <- c("sender","receiver",paste("r",seq(3,maxRAwidth),sep=""))

         dyads2 <- reshape(dyads1,idvar="senderNo",ids=row.names(dyads1),direction="long",
                           times=names(dyads1)[-1],timevar="receiverNo",
                           varying=list(names(dyads1)[-1]))

         dyads <- as.matrix(dyads2[!is.na(dyads2$receiver),c("sender","receiver")])

         if(verbose)print("finished stacking ragged dyad array")
       }else{ # not a ragged array
         if(debug) print("    unlisting dyad list to matrix")
         # check if weight was ommited
         if (all(RAlengths==2)){
           # assume default weight of 1
           dyads <- matrix(unlist(lapply(dyadList,function(x){
             c(as.numeric(x[1:2]),1)})),
             nrow=length(dyadList),ncol=3,byrow=TRUE)
           if(verbose) print('weights ommited from arcs/edges lines, assuming weight of 1')
         } else {
         # create matrix of v1, v2, weight
         dyads <- matrix(unlist(lapply(dyadList,function(x){
                                as.numeric(x[1:3])})),
                              nrow=length(dyadList),ncol=3,byrow=TRUE)
         }
       }
      ### done dealing with RA possiblity ###  all written by dschruth

      # check for non-numeric ids (bad coercion)
      if(any(is.na(dyads[,1:2,drop=FALSE]))){
        badRows<-lineNumber-(which(is.na(dyads[,1:2,drop=FALSE]),arr.ind=TRUE)[,1])
        stop('vertex id columns in arcs/edges definition contains non-numeric or NA values on line(s) ',paste(badRows,collapse=' '))
      }
      
      # check for non-integer vertex ids
      if(any(round(dyads[,1:2,drop=FALSE])!=dyads[,1:2,drop=FALSE])){
        badRows<-lineNumber-(which(round(dyads[,1:2,drop=FALSE])!=dyads[,1:2,drop=FALSE],arr.ind=TRUE)[,1])
        stop('vertex id columns in arcs/edges definition contains non-integer values on line(s) ',paste(badRows,collapse=' '))
      }
      
      # check for out of range vertex ids
      if((max(dyads[,1:2]) > nvertex)){  # nrow(dyads)==1 is for C95.net
        # figure out which rows are bad
        badRows<-1+lineNumber-(which(dyads[,1:2] > nvertex,arr.ind=TRUE)[,1])
        stop("vertex id(s) in arcs/edge definition is  out of range on line(s) ",paste(badRows,collapse=' '))
        #if(verbose) print("first dyad list (arcs?), is too short to be a full network, skipping to next dyad list (edges?)")
      }

       if(is.null(previousArcs) & is.null(previousEdges)){ #first time through (always an arc list?)
         # definitly creating a network, so increment the counter and names
         nnetworks <- nnetworks + 1
         network.names <- c(network.names, network.name)
         previousDyads <- dyads
         if(arcsLinePresent){
          directed <- TRUE
          previousArcs <- dyads
         } else {
           previousEdges <- dyads
           # there must not be an arcs block, so assume undirected
           directed <-FALSE
         }

        
       }else{ #second time through (always an edge list?)
         if(verbose) print(paste("previous dyads exist!!   symmetrizing edges and combining with arcs"))
         if(edgesLinePresent){
           # should only be edges
           dyads.flipped <- switchArcDirection(dyads)
           dyads <- rbind(previousArcs,dyads,dyads.flipped)
         }else{ 
           stop('reached sequence of multiple *Arcs blocks, parsing code must have bad logic')
         }
         previousArcs <- NULL # we've used 'em, so null it out
       }

       # check for multiple ties
       repeatLines<-anyDuplicated(dyads[,1:2,drop=FALSE])
       if(repeatLines>0){
         multiplex<-TRUE
         message('network contains duplicated dyads so will be marked as multiplex')
       }

       ## initialize the appropriate type of network
       # NOTE: network creation occurs TWICE for networks with both arcs and edges, but the first network
       # is overwritten by the second. Needlessly slow on large nets, but difficult to avoid, since 
       # we don't know if there is  a 2nd block on the first pass
       if(is2mode){
         temp <- network.initialize(n=nvertex, directed=directed,
                                    bipartite=nactors,multiple=multiplex)
       }else{
         temp <- network.initialize(n=nvertex, directed=directed,multiple=multiplex)
       }
       # add in the edges
       add.edges(temp,tail=dyads[,1],head=dyads[,2])
#          temp <- network(x=dyads[,1:2],directed=directed)#arcsLinePresent)#dschruth added
       if(dim(dyads)[2]>2){  #only try to set the edge value if there is a third column (there always is?)
         temp <- set.edge.attribute(temp,network.names[nnetworks], dyads[,3])
         if(verbose) print(paste("  edge weight attribute named",network.names[nnetworks],"created from edge/arc list"))
       }
       if(isDynamic){
         temp <- set.edge.attribute(temp,'pajekTiming', timesList)
         if(verbose) print("  edge timing attribute created from edge/arc list")
       }
       #TODO: detect and process any additional edge attributes may be token based and can be ommited (duplicate previous line)

       assign(network.names[nnetworks], temp)
       rm(temp)
       if(verbose) print("network created from edge/arc list")
#        if(arcsLinePresent) nextline <- TRUE    #{ print(" 'arcs' line followed by dyads present... skip past the current 'edges' line");}
        
# end of edge/arc adding
     }  
   } 

#
#   ----- matrix parsing -------
#
   if(any(grep("\\*Matrix", line, ignore.case = TRUE))){
     if(verbose) print(paste('parsing *Matrix block at line',lineNumber))
     if(length(line)>1){
       # if a network name is given, use that
       network.name <- strsplit(line[3],'\"')[[1]][2]
     }else{
       # otherwise name it acoding to the file name, adding a digit if we've seen multiple nets
       #network.name <- paste("network",nnetworks+1,sep="")
       network.name <- paste(network.title,ifelse(nnetworks>0,nnetworks,''),sep="")
     }
     nnetworks <- nnetworks + 1
     network.names <- c(network.names, network.name)
     temp0 <- as.matrix(read.table(file,skip=0,nrows=nvertex,as.is=TRUE))
     lineNumber<-lineNumber+nvertex
     lastColNum <- ncol(temp0)
     if(all(apply(temp0[,-lastColNum],1,sum)==temp0[,lastColNum])){
       if(verbose) print("removing final marginal sum column of matrix")
       temp0 <- temp0[,-lastColNum]
     }
     if(verbose) print(paste("  matrix dimensions: dim1",dim(temp0)[1],"na",nactors,"dim2",dim(temp0)[2],"ne",nevents)) #checking
     if(is2mode & (dim(temp0)[1]!=nactors | dim(temp0)[2]!=nevents)){
       stop("dimensions do not match bipartite specifications")
     }else{
       # convert the adjacency matrix to a network, using values as an edge attribute
       temp <- as.network.matrix(x=temp0,
                                 matrix.type='adjacency',
                                 bipartite=is2mode,
                                 ignore.eval=FALSE,
                                 names.eval=network.name)               #dschruth added "bipartate=is2mode" for two-mode
#       temp <- set.edge.attribute(temp,network.names[nnetworks],        dyads[,3])
               if(verbose) print("network created from matrix")
     }
     assign(network.names[nnetworks], temp)
     rm(temp)
   }

  # detect and report some formats that we cannot yet parse
  if(any(grep("\\*Arcslist", line, ignore.case = TRUE))){
     warning(paste('skipped *Arcslist block at line',lineNumber, ' read.paj does not yet know how to parse it '))
     #TODO: see http://vlado.fmf.uni-lj.si/vlado/podstat/AO/net/TinaList.net
  }

  if(any(grep("\\*Edgeslist", line, ignore.case = TRUE))){
    warning(paste('skipped *Edgeslist block at line',lineNumber, ' read.paj does not yet know how to parse it '))
    # TODO: see http://vlado.fmf.uni-lj.si/vlado/podstat/AO/net/TinaList.net
  }



 } # end file-parsing while loop
 if(verbose){
   print(paste('End of file reached at line',lineNumber))
 }


 #if(is.null(network.title)) network.title <- network.name

 if(debug){ 
   print(paste("nnetworks=",nnetworks))
   print(paste("network.names=",network.names))
   print(paste("vertex null?",is.null(vertex)))
   print(paste("network.title=",network.title))
   print(paste("vector null?",is.null(vector)))
   print(paste("colnames.vector=",colnames.vector))
   print(paste("nprojects=",length(projects)))
   print(paste("names.projects=",names(projects)))
 }

 if(verbose) print(paste("number of networks found:",nnetworks))  #dschruth added
 # ------------  post-processing --------------------
   if(nnetworks > 0){
     if(debug) print("assembling networks into 'project' before returning")
     # grab all the named networks from the environment
     # and put 'em in a list
     networksData<-lapply(network.names,function(netName){get(netName)})
     # TODO: delete networks from environment to clear up space?
     # this code takes the various objects that have been parsed from the .paj file and assembles
     # them into a network object (or list of network objects, a 'project'), doing some appropriate conversion
     projects <- postProcessProject( network.title,
                                     vector, 
                                     colnames.vector, 
                                     vertex,  # data for building vertices,
                                     nnetworks, # number of networks found,
                                     network.names,  # names of networks found
                                     networksData,
                                     projects,
                                     verbose
     )
   }  else { # networks have not been created, but need to check if only vertices have been found
     if(!is.null(vertex)){
       # need to initialize a network here to deal with the case where no arcs/edge in the file
       # Note that without the arcs/edge, we have no way to know if network was supposed to be directed or multiplex
       networksData<-list( network.initialize(n=nvertex, bipartite=nactors))
       projects <- postProcessProject( network.title,
                                       vector, 
                                       colnames.vector, 
                                       vertex,  # data for building vertices,
                                       nnetworks, # number of networks found,
                                       network.names = network.title,  # names of networks found
                                       networksData,
                                       projects,
                                       verbose)
     }
   }
 



   if(is.null(partition)){
     if(verbose) print(paste("number of projects",length(projects)))  #dschruth added
     # if there is only one 'project' (network) remove it from the list and return it that way. 
     if(length(projects)==1){
       projects <- projects[[1]]
     }
     if(nnetworks>1){
       if (verbose){
         print('appending network objects into a network.series')
       }
       class(projects) <- "network.series"
     }
   }else{
     names(partition) <- names.partition
     if (verbose){
       print('returning projects and partitions as seperate list elements')
     }
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


# this code takes the various objects that have been parsed from the .paj file and assembles
# them into a network object (or list of network objects, a 'project'), doing some appropriate conversion
# this is called whenever the main parsing loop believes that it has finished with a section of
# the .paj file describing a group of networks. 
# this code is extracted here because it can be called from two different places and must remain identical
postProcessProject<-function(
                         network.title,
                         vector, 
                         colnames.vector, 
                         vertex,  # data for building vertices,
                         nnetworks, # number of networks found,
                         network.names,  # names of networks found
                         networksData, # list of basic networks created
                         projects,
                         verbose
                         ){
  colnames(vector) <- colnames.vector
  colnames(vertex) <- c("vertex.numbers","vertex.names","cen1","cen2")[1:ncol(vertex)]
  networks <- vector("list",length=nnetworks)
  if(verbose) print(paste("working along network names",paste(network.names,collapse=', ')))
  for(i in seq(along=network.names)){
    temp <- networksData[[i]]
    if(!is.null(vertex)){  #Changed "vector" to "vertex"
      ##Start new addition from Alex Montgomery (Thanks AHM!)
      if (nrow(as.data.frame(vertex)) == network.size(temp)) {
        temp <- set.vertex.attribute(temp, "vertex.names",
                                     as.character(vertex[as.numeric(vertex[,1]),2]))
        if (ncol(vertex)>2) { # number of columns > 2 -> vertex has attributes
          vert.attr.nam <- c("na","vertex.names","x","y","z") #assume first three are coords (true?)
          # TODO: x y and z may be omitted, check if exist and numeric
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
      }
    }
    if(!is.null(network.title)){
      temp <- set.network.attribute(temp, "title", network.title) # not sure if this should also be the edges relation?
    }else{
      warning("null network title")
    }
    if(nrow(as.data.frame(vertex))== network.size(temp)){ #should i be doing this? why don't these numbers match all time
      temp <- set.vertex.attribute(temp,"vertex.names",as.character(vertex[as.numeric(vertex[,1]),2]))
    }
    
    networks[[i]] <- temp
    if (verbose) print(paste("processed and added",network.names[i],"to list of networks"))
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
  projNames<-names(projects)
  projects <- c(projects,list(networks))
  names(projects) <-c(projNames, network.title)
  return(projects)
}


# reads a single line of a file, splits it into tokens on ' ' and returns as string vector
readAndVectorizeLine <- function(file){
  line <- readLines(file, 1, ok = TRUE)
  if(!inherits(line,"try-error") & length(line)>0){
    line <- strsplit(line," ")[[1]]
    line <- line[line!=""]
  }
  line
}




read.paj.simplify <- function(x,file,verbose=FALSE) 
  {
   classx <- class(x)
   if(classx=="network"){
    cat(paste(file," is a single network object.\n",sep=""))
    assign(file,x)
    save(list=file,
         file=paste(file,".RData",sep=""))
    cat(paste("network saved as a 'network' object in ",file,".RData.\n",sep=""))
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
     cat(paste("network saved as a 'network' object in ",file,".RData.\n",sep=""))
     return(x$networks[[1]])
    }else{
     assign(file,x)
     save(list=file,
          file=paste(file,".RData",sep=""))
     cat(paste("network saved as a 'network.series' object in ",file,".RData.\n",sep=""))
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
      cat(paste("network saved as a 'network' object and a separate partition list in ",file,".RData.\n",sep=""))
     }else{
      cat(paste("network simplified to a 'network.series' object plus partition.\n",sep=""))
      cat(paste("network saved as a 'network.series' object and a separate partition list in ",file,".RData.\n",sep=""))
     }
    }
   }
   return(x)
} 


# swaps the first two columns (tail, heads) in a matrix
switchArcDirection <- function(edgelist){
edgelist[,1:2] <- edgelist[,2:1]
edgelist
}


