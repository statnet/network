# implementation of file reader for UCINET's DL EDGELIST1 format

#http://www.analytictech.com/ucinet/documentation/reference.rtf
# Edgelist1 
# This format is used on data forming a matrix in which the rows and columns refer to the same kinds of objects (e.g., an illness-by-illness proximity matrix, or a person-by-person network). The 1-mode matrix X is built from pairs of indices (a row and a column indicator). Pairs are typed one to a line, with indices separated by spaces or commas. The presence of a pair i,j indicates that there is a link from i to j, which is to say a non-zero value in x(i,j). Optionally, the pair may be followed by a value representing an attribute of the link, such as its strength or quality. If no value is present, it is assumed to be 1.0. If a pair is omitted altogether, it is assigned a value of 0.0. 

# also
# http://www.analytictech.com/networks/dataentry.htm

# DL n=5
# format = edgelist1
# labels:
#   george, sally, jim, billy, jane
# data:
#   1 2
# 1 3
# 2 3
# 3 1
# 4 3
# 
# Or the labels can be included in the data themselves:
#   DL n=5
# format = edgelist1
# labels embedded:
#   data:
#   george sally
# george jim
# sally jim
# billy george
# jane jim
# 
# Nodes are numbered in the order of first appearance. To control the order of the nodes in the resulting data matrix, just include the labels in the header section and also embed them in the data:
#   DL n=5
# format = edgelist1
# labels:
#   george, sally, jim, billy, jane
# labels embedded:
#   data:
#   george sally
# george jim
# sally jim
# billy george
# jane jim 


# example files:
# http://moreno.ss.uci.edu/pro-pro.dat
# http://moreno.ss.uci.edu/elegans.dat
# http://moreno.ss.uci.edu/hens.dat
# http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/ucidata.htm


read.dl<-function(file,verbose=TRUE){
  networkSize<-NULL
  format<-NULL
  vertex.names<-NULL
  dataMatrix<-NULL
  labelsEmbedded<-FALSE
  fileContents<-readLines(file)
  rowIndex<-1
  # first 2 characters of first line must be dl
  if(grep('^dl',fileContents[rowIndex],ignore.case=TRUE)<=0){
    stop("a DL formated file must begin with 'DL'")
  }
  # look for a declaration of the number of vertices
  # do as loop so can break after finding
  while (rowIndex < length(fileContents)){
    element<-fileContents[rowIndex]
    element<-toupper(element)
    index<-grep('N=',element)
    if (length(index) > 0){
      element<-substr(element,regexpr('N=',element,fixed=TRUE)+2,nchar(element))
      networkSize<-as.integer(element)
      rowIndex<-rowIndex+1
      break
    }
    rowIndex <-rowIndex+1
  }
  
  if(is.null(networkSize)){
    stop("unable to locate a network size definition in DL file (n=?)")
  }
  
  # look for format declaration
  while (rowIndex <= length(fileContents)){
    element<-fileContents[rowIndex]
    element<-toupper(element)
    index<-grep('FORMAT =',element,ignore.case = TRUE)
    if (length(index) > 0){
      element<-substr(element,regexpr('FORMAT = ',element,fixed=TRUE)+9,nchar(element))
      tokens<-strsplit(element,' ')
      format<-toupper(tokens[1])
      rowIndex<-rowIndex+1
      break
    }
    rowIndex<-rowIndex+1
  }
  if(verbose){
    message('format is ',format)
  }
  if(is.null(format)){
    stop('unable to locate the format tag in the DL file header')
  } 
  if(format!='EDGELIST1'){
    stop("the DL parser currently only supports the EDGELIST1 format")
  }
  
  # look for labels tag
  for (rIndex in rowIndex:length(fileContents)){
    element<-fileContents[rIndex]
    index<-grep('labels:',element,ignore.case = TRUE)
    # todo: also check for labels embedded
    if (length(index) > 0){
      if(verbose){
        message('found labels')
      }
      # assume the next row is labels
      rowIndex<-rowIndex+1
      labelRow<-fileContents[rowIndex]
      # comma seperated
      vertex.names<-strsplit(labelRow,', ')[[1]]
      # there must be as many labels as there are vertices
      if (length(vertex.names)!=networkSize){
        stop("number of vertex labels (",length(vertex.names),") does not match network size ",networkSize)
      }
      rIndex<-rIndex+1
      break
    }
    rIndex<-rIndex+1
  }
  
  # look for labels embedded tag
  for (rIndex in rowIndex:length(fileContents)){
    element<-fileContents[rIndex]
    index<-grep('labels embedded:',element,ignore.case = TRUE)
    # todo: also check for labels embedded
    if (length(index) > 0){
      labelsEmbedded<-TRUE
      rowIndex<-rowIndex+1
      break
    }
    rIndex<-rIndex+1
  }
  
  # look for the data row
  while (rowIndex <= length(fileContents)){
    element<-fileContents[rowIndex]
    index<-grep('data:',element,ignore.case = TRUE)
    if (length(index) > 0){
      # assume the next row is data
      rowIndex<-rowIndex+1
      # parse data until blank line or end
      while (rowIndex <= length(fileContents)){
        dataRow<-fileContents[rowIndex]
        if (dataRow==''){
          rowIndex<-rowIndex+1
          break
        }
        # space delimited
        dataMatrix<-rbind(dataMatrix,strsplit(dataRow,' ')[[1]])
        rowIndex<-rowIndex+1
      }
      break
    }
    rowIndex<-rowIndex+1
  }
  
  # optionally parse additional data rows or edgelists?
  
  
  # actuall create the network
  net<-network.initialize(networkSize)
  if(!is.null(vertex.names)){
    network.vertex.names(net)<-vertex.names
  }
  # if labels embedded, need to remap
  if (labelsEmbedded){
    # if the vertex names don't exist yet, create them
    if(is.null(vertex.names)){
      vertex.names<-unique(as.vector(dataMatrix[,1:2]))
      network.vertex.names(net)<-vertex.names
    }
    # remap the first two columns using vertex names
    dataMatrix[,1]<-match(dataMatrix[,1],vertex.names)
    dataMatrix[,2]<-match(dataMatrix[,2],vertex.names)
  }
  network.edgelist(dataMatrix,net)
  return(net)
}