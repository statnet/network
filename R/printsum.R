######################################################################
#
# printsum.R
#
# Written by Carter T. Butts <buttsc@uci.edu>; portions contributed by
# David Hunter <dhunter@stat.psu.edu> and Mark S. Handcock
# <handcock@u.washington.edu>.
#
# Last Modified 4/09/06
# Licensed under the GNU General Public License version 2 (June, 1991)
#
# Part of the R/network package
#
# This file contains various routines for printing/summarizing 
# network class objects.
#
# Contents:
#
#   print.network
#   print.summary.network
#   summary.network
#
######################################################################


# Printing for network class objects.
#
print.network<-function(x, matrix.type=which.matrix.type(x), 
                      mixingmatrices=FALSE, na.omit=TRUE, ...)
{
    cat("Network attributes:\n")
    for(i in 1:length(x$gal)){
      if (names(x$gal)[i]=="n"){
          attributeName<-"vertices"
          attributeValue<-x$gal[[i]]
      } else {
          attributeName<-names(x$gal)[i]
          attributeValue<-x$gal[[i]]
      }
      if(is.network(attributeValue)){
        if(attributeName=="design"){
         cat("  ",attributeName,"=\n")
         cat("       total missing =",network.edgecount(attributeValue),"\n")
         cat("     percent missing =",network.density(attributeValue),"\n")
        }else{
         cat("  ",attributeName,":\n",sep="")
         if(is.discrete(attributeName)){
          assign(paste("  ",attributeName,attrvalue))
          print(table(get(paste("  ",attributeName))))
          if(mixingmatrices){
           cat("\n","mixing matrix for ",attributeName,":\n",sep="")
           print(mixingmatrix(x,attributeName))
          }
         }else{
          print(summary(attributeName))
         }
        }
      }else{
        if(attributeName!="mnext"){
         cat("  ",attributeName,"=",attributeValue,"\n")
        }
      }
    }
    cat("   total edges=",network.edgecount(x),"\n")
    vna<-names(x$val[[1]])
    if(na.omit){
     vna<-vna[vna!="na"]
    }
    if(length(vna)==0){
     cat("\n","No nodal attributes","\n",sep="")
    }else{
     cat("\n","Nodal attribute names:","\n")
     cat("   ",vna,"\n")
    }
    cat("\n",matrix.type,"matrix:\n")    
    if(network.edgecount(x)>0)
      print(as.matrix.network(x,matrix.type=matrix.type))
    else
      cat("Empty Graph\n")
    invisible(x)
}


print.summary.network<-function(x, ...)
    print.network(structure(g,class="network"), ...)


# Summaries of network objects
#
summary.network<-function(object, na.omit=TRUE, mixingmatrices=FALSE, ...){
    cat("Network attributes:\n")
    for(i in 1:length(object$gal)){
      if (names(object$gal)[i]=="n"){
          attributeName<-"vertices"
          attributeValue<-object$gal[[i]]
      } else {
          attributeName<-names(object$gal)[i]
          attributeValue<-object$gal[[i]]
      }
      if(is.network(attributeValue)){
        if(attributeName=="design"){
         cat("  ",attributeName,"=\n")
         cat("       total missing =",network.edgecount(attributeValue),"\n")
         cat("     percent missing =",network.density(attributeValue),"\n")
        }else{
         if(attributeName!="vertex.names"){
          cat("  ",attributeName,":\n", sep="")
          if(is.discrete(attributeValue)){
           assign(paste("  ",attributeName),attributeValue)
           print(table(get(paste("  ",attributeName))))
           if(mixingmatrices){
            cat("\n","mixing matrix for ",attributeName,":\n",sep="")
            print(mixingmatrix(object,attributeName))
           }
          }else{
           print(summary(attributeValue))
          }
         }
        }
      }else{
        if(attributeName!="mnext"){
         cat("  ",attributeName,"=",attributeValue,"\n")
        }
      }
    }

    cat("   total edges=",network.edgecount(object),"\n")
    cat("   density =",network.density(object),"\n")

    vna<-names(object$val[[1]])
    if(na.omit){
     vna<-vna[vna!="na"]
    }
    edgevalues <- list.edge.attributes(object)
    if(na.omit){
     edgevalues<-edgevalues[edgevalues!="na"]
    }
    if(length(edgevalues)>0){
     cat("\n","Edge attributes:","\n")
     for (i in (1:length(edgevalues))){ 
       cat("       ",edgevalues[i],"\n")
     }
    }
    if(length(vna)==0){
     cat("\n","No nodal attributes","\n",sep="")
    }else{
     cat("\n","Nodal attributes:","\n")
     for (i in (1:length(vna))){ 
      attrvalue <- unlist(get.vertex.attribute(object, vna[i]))
      if(vna[i]=="respondent"){
       cat("       total missing =",sum(!attrvalue),"\n")
       cat("     percent missing =",mean(!attrvalue),"\n")
      }else{
       cat("\n",vna[i],":\n",sep="")
       if(is.discrete(attrvalue)){
        assign(paste("  ",vna[i]),attrvalue)
        print(table(get(paste("  ",vna[i]))))
        if(mixingmatrices){
         cat("\n","mixing matrix for ",vna[i],":\n",sep="")
         print(mixingmatrix(object,vna[i]))
        }
       }else{
        print(summary(attrvalue))
       }
      }
     }
    }
invisible()
}

