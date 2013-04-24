######################################################################
#
# printsum.R
#
# Written by Carter T. Butts <buttsc@uci.edu>; portions contributed by
# David Hunter <dhunter@stat.psu.edu> and Mark S. Handcock
# <handcock@u.washington.edu>.
#
# Last Modified 7/23/08
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
#   summary.character
#   summary.network
#
######################################################################


# Printing for network class objects.
#
print.network<-function(x, matrix.type=which.matrix.type(x), 
                      mixingmatrices=FALSE, na.omit=TRUE, ...)
{
    cat(" Network attributes:\n")
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
          cat(" ",attributeName,"=\n")
          cat("      total missing =",network.edgecount(attributeValue),"\n")
          cat("    percent missing =",network.density(attributeValue),"\n")
        }else{
          cat(" ",attributeName,":\n",sep="")
          if(is.discrete(attributeValue)){
            assign(paste(" ",attributeName,attributeValue))
            print(table(get(paste("  ",attributeName))))
            if(mixingmatrices){
              cat("\n","mixing matrix for ",attributeName,":\n",sep="")
              print(mixingmatrix(x,attributeName))
            }
         }else{
            print(summary(attributeValue))
         }
        }
      }else{
        if(attributeName!="mnext"){
           if(is.discrete(attributeValue)){
             assign(paste(" ",attributeName,attributeValue))
             print(table(get(paste("  ",attributeName))))
          }else{
             if(length(attributeValue) < 10){
               cat(" ",attributeName,"=",attributeValue,"\n")
             }else{
               cat(" ",attributeName,":\n", sep="")
               print(summary(attributeValue))
             }
          }
        }
      }
    }
    cat("  total edges=",network.edgecount(x,na.omit=FALSE),"\n")
    cat("    missing edges=",network.naedgecount(x),"\n")
    cat("    non-missing edges=",network.edgecount(x,na.omit=TRUE),"\n")
    vna<-names(x$val[[1]])
    if(na.omit){
      vna<-vna[vna!="na"]
    }
    if(length(vna)==0){
      cat("\n","No vertex attributes","\n",sep="")
    }else{
      cat("\n","Vertex attribute names:","\n")
      cat("   ",vna,"\n")
    }
    cat("\n",matrix.type,"matrix:\n")    
    if(network.edgecount(x)>0)
      print(as.matrix.network(x,matrix.type=matrix.type))
    else
      cat("Empty Graph\n")
    invisible(x)
}


#Print method for summary.character
print.summary.character <- function(x, max.print=10, ...){
  x <- as.factor(x)
  if(length(table(x)) <= max.print){
    summary(x)
  }else{
    cat(paste("   the first",max.print,"unique values are:\n"))
    summary(x)[1:max.print]
  }
  invisible(x)
}


#Print method for summary.network
print.summary.network<-function(x, ...){
    #Pull any extra goodies from summary.network (stored in gal)
    na.omit<-x%n%"summary.na.omit"
    mixingmatrices<-x%n%"summary.mixingmatrices"
    #Print the object
    class(x)<-"network"
    cat(" Network attributes:\n")
    for(i in 1:length(x$gal)){
      if (names(x$gal)[i]=="n"){
        attributeName<-"vertices"
        attributeValue<-x$gal[[i]]
      } else {
        attributeName<-names(x$gal)[i]
        attributeValue<-x$gal[[i]]
      }
      if(attributeName!="mnext"){
        if(is.network(attributeValue)){
          if(attributeName=="design"){
            cat(" ",attributeName,"=\n")
            cat("      total missing =",network.edgecount(attributeValue),"\n")
            cat("    percent missing =",network.density(attributeValue),"\n")
          }else{
            cat(" ",attributeName,"=\n")
            print(attributeValue)
          }
        }else{
          if(is.discrete(attributeValue)){
            assign(paste(" ",attributeName,attributeValue))
            print(table(get(paste("  ",attributeName))))
            if(mixingmatrices){
              cat("\n","mixing matrix for ",attributeName,":\n",sep="")
              print(mixingmatrix(x,attributeName))
            }
          }else{
            if(length(attributeValue) < 10){
              cat(" ",attributeName,"=",attributeValue,"\n")
            }else{
              cat("  ",attributeName,":\n", sep="")
              print(summary(attributeValue))
            }
          }
        }
      }
    }

    cat("  total edges=",network.edgecount(x),"\n")
    cat("  density =",network.density(x),"\n")

    vna<-names(x$val[[1]])
    if(na.omit){
      vna<-vna[vna!="na"]
    }
    edgevalues <- list.edge.attributes(x)
    if(na.omit){
      edgevalues<-edgevalues[edgevalues!="na"]
    }
    if(length(edgevalues)>0){
      cat("\n","Vertex attributes:")
      for (i in (1:length(edgevalues))){ 
        cat("       ",edgevalues[i],"\n")
      }
    }
    if(length(vna)==0){
      cat("\n","No vertex attributes","\n",sep="")
    }else{
      cat("\n","Vertex attributes:","\n")
      for (i in (1:length(vna))){ 
        attributeValue <- unlist(get.vertex.attribute(x, vna[i]))
        if(vna[i]=="respondent"){
          cat("\n      total missing =",sum(!attributeValue),"\n")
          cat("    percent missing =",mean(!attributeValue),"\n")
        }else{
          cat("\n  ",vna[i],":\n",sep="")
          if(is.discrete(attributeValue)){
            assign(paste("  ",vna[i]),attributeValue)
            print(table(get(paste("  ",vna[i]))))
            if(mixingmatrices){
              cat("\n"," mixing matrix for ",vna[i],":\n",sep="")
              print(mixingmatrix(x,vna[i]))
            }
          }else{
            if(length(attributeValue) < 10){
              cat("  ",attributeValue,"\n")
            }else{
              print(summary(attributeValue))
            }
          }
        }
      }
    }
    print.network(structure(x,class="network"), ...)
    invisible(x)
}


#An internal routine to handle summaries of characters
summary.character <- function(object, ...){
  class(object)<-c("summary.character",class(object))
  object
}


# Summaries of network objects
#
summary.network<-function(object, na.omit=TRUE, mixingmatrices=FALSE, ...){
  #Add printing parameters as network objects, and change the class
  object%n%"summary.na.omit"<-na.omit
  object%n%"summary.mixingmatrices"<-mixingmatrices
  class(object)<-"summary.network"
  #Return the object
  object
}

