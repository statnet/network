/*
######################################################################
#
# access.c
#
# Written by Carter T. Butts <buttsc@uci.edu>
# Last Modified 09/04/10
# Licensed under the GNU General Public License version 2 (June, 1991)
#
# Part of the R/network package
#
# This file contains routines related to access methods for network
# objects.
#
######################################################################
*/
 
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include "utils.h"
#include "access.h"


/*INTERNAL ROUTINES----------------------------------------------------*/


SEXP deleteEdgeAttribute(SEXP x, int e, const char *attrname)
/*Deletes the attribute named by attrname from edge with ID e.*/
{
  int pc=0;
  SEXP edge,atl;
  
  edge=VECTOR_ELT(getListElement(x,"mel"),e-1);
  PROTECT(atl=deleteListElement(getListElement(edge,"atl"),attrname)); pc++;
  edge=setListElement(edge,"atl",atl);

  UNPROTECT(pc);
  return x;
}


SEXP deleteNetworkAttribute(SEXP x, const char *attrname)
/*Deletes the network attribute named by attrname.*/
{
  int pc=0;
  SEXP gal;
  
  PROTECT(gal=deleteListElement(getListElement(x,"gal"),attrname)); pc++;
  setListElement(x,"gal",gal);

  UNPROTECT(pc);
  return x;
}


SEXP deleteVertexAttribute(SEXP x, int v, const char *attrname)
/*Deletes the attribute named by attrname from vertex with ID v.*/
{
  int pc=0;
  SEXP val,atts;
  
  val=getListElement(x,"val");
  PROTECT(atts=deleteListElement(VECTOR_ELT(val,v-1),attrname)); pc++;
  SET_VECTOR_ELT(val,v-1,atts);

  UNPROTECT(pc);
  return x;
}


SEXP getEdgeAttribute(SEXP x, int e, const char *str)
/*Returns a pointer to the attribute of edge e named by str, or else R_NilValue (if the edge and/or attribute is missing).*/
{
  SEXP el;

  /*Retrieve the edge, and sound a warning if not present.*/
  el=VECTOR_ELT(getListElement(x,"mel"),e-1);
  if(el==R_NilValue){
    warning("Attempt to get attribute %s for edge %e failed in getEdgeAttribute: no such edge.\n",str,e);
    return R_NilValue;
  }
  
  return getListElement(getListElement(el,"atl"),str);
}


SEXP getEdgeIDs(SEXP x, int v, int alter, const char *neighborhood, int naOmit)
/*Retrieve the IDs of all edges incident on v, in network x.  Outgoing or incoming edges are specified by neighborhood, while na.omit indicates whether or not missing edges should be omitted.  If alter>0, only edges whose alternate endpoints contain alter are returned.  The return value is a vector of edge IDs.*/
{
  SEXP eids,newids,mel,ilist,olist,eplist;
  int i,j,pc=0,ecount,*keep,dir;
  
  /*Enforce "combined" behavior unless x is directed*/
  dir=isDirected(x);
  /*Rprintf("getEdgeIDs: v=%d, a=%d, neighborhood=%s\n",v,alter,neighborhood);*/
  /*Begin by getting all edge IDs for the neighborhood in question*/
  if(dir&&(strcmp(neighborhood,"out")==0)){
    PROTECT(eids=coerceVector(VECTOR_ELT(getListElement(x,"oel"),v-1),INTSXP)); pc++;
  }else if(dir&&(strcmp(neighborhood,"in")==0)){
    PROTECT(eids=coerceVector(VECTOR_ELT(getListElement(x,"iel"),v-1),INTSXP)); pc++;
  }else{
    PROTECT(ilist=coerceVector(VECTOR_ELT(getListElement(x,"oel"),v-1), INTSXP)); pc++;
    PROTECT(olist=coerceVector(VECTOR_ELT(getListElement(x,"iel"),v-1), INTSXP)); pc++;
    /*Rprintf("\tAbout to enter union with list lengths %d and %d\n", length(ilist),length(olist));*/
    PROTECT(eids=vecUnion(ilist,olist)); pc++;
    /*Rprintf("\t\tEscaped vecUnion, new list is length %d\n",length(eids));*/
    /*    PROTECT(eids=vecUnion(coerceVector(VECTOR_ELT(getListElement(x,"oel"),v-1), INTSXP), coerceVector(VECTOR_ELT(getListElement(x,"iel"),v-1),INTSXP))); pc++;*/
  }
  /*Rprintf("\tIdentified %d candidate edges\n",length(eids));
  if(length(eids)>0)
    Rprintf("\t\tFirst edge is ID %d\n",INTEGER(eids)[0]);*/
    
  /*Remove any edges not containing alter (if given) and/or missing (if naOmit
  is TRUE).*/
  ecount=0;
  keep=(int *)R_alloc(length(eids),sizeof(int));
  mel=getListElement(x,"mel");
  for(i=0;i<length(eids);i++){
    keep[i]=1;
    if(alter>0){                     /*Remove edges not containing alter?*/
      /*Get the relevant endpoints of the edge in question*/
      if(dir&&(strcmp(neighborhood,"out")==0)){
        PROTECT(eplist=coerceVector(getListElement(VECTOR_ELT(mel, INTEGER(eids)[i]-1),"inl"),INTSXP)); pc++;
      }else if(dir&&(strcmp(neighborhood,"in")==0)){
        PROTECT(eplist=coerceVector(getListElement(VECTOR_ELT(mel, INTEGER(eids)[i]-1),"outl"),INTSXP)); pc++;
      }else{
        PROTECT(ilist=coerceVector(getListElement(VECTOR_ELT(mel, INTEGER(eids)[i]-1),"inl"),INTSXP)); pc++;
        PROTECT(olist=coerceVector(getListElement(VECTOR_ELT(mel, INTEGER(eids)[i]-1),"outl"),INTSXP)); pc++;
        PROTECT(eplist=vecAppend(ilist,olist)); pc++;
      }
      /*Check to see if any endpoint matches alter*/
      keep[i]=0;
      for(j=0;(j<length(eplist))&&(!keep[i]);j++)
        if(INTEGER(eplist)[j]==alter){
          keep[i]++;
        }
    }
    if(naOmit){                      /*Remove missing edges*/
      /*Rprintf("\t\t\tEntering Omit step...\n");*/
      if(INTEGER(coerceVector(getListElement(getListElement(VECTOR_ELT(mel, INTEGER(eids)[i]-1),"atl"),"na"),LGLSXP))[0])
        keep[i]=0;
      /*Rprintf("\t\t\tLeaving Omit step.\n");*/
    }
    ecount+=keep[i];
  }  
  /*Rprintf("\tecount=%d\n",ecount);*/
  PROTECT(newids=allocVector(INTSXP,ecount)); pc++;  /*Create edge ID list*/
  ecount=0;
  for(i=0;i<length(eids);i++)
    if(keep[i])
      INTEGER(newids)[ecount++]=INTEGER(eids)[i];
  
 /* Rprintf("\tReturning %d edge IDs\n",length(newids));
  if(length(newids)>0)
    Rprintf("\t\tFirst ID is %d\n",INTEGER(newids)[0]);*/
  /*Unprotect and return*/
  UNPROTECT(pc);
  return newids;
}


SEXP getEdges(SEXP x, int v, int alter, const char *neighborhood, int naOmit)
/*Retrieve all edges incident on v, in network x.  Outgoing or incoming edges are specified by neighborhood, while na.omit indicates whether or not missing edges should be omitted.  If alter>0, only edges whose alternate endpoints contain alter are returned.  The return value is a list of edges.*/
{
  SEXP eids,el,mel,eplist;
  int i,j,pc=0,ecount,*keep,dir;
  
  /*If x is undirected, enforce "combined" behavior*/
  dir=isDirected(x);
  /*Rprintf("getEdges: v=%d, a=%d, neighborhood=%s\n",v,alter,neighborhood);*/
  /*Begin by getting all edge IDs for the neighborhood in question*/
  if(dir&&(strcmp(neighborhood,"out")==0)){
    PROTECT(eids=coerceVector(VECTOR_ELT(getListElement(x,"oel"),v-1),INTSXP)); pc++;
  }else if(dir&&(strcmp(neighborhood,"in")==0)){
    PROTECT(eids=coerceVector(VECTOR_ELT(getListElement(x,"iel"),v-1),INTSXP)); pc++;
  }else{
    PROTECT(eids=vecUnion(coerceVector(VECTOR_ELT(getListElement(x,"oel"),v-1), INTSXP), coerceVector(VECTOR_ELT(getListElement(x,"iel"),v-1),INTSXP))); pc++;
  }
  
  /*Extract the edges associated with the eid list, removing any edges not
    containing alter (if given) and/or missing (if naOmit is TRUE).*/
  ecount=0;
  keep=(int *)R_alloc(length(eids),sizeof(int));
  mel=getListElement(x,"mel");
  for(i=0;i<length(eids);i++){
    keep[i]=1;
    if(alter>0){                     /*Remove edges not containing alter?*/
      /*Get the relevant endpoints of the edge in question*/
      if(dir&&(strcmp(neighborhood,"out")==0)){
        PROTECT(eplist=coerceVector(getListElement(VECTOR_ELT(mel, INTEGER(eids)[i]-1),"inl"),INTSXP)); pc++;
      }else if(dir&&(strcmp(neighborhood,"in")==0)){
        PROTECT(eplist=coerceVector(getListElement(VECTOR_ELT(mel, INTEGER(eids)[i]-1),"outl"),INTSXP)); pc++;
      }else{
        PROTECT(eplist=vecAppend(coerceVector(getListElement(VECTOR_ELT(mel, INTEGER(eids)[i]-1),"inl"),INTSXP),coerceVector(getListElement(VECTOR_ELT(mel, INTEGER(eids)[i]-1),"outl"),INTSXP))); pc++;
      }
      /*Check to see if any endpoint matches alter*/
      keep[i]=0;
      for(j=0;(j<length(eplist))&&(!keep[i]);j++)
        if(INTEGER(eplist)[j]==alter){
          keep[i]++;
        }
    }
    if(naOmit){                    /*Remove missing edges*/
      if(INTEGER(coerceVector(getListElement(getListElement(VECTOR_ELT(mel, INTEGER(eids)[i]-1),"atl"),"na"),LGLSXP))[0])
        keep[i]=0;
    }
    ecount+=keep[i];
  }  
  PROTECT(el=allocVector(VECSXP,ecount)); pc++;  /*Create edge list*/
  ecount=0;
  for(i=0;i<length(eids);i++)
    if(keep[i])
      SET_VECTOR_ELT(el,ecount++,VECTOR_ELT(mel,INTEGER(eids)[i]-1));
  
  /*Unprotect and return*/
  UNPROTECT(pc);
  return el;
}


SEXP getNeighborhood(SEXP x, int v, const char *type, int naOmit)
/*Return a vector containing the first-order vertex neighborhood of v in x, as specified by type.  If naOmit>0, missing edges are discarded; otherwise, they are employed as well.*/
{
  int pc=0,i,dir;
  SEXP el,eps,val=R_NilValue;
  
  /*Check for directedness of x*/
  dir=isDirected(x);
  /*Accumulate endpoints from the edge list*/
  PROTECT(eps=allocVector(INTSXP,0)); pc++;
  if(dir&&(strcmp(type,"in")==0)){                /*In => get tail list*/
    PROTECT(el = getEdges(x,v,0,"in",naOmit)); pc++;
    for(i=0;i<length(el);i++){
      PROTECT(eps=vecAppend(eps,coerceVector(getListElement(VECTOR_ELT(el,i), "outl"),INTSXP))); pc++;
    }
  }else if(dir&&(strcmp(type,"out")==0)){        /*Out => get head list*/
    PROTECT(el = getEdges(x,v,0,"out",naOmit)); pc++;
    for(i=0;i<length(el);i++){
      PROTECT(eps=vecAppend(eps,coerceVector(getListElement(VECTOR_ELT(el,i), "inl"),INTSXP))); pc++;
    }
  }else{                                         /*Combined => get both lists*/
    if(!dir){ /*Annoying kludge to deal with getEdges loop issue, part 1*/
      /*The issue here is that getEdges (reasonably?) enforces "combined"
      behavior for undirected graphs, returning any edge with v as an endpoint.
      This clashes with what we need to do here; as a workaround, we temporarily
      make x "directed" to change the behavior of getEdges (afterwards changing
      it back).  This works fine, but involves two unneeded write operations
      for what should be a read-only function.  As such, it should eventually
      be patched (probably by creating an option to force the behavior of
      getEdges).*/
      PROTECT(val=allocVector(LGLSXP,1)); pc++;
      LOGICAL(val)[0]=1;
      x=setNetworkAttribute(x,"directed",val);  /*Temporarily make directed*/
    }
    PROTECT(el = getEdges(x,v,0,"in",naOmit)); pc++;
    for(i=0;i<length(el);i++){
      PROTECT(eps=vecAppend(eps,coerceVector(getListElement(VECTOR_ELT(el,i), "outl"),INTSXP))); pc++;
    }
    PROTECT(el = getEdges(x,v,0,"out",naOmit)); pc++;
    for(i=0;i<length(el);i++){
      PROTECT(eps=vecAppend(eps,coerceVector(getListElement(VECTOR_ELT(el,i), "inl"),INTSXP))); pc++;
    }
    if(!dir){ /*Annoying kludge to deal with getEdges loop issue, part 2*/
      LOGICAL(val)[0]=0;
      x=setNetworkAttribute(x,"directed",val);  /*Restore to undirected*/
    }
  }

  /*Consolidate the endpoint list*/
  PROTECT(eps=vecUnique(eps)); pc++;

  /*Unprotect and return*/
  UNPROTECT(pc);
  return eps;
}


SEXP getNetworkAttribute(SEXP x, const char *str)
/*Returns a pointer to the network attribute of x named by str, or else R_NilValue.*/
{
  return getListElement(getListElement(x,"gal"),str);
}


int hasLoops(SEXP x)
/*Does x allow loops?*/
{
  SEXP atptr;
  
  atptr = coerceVector(getNetworkAttribute(x,"loops"),LGLSXP);
  
  return INTEGER(atptr)[0];
}


int isAdjacent(SEXP x, int vi, int vj, int naOmit)
/*Returns 0 if not adjacent, 1 if adjacent, and NA_INTEGER if missing and naOmit=0.*/
{
  SEXP mel,el,edge,endpts;
  int i,j,flag,isna,matchna,pc=0;
  
  /*Rprintf("\tInternal isAdjacent: seeking (%d,%d) w/naOmit=%d\n",vi,vj,naOmit);*/
  mel=getListElement(x,"mel");

  /*Start by hunting the outgoing edges of vi*/
  PROTECT(el=coerceVector(VECTOR_ELT(getListElement(x,"oel"),vi-1),INTSXP)); pc++;
  /*Rprintf("\t\tGot outgoing edge list for %d\n",vi);*/
  matchna=0;
  for(i=0;i<length(el);i++){
    /*Rprintf("\t\t\tCurrent edge is %d\n",INTEGER(el)[i]);*/
    edge=VECTOR_ELT(mel,INTEGER(el)[i]-1);
    isna=INTEGER(getListElement(getListElement(edge,"atl"),"na"))[0];  /*Missing edge?*/
    /*Rprintf("\t\t\tEdge missing status=%d\n",isna);*/
    PROTECT(endpts=coerceVector(getListElement(edge,"inl"),INTSXP)); pc++;      /*Get endpoints*/
    /*Rprintf("\t\t\tGot endpoints...looking for a match\n");*/
    flag=0;
    for(j=0;(!flag)&&(j<length(endpts));j++)   /*Check head of edge for vj*/
      if(INTEGER(endpts)[j]==vj){
        if(!isna){                             /*Return 1 on a clean match*/
          UNPROTECT(pc);
          return 1;
        }else{                  /*If matches but missing, note and move on*/
          matchna++;
          flag++;
        }
      }
  }
  /*Rprintf("\t\tDidn't find match...");*/
  /*Take stock of the situation*/
  if(isDirected(x)){            /*If directed, we're done here...*/
    if(matchna&&(!naOmit)){
      UNPROTECT(pc);
      return NA_INTEGER;          /*Matched a missing edge, not discounting*/
    }else{
      UNPROTECT(pc);
      return 0;                   /*Matched a missing edge, discounting*/
    }
  }

  /*If we're still here, x is undirected.  Now, we try vi's inedges.*/
  el=VECTOR_ELT(getListElement(x,"iel"),vi-1);
  PROTECT(el=coerceVector(VECTOR_ELT(getListElement(x,"iel"),vi-1),INTSXP)); pc++;
  for(i=0;i<length(el);i++){
    edge=VECTOR_ELT(mel,INTEGER(el)[i]-1);
    isna=INTEGER(getListElement(getListElement(edge,"atl"),"na"))[0];  /*Missing edge?*/
    PROTECT(endpts=coerceVector(getListElement(edge,"outl"),INTSXP)); pc++;      /*Get endpoints*/
    flag=0;
    for(j=0;(!flag)&&(j<length(endpts));j++)   /*Check tail of edge for vj*/
      if(INTEGER(endpts)[j]==vj){
        if(!isna){                             /*Return 1 on a clean match*/
          UNPROTECT(pc);
          return 1;
        }else{                  /*If matches but missing, note and move on*/
          matchna++;
          flag++;
        }
      }
  }
  /*Make the final decision*/
  if(matchna&&(!naOmit)){
    UNPROTECT(pc);
    return NA_INTEGER;          /*Matched a missing edge, not discounting*/
  }else{
    UNPROTECT(pc);
    return 0;                   /*Matched a missing edge, discounting*/
  }
}


int isDirected(SEXP x)
/*Is x directed?*/
{
  SEXP atptr;
  
  atptr = coerceVector(getNetworkAttribute(x,"directed"),LGLSXP);
  
  return INTEGER(atptr)[0];
}


int isHyper(SEXP x)
/*Is x hypergraphic?*/
{
  SEXP atptr;
  
  atptr = coerceVector(getNetworkAttribute(x,"hyper"),LGLSXP);
  
  return INTEGER(atptr)[0];
}


int isLoop(SEXP outl, SEXP inl)
/*Return 1 if outl and inl have any elements in common.  Note that both lists must be of type INTSXP.*/
{
  int i,j;
  
  for(i=0;i<length(outl);i++)
    for(j=0;j<length(inl);j++)
      if(INTEGER(outl)[i]==INTEGER(inl)[j])
        return 1;
  return 0;
}


int isMultiplex(SEXP x)
/*Is x multiplex?*/
{
  SEXP atptr;
  
  atptr = coerceVector(getNetworkAttribute(x,"multiple"),LGLSXP);
  
  return INTEGER(atptr)[0];
}


int isNetwork(SEXP x)
/*Is x a network object?*/
{
  SEXP classes;
  int flag=0,i;
  
  PROTECT(classes = getAttrib(x,install("class")));
  for(i=0;(!flag)&&(i<length(classes));i++)
    if(strcmp(CHAR(STRING_ELT(classes,i)),"network")==0)
      flag++;

  UNPROTECT(1);
  return flag;
}


int networkEdgecount(SEXP x, int naOmit)
/*Count the number of active edges in x.  If naOmit>0, then missing edges are not counted; otherwise, all edges are included.  (Note: this is the internal version.)*/
{
  int i,ecount=0,pc=0;
  SEXP mel,na;
  
  mel=getListElement(x,"mel");
  
  if(naOmit){                                   /*Omit missing edges*/
    for(i=0;i<length(mel);i++)
      if(VECTOR_ELT(mel,i)!=R_NilValue){
        PROTECT(na=coerceVector(getEdgeAttribute(x,i+1,"na"),LGLSXP));
        if(!INTEGER(na)[0])
         ecount++;
        UNPROTECT(1);
      }
  }else{                                       /*Count all edges*/
    for(i=0;i<length(mel);i++)
      ecount+=(VECTOR_ELT(mel,i)!=R_NilValue);
  }

  /*Unprotect and return*/
  UNPROTECT(pc);
  return ecount;
}


int networkSize(SEXP x)
/*Return the order of x.*/
{
  SEXP atptr;
  
  atptr = coerceVector(getNetworkAttribute(x,"n"),INTSXP);
  
  return INTEGER(atptr)[0];
}


SEXP setNetworkAttribute(SEXP x, const char *attrname, SEXP value)
/*Set the attribute whose name is pointed to by attrname in x to be equal to value.*/
{
  int pc=0;
  SEXP gal;
  
  gal=getListElement(x,"gal");                       /*Get the gal pointer*/
  PROTECT(gal=setListElement(gal,attrname,value)); pc++;   /*Set attribute*/
  x=setListElement(x,"gal",gal);                    /*Write new gal into x*/

  UNPROTECT(pc);
  return x;
}


SEXP setVertexAttribute(SEXP x, const char *attrname, SEXP value, int v)
/*Sets the attribute in attrname to value, for vertex v.  Existing attribute entries are overwritten by this routine, where present; new attributes are added, if they do not exist.*/
{
  int pc=0;
  SEXP val,vl;
  
  /*Get the vertex attribute list*/
  val=getListElement(x,"val");

  /*Update the attribute list*/
  PROTECT(vl=setListElement(VECTOR_ELT(val,v-1),attrname,value)); pc++;
  SET_VECTOR_ELT(val,v-1,vl);

  /*Unprotect and return*/
  UNPROTECT(pc);
  return x;
}


/*R-CALLABLE ROUTINES--------------------------------------------------*/


SEXP addEdge_R(SEXP x, SEXP tail, SEXP head, SEXP namesEval, SEXP valsEval, SEXP edgeCheck)
{
  int pc=0,i,j,mnext;
  SEXP el,atl,atlnam,navec,inl,outl,elnam,mel,newmel,oel,iel,ptr,elem,mnptr,gal;
  char buf[64];

  /*Make sure that we can read the head and tail lists*/
  PROTECT(inl = coerceVector(head, INTSXP)); pc++;
  PROTECT(outl = coerceVector(tail, INTSXP)); pc++;
  
  /*No matter what, ensure that all vertex references are legal; otherwise,*/
  /*addEdge_R will segfault on an illegal head/tail specification.*/
  if((vecMin(inl)<1.0)||(vecMin(outl)<1.0)|| (vecMax(inl)>(double)networkSize(x)) ||(vecMax(outl)>(double)networkSize(x)))
    error("(edge check) Illegal vertex reference in addEdge_R.  Exiting.");
  
  /*If necessary, verify that new edge satisfies existing graph requirements*/
  PROTECT(edgeCheck = coerceVector(edgeCheck, LGLSXP)); pc++;
  if(INTEGER(edgeCheck)[0]){
    if(length(inl)*length(outl)==0)
      error("(edge check) Empty head/tail list in addEdge_R.  Exiting.");
    if(!isHyper(x))
      if(MAX(length(inl),length(outl))>1)
        error("(edge check) Attempted to add hyperedge where hyper==FALSE in addEdge_R.  Exiting.");
    if(!hasLoops(x))
      if(isLoop(outl,inl))
        error("(edge check) Attempted to add loop-like edge where loops==FALSE in addEdge_R.  Exiting.");
    if((!isMultiplex(x))&&(length(getListElement(x,"mel"))>0)){
      mel=getListElement(x,"mel");
      if(isDirected(x)){
        for(i=0;i<length(mel);i++)
          if(vecEq(coerceVector(getListElement(VECTOR_ELT(mel,i),"outl"), INTSXP),outl) && vecEq(coerceVector(getListElement(VECTOR_ELT(mel,i),"inl"), INTSXP),inl))
            error("(edge check) Attempted to add multiplex edge where multiple==FALSE in addEdge_R.  Exiting.");
      }else{
        for(i=0;i<length(mel);i++)
          if((vecEq(coerceVector(getListElement(VECTOR_ELT(mel,i),"outl"), INTSXP),outl) && vecEq(coerceVector(getListElement(VECTOR_ELT(mel,i),"inl"), INTSXP),inl)) || (vecEq(coerceVector(getListElement(VECTOR_ELT(mel,i),"outl"), INTSXP),inl) && vecEq(coerceVector(getListElement(VECTOR_ELT(mel,i),"inl"), INTSXP),outl)))
            error("(edge check) Attempted to add multiplex edge where multiple==FALSE in addEdge_R.  Exiting.");
      }
    }
  }

  /*Create edge attribute list*/
  /*Rprintf("Creating edge attribute list (atl)\n");*/
  PROTECT(atl = coerceVector(valsEval,VECSXP)); pc++;
  /*Rprintf("\tSurvived coerce\n");*/
  if(length(atl)>0){       /*Deal with attribute names*/
    /*Rprintf("\tDealting with atl names\n");*/
    PROTECT(atlnam = coerceVector(namesEval,STRSXP)); pc++; /*Coerce to str*/
    /*Rprintf("\t\tSurvived coerce -- now checking length\n");*/
    if(length(atlnam)>length(atl)){
      warning("Too many labels in addEdge: wanted %d, got %d.  Truncating name list.\n",length(atl),length(atlnam));
      PROTECT(atlnam = contractList(atlnam,length(atl))); pc++;
    }else if(length(atlnam)<length(atl)){
      warning("Too few labels in addEdge: wanted %d, got %d.  Naming numerically.\n",length(atl),length(atlnam));
      i=length(atlnam);
      PROTECT(atlnam = enlargeList(atlnam,length(atl)-i)); pc++;
      for(j=i;j<length(atl);j++){
        sprintf(buf,"%d",j);
        SET_STRING_ELT(atlnam,j,mkChar(buf));
      }
    }
    setAttrib(atl,R_NamesSymbol,atlnam);  /*Write attribute names*/
  }
  /*Set the required attribute, na, if not present*/
  /*Rprintf("\tSetting na attribute\n");*/
  if(getListElement(atl,"na")==R_NilValue){
    PROTECT(navec = allocVector(LGLSXP,1)); pc++;
    LOGICAL(navec)[0]=0;
    PROTECT(atl=setListElement(atl,"na",navec)); pc++;
  }
  
  /*Allocate memory for the edge list, and generate edge*/
  /*Rprintf("Creating edge list (el)\n");*/
  PROTECT(el = allocVector(VECSXP, 3)); pc++;
  
  PROTECT(elnam = allocVector(STRSXP, 3)); pc++;
  SET_STRING_ELT(elnam, 0, mkChar("inl"));
  SET_STRING_ELT(elnam, 1, mkChar("outl"));
  SET_STRING_ELT(elnam, 2, mkChar("atl"));
  SET_VECTOR_ELT(el,0,inl);
  SET_VECTOR_ELT(el,1,outl);
  SET_VECTOR_ELT(el,2,atl);
  setAttrib(el,R_NamesSymbol,elnam);
  
  /*PROTECT(el = concatList(3,1,inl,outl,atl,"inl","outl","atl")); pc++;*/
  
  /*Add edge to master edge list (regrettably, seems to require regenerating!)*/
  /*Rprintf("Adding el to mel\n");*/
  PROTECT(mnptr=coerceVector(getListElement(getListElement(x,"gal"), "mnext"),INTSXP)); pc++;
  if(length(mnptr)==0)
    mnext=1;
  else
    mnext=INTEGER(mnptr)[0];    
  mel=getListElement(x,"mel");
  PROTECT(newmel = enlargeList(mel,1)); pc++;
  SET_VECTOR_ELT(newmel,mnext-1,el);
  x=setListElement(x,"mel",newmel);

  /*Add the edge reference to the outgoing edge lists*/
  /*Rprintf("Adding el to the oels\n");*/
  oel = getListElement(x,"oel");
  for(i=0;i<length(outl);i++){
    PROTECT(ptr=coerceVector(VECTOR_ELT(oel,INTEGER(outl)[i]-1),INTSXP)); pc++;
    if(length(ptr)>0){
      PROTECT(elem = allocVector(INTSXP,length(ptr)+1)); pc++;
      for(j=0;(j<length(ptr))&& (INTEGER(coerceVector(getListElement(VECTOR_ELT(newmel,INTEGER(ptr)[j]),"inl"),INTSXP))[0]<INTEGER(outl)[0]);j++)
        INTEGER(elem)[j]=INTEGER(ptr)[j];
      INTEGER(elem)[j++]=mnext;
      for(;(j-1<length(ptr));j++)
        INTEGER(elem)[j]=INTEGER(ptr)[j-1];
      SET_VECTOR_ELT(oel,INTEGER(outl)[i]-1,elem);
    }else{
      PROTECT(elem = allocVector(INTSXP,1)); pc++;
      INTEGER(elem)[0]=mnext;
      SET_VECTOR_ELT(oel,INTEGER(outl)[i]-1,elem);
    }
  }

  /*Add the edge reference to the incoming edge lists*/
  /*Rprintf("Adding el to the iels\n");*/
  iel = getListElement(x,"iel");
  for(i=0;i<length(inl);i++){
    PROTECT(ptr=coerceVector(VECTOR_ELT(iel,INTEGER(inl)[i]-1),INTSXP)); pc++;
    if(length(ptr)>0){
      PROTECT(elem = allocVector(INTSXP,length(ptr)+1)); pc++;
      for(j=0;(j<length(ptr))&& (INTEGER(coerceVector(getListElement(VECTOR_ELT(newmel,INTEGER(ptr)[j]),"outl"),INTSXP))[0]<INTEGER(inl)[0]);j++)
        INTEGER(elem)[j]=INTEGER(ptr)[j];
      INTEGER(elem)[j++]=mnext;
      for(;(j-1<length(ptr));j++)
        INTEGER(elem)[j]=INTEGER(ptr)[j-1];
      SET_VECTOR_ELT(iel,INTEGER(inl)[i]-1,elem);
    }else{
      PROTECT(elem = allocVector(INTSXP,1)); pc++;
      INTEGER(elem)[0]=mnext;
      SET_VECTOR_ELT(iel,INTEGER(inl)[i]-1,elem);
    }
  }
  /*Increment the edge counter*/
  /*Rprintf("Incrementing mnext\n");*/
  mnptr=getListElement(getListElement(x,"gal"),"mnext");
  if(mnptr==R_NilValue){               /*Create from scratch, if missing*/
    PROTECT(mnptr = allocVector(INTSXP,1)); pc++;
    INTEGER(mnptr)[0]=1;
    gal=getListElement(x,"gal");
    PROTECT(gal=setListElement(gal,"mnext",mnptr)); pc++;
    x=setListElement(x,"gal",gal);
  }else if(!isInteger(mnptr)){         /*If needed, coerce to integer format*/
    PROTECT(mnptr = coerceVector(mnptr, INTSXP)); pc++;
    gal=getListElement(x,"gal");
    PROTECT(gal=setListElement(gal,"mnext",mnptr)); pc++;
    x=setListElement(x,"gal",gal);
  }
  INTEGER(mnptr)[0]=mnext+1;
  /*Rprintf("\tCompleted increment.\n");*/
  
  /*Unprotect and return*/
  UNPROTECT(pc);
  return x;
}


SEXP addEdges_R(SEXP x, SEXP tail, SEXP head, SEXP namesEval, SEXP valsEval, SEXP edgeCheck)
/*This version is a temporary fix for the problem of long edge addition times, and will be replaced when the structure of mel is replaced.*/
/*Adds multiple edges to x.  Note that we assume tail, head, et al. to be lists of identical length.  By contrast, edgeCheck should be a single logical value.*/
{
  int pc=0,opc,i,j,mnext,z;
  SEXP el,atl,atlnam,navec,inl,outl,elnam,mel,newmel,oel,iel,ptr,elem,mnptr,gal;
  char buf[64];
  
  /*Create enlarged master edge list*/
  PROTECT(mnptr=coerceVector(getListElement(getListElement(x,"gal"), "mnext"),INTSXP)); pc++;
  if(length(mnptr)==0)
    mnext=1;
  else
    mnext=INTEGER(mnptr)[0];    
  mel=getListElement(x,"mel");
  PROTECT(newmel = enlargeList(mel,length(tail))); pc++;

  /*Rprintf("addEdges: adding %d edges\n",length(tail));*/
  for(z=0;z<length(tail);z++){  /*Add each edge in turn*/
/*-----------------------*/
    opc=pc;  /*Save old protection count*/
    /*Make sure that we can read the head and tail lists*/
    PROTECT(inl = coerceVector(VECTOR_ELT(head,z), INTSXP)); pc++;
    PROTECT(outl = coerceVector(VECTOR_ELT(tail,z), INTSXP)); pc++;
  
    /*No matter what, ensure that all vertex references are legal; otherwise,*/
    /*addEdges_R will segfault on an illegal head/tail specification.*/
    if((vecMin(inl)<1.0)||(vecMin(outl)<1.0)|| (vecMax(inl)>(double)networkSize(x)) ||(vecMax(outl)>(double)networkSize(x)))
      error("(edge check) Illegal vertex reference in addEdges_R.  Exiting.");
  
    /*If necessary, verify that new edge satisfies existing graph requirements*/
    PROTECT(edgeCheck = coerceVector(edgeCheck, LGLSXP)); pc++;
    if(INTEGER(edgeCheck)[0]){
      if(length(inl)*length(outl)==0)
        error("(edge check) Empty head/tail list in addEdges_R.  Exiting.");
      if(!isHyper(x))
        if(MAX(length(inl),length(outl))>1)
          error("(edge check) Attempted to add hyperedge where hyper==FALSE in addEdges_R.  Exiting.");
      if(!hasLoops(x))
        if(isLoop(outl,inl))
          error("(edge check) Attempted to add loop-like edge where loops==FALSE in addEdges_R.  Exiting.");
      if((!isMultiplex(x))&&(length(getListElement(x,"mel"))>0)){
        mel=getListElement(x,"mel");
        if(isDirected(x)){
          for(i=0;i<length(mel);i++)
            if(vecEq(coerceVector(getListElement(VECTOR_ELT(mel,i),"outl"), INTSXP),outl) && vecEq(coerceVector(getListElement(VECTOR_ELT(mel,i),"inl"), INTSXP),inl))
              error("(edge check) Attempted to add multiplex edge where multiple==FALSE in addEdges_R.  Exiting.");
        }else{
          for(i=0;i<length(mel);i++)
            if((vecEq(coerceVector(getListElement(VECTOR_ELT(mel,i),"outl"), INTSXP),outl) && vecEq(coerceVector(getListElement(VECTOR_ELT(mel,i),"inl"), INTSXP),inl)) || (vecEq(coerceVector(getListElement(VECTOR_ELT(mel,i),"outl"), INTSXP),inl) && vecEq(coerceVector(getListElement(VECTOR_ELT(mel,i),"inl"), INTSXP),outl)))
              error("(edge check) Attempted to add multiplex edge where multiple==FALSE in addEdges_R.  Exiting.");
        }
      }
    }

    /*Create edge attribute list*/
    /*Rprintf("Creating edge attribute list (atl)\n");*/
    PROTECT(atl = coerceVector(VECTOR_ELT(valsEval,z),VECSXP)); pc++;
    /*Rprintf("\tSurvived coerce\n");*/
    if(length(atl)>0){       /*Deal with attribute names*/
      /*Rprintf("\tDealting with atl names\n");*/
      PROTECT(atlnam = coerceVector(VECTOR_ELT(namesEval,z),STRSXP)); pc++; /*Coerce to str*/
      /*Rprintf("\t\tSurvived coerce -- now checking length\n");*/
      if(length(atlnam)>length(atl)){
        warning("Too many labels in addEdges: wanted %d, got %d.  Truncating name list.\n",length(atl),length(atlnam));
        PROTECT(atlnam = contractList(atlnam,length(atl))); pc++;
      }else if(length(atlnam)<length(atl)){
        warning("Too few labels in addEdges: wanted %d, got %d.  Naming numerically.\n",length(atl),length(atlnam));
        i=length(atlnam);
        PROTECT(atlnam = enlargeList(atlnam,length(atl)-i)); pc++;
        for(j=i;j<length(atl);j++){
          sprintf(buf,"%d",j);
          SET_STRING_ELT(atlnam,j,mkChar(buf));
        }
      }
      setAttrib(atl,R_NamesSymbol,atlnam);  /*Write attribute names*/
    }
    /*Set the required attribute, na, if not present*/
    /*Rprintf("\tSetting na attribute\n");*/
    if(getListElement(atl,"na")==R_NilValue){
      PROTECT(navec = allocVector(LGLSXP,1)); pc++;
      LOGICAL(navec)[0]=0;
      PROTECT(atl=setListElement(atl,"na",navec)); pc++;
    }
  
    /*Allocate memory for the edge list, and generate edge*/
    /*Rprintf("Creating edge list (el)\n");*/
    PROTECT(el = allocVector(VECSXP, 3)); pc++;
  
    PROTECT(elnam = allocVector(STRSXP, 3)); pc++;
    SET_STRING_ELT(elnam, 0, mkChar("inl"));
    SET_STRING_ELT(elnam, 1, mkChar("outl"));
    SET_STRING_ELT(elnam, 2, mkChar("atl"));
    SET_VECTOR_ELT(el,0,inl);
    SET_VECTOR_ELT(el,1,outl);
    SET_VECTOR_ELT(el,2,atl);
    setAttrib(el,R_NamesSymbol,elnam);
  
    /*PROTECT(el = concatList(3,1,inl,outl,atl,"inl","outl","atl")); pc++;*/
    /*Add edge to master edge list*/
    /*Rprintf("Adding el to newmel\n");*/
    SET_VECTOR_ELT(newmel,mnext-1,el);

    /*Add the edge reference to the outgoing edge lists*/
    /*Rprintf("Adding el to the oels\n");*/
    oel = getListElement(x,"oel");
    for(i=0;i<length(outl);i++){
      PROTECT(ptr=coerceVector(VECTOR_ELT(oel,INTEGER(outl)[i]-1),INTSXP)); pc++;
      if(length(ptr)>0){
        PROTECT(elem = allocVector(INTSXP,length(ptr)+1)); pc++;
        for(j=0;(j<length(ptr))&& (INTEGER(coerceVector(getListElement(VECTOR_ELT(newmel,INTEGER(ptr)[j]),"inl"),INTSXP))[0]<INTEGER(outl)[0]);j++)
          INTEGER(elem)[j]=INTEGER(ptr)[j];
        INTEGER(elem)[j++]=mnext;
        for(;(j-1<length(ptr));j++)
          INTEGER(elem)[j]=INTEGER(ptr)[j-1];
        SET_VECTOR_ELT(oel,INTEGER(outl)[i]-1,elem);
      }else{
        PROTECT(elem = allocVector(INTSXP,1)); pc++;
        INTEGER(elem)[0]=mnext;
        SET_VECTOR_ELT(oel,INTEGER(outl)[i]-1,elem);
      }
    }

    /*Add the edge reference to the incoming edge lists*/
    /*Rprintf("Adding el to the iels\n");*/
    iel = getListElement(x,"iel");
    for(i=0;i<length(inl);i++){
      PROTECT(ptr=coerceVector(VECTOR_ELT(iel,INTEGER(inl)[i]-1),INTSXP)); pc++;
      if(length(ptr)>0){
        PROTECT(elem = allocVector(INTSXP,length(ptr)+1)); pc++;
        for(j=0;(j<length(ptr))&& (INTEGER(coerceVector(getListElement(VECTOR_ELT(newmel,INTEGER(ptr)[j]),"outl"),INTSXP))[0]<INTEGER(inl)[0]);j++)
          INTEGER(elem)[j]=INTEGER(ptr)[j];
        INTEGER(elem)[j++]=mnext;
        for(;(j-1<length(ptr));j++)
          INTEGER(elem)[j]=INTEGER(ptr)[j-1];
        SET_VECTOR_ELT(iel,INTEGER(inl)[i]-1,elem);
      }else{
        PROTECT(elem = allocVector(INTSXP,1)); pc++;
        INTEGER(elem)[0]=mnext;
        SET_VECTOR_ELT(iel,INTEGER(inl)[i]-1,elem);
      }
    }
    /*Increment the edge counter*/
    /*Rprintf("Incrementing mnext\n");*/
    mnptr=getListElement(getListElement(x,"gal"),"mnext");
    if(mnptr==R_NilValue){               /*Create from scratch, if missing*/
      PROTECT(mnptr = allocVector(INTSXP,1)); pc++;
      INTEGER(mnptr)[0]=1;
      gal=getListElement(x,"gal");
      PROTECT(gal=setListElement(gal,"mnext",mnptr)); pc++;
      x=setListElement(x,"gal",gal);
    }else if(!isInteger(mnptr)){         /*If needed, coerce to integer format*/
      PROTECT(mnptr = coerceVector(mnptr, INTSXP)); pc++;
      gal=getListElement(x,"gal");
      PROTECT(gal=setListElement(gal,"mnext",mnptr)); pc++;
      x=setListElement(x,"gal",gal);
    }
    INTEGER(mnptr)[0]=(++mnext);
    /*Rprintf("\tCompleted increment.\n");*/
    UNPROTECT(pc-opc);  /*Unprotect what we can*/
    pc=opc;
/*-----------------------*/
  }
  
  /*Update mel*/
  x=setListElement(x,"mel",newmel);

  /*Unprotect and return*/
  UNPROTECT(pc);
  return x;
}


SEXP addEdges_R_old(SEXP x, SEXP tail, SEXP head, SEXP namesEval, SEXP valsEval, SEXP edgeCheck)
/*This is the original version of addEdges_R, kept temporarily.*/
/*Adds multiple edges to x.  Note that we assume tail, head, et al. to be lists of identical length.  By contrast, edgeCheck should be a single logical value.*/
{
  int i,pc=0;
  
  /*Rprintf("addEdges: adding %d edges\n",length(tail));*/
  for(i=0;i<length(tail);i++){
    x=addEdge_R(x,VECTOR_ELT(tail,i),VECTOR_ELT(head,i), VECTOR_ELT(namesEval,i),VECTOR_ELT(valsEval,i),edgeCheck);
  }
  
  UNPROTECT(pc);
  return x;
}


SEXP addVertices_R(SEXP x, SEXP nv, SEXP vattr)
{
  int pc=0,n,ninc,i;
  SEXP ns,oel,iel,val,el,atts,newna;
  
  /*Update the network size attribute*/
  PROTECT(nv=coerceVector(nv,INTSXP)); pc++;
  ninc=INTEGER(nv)[0];
  n=networkSize(x);
  /*Rprintf("Entered addVertices; adding %d vertices, n=%d\n",ninc,n);*/
  PROTECT(ns=allocVector(INTSXP,1)); pc++;
  INTEGER(ns)[0]=n+ninc;
  x=setNetworkAttribute(x,"n",ns);

  /*Add entries to the outgoing/incoming edge lists*/
  PROTECT(iel=enlargeList(getListElement(x,"iel"),ninc)); pc++;
  PROTECT(oel=enlargeList(getListElement(x,"oel"),ninc)); pc++;
  for(i=n;i<n+ninc;i++){
    PROTECT(el=allocVector(INTSXP,0)); pc++;
    SET_VECTOR_ELT(iel,i,el);
    PROTECT(el=allocVector(INTSXP,0)); pc++;
    SET_VECTOR_ELT(oel,i,el);
  }
  x=setListElement(x,"iel",iel);
  x=setListElement(x,"oel",oel);
  /*Rprintf("\tiel now length %d, oel now length %d\n",length(iel),length(oel));*/
    
  /*Set up the vertex attributes*/
  PROTECT(val=enlargeList(getListElement(x,"val"),ninc)); pc++;
  for(i=n;i<n+ninc;i++){
    /*Create an attribute vector for this edge*/
    if(vattr==R_NilValue){                    /*Need to create from scratch*/
      PROTECT(atts=allocVector(VECSXP,0)); pc++;
      PROTECT(newna=allocVector(LGLSXP,1)); pc++;
      INTEGER(newna)[0]=0;
      PROTECT(atts=setListElement(atts,"na",newna)); pc++;
    }else{                                   /*Can use existing vector*/
      atts=VECTOR_ELT(vattr,i-n);
      if(getListElement(atts,"na")==R_NilValue){  /*Install "na" if absent*/
        PROTECT(newna=allocVector(LGLSXP,1)); pc++;
        INTEGER(newna)[0]=0;
        PROTECT(atts=setListElement(atts,"na",newna)); pc++;
      }
    }
    /*Install the attribute vector*/
    SET_VECTOR_ELT(val,i,atts);
  }
  x=setListElement(x,"val",val);

  /*
  for(i=0;i<networkSize(x);i++){
    el=VECTOR_ELT(getListElement(x,"oel"),i);
    Rprintf("%d:",i+1);
    for(j=0;j<length(el);j++)
      Rprintf(" %d",INTEGER(el)[j]);
    Rprintf("\n");
  }
  for(i=0;i<networkSize(x);i++){
    el=VECTOR_ELT(getListElement(x,"iel"),i);
    Rprintf("%d:",i+1);
    for(j=0;j<length(el);j++)
      Rprintf(" %d",INTEGER(el)[j]);
    Rprintf("\n");
  }
  */
  
  /*Unprotect and return*/
  UNPROTECT(pc);
  return x;
}


SEXP deleteEdgeAttribute_R(SEXP x, SEXP attrname)
/*Removes the attribute(s) named by attrname from all edges.  This is a front-end to the internal version.*/
{
  int i,j,pc=0,n;
  SEXP anam,mel;
  
  /*Remove the attributes....*/
  mel=getListElement(x,"mel");
  n=length(mel);
  PROTECT(anam=coerceVector(attrname,STRSXP)); pc++;
  for(i=0;i<length(anam);i++)
    for(j=0;j<n;j++)
      if(VECTOR_ELT(mel,j)!=R_NilValue){
        x=deleteEdgeAttribute(x,j+1,CHAR(STRING_ELT(anam,i)));
      }

  /*Return the result*/
  UNPROTECT(pc);
  return x;
}


SEXP deleteEdges_R(SEXP x, SEXP eid)
/*Removes the edges contained in vector eid from the specified network object.*/
{
  int pc=0,i,j,e,opc;
  SEXP mel,el,head,tail,oel,iel,newvec;

  /*Rprintf("deleteEdges; removing %d edges\n",length(eid));*/
  /*Coerce eid to the appropriate form*/
  PROTECT(eid=coerceVector(eid,INTSXP)); pc++;
  /*Get key pointers*/
  mel=getListElement(x,"mel");
  iel=getListElement(x,"iel");
  oel=getListElement(x,"oel");

  /*Remove the edges*/
  for(i=0;i<length(eid);i++){
    e=INTEGER(eid)[i];                   /*Get edge number*/
    /*Rprintf("\t%d\n",e);*/
    el=VECTOR_ELT(mel,e-1);              /*Get edge*/
    /*Rprintf("\t\tGot it.\n");*/
    if(el!=R_NilValue){                  /*Only delete if present!*/
      /*Rprintf("\t\tRemoving from iel/oel\n");*/
      /*Obtain head and tail lists*/
      opc=pc;
      PROTECT(head=coerceVector(getListElement(el,"inl"),INTSXP)); pc++;
      PROTECT(tail=coerceVector(getListElement(el,"outl"),INTSXP)); pc++;
      /*For each endpoint, remove edge from incoming/outgoing lists*/
      for(j=0;j<length(head);j++){
        newvec=VECTOR_ELT(iel,INTEGER(head)[j]-1);
        PROTECT(newvec=vecRemove(newvec,(double)e)); pc++;
        SET_VECTOR_ELT(iel,INTEGER(head)[j]-1,newvec);
      }
      for(j=0;j<length(tail);j++){
        newvec=VECTOR_ELT(oel,INTEGER(tail)[j]-1);
        PROTECT(newvec=vecRemove(newvec,(double)e)); pc++;
        SET_VECTOR_ELT(oel,INTEGER(tail)[j]-1,newvec);
      }
      /*Remove edge from mel (garbage collection will recover memory)*/
      SET_VECTOR_ELT(mel,e-1,R_NilValue);
      /*Undo protection stack additions*/
      if(pc>opc){
        UNPROTECT(pc-opc);
        pc=opc;
      }
    }
  }
  /*Rprintf("\tdone!\n");*/

  /*Unprotect and return*/
  UNPROTECT(pc);
  return x;
}


SEXP deleteNetworkAttribute_R(SEXP x, SEXP attrname)
/*Removes the attribute(s) named by attrname from gal.  This is a front-end to the internal version.*/
{
  int i,pc=0;
  SEXP anam;
  
  /*Remove the attributes....*/
  PROTECT(anam=coerceVector(attrname,STRSXP)); pc++;
  for(i=0;i<length(anam);i++){
    x=deleteNetworkAttribute(x,CHAR(STRING_ELT(anam,i)));
  }
  
  /*Return the result*/
  UNPROTECT(pc);
  return x;
}


SEXP deleteVertexAttribute_R(SEXP x, SEXP attrname)
/*Removes the attribute(s) named by attrname from all vertices.  This is a front-end to the internal version.*/
{
  int i,j,pc=0,n;
  SEXP anam;
  
  /*Remove the attributes....*/
  n=networkSize(x);
  PROTECT(anam=coerceVector(attrname,STRSXP)); pc++;
  for(i=0;i<length(anam);i++)
    for(j=0;j<n;j++){
      x=deleteVertexAttribute(x,j+1,CHAR(STRING_ELT(anam,i)));
    }
    
  /*Return the result*/
  UNPROTECT(pc);
  return x;
}


SEXP deleteVertices_R(SEXP x, SEXP vid)
/*Removes the vertices contained in vector vid (and all associated edges) from the specified network object.*/
{
  int i,count,pc=0;
  char neigh[]="combined";
  SEXP eids,nord,newsize,val,iel,oel;
  
  /*Coerce vid to integer form, and remove any duplicates*/
  /*Rprintf("Size on entry: %d, length of vid:%d\n",networkSize(x),length(vid));*/
  PROTECT(vid=coerceVector(vecUnique(vid),INTSXP)); pc++;

  /*Get rid of all edges having any of the vid vertices as endpoints*/
  /*Rprintf("\tUnique vid entries:%d\n",length(vid));*/
  for(i=0;i<length(vid);i++){
    PROTECT(eids=getEdgeIDs(x,INTEGER(vid)[i],0,neigh,0)); pc++;
    /*Rprintf("\tgetIDs claims %d edges to remove.\n",length(eids));
    if(length(eids)>0)
      Rprintf("\tFirst ID is %d\n",INTEGER(eids)[0]);*/
    x=deleteEdges_R(x,eids);
  }

  /*Permute the vertices in vid to the end of the graph*/
  /*Rprintf("\tPreparing to permute\n");*/
  PROTECT(nord=allocVector(INTSXP,networkSize(x))); pc++;
  count=0;
  for(i=0;i<networkSize(x);i++)
    if(!vecIsIn(i+1.0,vid))
      INTEGER(nord)[count++]=i+1;
  for(i=0;i<length(vid);i++)
    INTEGER(nord)[count+i]=INTEGER(vid)[i];
  x=permuteVertexIDs_R(x,nord);
  /*Rprintf("\tPermutation complete\n");*/
  
  /*Update the network size*/
  /*Rprintf("\tUpdating network size\n");*/
  PROTECT(newsize=allocVector(INTSXP,1)); pc++;
  INTEGER(newsize)[0]=networkSize(x)-length(vid);
  x=setNetworkAttribute(x,"n",newsize);
 
  /*Finally, get rid of the old vertices*/
  /*Rprintf("\tContracting vertex lists\n");*/
  PROTECT(val=contractList(getListElement(x,"val"),INTEGER(newsize)[0])); pc++;
  PROTECT(iel=contractList(getListElement(x,"iel"),INTEGER(newsize)[0])); pc++;
  PROTECT(oel=contractList(getListElement(x,"oel"),INTEGER(newsize)[0])); pc++;
  x=setListElement(x,"val",val);
  x=setListElement(x,"iel",iel);
  x=setListElement(x,"oel",oel);
  /*Rprintf("Final size: %d, list lengths: %d %d %d\n",networkSize(x), length(val),length(iel),length(oel));*/

  /*Unprotect and return*/
  UNPROTECT(pc);
  return x;
}


SEXP getEdgeIDs_R(SEXP x, SEXP v, SEXP alter, SEXP neighborhood, SEXP naOmit)
/*Retrieve the IDs of all edges incident on v, in network x.  Outgoing or incoming edges are specified by neighborhood, while na.omit indicates whether or not missing edges should be omitted.  If alter!=NULL, only edges whose alternate endpoints contain alter are returned.  The return value is a vector of edge IDs.  (Note: this is an R-callable wrapper for the internal version.)*/
{
  int pc=0,a,naval;
  
  /*Make sure that all variables are adequately dealt with*/
  PROTECT(v=coerceVector(v,INTSXP)); pc++;
  PROTECT(alter=coerceVector(alter,INTSXP)); pc++;
  PROTECT(naOmit=coerceVector(naOmit,LGLSXP)); pc++;
  if(length(alter)==0)
    a=0;
  else
    a=INTEGER(alter)[0];
  if(length(naOmit)==0)
    naval=0;
  else
    naval=INTEGER(naOmit)[0];
  
  /*Unprotect and return*/
  UNPROTECT(pc);
  return getEdgeIDs(x,INTEGER(v)[0],a,CHAR(STRING_ELT(neighborhood,0)),naval);
}


SEXP getEdges_R(SEXP x, SEXP v, SEXP alter, SEXP neighborhood, SEXP naOmit)
/*Retrieve all edges incident on v, in network x.  Outgoing or incoming edges are specified by neighborhood, while na.omit indicates whether or not missing edges should be omitted.  If alter!=NULL, only edges whose alternate endpoints contain alter are returned.  The return value is a list of edges.  (Note: this is an R-callable wrapper for the internal version.)*/
{
  int pc=0,a,naval;
  
  /*Make sure that all variables are adequately dealt with*/
  PROTECT(v=coerceVector(v,INTSXP)); pc++;
  PROTECT(alter=coerceVector(alter,INTSXP)); pc++;
  PROTECT(naOmit=coerceVector(naOmit,LGLSXP)); pc++;
  if(length(alter)==0)
    a=0;
  else
    a=INTEGER(alter)[0];
  if(length(naOmit)==0)
    naval=0;
  else
    naval=INTEGER(naOmit)[0];
  
  /*Unprotect and return*/
  UNPROTECT(pc);
  return getEdges(x,INTEGER(v)[0],a,CHAR(STRING_ELT(neighborhood,0)),naval);
}


SEXP getNeighborhood_R(SEXP x, SEXP v, SEXP type, SEXP naOmit)
/*Return a vector containing the first-order vertex neighborhood of v in x, as specified by type.  If naOmit==TRUE, missing edges are discarded; otherwise, they are employed as well.  (Note: this is an R-callable wrapper for the internal version.)*/
{
  int pc=0,naval;
  
  /*Make sure that all variables are adequately dealt with*/
  PROTECT(v=coerceVector(v,INTSXP)); pc++;
  PROTECT(naOmit=coerceVector(naOmit,LGLSXP)); pc++;
  if(length(naOmit)==0)
    naval=0;
  else
    naval=INTEGER(naOmit)[0];

  /*Unprotect and return*/
  /*Rprintf("getNeighborhood_R: type=%s\n",CHAR(STRING_ELT(type,0)));*/
  UNPROTECT(pc);
  return getNeighborhood(x,INTEGER(v)[0],CHAR(STRING_ELT(type,0)),naval);
}


SEXP isAdjacent_R(SEXP x, SEXP vi, SEXP vj, SEXP naOmit)
/*Returns TRUE iff exists an edge in x connecting vi and vj.  Where na.omit==TRUE, edges flagged as missing are ignored.  Note that vi and vj can be vectors, in which case adjacency is tested for each pair.  (The vectors must be of the same length!)  If vi and/or vj refer to nonexistent vertices, NAs are returned.*/
{
  SEXP ans;
  int i,n,pc=0;

  /*Verify that this is a network object*/
  if(!isNetwork(x))
    error("isAdjacent_R requires an argument of class network.\n");

  /*Ensure that all arguments are of the right type*/
  PROTECT(vi = coerceVector(vi, INTSXP)); pc++;
  PROTECT(vj = coerceVector(vj, INTSXP)); pc++;
  PROTECT(naOmit = coerceVector(naOmit, LGLSXP)); pc++;
  PROTECT(ans = allocVector(LGLSXP,length(vi))); pc++;
  n=networkSize(x);
  /*Rprintf("Checking adjacency for pair (%d,%d), na.omit=%d\n",INTEGER(vi)[0], INTEGER(vj)[0],INTEGER(naOmit)[0]);*/

  /*Call the internal version of isAdjacent*/
  for(i=0;i<length(vi);i++)
    if((INTEGER(vi)[i]<1)||(INTEGER(vj)[i]<1)||(INTEGER(vi)[i]>n)|| (INTEGER(vj)[i]>n))
      INTEGER(ans)[i]=NA_INTEGER;          /*Return NA on a bad query*/
    else
      INTEGER(ans)[i]=isAdjacent(x,INTEGER(vi)[i],INTEGER(vj)[i], INTEGER(naOmit)[0]);

  /*Return the result*/
  UNPROTECT(pc);
  return ans;
}


SEXP isNANetwork_R(SEXP x, SEXP y)
/*Given input network x, create an edge in y for every edge of x having edge attribute na==TRUE.  It is assumed that y is preallocated to be the same size and type as x -- this function just writes the edges into place.*/
{
  SEXP hl,tl,nel,vel,mel,edgeCheck;
  int i,pc=0,count=0;
  
  /*Get the master edge list of x*/
  mel=getListElement(x,"mel");
  
  /*Pre-allocate head/tail lists -- we'll shorten later*/
  PROTECT(hl=allocVector(VECSXP,length(mel))); pc++;
  PROTECT(tl=allocVector(VECSXP,length(mel))); pc++;
  
  /*Move through the edges, copying head/tail lists only when missing*/
  for(i=0;i<length(mel);i++){
    if(VECTOR_ELT(mel,i)!=R_NilValue){
      if(INTEGER(getListElement(getListElement(VECTOR_ELT(mel,i),"atl"), "na"))[0]){
       SET_VECTOR_ELT(hl,count,duplicate(getListElement(VECTOR_ELT(mel,i), "inl")));
       SET_VECTOR_ELT(tl,count++,duplicate(getListElement(VECTOR_ELT(mel,i), "outl")));
      }
    }
  }
  
  /*Contract lists, and allocate empty space for namesEval/valsEval*/
  PROTECT(hl=contractList(hl,count)); pc++;  
  PROTECT(tl=contractList(tl,count)); pc++;  
  PROTECT(nel=allocVector(VECSXP,count)); pc++;
  PROTECT(vel=allocVector(VECSXP,count)); pc++;
  for(i=0;i<count;i++){
    SET_VECTOR_ELT(nel,i,R_NilValue);
    SET_VECTOR_ELT(vel,i,R_NilValue);
  }  

  /*Write edges into y*/
  PROTECT(edgeCheck=allocVector(INTSXP,1)); pc++;
  INTEGER(edgeCheck)[0]=0;
  y=addEdges_R(y,tl,hl,nel,vel,edgeCheck);

  /*Unprotect and return*/
  UNPROTECT(pc);
  return y;
}


SEXP networkEdgecount_R(SEXP x, SEXP naOmit)
/*Count the number of active edges in x.  If naOmit==TRUE, then missing edges are not counted; otherwise, all edges are included.*/
{
  int pc=0,naomit;
  SEXP ans;
  
  /*Take care of preliminaries*/
  PROTECT(naOmit=coerceVector(naOmit,LGLSXP)); pc++;
  naomit=INTEGER(naOmit)[0];
  PROTECT(ans=allocVector(INTSXP,1)); pc++;
  
  /*Get the result*/
  INTEGER(ans)[0]=networkEdgecount(x,naomit);

  /*Unprotect and return*/
  UNPROTECT(pc);
  return ans;
}


SEXP permuteVertexIDs_R(SEXP x, SEXP vids)
/*Permute the vertices of x according to vids, where vids is a permutation vector; note that permutation is performed in terms of the internal vertex IDs, which correspond to order of storage in val, iel, oel, etc.  This obviously involves a great deal of edge modification, and as such it can be quite time consuming for graphs with many edges!  (OTOH, do note that edges are only modified if they need to be.  Thus, typical complexity will scale with mean degree times the number of switched vertices.)*/
{
  int i,j,k,pc=0,ccount=0,flag=0;
  char neigh[] = "combined";
  SEXP eids,cvids,cpos,val,iel,oel,epl,mel,idlist,edge;

  /*Set up the initial variables*/
  PROTECT(vids=coerceVector(vids,INTSXP)); pc++;
  PROTECT(cpos=allocVector(INTSXP,length(vids))); pc++;
  PROTECT(cvids=allocVector(INTSXP,length(vids))); pc++;
  PROTECT(eids=allocVector(INTSXP,0)); pc++;

  /*Determine which vertices have moved, and accumulate affected edges*/
  for(i=0;i<networkSize(x);i++)
    if(INTEGER(vids)[i]!=i+1){
      INTEGER(cpos)[ccount]=i+1;                      /*New positions*/
      INTEGER(cvids)[ccount++]=INTEGER(vids)[i];      /*Old positions*/
      PROTECT(idlist=coerceVector(getEdgeIDs(x,INTEGER(vids)[i],0, neigh,0),INTSXP)); pc++;
      PROTECT(eids=vecAppend(eids,idlist)); pc++;
    }
  PROTECT(cpos=contractList(cpos,ccount)); pc++;    /*Shrink vID lists*/
  PROTECT(cvids=contractList(cvids,ccount)); pc++;
  PROTECT(eids=vecUnique(eids)); pc++;             /*Remove duplicates*/

  /*For each affected edge, revise vertex IDs as needed*/
  mel=getListElement(x,"mel");
  for(i=0;i<length(eids);i++){
    edge=VECTOR_ELT(mel,INTEGER(eids)[i]-1);
    /*Correct the head list (inl)*/
    epl=getListElement(edge,"inl");
    PROTECT(epl=coerceVector(epl,INTSXP)); pc++;
    for(j=0;j<length(epl);j++){
      flag=0;
      for(k=0;(k<length(cpos))&&(!flag);k++)
        if(INTEGER(epl)[j]==INTEGER(cvids)[k]){
          INTEGER(epl)[j]=INTEGER(cpos)[k];
          flag++;
        }
    }
    edge=setListElement(edge,"inl",epl);
    /*Correct the tail list (outl)*/
    epl=getListElement(edge,"outl");
    PROTECT(epl=coerceVector(epl,INTSXP)); pc++;
    for(j=0;j<length(epl);j++){
      flag=0;
      for(k=0;(k<length(cpos))&&(!flag);k++)
        if(INTEGER(epl)[j]==INTEGER(cvids)[k]){
          INTEGER(epl)[j]=INTEGER(cpos)[k];
          flag++;
        }
    }
    edge=setListElement(edge,"outl",epl);
  }

  /*Now, reorder the vertex properties*/
  PROTECT(val=permuteList(getListElement(x,"val"),vids)); pc++;
  PROTECT(iel=permuteList(getListElement(x,"iel"),vids)); pc++;
  PROTECT(oel=permuteList(getListElement(x,"oel"),vids)); pc++;
  x=setListElement(x,"val",val);
  x=setListElement(x,"iel",iel);
  x=setListElement(x,"oel",oel);

  /*Unprotect and return*/
  UNPROTECT(pc);
  return x;
}


SEXP setEdgeAttribute_R(SEXP x, SEXP attrname, SEXP value, SEXP e)
/*Sets the attribute in attrname to the values in (list) value, for each corresponding edge with respective ID in vector e.  If an edge referred to in e does not actually exist (i.e., its entry in mel is NULL), then that edge (and the associated value) is silently skipped.  Note that existing attribute entries are overwritten by this routine, where present...if the named attribute does not exist, a new entry is created for each edge in question.*/
{
  int i,pc=0;
  SEXP mel,el,atl;
  
  /*Coerce edge IDs into integer format, and get the master edge list*/
  PROTECT(e=coerceVector(e,INTSXP)); pc++;
  mel=getListElement(x,"mel");
  
  /*For each edge ID in e, set the appropriate attribute to the
  indicated value*/
  for(i=0;i<length(e);i++){
    el=VECTOR_ELT(mel,INTEGER(e)[i]-1);   /*Get the edge*/
    if(el!=R_NilValue){                   /*Only proceed if edge exists*/
      atl=getListElement(el,"atl");         /*Get attr list*/
      PROTECT(atl=setListElement(atl,CHAR(STRING_ELT(attrname,0)), VECTOR_ELT(value,i)));                      /*Add/alter attribute*/
      /*Note: b/c atl might have been extended, we have to re-write it into*/
      /*the edge object.  The same is not true for the edge itself, b/c we*/
      /*know that every edge must contain the element "atl"; the return*/
      /*value is guaranteed to be the same as the original value, and we're*/
      /*safe.  If setListElement ever changes this behavior, the following*/
      /*line will no longer work as-is, and an additional line resetting el*/
      /*within mel will be required!  You have been warned!*/
      el=setListElement(el,"atl",atl);
      UNPROTECT(1);
    }
  }
    
  /*Unprotect and return*/
  UNPROTECT(pc);
  return x;
}


SEXP setEdgeValue_R(SEXP x, SEXP attrname, SEXP value, SEXP e)
/*Set attribute attrname on the edges of x indexed by e, using the values in (adjacency matrix) value.  Loops and multiplex edges are allowed here, but hypergraphs are not!*/
{
  int i,pc=0,type,h,t,n;
  const char *anam;
  SEXP mel,el,atl,newval=R_NilValue;
  
  /*Set things up*/
  mel=getListElement(x,"mel");
  PROTECT(e=coerceVector(e,INTSXP)); pc++;
  type=TYPEOF(value);
  anam=CHAR(STRING_ELT(attrname,0));
  n=networkSize(x);
  
  /*For each edge in use, set equal to the appropriate entry from value*/
  for(i=0;i<length(e);i++){
    el=VECTOR_ELT(mel,INTEGER(e)[i]-1);
    if(el!=R_NilValue){
      h=INTEGER(coerceVector(getListElement(el,"inl"),INTSXP))[0];
      t=INTEGER(coerceVector(getListElement(el,"outl"),INTSXP))[0];
      atl=getListElement(el,"atl");
      switch(type){
        case INTSXP:
          PROTECT(newval=allocVector(INTSXP,1)); pc++;
          INTEGER(newval)[0]=INTEGER(value)[t-1+(h-1)*n];
          break;
        case LGLSXP:
          PROTECT(newval=allocVector(LGLSXP,1)); pc++;
          INTEGER(newval)[0]=INTEGER(value)[t-1+(h-1)*n];
          break;
        case REALSXP:
          PROTECT(newval=allocVector(REALSXP,1)); pc++;
          REAL(newval)[0]=REAL(value)[t-1+(h-1)*n];
          break;
        case RAWSXP:
          PROTECT(newval=allocVector(RAWSXP,1)); pc++;
          RAW(newval)[0]=RAW(value)[t-1+(h-1)*n];
          break;
        case VECSXP:
          PROTECT(newval=allocVector(VECSXP,1)); pc++;
          SET_VECTOR_ELT(newval,0,VECTOR_ELT(value,t-1+(h-1)*n));
          break;
        case STRSXP:
          PROTECT(newval=allocVector(STRSXP,1)); pc++;
          SET_STRING_ELT(newval,0,STRING_ELT(value,t-1+(h-1)*n));
          break;
        case CPLXSXP:
          PROTECT(newval=allocVector(CPLXSXP,1)); pc++;
          COMPLEX(newval)[0].r=COMPLEX(value)[t-1+(h-1)*n].r;
          COMPLEX(newval)[0].i=COMPLEX(value)[t-1+(h-1)*n].i;
          break;
        default:
          UNIMPLEMENTED_TYPE("setEdgeValue_R",type);
      }
      PROTECT(atl=setListElement(atl,anam,newval)); pc++;
      el=setListElement(el,"atl",atl);
    }
  }
  
  /*Unprotect and return*/
  UNPROTECT(pc);
  return x;
}


SEXP setNetworkAttribute_R(SEXP x, SEXP attrname, SEXP value)
/*Set the attribute whose name is given by attrname in x to be equal to value.*/
{
  int i,pc=0;
  
  /*Coerce the attribute names*/
  PROTECT(attrname=coerceVector(attrname,STRSXP)); pc++;

  /*Perform the assignment*/
  for(i=0;i<length(attrname);i++)
    x=setNetworkAttribute(x,CHAR(STRING_ELT(attrname,i)),VECTOR_ELT(value,i));

  UNPROTECT(pc);
  return x;
}


SEXP setVertexAttribute_R(SEXP x, SEXP attrname, SEXP value, SEXP v)
/*sets the attribute in attrname to the values in (list) value, for each corresponding vertex with ID in vector v.  Existing attribute entries are overwritten by this routine, where present; new attributes are added, if they do not exist.*/
{
  int i,pc=0;
  SEXP val,vl;
  
  /*Coerce vertex IDs into integer format, and get the vertex attribute list*/
  PROTECT(v=coerceVector(v,INTSXP)); pc++;
  val=getListElement(x,"val");

  /*Update the attribute lists*/
  for(i=0;i<length(v);i++){
    PROTECT(vl=setListElement(VECTOR_ELT(val,INTEGER(v)[i]-1), CHAR(STRING_ELT(attrname,0)),VECTOR_ELT(value,i))); /*Protect temporarily*/
    SET_VECTOR_ELT(val,INTEGER(v)[i]-1,vl);
    UNPROTECT(1);  /*Remove protection to avoid stack overflow*/
  }

  /*Unprotect and return*/
  UNPROTECT(pc);
  return x;
}


