/*
######################################################################
#
# utils.c
#
# Written by Carter T. Butts <buttsc@uci.edu>
# Last Modified 4/12/06
# Licensed under the GNU General Public License version 2 (June, 1991)
#
# Part of the R/network package
#
# This file contains basic utility routines.
#
######################################################################
*/
 
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include "utils.h"


/*LIST ACCESS/MODIFICATION ROUTINES-----------------------------------------*/


SEXP deleteListElement(SEXP list, char *str)
/*Given a list and a character string, return a new list with the element whose name matches said string removed.  If this is the only element of list, NULL is returned; if the element is not found, list is returned unmodified.*/
{
  int pc=0,i,flag;
  SEXP newlist, names, newnames;
  
  /*If this is obviously pointless, return the original pointer*/
  if(length(list)==0)
    return list;
  /*Evidently, we should try searching for the element...*/
  names=getAttrib(list, R_NamesSymbol);
  PROTECT(newlist=allocVector(VECSXP,length(list)-1)); pc++;
  PROTECT(newnames=allocVector(STRSXP,length(list)-1)); pc++;
  flag=0;
  for(i=0;(i<length(list)-1)&&(!flag);i++){
    if(strcmp(CHAR(STRING_ELT(names,i)),str)==0)   /*Do we have a match?*/
      flag++;                                        /*If so, set flag*/
    else{
      SET_VECTOR_ELT(newlist,i,VECTOR_ELT(list,i));  /*Set the new pointer*/
      SET_STRING_ELT(newnames,i,mkChar(CHAR(STRING_ELT(names,i))));
    }
  }
  /*By now, we have either found our element, or are at length(newlist)-1*/
  if(flag){
    for(;i<length(list);i++){
      SET_VECTOR_ELT(newlist,i-1,VECTOR_ELT(list,i));
      SET_STRING_ELT(newnames,i-1,mkChar(CHAR(STRING_ELT(names,i))));
    }
    setAttrib(newlist,R_NamesSymbol,newnames);
  }else{
    if(strcmp(CHAR(STRING_ELT(names,i)),str)!=0)
      newlist=list;
    else
      setAttrib(newlist,R_NamesSymbol,newnames);
  }
  
  /*Unprotect and return*/
  UNPROTECT(pc);
  return newlist;
}


SEXP getListElement(SEXP list, char *str)
/*Given a list and a character string, return a pointer to the element with the specified name (or else NULL).  This is taken from the Writing R Extensions manual.*/
{
  SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
  int i;
     
  for (i = 0; i < length(list); i++)
    if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
      elmt = VECTOR_ELT(list, i);
      break;
    }
  return elmt;
}


SEXP setListElement(SEXP list, char *str, SEXP elem)
/*Given a list, an element, and a character string, write the element with the specified name to the list.  If an element by that name already exists, it is replaced.*/
{
  SEXP names, newlist;
  int i, pc=0;

  /*If list is null or of zero length, we immeadiately know what to do*/
  if(length(list)==0){
    PROTECT(newlist=allocVector(VECSXP,1)); pc++;
    SET_VECTOR_ELT(newlist,0,elem);
    PROTECT(names=allocVector(STRSXP,1)); pc++;
    SET_STRING_ELT(names,0,mkChar(str));
    setAttrib(newlist,R_NamesSymbol,names);
    UNPROTECT(pc);
    return newlist;
  }
     
  /*Rprintf("\t\tEntered setListElement looking for %s.\n",str);*/
  /*Try to use an existing element, if possible*/
  names = getAttrib(list, R_NamesSymbol);
  for (i = 0; i < length(list); i++)
    if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
      SET_VECTOR_ELT(list, i, elem);
      /*Rprintf("\t\t\tUsing existing entry (%d)\n",i);*/
      return list;
    }

  /*If not possible, add as a new element*/
  /*Rprintf("\t\t\tNot found; creating new entry\n");*/
  PROTECT(newlist = enlargeList(list,1)); pc++;
  SET_VECTOR_ELT(newlist,length(newlist)-1,elem);
  PROTECT(names = getAttrib(newlist, R_NamesSymbol)); pc++;
  SET_STRING_ELT(names,length(newlist)-1,mkChar(str));
  setAttrib(newlist,R_NamesSymbol,names);

  UNPROTECT(pc);
  return newlist;
}


SEXP enlargeList(SEXP list, int n)
/*Return a pointer to an enlarged version of list, where the length is increased by n steps.*/
{
  int i,pc=0;
  SEXP newlist=R_NilValue, names, newnames;
  
  /*Rprintf("\t\tenlargeList entered, extending length %d by %d\n",length(list),n);*/
  if(n>0){
    switch(TYPEOF(list)){
      case VECSXP:
        PROTECT(newlist = allocVector(VECSXP, length(list)+n)); pc++;
        PROTECT(newnames = allocVector(STRSXP, length(list)+n)); pc++;
        names = getAttrib(list, R_NamesSymbol);
        for(i=0;i<length(list);i++){
          SET_VECTOR_ELT(newlist,i,VECTOR_ELT(list,i));
          if(length(names)>i)
            SET_STRING_ELT(newnames,i,STRING_ELT(names,i));
        }
        if(length(names)>0)
          setAttrib(newlist,R_NamesSymbol,newnames);
        break;
      case STRSXP:
        PROTECT(newlist = allocVector(STRSXP, length(list)+n));  pc++;
        PROTECT(newnames = allocVector(STRSXP, length(list)+n)); pc++;
        names = getAttrib(list, R_NamesSymbol);
        for(i=0;i<length(list);i++){
          SET_STRING_ELT(newlist,i,STRING_ELT(list,i));
          if(length(names)>i)
            SET_STRING_ELT(newnames,i,STRING_ELT(names,i));
        }
        if(length(names)>0)
          setAttrib(newlist,R_NamesSymbol,newnames);
        break;
      case INTSXP:
        PROTECT(newlist = allocVector(INTSXP, length(list)+n));  pc++;
        PROTECT(newnames = allocVector(STRSXP, length(list)+n)); pc++;
        names = getAttrib(list, R_NamesSymbol);
        for(i=0;i<length(list);i++){
          INTEGER(newlist)[i]=INTEGER(list)[i];
          if(length(names)>i)
            SET_STRING_ELT(newnames,i,STRING_ELT(names,i));
        }
        if(length(names)>0)
          setAttrib(newlist,R_NamesSymbol,newnames);
        break;
      case REALSXP:
        PROTECT(newlist = allocVector(REALSXP, length(list)+n));  pc++;
        PROTECT(newnames = allocVector(STRSXP, length(list)+n)); pc++;
        names = getAttrib(list, R_NamesSymbol);
        for(i=0;i<length(list);i++){
          REAL(newlist)[i]=REAL(list)[i];
          if(length(names)>i)
            SET_STRING_ELT(newnames,i,STRING_ELT(names,i));
        }
        if(length(names)>0)
          setAttrib(newlist,R_NamesSymbol,newnames);
        break;
      case RAWSXP:
        PROTECT(newlist = allocVector(RAWSXP, length(list)+n));  pc++;
        PROTECT(newnames = allocVector(STRSXP, length(list)+n)); pc++;
        names = getAttrib(list, R_NamesSymbol);
        for(i=0;i<length(list);i++){
          RAW(newlist)[i]=RAW(list)[i];
          if(length(names)>i)
            SET_STRING_ELT(newnames,i,STRING_ELT(names,i));
        }
        if(length(names)>0)
          setAttrib(newlist,R_NamesSymbol,newnames);
        break;
      case LGLSXP:
        PROTECT(newlist = allocVector(LGLSXP, length(list)+n));  pc++;
        PROTECT(newnames = allocVector(STRSXP, length(list)+n)); pc++;
        names = getAttrib(list, R_NamesSymbol);
        for(i=0;i<length(list);i++){
          INTEGER(newlist)[i]=INTEGER(list)[i];
          if(length(names)>i)
            SET_STRING_ELT(newnames,i,STRING_ELT(names,i));
        }
        if(length(names)>0)
          setAttrib(newlist,R_NamesSymbol,newnames);
        break;
      default:
        UNIMPLEMENTED_TYPE("enlargeList",TYPEOF(list));
    }
    UNPROTECT(pc);
    return newlist;
  }else{
    return list;
  }
}


SEXP contractList(SEXP list, int n)
/*Return a pointer to a contracted version of list, where only the first n items are selected.  If n>=length(list), then list is returned.*/
{
  int i,pc=0;
  SEXP newlist=R_NilValue, names, newnames;
  
  /*Rprintf("\t\tcontractList entered, changing length from %d to %d\n",length(list),n);*/
  if(n<length(list)){
    n=MAX(n,0);
    switch(TYPEOF(list)){
      case VECSXP:
        PROTECT(newlist = allocVector(VECSXP, n)); pc++;
        PROTECT(newnames = allocVector(STRSXP, n)); pc++;
        names = getAttrib(list, R_NamesSymbol);
        for(i=0;i<n;i++){
          SET_VECTOR_ELT(newlist,i,VECTOR_ELT(list,i));
          if(length(names)>i)
            SET_STRING_ELT(newnames,i,STRING_ELT(names,i));
        }
        if(length(names)>0)
          setAttrib(newlist,R_NamesSymbol,newnames);
        break;
      case STRSXP:
        PROTECT(newlist = allocVector(STRSXP, n)); pc++;
        PROTECT(newnames = allocVector(STRSXP, n)); pc++;
        names = getAttrib(list, R_NamesSymbol);
        for(i=0;i<n;i++){
          SET_STRING_ELT(newlist,i,STRING_ELT(list,i));
          if(length(names)>i)
            SET_STRING_ELT(newnames,i,STRING_ELT(names,i));
        }
        if(length(names)>0)
          setAttrib(newlist,R_NamesSymbol,newnames);
        break;
      case INTSXP:
        PROTECT(newlist = allocVector(INTSXP, n)); pc++;
        PROTECT(newnames = allocVector(STRSXP, n)); pc++;
        names = getAttrib(list, R_NamesSymbol);
        for(i=0;i<n;i++){
          INTEGER(newlist)[i]=INTEGER(list)[i];
          if(length(names)>i)
            SET_STRING_ELT(newnames,i,STRING_ELT(names,i));
        }
        if(length(names)>0)
          setAttrib(newlist,R_NamesSymbol,newnames);
        break;
      case LGLSXP:
        PROTECT(newlist = allocVector(LGLSXP, n)); pc++;
        PROTECT(newnames = allocVector(STRSXP, n)); pc++;
        names = getAttrib(list, R_NamesSymbol);
        for(i=0;i<n;i++){
          INTEGER(newlist)[i]=INTEGER(list)[i];
          if(length(names)>i)
            SET_STRING_ELT(newnames,i,STRING_ELT(names,i));
        }
        if(length(names)>0)
          setAttrib(newlist,R_NamesSymbol,newnames);
        break;
      case REALSXP:
        PROTECT(newlist = allocVector(REALSXP, n)); pc++;
        PROTECT(newnames = allocVector(STRSXP, n)); pc++;
        names = getAttrib(list, R_NamesSymbol);
        for(i=0;i<n;i++){
          REAL(newlist)[i]=REAL(list)[i];
          if(length(names)>i)
            SET_STRING_ELT(newnames,i,STRING_ELT(names,i));
        }
        if(length(names)>0)
          setAttrib(newlist,R_NamesSymbol,newnames);
        break;
      case RAWSXP:
        PROTECT(newlist = allocVector(RAWSXP, n)); pc++;
        PROTECT(newnames = allocVector(STRSXP, n)); pc++;
        names = getAttrib(list, R_NamesSymbol);
        for(i=0;i<n;i++){
          RAW(newlist)[i]=RAW(list)[i];
          if(length(names)>i)
            SET_STRING_ELT(newnames,i,STRING_ELT(names,i));
        }
        if(length(names)>0)
          setAttrib(newlist,R_NamesSymbol,newnames);
        break;
      default:
        UNIMPLEMENTED_TYPE("contractList",TYPEOF(list));
    }
    UNPROTECT(pc);
    return newlist;
  }else{
    return list;
  }
}


SEXP concatList(int nel, int names, ...)
/*This is a highly experimental function to build a list object by concatenating the specified arguments.  nel must give the number of list elements included, and names=1 iff names are supplied.  In the latter case, these must be strings, and must be given as arguments in order following the list elements.*/
{
  int i,pc=0;
  SEXP list,lnam;
  va_list ap;
  
  error("concatList doesn't work yet!  Sorry....\n");
  /*Rprintf("\t\tEntered concatList w/%d arguments; names=%d\n",nel,names);*/
  va_start(ap, names);                      /*Initialize the argument list*/
  PROTECT(list = allocVector(VECSXP,nel)); pc++;    /*Allocate list memory*/
  for(i=0;i<nel;i++)                                /*Add elements to list*/
/*    SET_VECTOR_ELT(list,i,va_arg(ap,(SEXPREC) *))*/;
  if(names){
    /*Rprintf("\t\t\tAdding names\n");*/
    PROTECT(lnam = allocVector(STRSXP,nel)); pc++;  /*Allocate name memory*/
    for(i=0;i<nel;i++)
      SET_STRING_ELT(lnam,i,mkChar(va_arg(ap,char *)));
    setAttrib(list,R_NamesSymbol,lnam);
  }
  va_end(ap);
  
  UNPROTECT(pc);
  return list;
}


SEXP permuteList(SEXP list, SEXP ord)
/*Return a new list, sorted by ord (which must be an integer vector, with R-style indices over 1:length(list)).  Equivalent to list[ord].*/
{
  SEXP newlist;
  int i,pc=0;
  
  if(length(list)==0)
    return list;

  PROTECT(newlist=allocVector(TYPEOF(list),length(list))); pc++;
  switch(TYPEOF(list)){
    case VECSXP:
      for(i=0;i<length(list);i++)
        SET_VECTOR_ELT(newlist,i,VECTOR_ELT(list,INTEGER(ord)[i]-1));
      break;
    case STRSXP:
      for(i=0;i<length(list);i++)
        SET_STRING_ELT(newlist,i,STRING_ELT(list,INTEGER(ord)[i]-1));
      break;
    case INTSXP:
      for(i=0;i<length(list);i++)
        INTEGER(newlist)[i]=INTEGER(list)[INTEGER(ord)[i]-1];
      break;
    case LGLSXP:
      for(i=0;i<length(list);i++)
        INTEGER(newlist)[i]=INTEGER(list)[INTEGER(ord)[i]-1];
      break;
    case REALSXP:
      for(i=0;i<length(list);i++)
        REAL(newlist)[i]=REAL(list)[INTEGER(ord)[i]-1];
      break;
    case RAWSXP:
      for(i=0;i<length(list);i++)
        RAW(newlist)[i]=RAW(list)[INTEGER(ord)[i]-1];
      break;
  }    

  /*Unprotect and return*/
  UNPROTECT(pc);
  return newlist;
}


/*VECTOR COMPARISON ROUTINES------------------------------------------------*/


int vecEq(SEXP a, SEXP b)
/*Check for the exact equality of two vectors.  This routine tries to be fairly intelligent about type, although only REALSXP, INTSXP, LGLSXP, CPLXSXP, RAWSXP, and STRSXP are currently supported.*/
{
  int i,type;

  if(a==R_NilValue){       /*First, see if one or both are NULL*/
    if(b==R_NilValue)
      return 1;
    else
      return 0;  
  }else{
    if(b==R_NilValue)
      return 0;
  }
  if(length(a)!=length(b))  /*Next, check for length*/
    return 0;
  type=TYPEOF(a);           /*Check for type equivalence*/
  if(TYPEOF(b)!=type)
    return 0;
  switch(type){             /*Types are the same -- check for equal entries*/
    case REALSXP:
      for(i=0;i<length(a);i++)
        if(REAL(a)[i]!=REAL(b)[i])
          return 0;
      return 1;
      break;
    case INTSXP:
      for(i=0;i<length(a);i++)
        if(INTEGER(a)[i]!=INTEGER(b)[i])
          return 0;
      return 1;
      break;
    case LGLSXP:
      for(i=0;i<length(a);i++)
        if(INTEGER(a)[i]!=INTEGER(b)[i])
          return 0;
      return 1;
      break;
    case STRSXP:
      for(i=0;i<length(a);i++)
        if(strcmp(CHAR(STRING_ELT(a,i)),CHAR(STRING_ELT(b,i)))!=0)
          return 0;
      return 1;
      break;
    case CPLXSXP:
      for(i=0;i<length(a);i++)
        if((COMPLEX(a)[i].r!=COMPLEX(b)[i].r)|| (COMPLEX(a)[i].i!=COMPLEX(b)[i].i))
          return 0;
      return 1;
      break;
    case RAWSXP:
      for(i=0;i<length(a);i++)
        if(RAW(a)[i]!=RAW(b)[i])
          return 0;
      return 1;
      break;
    default:
      UNIMPLEMENTED_TYPE("vecEq",type);
  }
  /*Should never get here!*/
  return -1;
}


int vecIsIn(double a, SEXP b)
/*Return 1 if a is contained in b, or else 0.  Note that b is coerced to double form for purposes of comparison.*/
{
  int i;

  /*Proceed based on type*/  
  switch(TYPEOF(b)){
    case INTSXP:
      for(i=0;i<length(b);i++)
        if((double)INTEGER(b)[i]==a)
          return 1;
      break;
    case LGLSXP:
      for(i=0;i<length(b);i++)
        if((double)INTEGER(b)[i]==a)
          return 1;
      break;
    case REALSXP:
      for(i=0;i<length(b);i++)
        if((double)REAL(b)[i]==a)
          return 1;
      break;
    case RAWSXP:
      for(i=0;i<length(b);i++)
        if((double)RAW(b)[i]==a)
          return 1;
      break;
    default:
      UNIMPLEMENTED_TYPE("vecIsIn",TYPEOF(b));
  }

  /*If still here, a isn't in b*/
  return 0;
}



double vecMax(SEXP a)
/*Give the maximum of a.  (a is coerced to a double vector, first.)*/
{
  int i;
  double maxval;
  
  PROTECT(a = coerceVector(a,REALSXP));
  if(length(a)==0){
    UNPROTECT(1);
    return NA_REAL;
  }
  maxval=REAL(a)[0];
  for(i=1;i<length(a);i++)
    maxval=MAX(maxval,REAL(a)[i]);

  UNPROTECT(1);
  return maxval;
}


double vecMin(SEXP a)
/*Give the minimum of a.  (a is coerced to a double vector, first.)*/
{
  int i;
  double minval;
  
  PROTECT(a = coerceVector(a,REALSXP));
  if(length(a)==0){
    UNPROTECT(1);
    return NA_REAL;
  }
  minval=REAL(a)[0];
  for(i=1;i<length(a);i++)
    minval=MIN(minval,REAL(a)[i]);

  UNPROTECT(1);
  return minval;
}


/*VECTOR MODIFICATION ROUTINES----------------------------------------------*/


SEXP vecAppend(SEXP a, SEXP b)
/*Create and return a vector which is the concatenation of a and b.*/
{
  int i,type,pc=0;
  SEXP v=R_NilValue;
  
  /*Check types*/
  type=TYPEOF(a);
  if(type!=TYPEOF(b))
    error("Type mismatch in vecAppend; types were %d and %d.\n",type,TYPEOF(b));

  /*Create the new vector and copy elements in place*/
  switch(type){
    case INTSXP:
      PROTECT(v=allocVector(INTSXP,length(a)+length(b))); pc++;
      for(i=0;i<length(a);i++)
        INTEGER(v)[i]=INTEGER(a)[i];
      for(i=0;i<length(b);i++)
        INTEGER(v)[i+length(a)]=INTEGER(b)[i];
      break;
    case LGLSXP:
      PROTECT(v=allocVector(LGLSXP,length(a)+length(b))); pc++;
      for(i=0;i<length(a);i++)
        INTEGER(v)[i]=INTEGER(a)[i];
      for(i=0;i<length(b);i++)
        INTEGER(v)[i+length(a)]=INTEGER(b)[i];
      break;
    case REALSXP:
      PROTECT(v=allocVector(REALSXP,length(a)+length(b))); pc++;
      for(i=0;i<length(a);i++)
        REAL(v)[i]=REAL(a)[i];
      for(i=0;i<length(b);i++)
        REAL(v)[i+length(a)]=REAL(b)[i];
      break;
    case RAWSXP:
      PROTECT(v=allocVector(RAWSXP,length(a)+length(b))); pc++;
      for(i=0;i<length(a);i++)
        RAW(v)[i]=RAW(a)[i];
      for(i=0;i<length(b);i++)
        RAW(v)[i+length(a)]=RAW(b)[i];
      break;
    case VECSXP:
      PROTECT(v=allocVector(VECSXP,length(a)+length(b))); pc++;
      for(i=0;i<length(a);i++)
        SET_VECTOR_ELT(v,i,VECTOR_ELT(a,i));
      for(i=0;i<length(b);i++)
        SET_VECTOR_ELT(v,i+length(a),VECTOR_ELT(b,i));
      break;
    default:
      UNIMPLEMENTED_TYPE("vecAppend",type);
  }

  /*Unprotect and return*/
  UNPROTECT(pc);
  return v;
}


SEXP vecRemove(SEXP v, double e)
/*Removes all instances of e from v.  Although passed as a double, e is recast appropriately for v's data type (to the extent possible).*/
{
  int type,i,count=0,nl,pc=0;
  SEXP newvec=R_NilValue;

  /*Determine type, and proceed accordingly*/
  type=TYPEOF(v);
  switch(type){
    case INTSXP:
      for(i=0;i<length(v);i++)
        count+=(((double)INTEGER(v)[i])==e);
      nl=length(v)-count;
      PROTECT(newvec=allocVector(INTSXP,nl)); pc++;
      count=0;
      for(i=0;i<length(v);i++)
        if(((double)INTEGER(v)[i])!=e)
          INTEGER(newvec)[count++]=INTEGER(v)[i];
      break;
    case LGLSXP:
      for(i=0;i<length(v);i++)
        count+=(((double)INTEGER(v)[i])==e);
      nl=length(v)-count;
      PROTECT(newvec=allocVector(LGLSXP,nl)); pc++;
      count=0;
      for(i=0;i<length(v);i++)
        if(((double)INTEGER(v)[i])!=e)
          INTEGER(newvec)[count++]=INTEGER(v)[i];
      break;
    case REALSXP:
      for(i=0;i<length(v);i++)
        count+=((REAL(v)[i])==e);
      nl=length(v)-count;
      PROTECT(newvec=allocVector(REALSXP,nl)); pc++;
      count=0;
      for(i=0;i<length(v);i++)
        if((REAL(v)[i])!=e)
          REAL(newvec)[count++]=REAL(v)[i];
      break;
    case RAWSXP:
      for(i=0;i<length(v);i++)
        count+=(((double)RAW(v)[i])==e);
      nl=length(v)-count;
      PROTECT(newvec=allocVector(RAWSXP,nl)); pc++;
      count=0;
      for(i=0;i<length(v);i++)
        if(((double)RAW(v)[i])!=e)
          RAW(newvec)[count++]=RAW(v)[i];
      break;
    default:
      UNIMPLEMENTED_TYPE("vecRemove",type);
  }

  /*Unprotect and exit*/
  UNPROTECT(pc);
  return newvec;
}


SEXP vecUnion(SEXP a, SEXP b)
/*Equivalent to unique(c(a,b)) in R space.  Note that a and b must be of the same type!*/
{
  SEXP merged;
  
  /*Rprintf("\tvecUnion: lists are length %d and %d\n",length(a),length(b));
  if(IS_INTEGER(a)&&(length(a)>0))
    Rprintf("\t\t\tFirst element of a=%d\n",INTEGER(a)[0]);
  if(IS_INTEGER(b)&&(length(b)>0))
    Rprintf("\t\t\tFirst element of b=%d\n",INTEGER(b)[0]);*/
  PROTECT(merged=vecAppend(a,b));
  /*Rprintf("\t\tAppended list is length %d\n",length(merged));
  if(IS_INTEGER(merged)&&(length(merged)>0))
    Rprintf("\t\t\tFirst list element=%d\n",INTEGER(merged)[0]);*/
  PROTECT(merged=vecUnique(merged));

  UNPROTECT(2);
  return merged;
}


SEXP vecUnique(SEXP a)
{
  int pc=0,*dup,dcount=0,i,j;
  SEXP newv=R_NilValue;

  /*Proceed by type*/
  switch(TYPEOF(a)){
    case INTSXP:
      /*Identify duplicates*/
      dup=(int *)R_alloc(length(a),sizeof(int));
      for(i=0;i<length(a);i++)
        dup[i]=0;
      for(i=0;i<length(a);i++)
        if(!dup[i]){
          for(j=i+1;j<length(a);j++)
            if((!dup[j])&&(INTEGER(a)[i]==INTEGER(a)[j])){
              dcount++;
              dup[j]++;
            }
        }
      /*Build vector from non-duplicated entries*/
      PROTECT(newv=allocVector(INTSXP,length(a)-dcount)); pc++;
      dcount=0;
      for(i=0;i<length(a);i++)
        if(!dup[i])
          INTEGER(newv)[dcount++]=INTEGER(a)[i];
      break;
    case LGLSXP:
      /*Identify duplicates*/
      dup=(int *)R_alloc(length(a),sizeof(int));
      for(i=0;i<length(a);i++)
        dup[i]=0;
      for(i=0;i<length(a);i++)
        if(!dup[i]){
          for(j=i+1;j<length(a);j++)
            if((!dup[j])&&(INTEGER(a)[i]==INTEGER(a)[j])){
              dcount++;
              dup[j]++;
            }
        }
      /*Build vector from non-duplicated entries*/
      PROTECT(newv=allocVector(LGLSXP,length(a)-dcount)); pc++;
      dcount=0;
      for(i=0;i<length(a);i++)
        if(!dup[i])
          INTEGER(newv)[dcount++]=INTEGER(a)[i];
      break;
    case REALSXP:
      /*Identify duplicates*/
      dup=(int *)R_alloc(length(a),sizeof(int));
      for(i=0;i<length(a);i++)
        dup[i]=0;
      for(i=0;i<length(a);i++)
        if(!dup[i]){
          for(j=i+1;j<length(a);j++)
            if((!dup[j])&&(REAL(a)[i]==REAL(a)[j])){
              dcount++;
              dup[j]++;
            }
        }
      /*Build vector from non-duplicated entries*/
      PROTECT(newv=allocVector(REALSXP,length(a)-dcount)); pc++;
      dcount=0;
      for(i=0;i<length(a);i++)
        if(!dup[i])
          REAL(newv)[dcount++]=REAL(a)[i];
      break;
    case RAWSXP:
      /*Identify duplicates*/
      dup=(int *)R_alloc(length(a),sizeof(int));
      for(i=0;i<length(a);i++)
        dup[i]=0;
      for(i=0;i<length(a);i++)
        if(!dup[i]){
          for(j=i+1;j<length(a);j++)
            if((!dup[j])&&(RAW(a)[i]==RAW(a)[j])){
              dcount++;
              dup[j]++;
            }
        }
      /*Build vector from non-duplicated entries*/
      PROTECT(newv=allocVector(RAWSXP,length(a)-dcount)); pc++;
      dcount=0;
      for(i=0;i<length(a);i++)
        if(!dup[i])
          RAW(newv)[dcount++]=RAW(a)[i];
      break;
    default:
      UNIMPLEMENTED_TYPE("vecUnion",TYPEOF(a));
  }

  /*Unprotect and return*/
  UNPROTECT(pc);
  return newv;
}
