/*
######################################################################
#
# constructors.c
#
# Written by Carter T. Butts <buttsc@uci.edu>
# Last Modified 03/04/19
# Licensed under the GNU General Public License version 2 (June, 1991)
# or greater
#
# Part of the R/network package
#
# This file contains routines related to constructor methods for 
# network objects.
#
######################################################################
*/
 
#include <stdio.h>
#include <stdlib.h>
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include "utils.h"
#include "constructors.h"


/*INTERNAL ROUTINES----------------------------------------------------*/



/*R-CALLABLE ROUTINES--------------------------------------------------*/


SEXP copyEdges_R(SEXP x, SEXP y)
/*Copy all edges from network x into network y.  Note that y is assumed to have been initialized so as to have the same size as x.*/
{
  int pc=0;
  SEXP mel,mel2,iel,iel2,oel,oel2;
  
  mel=getListElement(x,"mel");
  PROTECT(mel2=duplicate(mel)); pc++;
  PROTECT(y=setListElement(y,"mel",mel2)); pc++;
  iel=getListElement(x,"iel");
  PROTECT(iel2=duplicate(iel)); pc++;
  PROTECT(y=setListElement(y,"iel",iel2)); pc++;
  oel=getListElement(x,"oel");
  PROTECT(oel2=duplicate(oel)); pc++;
  y=setListElement(y,"oel",oel2);

  UNPROTECT(pc);
  return y;  
}


SEXP copyNetwork_R(SEXP x)
{
  int pc=0;
  SEXP y;

  PROTECT(y=duplicate(x)); pc++;

  UNPROTECT(pc);
  return y;
}


SEXP copyNetworkAttributes_R(SEXP x, SEXP y)
/*Copy all network attributes from network x into network y.*/
{
  int pc=0;
  SEXP gal,gal2;
  
  gal=getListElement(x,"gal");
  PROTECT(gal2=duplicate(gal)); pc++;
  y=setListElement(y,"gal",gal2);

  UNPROTECT(pc);
  return y;
}


SEXP copyVertexAttributes_R(SEXP x, SEXP y)
/*Copy all vertex attributes from network x into network y.  Note that y is assumed to have been initialized so as to have the same size as x.*/
{
  int pc=0;
  SEXP val,val2;
  
  val=getListElement(x,"val");
  PROTECT(val2=duplicate(val)); pc++;
  y=setListElement(y,"val",val2);

  UNPROTECT(pc);
  return y;
}

