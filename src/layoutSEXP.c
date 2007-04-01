/*
######################################################################
#
# layoutSEXP.c
#
# Written by Carter T. Butts <buttsc@uci.edu>
# Last Modified 11/11/06
# Licensed under the GNU General Public License version 2 (June, 1991)
#
# Part of the R/network package
#
# This file contains routines related to layoutSEXP methods for network
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
#include "layoutSEXP.h"

/*TWO-DIMENSIONAL LAYOUT ROUTINES--------------------------------------*/

SEXP network_layout_fg_R(SEXP nw, SEXP niter, SEXP param, SEXP loc)
/*
Calculate a two-dimensional Fruchterman-Reingold layout for (symmetrized) 
adjacency matrix d.  Positions (stored in loc) should be initialized
prior to calling this routine.
*/
{
  double frk,t,ded,xd,yd;
  double *dx,*dy;
  double rf,af;
  int niteration;
  double maxdelta, volume, coolexp, repulserad; 
  long int n;
  int j,k;
  int i;
  
  /*Define various things*/
  n=networkSize(nw);
//  Rprintf("n %d\n",n);
  niteration=(int)REAL(niter)[0];
//  Rprintf("niteration %d\n",niteration);
  maxdelta=REAL(param)[0];
  volume=REAL(param)[1];
  coolexp=REAL(param)[2];
  repulserad=REAL(param)[3];
//  Rprintf("maxdelta %f %f %f %f\n",maxdelta,volume,coolexp,repulserad);

//  Rprintf("loc %f %f %f %f\n",REAL(loc)[0],REAL(loc)[1],REAL(loc)[n],REAL(loc)[n+1]);

  frk=sqrt(volume/(double)n); /*Define the F-R constant*/

  /*Allocate memory for transient structures*/
  dx=(double *)R_alloc(n,sizeof(double));
  dy=(double *)R_alloc(n,sizeof(double));
  /*Run the annealing loop*/
  for(i=niteration;i>=0;i--){
    /*Set the temperature (maximum move/iteration)*/
    t=maxdelta*pow(i/(double)niteration,coolexp);
    /*Clear the deltas*/
    for(j=0;j<n;j++){
      dx[j]=0.0;
      dy[j]=0.0;
    }
    /*Increment deltas for each undirected pair*/
    for(j=0;j<n;j++)
      for(k=j+1;k<n;k++){
        /*Obtain difference vector*/
        xd=REAL(loc)[  j]-REAL(loc)[  k];
        yd=REAL(loc)[n+j]-REAL(loc)[n+k];
        ded=sqrt(xd*xd+yd*yd);  /*Get dyadic euclidean distance*/
        xd/=ded;                /*Rescale differences to length 1*/
        yd/=ded;
        /*Calculate repulsive "force"*/
        rf=frk*frk*(1.0/ded-ded*ded/repulserad);
        dx[j]+=xd*rf;        /*Add to the position change vector*/
        dx[k]-=xd*rf;
        dy[j]+=yd*rf;
        dy[k]-=yd*rf;
        /*Calculate the attractive "force"*/
//  Rprintf("j %d k %d adj %d\n",j,k,isAdjacent(nw,j+1,k+1,0));
        if(isAdjacent(nw,j+1,k+1,0)||isAdjacent(nw,k+1,j+1,0)){
          af=ded*ded/frk;
          dx[j]-=xd*af;        /*Add to the position change vector*/
          dx[k]+=xd*af;
          dy[j]-=yd*af;
          dy[k]+=yd*af;
        }
      }
    /*Dampen motion, if needed, and move the points*/
    for(j=0;j<n;j++){
      ded=sqrt(dx[j]*dx[j]+dy[j]*dy[j]);
      if(ded>t){                 /*Dampen to t*/
        ded=t/ded;
        dx[j]*=ded;
        dy[j]*=ded;
      }
      REAL(loc)[  j]+=dx[j];               /*Update positions*/
      REAL(loc)[n+j]+=dy[j];
    }
//  Rprintf("%d of %d to go\n",i,niteration);
  }
//  Rprintf("loc %f %f %f %f\n",REAL(loc)[0],REAL(loc)[1],REAL(loc)[n],REAL(loc)[n+1]);
  return(loc);
}
