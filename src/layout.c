/*
######################################################################
#
# layout.c
#
# Written by Carter T. Butts <buttsc@uci.edu>
# Last Modified 1/5/05
# Licensed under the GNU General Public License version 2 (June, 1991)
#
# Part of the R/network package
#
# This file contains routines related to computation of vertex layouts
# for plot.network (i.e., the plot.network.layout.* functions).  Note that
# this is ported directly from the sna package (also by Carter Butts).
#
######################################################################
*/
 
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <R.h>
#include "layout.h"

/*TWO-DIMENSIONAL LAYOUT ROUTINES--------------------------------------*/

void network_layout_fruchtermanreingold_R(int *d, double *pn, int *pniter, 
double *pmaxdelta, double *pvolume, double *pcoolexp, double *prepulserad,
double *x, double *y)
/*
Calculate a two-dimensional Fruchterman-Reingold layout for (symmetrized) 
adjacency matrix d.  Positions (stored in (x,y)) should be initialized
prior to calling this routine.
*/
{
  double frk,maxdelta,volume,coolexp,repulserad,t,ded,xd,yd,*dx,*dy;
  double rf,af;
  long int n,j,k;
  int niter,i;
  
  /*Define various things*/
  n=(long int)*pn;
  niter=*pniter;
  maxdelta=*pmaxdelta;
  volume=*pvolume;
  coolexp=*pcoolexp;
  repulserad=*prepulserad;
  frk=sqrt(volume/(double)n); /*Define the F-R constant*/

  /*Allocate memory for transient structures*/
  dx=(double *)R_alloc(n,sizeof(double));
  dy=(double *)R_alloc(n,sizeof(double));
  /*Run the annealing loop*/
  for(i=niter;i>=0;i--){
    /*Set the temperature (maximum move/iteration)*/
    t=maxdelta*pow(i/(double)niter,coolexp);
    /*Clear the deltas*/
    for(j=0;j<n;j++){
      dx[j]=0.0;
      dy[j]=0.0;
    }
    /*Increment deltas for each undirected pair*/
    for(j=0;j<n;j++)
      for(k=j+1;k<n;k++){
        /*Obtain difference vector*/
        xd=x[j]-x[k];
        yd=y[j]-y[k];
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
        if(d[j+k*n]||d[k+j*n]){
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
      x[j]+=dx[j];               /*Update positions*/
      y[j]+=dy[j];
    }
  }
}

void network_layout_kamadakawai_R(int *d, double *pn, int *pniter, double *elen, double *pinitemp, double *pcoolexp, double *pkkconst, double *psigma, double *x, double *y)
{
  double initemp,coolexp,sigma,temp,candx,candy;
  double dpot,odis,ndis,osqd,nsqd,kkconst;
  int niter;
  long int n,i,j,k;
  
  /*Define various things*/
  n=(long int)*pn;
  niter=*pniter;
  initemp=*pinitemp;
  coolexp=*pcoolexp;
  kkconst=*pkkconst;
  sigma=*psigma;
  GetRNGstate();   /*Get the RNG state*/
  
  /*Perform the annealing loop*/
  temp=initemp;
  for(i=0;i<niter;i++){
    /*Update each vertex*/
    for(j=0;j<n;j++){
      /*Draw the candidate via a gaussian perturbation*/
      candx=rnorm(x[j],sigma*temp/initemp);
      candy=rnorm(y[j],sigma*temp/initemp);
      /*Calculate the potential difference for the new position*/
      dpot=0.0;
      for(k=0;k<n;k++)  /*Potential differences for pairwise effects*/
        if(j!=k){
          odis=sqrt((x[j]-x[k])*(x[j]-x[k])+(y[j]-y[k])*(y[j]-y[k]));
          ndis=sqrt((candx-x[k])*(candx-x[k])+(candy-y[k])*(candy-y[k]));
          osqd=(odis-elen[j+k*n])*(odis-elen[j+k*n]);
          nsqd=(ndis-elen[j+k*n])*(ndis-elen[j+k*n]);
          dpot+=kkconst*(osqd-nsqd)/(elen[j+k*n]*elen[j+k*n]);
        }
      /*Make a keep/reject decision*/
      if(log(runif(0.0,1.0))<dpot/temp){
        x[j]=candx;
        y[j]=candy;
      }
    }
    /*Cool the system*/
    temp*=coolexp;
  }
  PutRNGstate();   /*Update the RNG*/
}

