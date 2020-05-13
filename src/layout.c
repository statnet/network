/*
######################################################################
#
# layout.c
#
# Written by Carter T. Butts <buttsc@uci.edu>
# Last Modified 9/6/10
# Licensed under the GNU General Public License version 2 (June, 1991)
# or greater
#
# Part of the R/network package
#
# This file contains routines related to computation of vertex layouts
# for plot.network (i.e., the plot.network.layout.* functions).  Note that
# this was originally  ported directly from the sna package (also by Carter 
# Butts), although some bits may have evolved.
#
######################################################################
*/
 
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <R.h>
#include "layout.h"

/*TWO-DIMENSIONAL LAYOUT ROUTINES--------------------------------------*/

void network_layout_fruchtermanreingold_R(double *d, double *pn, double *pm, 
int *pniter, double *pmaxdelta, double *pvolume, double *pcoolexp, double 
*prepulserad, int *pncell, double *pcjit, double *pcppr, double *pcpcr, double
*pcccr, double *x, double *y)
/*
Calculate a two-dimensional Fruchterman-Reingold layout for (symmetrized) 
edgelist matrix d (2 column).  Positions (stored in (x,y)) should be initialized
prior to calling this routine.
*/
{
  double frk,maxdelta,volume,coolexp,repulserad,t,ded,xd,yd,*dx,*dy;
  double rf,af,xmax,xmin,ymax,ymin,xwid,ywid,cjit,cppr,cpcr,cccr,celldis;
  long int n,j,k,l,m;
  int niter,i,*cellid,ncell,ix,iy,jx,jy;
  char *vmax;
  vcell *vcells,*p,*p2;
  vlist *vlp,*vlp2;
  
  /*Define various things*/
  n=(long int)*pn;

  if (n <= 1) return; /* quick return when too few nodes to layout */

  m=(long int)*pm;
  niter=*pniter;
  maxdelta=*pmaxdelta;
  volume=*pvolume;
  coolexp=*pcoolexp;
  repulserad=*prepulserad;
  ncell=*pncell;
  cjit=*pcjit;
  cppr=*pcppr;
  cpcr=*pcpcr;
  cccr=*pcccr;
  frk=sqrt(volume/(double)n); /*Define the F-R constant*/
  xmin=ymin=R_PosInf;
  xmax=ymax=R_NegInf;

  /*Allocate memory for transient structures*/
  dx=(double *)R_alloc(n,sizeof(double));
  dy=(double *)R_alloc(n,sizeof(double));
  cellid=(int *)R_alloc(n,sizeof(int));
  /*Run the annealing loop*/
  for(i=niter;i>=0;i--){
    /*Check for interrupts, before messing with temporary storage*/
    R_CheckUserInterrupt();
    /*Allocate cell structures for this iteration*/
    GetRNGstate();
    vmax=vmaxget();
    xmin=ymin=R_PosInf;
    xmax=ymax=R_NegInf;
    for(j=0;j<n;j++){            /*Get current extrema to form cells*/
      xmin=MIN(xmin,x[j]);
      ymin=MIN(ymin,y[j]);
      xmax=MAX(xmax,x[j]);
      ymax=MAX(ymax,y[j]);
    }
    xmin-=0.0001*(xmax-xmin);
    ymin-=0.0001*(ymax-ymin);
    xmax+=0.0001*(xmax-xmin);
    ymax+=0.0001*(ymax-ymin);
    xwid=(xmax-xmin)/((double)ncell);
    ywid=(ymax-ymin)/((double)ncell);
    vcells=NULL;
    for(j=0;j<n;j++){   /*Assign each vertex to a cell*/
      jx=MAX(MIN(x[j]+rnorm(0.0,xwid*cjit),xmax),xmin);  /*Jitter for memb*/
      jy=MAX(MIN(y[j]+rnorm(0.0,ywid*cjit),ymax),ymin);
      cellid[j]=(int)(floor((jx-xmin)/xwid)+ncell*floor((jy-ymin)/ywid));
      /*Find j's cell (or create an entry, if not already present)*/
      for(p=vcells;(p!=NULL)&&(p->next!=NULL)&&(p->id!=cellid[j]);p=p->next);
      if(p==NULL){                  /*Head was null; initiate*/
        vcells=p=(vcell *)R_alloc(1,sizeof(vcell));
        p->id=cellid[j];
        p->next=NULL;
        p->memb=NULL;
        p->count=0.0;
        p->xm=0.0;
        p->ym=0.0;
      }else if(p->id!=cellid[j]){   /*Got to end, insert new element*/
        p->next=(vcell *)R_alloc(1,sizeof(vcell));
        p=p->next;
        p->id=cellid[j];
        p->next=NULL;
        p->memb=NULL;
        p->count=0.0;
        p->xm=0.0;
        p->ym=0.0;
      }
      /*Add j to the membership stack for this cell*/
      p->count++;
      vlp=(vlist *)R_alloc(1,sizeof(vlist));
      vlp->v=j;
      vlp->next=p->memb;
      p->memb=vlp;
      p->xm=((p->xm)*((p->count)-1.0)+x[j])/(p->count);
      p->ym=((p->ym)*((p->count)-1.0)+y[j])/(p->count);
    }
    PutRNGstate();
    /*Set the temperature (maximum move/iteration)*/
    t=maxdelta*pow(i/(double)niter,coolexp);
    /*Clear the deltas*/
    for(j=0;j<n;j++){
      dx[j]=0.0;
      dy[j]=0.0;
    }
    /*Increment deltas for general force effects, using cells*/
    for(p=vcells;p!=NULL;p=p->next)          /*Add forces at the cell level*/
      for(p2=p;p2!=NULL;p2=p2->next){
        /*Get cell identities*/
        ix=(p->id)%ncell;
        jx=(p2->id)%ncell;
        iy=(int)floor((p->id)/ncell);
        jy=(int)floor((p2->id)/ncell);
        celldis=(double)((ix-jx)*(ix-jx)+(iy-jy)*(iy-jy)); /*Sq cell/cell dist*/
        if(celldis<=cppr+0.001){ /*Use point/point calculations (exact)*/
          for(vlp=p->memb;vlp!=NULL;vlp=vlp->next)
            for(vlp2=((p==p2)?(vlp->next):(p2->memb));vlp2!=NULL; vlp2=vlp2->next){
              /*Obtain difference vector*/
              xd=x[vlp->v]-x[vlp2->v];
              yd=y[vlp->v]-y[vlp2->v];
              ded=sqrt(xd*xd+yd*yd);  /*Get dyadic euclidean distance*/
              xd/=ded;                /*Rescale differences to length 1*/
              yd/=ded;
              /*Calculate repulsive "force"*/
              rf=frk*frk*(1.0/ded-ded*ded/repulserad);
              dx[vlp->v]+=xd*rf;        /*Add to the position change vector*/
              dx[vlp2->v]-=xd*rf;
              dy[vlp->v]+=yd*rf;
              dy[vlp2->v]-=yd*rf;
            }
        }else if(celldis<=cpcr+0.001){ /*Use point/cell calculations (approx)*/
          /*Add force increments to each member of p and p2*/
          for(vlp=p->memb;vlp!=NULL;vlp=vlp->next){
            xd=x[vlp->v]-(p2->xm);
            yd=y[vlp->v]-(p2->ym);
            ded=sqrt(xd*xd+yd*yd);  /*Get dyadic euclidean distance*/
            xd/=ded;                /*Rescale differences to length 1*/
            yd/=ded;
            /*Calculate repulsive "force"*/
            rf=frk*frk*(1.0/ded-ded*ded/repulserad);
            /*Add to dx and dy*/
            dx[vlp->v]+=xd*rf*(p2->count);
            dy[vlp->v]+=yd*rf*(p2->count);
          }
          for(vlp=p2->memb;vlp!=NULL;vlp=vlp->next){
            xd=x[vlp->v]-(p->xm);
            yd=y[vlp->v]-(p->ym);
            ded=sqrt(xd*xd+yd*yd);  /*Get dyadic euclidean distance*/
            xd/=ded;                /*Rescale differences to length 1*/
            yd/=ded;
            /*Calculate repulsive "force"*/
            rf=frk*frk*(1.0/ded-ded*ded/repulserad);
            /*Add to dx and dy*/
            dx[vlp->v]+=xd*rf*(p->count);
            dy[vlp->v]+=yd*rf*(p->count);
          }
        }else if(celldis<=cccr+0.001){  /*Use cell/cell calculations (crude!)*/
          xd=(p->xm)-(p2->xm);
          yd=(p->ym)-(p2->ym);
          ded=sqrt(xd*xd+yd*yd);  /*Get dyadic euclidean distance*/
          xd/=ded;                /*Rescale differences to length 1*/
          yd/=ded;
          /*Calculate repulsive "force"*/
          rf=frk*frk*(1.0/ded-ded*ded/repulserad);
          /*Add force increment to each member of p and p2*/
          for(vlp=p->memb;vlp!=NULL;vlp=vlp->next){
            dx[vlp->v]+=xd*rf*(p2->count);
            dy[vlp->v]+=yd*rf*(p2->count);
          }
          for(vlp=p2->memb;vlp!=NULL;vlp=vlp->next){
            dx[vlp->v]-=xd*rf*(p->count);
            dy[vlp->v]-=yd*rf*(p->count);
          }
        }
      }
    /*Calculate attraction along edges*/
    for(j=0;j<m;j++){
      k=(long int)d[j]-1;     /*Subtract 1, b/c R uses 1:n, not 0:(n-1)*/
      l=(long int)d[j+m]-1;
      xd=x[k]-x[l];
      yd=y[k]-y[l];
      ded=sqrt(xd*xd+yd*yd);  /*Get dyadic euclidean distance*/
      af=ded*ded/frk;
      dx[k]-=xd*af;           /*Add to the position change vector*/
      dx[l]+=xd*af;
      dy[k]-=yd*af;
      dy[l]+=yd*af;
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
    /*Free memory for cell membership (or at least unprotect it)*/
    vmaxset(vmax);
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

