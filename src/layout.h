/*
######################################################################
#
# layout.h
#
# Written by Carter T. Butts <buttsc@uci.edu>
# Last Modified 9/6/10
# Licensed under the GNU General Public License version 2 (June, 1991)
#
# Part of the R/network package
#
# This file contains headers for layout.c.
#
######################################################################
*/
#ifndef LAYOUT_H
#define LAYOUT_H

/*DECLARATIONS/INCLUSIONS---------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <R.h>
#include <Rmath.h>
#include "utils.h"

/*Simple list structures to be used for temporary storage of vertex sets.*/
typedef struct vlisttype{
  long int v;
  struct vlisttype *next;
} vlist;

typedef struct vcelltype{
  int id;
  double count,xm,ym;
  struct vlisttype *memb;
  struct vcelltype *next;
} vcell;


/*R-CALLABLE ROUTINES-------------------------------------------------------*/

void network_layout_fruchtermanreingold_R(double *d, double *pn, double *pm, 
int *pniter, double *pmaxdelta, double *pvolume, double *pcoolexp, double 
*prepulserad, int *pncell, double *pcjit, double *pcppr, double *pcpcr, double
*pcccr, double *x, double *y);

void network_layout_kamadakawai_R(int *d, double *pn, int *pniter, 
double *elen, double *pinitemp, double *pcoolexp, double *pkkconst, 
double *psigma, double *x, double *y);

#endif
