/*
######################################################################
#
# layout.h
#
# Written by Carter T. Butts <buttsc@uci.edu>
# Last Modified 1/6/05
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


/*R-CALLABLE ROUTINES-------------------------------------------------------*/

void network_layout_fruchtermanreingold_R(int *d, double *pn, int *pniter, 
double *pmaxdelta, double *pvolume, double *pcoolexp, double *prepulserad,
double *x, double *y);

void network_layout_fruchtermanreingold2_R(int *d, double *pn, int *pniter, 
double *pmaxdelta, double *pvolume, double *pcoolexp, double *prepulserad,
double *x, double *y, int *fixed);

void network_layout_kamadakawai_R(int *d, double *pn, int *pniter, 
double *elen, double *pinitemp, double *pcoolexp, double *pkkconst, 
double *psigma, double *x, double *y);

#endif
