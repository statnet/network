/*
######################################################################
#
# constructors.h
#
# Written by Carter T. Butts <buttsc@uci.edu>
# Last Modified 4/7/06
# Licensed under the GNU General Public License version 2 (June, 1991)
#
# Part of the R/network package
#
# This file contains headers for constructors.c.
#
######################################################################
*/
#ifndef CONSTRUCTORS_H
#define CONSTRUCTORS_H


/*DECLARATIONS/INCLUSIONS---------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>


/*INTERNAL ROUTINES---------------------------------------------------------*/



/*R-CALLABLE ROUTINES-------------------------------------------------------*/

SEXP copyEdges_R(SEXP x, SEXP y);

SEXP copyNetwork_R(SEXP x);

SEXP copyNetworkAttributes_R(SEXP x, SEXP y);

SEXP copyVertexAttributes_R(SEXP x, SEXP y);

#endif
