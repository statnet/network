/*
######################################################################
#
# access.h
#
# Written by Carter T. Butts <buttsc@uci.edu>
# Last Modified 3/25/08
# Licensed under the GNU General Public License version 2 (June, 1991)
#
# Part of the R/network package
#
# This file contains headers for access.c.
#
######################################################################
*/
#ifndef ACCESS_H
#define ACCESS_H


/*DECLARATIONS/INCLUSIONS---------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>


/*INTERNAL ROUTINES---------------------------------------------------------*/

SEXP deleteEdgeAttribute(SEXP x, int e, const char *attrname);

SEXP deleteNetworkAttribute(SEXP x, const char *attrname);

SEXP deleteVertexAttribute(SEXP x, int v, const char *attrname);

SEXP getEdgeAttribute(SEXP x, int e, const char *str);

SEXP getEdgeIDs(SEXP x, int v, int alter, const char *neighborhood, int naOmit);

SEXP getEdges(SEXP x, int v, int alter, const char *neighborhood, int naOmit);

SEXP getNeighborhood(SEXP x, int v, const char *type, int naOmit);

SEXP getNetworkAttribute(SEXP x, const char *str);

int hasLoops(SEXP x);

int isAdjacent(SEXP x, int vi, int vj, int naOmit);

int isDirected(SEXP x);

int isHyper(SEXP x);

int isLoop(SEXP outl, SEXP inl);

int isMultiplex(SEXP x);

int networkEdgecount(SEXP x, int naOmit);

int networkSize(SEXP x);

SEXP setNetworkAttribute(SEXP x, const char *attrname, SEXP value);

SEXP setVertexAttribute(SEXP x, const char *attrname, SEXP value, int v);


/*R-CALLABLE ROUTINES-------------------------------------------------------*/

SEXP addEdge_R(SEXP x, SEXP tail, SEXP head, SEXP namesEval, SEXP valsEval, SEXP edgeCheck);

SEXP addEdges_R(SEXP x, SEXP tail, SEXP head, SEXP namesEval, SEXP valsEval, SEXP edgeCheck);

SEXP addVertices_R(SEXP x, SEXP nv, SEXP vattr);

SEXP deleteEdgeAttribute_R(SEXP x, SEXP attrname);

SEXP deleteEdges_R(SEXP x, SEXP eid);

SEXP deleteNetworkAttribute_R(SEXP x, SEXP attrname);

SEXP deleteVertexAttribute_R(SEXP x, SEXP attrname);

SEXP deleteVertices_R(SEXP x, SEXP vid);

SEXP getEdgeIDs_R(SEXP x, SEXP v, SEXP alter, SEXP neighborhood, SEXP naOmit);

SEXP getEdges_R(SEXP x, SEXP v, SEXP alter, SEXP neighborhood, SEXP naOmit);

SEXP getNeighborhood_R(SEXP x, SEXP v, SEXP type, SEXP naOmit);

SEXP isAdjacent_R(SEXP x, SEXP vi, SEXP vj, SEXP naOmit);

SEXP isNANetwork_R(SEXP x, SEXP y);

SEXP networkEdgecount_R(SEXP x, SEXP naOmit);

SEXP permuteVertexIDs_R(SEXP x, SEXP vids);

SEXP setEdgeAttribute_R(SEXP x, SEXP attrname, SEXP value, SEXP e);

SEXP setEdgeValue_R(SEXP x, SEXP attrname, SEXP value, SEXP e);

SEXP setNetworkAttribute_R(SEXP x, SEXP attrname, SEXP value);

SEXP setVertexAttribute_R(SEXP x, SEXP attrname, SEXP value, SEXP v);

#endif
