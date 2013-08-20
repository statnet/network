/*
######################################################################
#
# utils.h
#
# Written by Carter T. Butts <buttsc@uci.edu>
# Last Modified 08/20/13
# Licensed under the GNU General Public License version 2 (June, 1991)
#
# Part of the R/network package
#
# This file contains headers for utils.c.
#
######################################################################
*/
#ifndef UTILS_H
#define UTILS_H

/*DECLARATIONS/INCLUSIONS---------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

#define MIN(a,b) ((a)<(b) ? (a) : (b))
#define MAX(a,b) ((a)<(b) ? (b) : (a))


/*LIST ACCESS/MODIFICATION ROUTINES-----------------------------------------*/

SEXP deleteListElement(SEXP list, const char *str);

SEXP getListElement(SEXP list, const char *str);

SEXP setListElement(SEXP list, const char *str, SEXP elem);

SEXP enlargeList(SEXP list, int n);

SEXP contractList(SEXP list, int n);

SEXP concatList(int nel, int names, ...);

SEXP permuteList(SEXP list, SEXP ord);


/*VECTOR COMPARISON/TEST ROUTINES-------------------------------------------*/

int vecAnyNA(SEXP a);

int vecEq(SEXP a, SEXP b);

int vecIsIn(double a, SEXP b);

double vecMax(SEXP a);

double vecMin(SEXP a);


/*VECTOR MODIFICATION ROUTINES----------------------------------------------*/

SEXP vecAppend(SEXP a, SEXP b);

SEXP vecRemove(SEXP v, double e);

SEXP vecUnion(SEXP a, SEXP b);

SEXP vecUnique(SEXP a);

#endif
