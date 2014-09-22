/*
######################################################################
#
# geodist.h
#
# copyright (c) 2004, Carter T. Butts <buttsc@uci.edu>
# Last Modified 4/26/09
# Licensed under the GNU General Public License version 2 (June, 1991)
#
# Part of the R/sna package
#
# This file contains headers for geodist.c.
#
######################################################################
*/
#ifndef GEODIST_H
#define GEODIST_H

/*DECLARATIONS/INCLUSIONS---------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include "sna_utils.h"


/*R-CALLABLE ROUTINES-------------------------------------------------------*/


SEXP geodist_R(SEXP mat, SEXP sn, SEXP m, SEXP scheckna, SEXP scalcsig, SEXP scalcpred);

SEXP geodist_val_R(SEXP mat, SEXP sn, SEXP sm, SEXP scheckna, SEXP scalcsig, SEXP scalcpred);


#endif
