#include <R.h>
#include <Rinternals.h>

#ifndef NETWORK_H
#define NETWORK_H

SEXP (*getListElement)(SEXP list, const char *str);

SEXP (*setListElement)(SEXP list, const char *str, SEXP elem);

#endif
