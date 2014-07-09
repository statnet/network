#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "network.h"

#ifndef NETWORK_INIT_H
#define NETWORK_INIT_H

void import_network_functions(){
  getListElement = (SEXP (*)(SEXP list, const char *str)) 
    R_GetCCallable("network","getListElement");
  setListElement = (SEXP (*)(SEXP list, const char *str, SEXP elem))
R_GetCCallable("network","setListElement");
}

#endif
