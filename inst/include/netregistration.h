#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "network.h"

#ifndef NETREGISTRATION_H
#define NETREGISTRATION_H

void netRegisterFunctions(void){

  getListElement = (SEXP (*)(SEXP list, const char *str)) 
    R_GetCCallable("network","getListElement");
  setListElement = (SEXP (*)(SEXP list, const char *str, SEXP elem))
R_GetCCallable("network","setListElement");

  /* Legacy networkapi functions */
  netGetEdgeAttrib_ptr = (SEXP (*)(SEXP, int, const char*)) R_FindSymbol("getEdgeAttribute", "network", NULL);
  netGetEdgeIDs_ptr = (SEXP (*)(SEXP, int, int, const char*, int))R_FindSymbol("getEdgeIDs", "network", NULL);
  netGetEdges_ptr = (SEXP (*)(SEXP, int, int, const char*, int))R_FindSymbol("getEdges", "network", NULL);
  netGetNeighborhood_ptr = (SEXP (*)(SEXP, int, const char*, int))R_FindSymbol("getNeighborhood", "network", NULL);
  netGetNetAttrib_ptr = (SEXP (*)(SEXP, const char*))R_FindSymbol("getNetworkAttribute", "network", NULL);
  netHasLoops_ptr = (int (*)(SEXP))R_FindSymbol("hasLoops", "network", NULL);
  netIsAdj_ptr = (int (*)(SEXP, int, int, int))R_FindSymbol("isAdjacent", "network", NULL);
  netIsDir_ptr = (int (*)(SEXP))R_FindSymbol("isDirected", "network", NULL);
  netIsHyper_ptr = (int (*)(SEXP))R_FindSymbol("isHyper", "network", NULL);
  netIsLoop_ptr = (int (*)(SEXP, SEXP))R_FindSymbol("isLoop", "network", NULL);
  netIsMulti_ptr = (int (*)(SEXP))R_FindSymbol("isMultiplex", "network", NULL);
  netIsNetwork_ptr = (int (*)(SEXP))R_FindSymbol("isNetwork", "network", NULL);
  netNetEdgecount_ptr = (int (*)(SEXP, int))R_FindSymbol("networkEdgecount", "network", NULL);
  netNetSize_ptr = (int (*)(SEXP))R_FindSymbol("networkSize", "network", NULL);

  /*Register modification routines*/
  netAddEdge_ptr = (SEXP (*)(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP))R_FindSymbol("addEdge_R", "network", NULL);
  netAddEdges_ptr = (SEXP (*)(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP))R_FindSymbol("addEdges_R", "network", NULL);
  netDelEdgeAttrib_ptr = (SEXP (*)(SEXP, int, const char*))R_FindSymbol("deleteEdgeAttribute", "network", NULL);
  netDelNetAttrib_ptr = (SEXP (*)(SEXP, const char*))R_FindSymbol("deleteNetworkAttribute", "network", NULL);
  netDelVertexAttrib_ptr = (SEXP (*)(SEXP, int, const char*))R_FindSymbol("deleteVertexAttribute", "network", NULL);
  netSetNetAttrib_ptr = (SEXP (*)(SEXP, const char*, SEXP))R_FindSymbol("setNetworkAttribute", "network", NULL);
  netSetVertexAttrib_ptr = (SEXP (*)(SEXP, const char*, SEXP, int))R_FindSymbol("setVertexAttribute", "network", NULL);
}

#endif
