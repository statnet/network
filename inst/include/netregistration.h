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

  /*Register access routines*/
  netGetEdgeAttrib_ptr = (SEXP (*)(SEXP, int, const char*)) R_GetCCallable("network", "getEdgeAttribute");
  netGetEdgeIDs_ptr = (SEXP (*)(SEXP, int, int, const char*, int)) R_GetCCallable("network", "getEdgeIDs");
  netGetEdges_ptr = (SEXP (*)(SEXP, int, int, const char*, int)) R_GetCCallable("network", "getEdges");
  netGetNeighborhood_ptr = (SEXP (*)(SEXP, int, const char*, int)) R_GetCCallable("network", "getNeighborhood");
  netGetNetAttrib_ptr = (SEXP (*)(SEXP, const char*)) R_GetCCallable("network", "getNetworkAttribute");
  netHasLoops_ptr = (int (*)(SEXP)) R_GetCCallable("network", "hasLoops");
  netIsAdj_ptr = (int (*)(SEXP, int, int, int)) R_GetCCallable("network", "isAdjacent");
  netIsDir_ptr = (int (*)(SEXP)) R_GetCCallable("network", "isDirected");
  netIsHyper_ptr = (int (*)(SEXP)) R_GetCCallable("network", "isHyper");
  netIsLoop_ptr = (int (*)(SEXP, SEXP)) R_GetCCallable("network", "isLoop");
  netIsMulti_ptr = (int (*)(SEXP)) R_GetCCallable("network", "isMultiplex");
  netIsNetwork_ptr = (int (*)(SEXP)) R_GetCCallable("network", "isNetwork");
  netNetEdgecount_ptr = (int (*)(SEXP, int)) R_GetCCallable("network", "networkEdgecount");
  netNetSize_ptr = (int (*)(SEXP)) R_GetCCallable("network", "networkSize");
 
  /*Register modification routines*/
  netAddEdge_ptr = (SEXP (*)(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP)) R_GetCCallable("network", "addEdge_R");
  netAddEdges_ptr = (SEXP (*)(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP)) R_GetCCallable("network", "addEdges_R");
  netDelEdgeAttrib_ptr = (SEXP (*)(SEXP, int, const char*)) R_GetCCallable("network", "deleteEdgeAttribute");
  netDelVertexAttrib_ptr = (SEXP (*)(SEXP, int, const char*)) R_GetCCallable("network", "deleteVertexAttribute");
  netDelNetAttrib_ptr = (SEXP (*)(SEXP, const char*)) R_GetCCallable("network", "deleteNetworkAttribute");
  netSetNetAttrib_ptr = (SEXP (*)(SEXP, const char*, SEXP)) R_GetCCallable("network", "setNetworkAttribute");
  netSetVertexAttrib_ptr = (SEXP (*)(SEXP, const char*, SEXP, int)) R_GetCCallable("network", "setVertexAttribute");

}
#endif
