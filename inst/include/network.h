#include <R.h>
#include <Rinternals.h>

#ifndef NETWORK_H
#define NETWORK_H

SEXP (*getListElement)(SEXP list, const char *str);
SEXP (*setListElement)(SEXP list, const char *str, SEXP elem);

/* Legacy networkapi.h functions */
/* Access functions*/
SEXP (*netGetEdgeAttrib_ptr)(SEXP, int, const char*);
SEXP (*netGetEdgeIDs_ptr)(SEXP, int, int, const char*, int);
SEXP (*netGetEdges_ptr)(SEXP, int, int, const char*, int);
SEXP (*netGetNeighborhood_ptr)(SEXP, int, const char*, int);
SEXP (*netGetNetAttrib_ptr)(SEXP, const char*);
int (*netHasLoops_ptr)(SEXP);
int (*netIsAdj_ptr)(SEXP, int, int, int);
int (*netIsDir_ptr)(SEXP);
int (*netIsHyper_ptr)(SEXP);
int (*netIsLoop_ptr)(SEXP, SEXP);
int (*netIsMulti_ptr)(SEXP);
int (*netIsNetwork_ptr)(SEXP);
int (*netNetEdgecount_ptr)(SEXP, int);
int (*netNetSize_ptr)(SEXP);

/*Modification functions*/
SEXP (*netAddEdge_ptr)(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP (*netAddEdges_ptr)(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP (*netDelEdgeAttrib_ptr)(SEXP, int, const char*);
SEXP (*netDelNetAttrib_ptr)(SEXP, const char*);
SEXP (*netDelVertexAttrib_ptr)(SEXP, int, const char*);
SEXP (*netSetNetAttrib_ptr)(SEXP, const char*, SEXP);
SEXP (*netSetVertexAttrib_ptr)(SEXP, const char*, SEXP, int);

/*Access functions*/
#define netGetEdgeAttrib (*netGetEdgeAttrib_ptr)
#define netGetEdgeIDs (*netGetEdgeIDs_ptr)
#define netGetEdges (*netGetEdges_ptr)
#define netGetNeighborhood (*netGetNeighborhood_ptr)
#define netGetNetAttrib (*netGetNetAttrib_ptr)
#define netHasLoops (*netHasLoops_ptr)
#define netIsAdj (*netIsAdj_ptr)
#define netIsDir (*netIsDir_ptr)
#define netIsHyper (*netIsHyper_ptr)
#define netIsLoop (*netIsLoop_ptr)
#define netIsMulti (*netIsMulti_ptr)
#define netIsNetwork (*netIsNetwork_ptr)
#define netNetEdgecount (*netNetEdgecount_ptr)
#define netNetSize (*netNetSize_ptr)

/*Modification functions*/
#define netAddEdge (*netAddEdge_ptr)
#define netAddEdges (*netAddEdges_ptr)
#define netDelEdgeAttrib (*netDelEdgeAttrib_ptr)
#define netDelNetAttrib (*netDelNetAttrib_ptr)
#define netDelVertexAttrib (*netDelVertexAttrib_ptr)
#define netSetNetAttrib (*netSetNetAttrib_ptr)
#define netSetVertexAttrib (*netSetVertexAttrib_ptr)

#endif
