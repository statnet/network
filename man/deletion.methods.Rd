% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/access.R
\name{deletion.methods}
\alias{deletion.methods}
\alias{delete.edges}
\alias{delete.edges.network}
\alias{delete.vertices}
\alias{delete.vertices.network}
\title{Remove Elements from a Network Object}
\usage{
delete.edges(x, eid, ...)

\method{delete.edges}{network}(x, eid, ...)

delete.vertices(x, vid, ...)

\method{delete.vertices}{network}(x, vid, ...)
}
\arguments{
\item{x}{an object of class \code{network}.}

\item{eid}{a vector of edge IDs.}

\item{...}{additional arguments to methods.}

\item{vid}{a vector of vertex IDs.}
}
\value{
Invisibly, a pointer to the updated network; these functions modify
their arguments in place.
}
\description{
\code{delete.edges} removes one or more edges (specified by
their internal ID numbers) from a network; \code{delete.vertices}
performs the same task for vertices (removing all associated edges in
the process).
}
\details{
Note that an edge's ID number corresponds to its order within
\code{x$mel}.  To determine edge IDs, see \code{\link{get.edgeIDs}}.
Likewise, vertex ID numbers reflect the order with which vertices are
listed internally (e.g., the order of \code{x$oel} and \code{x$iel}, or
that used by \code{as.matrix.network.adjacency}).  When vertices are
removed from a network, all edges having those vertices as endpoints are
removed as well.  When edges are removed, the remaining edge ids are NOT
permuted and \code{NULL} elements will be left on the list of edges, which
may complicate some functions that require eids (such as
\code{\link{set.edge.attribute}}).  The function \code{\link{valid.eids}}
provides a means to determine the set of valid (non-NULL) edge ids.

Edges can also be added/removed via the extraction/replacement operators.
See the associated man page for details.
}
\examples{
#Create a network with three edges
m<-matrix(0,3,3)
m[1,2]<-1; m[2,3]<-1; m[3,1]<-1
g<-network(m)

as.matrix.network(g)
delete.edges(g,2)              #Remove an edge
as.matrix.network(g)
delete.vertices(g,2)           #Remove a vertex
as.matrix.network(g)

#Can also remove edges using extraction/replacement operators
g<-network(m)
g[1,2]<-0                      #Remove an edge
g[,]
g[,]<-0                        #Remove all edges
g[,]

}
\references{
Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).
\doi{10.18637/jss.v024.i02}
}
\seealso{
\code{\link{get.edgeIDs}}, \code{\link{network.extraction}},
\code{\link{valid.eids}}
}
\author{
Carter T. Butts \email{buttsc@uci.edu}
}
\keyword{classes}
\keyword{graphs}
