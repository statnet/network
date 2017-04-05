\name{deletion.methods}
\alias{deletion.methods}
\alias{delete.edges}
\alias{delete.vertices}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Remove Elements from a Network Object }
\description{
  \code{delete.edges} removes one or more edges (specified by their internal ID numbers) from a network; \code{delete.vertices} performs the same task for vertices (removing all associated edges in the process).
}
\usage{
delete.edges(x, eid)
delete.vertices(x, vid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{network}. }
  \item{eid}{ a vector of edge IDs. }
  \item{vid}{ a vector of vertex IDs. }
}
\details{
  Note that an edge's ID number corresponds to its order within \code{x$mel}.  To determine edge IDs, see \code{\link{get.edgeIDs}}.  Likewise, vertex ID numbers reflect the order with which vertices are listed internally (e.g., the order of \code{x$oel} and \code{x$iel}, or that used by \code{as.matrix.network.adjacency}).  When vertices are removed from a network, all edges having those vertices as endpoints are removed as well.  
  
Edges can also be added/removed via the extraction/replacement operators.  See the associated man page for details.
}
\value{
  Invisibly, a pointer to the updated network; these functions modify their arguments in place.
}
\references{ Butts, C.T.  2002.  ``Memory Structures for Relational Data in R: Classes and Interfaces''  Working Paper. }
\author{ Carter T. Butts \email{buttsc@uci.edu} }
%\note{ ~~further notes~~ }
%
% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{get.edgeIDs}}, \code{\link{network.extraction}}}
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
\keyword{ classes }% at least one, from doc/KEYWORDS
\keyword{ graphs }% __ONLY ONE__ keyword per line
