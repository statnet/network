\name{get.edges}
\alias{get.edges}
\alias{get.edgeIDs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Retrieve Edges or Edge IDs Associated with a Given Vertex }
\description{
  \code{get.edges} retrieves a list of edges incident on a given vertex; \code{get.edgeIDs} returns the internal identifiers for those edges, instead.  Both allow edges to be selected based on vertex neighborhood and (optionally) an additional endpoint.
}
\usage{
get.edges(x, v, alter = NULL, neighborhood = c("out", "in",
    "combined"), na.omit = TRUE)
get.edgeIDs(x, v, alter=NULL, neighborhood=c("out","in","combined"),
    na.omit=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{network} }
  \item{v}{ a vertex ID }
  \item{alter}{ optionally, the ID of another vertex }
  \item{neighborhood}{ an indicator for whether we are interested in in-edges, out-edges, or both (relative to \code{v}) }
  \item{na.omit}{ logical; should we omit missing edges? }
}
\details{
  By default, \code{get.edges} returns all out-, in-, or out- and in-edges containing \code{v}.  (\code{get.edgeIDs} is identical, save in its return value.)  Specifying a vertex in \code{alter} causes these edges to be further selected such that alter must also belong to the edge -- this can be used to extract edges between two particular vertices.  Omission of missing edges is accomplished via \code{na.omit}.
}
\value{
  For \code{get.edges}, a list of edges.  For \code{get.edgeIDs}, a vector of edge ID numbers.
}
\references{ Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  \url{http://www.jstatsoft.org/v24/i02/}
}
\author{ Carter T. Butts \email{buttsc@uci.edu}}
%\note{ ~~further notes~~ }
%
% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{get.neighborhood}} }
\examples{
#Create a network with three edges
m<-matrix(0,3,3)
m[1,2]<-1; m[2,3]<-1; m[3,1]<-1
g<-network(m)

get.edges(g,1,neighborhood="out")
get.edgeIDs(g,1,neighborhood="in")
}
\keyword{ classes }% at least one, from doc/KEYWORDS
\keyword{ graphs }% __ONLY ONE__ keyword per line
