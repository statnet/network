\name{add.vertices}
\alias{add.vertices}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Add Vertices to an Existing Network }
\description{
  \code{add.vertices} adds a specified number of vertices to an existing network; if desired, attributes for the new vertices may be specified as well.
}
\usage{
add.vertices(x, nv, vattr = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{network} }
  \item{nv}{ the number of vertices to add }
  \item{vattr}{ optionally, a list of attributes with one entry per new vertex }
}
\details{
Note that the attribute format used here is based on the internal (vertex-wise) storage method, as opposed to the attribute-wise format used by \code{\link{network}}.
}
\value{
  Invisibly, a pointer to the updated \code{network} object; \code{add.vertices} modifies its argument in place.
}
\references{ Butts, C.T.  2002.  ``Memory Structures for Relational Data in R: Classes and Interfaces''  Working Paper. }
\author{ Carter T. Butts \email{buttsc@uci.edu}}
%\note{ ~~further notes~~ }
%
% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{network}}, \code{\link{get.vertex.attribute}}, \code{\link{set.vertex.attribute}} }
\examples{
#Initialize a network object
g<-network.initialize(5)
g

#Add five more vertices
add.vertices(g,5)
g
}
\keyword{ classes }% at least one, from doc/KEYWORDS
\keyword{ graphs }% __ONLY ONE__ keyword per line
