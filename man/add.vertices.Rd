\name{add.vertices}
\alias{add.vertices}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Add Vertices to an Existing Network }
\description{
  \code{add.vertices} adds a specified number of vertices to an existing network; if desired, attributes for the new vertices may be specified as well.
}
\usage{
add.vertices(x, nv, vattr = NULL, last.mode = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{network} }
  \item{nv}{ the number of vertices to add }
  \item{vattr}{ optionally, a list of attributes with one entry per new vertex }
  \item{last.mode}{ logical; should the new vertices be added to the last (rather than the first) mode of a bipartite network?}
}
\details{
New vertices are generally appended to the end of the network (i.e., their vertex IDs begin with \code{network.size(x)} an count upward).  The one exception to this rule is when \code{x} is bipartite and \code{last.mode==FALSE}.  In this case, new vertices are added to the end of the first mode, with existing second-mode vertices being permuted upward in ID.  (\code{x}'s \code{bipartite} attribute is adjusted accordingly.)

Note that the attribute format used here is based on the internal (vertex-wise) storage method, as opposed to the attribute-wise format used by \code{\link{network}}.
}
\value{
  Invisibly, a pointer to the updated \code{network} object; \code{add.vertices} modifies its argument in place.
}
\references{ Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  \url{http://www.jstatsoft.org/v24/i02/}
}
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
