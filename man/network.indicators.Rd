\name{network.indicators}
\alias{network.indicators}
\alias{has.loops}
\alias{is.directed}
\alias{is.hyper}
\alias{is.multiplex}
\alias{is.bipartite}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Indicator Functions for Network Properties }
\description{
  Various indicators for properties of \code{network} class objects.
}
\usage{
has.loops(x)
is.bipartite(x)
is.directed(x)
is.hyper(x)
is.multiplex(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{network} }
}
\details{
  These methods are the standard means of assessing the state of a \code{network} object; other methods can (and should) use these routines in governing their own behavior.  As such, improper setting of the associated attributes may result in unpleasantly creative results.  (See the \code{edge.check} argument to \code{\link{add.edges}} for an example of code which makes use of these network properties.)

  The functions themselves behave has follows:
  
  \code{has.loops} returns \code{TRUE} iff \code{x} is allowed to contain loops (or loop-like edges, in the hypergraphic case).
  
  \code{is.bipartite} returns \code{TRUE} iff the \code{x} has been explicitly bipartite-coded. Values of \code{bipartite=NULL}, and \code{bipartite=FALSE} will evaluate to \code{FALSE}, numeric values of \code{bipartite>=0} will evaluate to \code{TRUE}. (The value \code{bipartite==0} indicates that it is a bipartite network with a zero-sized first partition.) Note that \code{is.bipartite} refers only to the storage properties of \code{x} and how it should be treated by some algorithms; \code{is.bipartite(x)==FALSE} it does \emph{not} mean that \code{x} cannot admit a bipartition!

  \code{is.directed} returns \code{TRUE} iff the edges of \code{x} are to be interpreted as directed.

  \code{is.hyper} returns \code{TRUE} iff \code{x} is allowed to contain hypergraphic edges.

  \code{is.multiplex} returns \code{TRUE} iff \code{x} is allowed to contain multiplex edges.

}
\value{
  \code{TRUE} or \code{FALSE}
}
\references{ Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  \url{http://www.jstatsoft.org/v24/i02/}
}
\author{ Carter T. Butts \email{buttsc@uci.edu} }
%\note{ ~~further notes~~ }
%
% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{network}}, \code{\link{get.network.attribute}}, \code{set.network.attribute}, \code{\link{add.edges}} }
\examples{
g<-network.initialize(5)    #Initialize the network
is.bipartite(g)
is.directed(g)
is.hyper(g)
is.multiplex(g)
has.loops(g)
}
\keyword{ classes }% at least one, from doc/KEYWORDS
\keyword{ graphs }% __ONLY ONE__ keyword per line
