\name{network.initialize}
\alias{network.initialize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Initialize a Network Class Object }
\description{
  Create and initialize a \code{network} object with \code{n} vertices.
}
\usage{
network.initialize(n, directed = TRUE, hyper = FALSE, loops = FALSE, 
    multiple = FALSE, bipartite = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ the number of vertices to initialize }
  \item{directed}{logical; should edges be interpreted as directed?}
  \item{hyper}{logical; are hyperedges allowed?}
  \item{loops}{logical; should loops be allowed?}
  \item{multiple}{logical; are multiplex edges allowed?}
  \item{bipartite}{count; should the network be interpreted as bipartite? If present (i.e., non-NULL) it is the count of the number of actors in the bipartite network. In this case, the number of nodes is equal to the number of actors plus the number of events (with all actors preceeding all events). The edges are then interpreted as nondirected.}
}
\details{
  Generally, \code{network.initialize} is called by other constructor functions as part of the process of creating a network.
}
\value{
  An object of class \code{network}
}
\references{ Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  \url{http://www.jstatsoft.org/v24/i02/}
}
\author{ Carter T. Butts \email{buttsc@uci.edu} }
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{network}}, \code{\link{as.network.matrix}} }
\examples{
g<-network.initialize(5)  #Create an empty graph on 5 vertices
}
\keyword{ classes }% at least one, from doc/KEYWORDS
\keyword{ graphs }% __ONLY ONE__ keyword per line
