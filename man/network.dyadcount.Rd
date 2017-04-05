\name{network.dyadcount}
\alias{network.dyadcount}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Return the Number of (Possibly Directed) Dyads in a Network Object }
\description{
  \code{network.dyadcount} returns the number of possible dyads within a \code{network}, removing those flagged as missing if desired.  If the network is directed, directed dyads are counted accordingly.
}
\usage{
network.dyadcount(x, na.omit = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{network} }
  \item{na.omit}{ logical; omit edges with \code{na==TRUE} from the count? }
}
\details{
  The return value \code{network.dyadcount} is equal to the number of dyads, minus the number of \code{NULL} edges (and missing edges, if \code{na.omit==TRUE}).  If \code{x} is directed, the number of directed dyads is returned. If the network allows loops, the number of possible entries on the diagnonal is added.   Allthough the function does not give an error on multiplex networks or hypergraphs, the results probably don't make sense.
}
\value{
  The number of dyads in the network
}
\references{ Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  \url{http://www.jstatsoft.org/v24/i02/}
}
\author{ Mark S. Handcock \email{handcock@stat.washington.edu}, skyebend }


\seealso{ \code{\link{get.network.attribute}}, \code{\link{network.edgecount}}, \code{\link{is.directed}} }
\examples{
#Create a directed network with three edges
m<-matrix(0,3,3)
m[1,2]<-1; m[2,3]<-1; m[3,1]<-1
g<-network(m)
network.dyadcount(g)==6                 #Verify the directed dyad count
g<-network(m|t(m),directed=FALSE)
network.dyadcount(g)==3                         #nC2 in undirected case
}
\keyword{ classes }% at least one, from doc/KEYWORDS
\keyword{ graphs }% __ONLY ONE__ keyword per line
