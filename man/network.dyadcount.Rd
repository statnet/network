\name{network.dyadcount}
\alias{network.dyadcount}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Return the Number of Edges in a Network Object }
\description{
  \code{network.dyadcount} returns the number of dyads within a \code{network}, removing those flagged as missing if desired.
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
  The return value \code{network.dyadcount} is equal to the number of dyads, minus the number of \code{NULL} edges (and missing edges, if \code{na.omit==TRUE}).
}
\value{
  The number of dyads in the network
}
\references{ Butts, C.T.  2002.  ``Memory Structures for Relational Data in R: Classes and Interfaces''  Working Paper. }
\author{ Mark S. Handcock \email{handcock@stat.washington.edu} }
%\note{ ~~further notes~~ }
%
% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{get.network.attribute}} }
\examples{
#Create a network with three edges
m<-matrix(0,3,3)
m[1,2]<-1; m[2,3]<-1; m[3,1]<-1
g<-network(m)
network.dyadcount(g)==6                         #Verify the dyad count
g<-network(m|t(m),directed=FALSE)
network.dyadcount(g)==3                         #nC2 in undirected case
}
\keyword{ classes }% at least one, from doc/KEYWORDS
\keyword{ graphs }% __ONLY ONE__ keyword per line
