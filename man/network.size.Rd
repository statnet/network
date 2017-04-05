\name{network.size}
\alias{network.size}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Return the Size of a Network }
\description{
  \code{network.size} returns the order of its argument (i.e., number of vertices). 
}
\usage{
network.size(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{network} }
}
\details{
  \code{network.size(x)} is equivalent to \code{get.network.attribute(x,"n")}; the function exists as a convenience.
}
\value{
  The network size
}
\references{ Butts, C.T.  2002.  ``Memory Structures for Relational Data in R: Classes and Interfaces''  Working Paper. }
\author{ Carter T. Butts \email{buttsc@uci.edu} }
%\note{ ~~further notes~~ }
%
% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{get.network.attribute}} }
\examples{
#Initialize a network
g<-network.initialize(7)
network.size(g)
}
\keyword{ classes }% at least one, from doc/KEYWORDS
\keyword{ graphs }% __ONLY ONE__ keyword per line
