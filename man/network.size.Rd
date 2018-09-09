\name{network.size}
\alias{network.size}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Return the Size of a Network }
\description{
  \code{network.size} returns the order of its argument (i.e., number of vertices). 
}
\usage{
network.size(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{network} }
  \item{\dots}{additional arguments, not used}
}
\details{
  \code{network.size(x)} is equivalent to \code{get.network.attribute(x,"n")}; the function exists as a convenience.
}
\value{
  The network size
}
\references{ Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  \url{http://www.jstatsoft.org/v24/i02/}
}
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
