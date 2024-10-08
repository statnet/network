% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/access.R
\name{network.size}
\alias{network.size}
\title{Return the Size of a Network}
\usage{
network.size(x, ...)
}
\arguments{
\item{x}{an object of class \code{network}}

\item{\dots}{additional arguments, not used}
}
\value{
The network size
}
\description{
\code{network.size} returns the order of its argument (i.e., number of
vertices).
}
\details{
\code{network.size(x)} is equivalent to \code{get.network.attribute(x,"n")};
the function exists as a convenience.
}
\examples{

#Initialize a network
g<-network.initialize(7)
network.size(g)

}
\references{
Butts, C. T.  (2008).  \dQuote{network: a Package for Managing
Relational Data in R.} \emph{Journal of Statistical Software}, 24(2).
\doi{10.18637/jss.v024.i02}
}
\seealso{
\code{\link{get.network.attribute}}
}
\author{
Carter T. Butts \email{buttsc@uci.edu}
}
\keyword{classes}
\keyword{graphs}
