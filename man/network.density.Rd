\name{network.density}
\alias{network.density}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Compute the Density of a Network }
\description{
  \code{network.density} computes the density of its argument.
}
\usage{
network.density(x, na.omit=TRUE, discount.bipartite=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{network} }
  \item{na.omit}{ logical; omit missing edges from extant edges when assessing density?}
  \item{discount.bipartite}{ logical; if \code{x} is bipartite, should \dQuote{forbidden} edges be excluded from the count of potential edges? }
}
\details{
  The density of a network is defined as the ratio of extant edges to potential edges. We do not currently consider edge values; missing edges are omitted from extent (but not potential) edge count when \code{na.omit==TRUE}.
}
\value{
  The network density.
}
\references{ Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  \url{http://www.jstatsoft.org/v24/i02/}

Wasserman, S. and Faust, K.  (1994).  \emph{Social Network Analysis: Methods and Applications.}  Cambridge: Cambridge University Press. }
\author{ Carter T. Butts \email{buttsc@uci.edu} }
%\note{  }
%
\section{Warning }{\code{network.density} relies on network attributes (see \link{network.indicators}) to determine the properties of the underlying network object.  If these are set incorrectly (e.g., multiple edges in a non-multiplex network, network coded with directed edges but set to \dQuote{undirected}, etc.), surprising results may ensue. }

\seealso{ \code{\link{network.edgecount}}, \code{\link{network.size}} }
\examples{
#Create an arbitrary adjacency matrix
m<-matrix(rbinom(25,1,0.5),5,5)
diag(m)<-0

g<-network.initialize(5)    #Initialize the network
network.density(g)          #Calculate the density
}
\keyword{ graphs }% at least one, from doc/KEYWORDS
