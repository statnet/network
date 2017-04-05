\name{is.bipartite}
\alias{is.bipartite}
\title{Test to see is the network can be interpreted as bipartite}
\description{
  Tests to see is the \code{network} object can be interpreted as a
  bipartite network.
}
\usage{
is.bipartite(x)
}
\arguments{
  \item{x}{for \code{network}, a matrix giving the network structure
    in adjacency, incidence, or edgelist form; otherwise, an
    object of class \code{network}.}
}
\value{
  a logical FALSE if the \code{bipartite} attribute of the \code{network} object is not present. Otherwise the count of the number of actors in the bipartite network. In this case, the number of nodes is equal to the number of actors plus the number of events (with all actors preceeding all events). The edges are then interpreted as nondirected.}
}
\references{ Butts, C.T.  2002.  ``Memory Structures for Relational Data in R: Classes and Interfaces''  Working Paper. }
\author{ Carter T. Butts \email{buttsc@uci.edu} and David Hunter \email{dhunter@stat.psu.edu} }
\seealso{\code{\link{network.initialize}}, \code{\link{attribute.methods}}, \code{\link{as.network.matrix}}, \code{\link{as.matrix.network}}, \code{\link{deletion.methods}}, \code{\link{edgeset.constructors}}, \code{\link{network.indicators}}, \code{\link{plot.network}}}
%\examples{
%m <- matrix(rbinom(25,1,.4),5,5)
%diag(m) <- 0
%g <- network(m, directed=FALSE)
%}
\keyword{classes}
\keyword{graphs}
