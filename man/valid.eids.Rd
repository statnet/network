\name{valid.eids}
\alias{valid.eids}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Get the valid edge which are valid in a network
}
\description{
  Returns a vector of valid edge ids (corresponding to non-NULL edges) for a network that may have some delete edges.
}
\usage{
valid.eids(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 a network object, possibly with some deleted edges. 
}
}
\details{
The edge ids used in the network package are positional indices on the internal "mel" list. When edges are removed using \code{\link{delete.edges}} \code{NULL} elements are left on the list.  The function \code{valid.eids} returns the ids of all the valid (non-null) edge ids for its \code{network} argument. 
}
\value{
a vector of integer ids corresponding to the non-null edges in x
}

\author{
skyebend
}
\note{
 If it is known that x has no deleted edges, \code{seq_along(x$mel)} is a faster way to generate the sequence of possible edge ids.
}


\seealso{
See also \code{\link{delete.edges}}
}
\examples{
net<-network.initialize(100)
add.edges(net,1:99,2:100)
delete.edges(net,eid=5:95)
# get the ids of the non-deleted edges
valid.eids(net)
}

