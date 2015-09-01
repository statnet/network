\name{has.edges}
\alias{has.edges}
\alias{is.isolate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determine if specified vertices of a network have any edges (are not isolates)
}
\description{
Returns a logical value for each specified vertex, indicating if it has any incident (in or out) edges.  Checks all vertices by default
}
\usage{
has.edges(net, v = seq_len(network.size(net)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{
 a \code{\link{network}} object to be queried
}
  \item{v}{
  integer vector of vertex ids to check
}
}

\value{
returns a logical vector with the same length as v, with TRUE if the vertex is involved in any edges, FALSE if it is an isolate. 
}

\author{
skyebend
}

\examples{
test<-network.initialize(5)
test[1,2]<-1
has.edges(test)
has.edges(test,v=5)
}

