\name{is.adjacent}
\alias{is.adjacent}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Determine Whether Two Vertices Are Adjacent }
\description{
  \code{is.adjacent} returns \code{TRUE} iff \code{vi} is adjacent to \code{vj} in \code{x}.  Missing edges may be omitted or not, as per \code{na.omit}.
}
\usage{
is.adjacent(x, vi, vj, na.omit = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{network} }
  \item{vi}{ a vertex ID }
  \item{vj}{ a second vertex ID }
  \item{na.omit}{ logical; should missing edges count when assessing adjacency? }
}
\details{
  Vertex \eqn{v} is said to be adjacent to vertex \eqn{v'} within directed network \eqn{G} iff there exists some edge whose tail set contains \eqn{v} and whose head set contains \eqn{v'}.  In the undirected case, head and tail sets are exchangeable, and thus \eqn{v} is adjacent to \eqn{v'} if there exists an edge such that \eqn{v} belongs to one endpoint set and \eqn{v'} belongs to the other.  (In dyadic graphs, these sets are of cardinality 1, but this may not be the case where hyperedges are admitted.)

  Adjacency can also be determined via the extraction/replacement operators.  See the associated man page for details.
}
\value{
  A logical, giving the status of the (i,j) edge
}
\references{ Wasserman, S. and Faust, K.  1994.  \emph{Social Network Analysis: Methods and Applications}.  Cambridge: Cambridge University Press. }
\author{ Carter T. Butts \email{buttsc@uci.edu} }
%\note{ ~~further notes~~ }
%
% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{get.neighborhood}}, \code{\link{network.extraction}}}
\examples{
#Create a very simple graph
g<-network.initialize(3)
add.edge(g,1,2)
is.adjacent(g,1,2)  #TRUE
is.adjacent(g,2,1)  #FALSE
g[1,2]==1           #TRUE
g[2,1]==1           #FALSE
}
\keyword{ graphs }% at least one, from doc/KEYWORDS
