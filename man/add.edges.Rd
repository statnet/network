\name{add.edges}
\alias{add.edges}
\alias{add.edge}
\title{Add Edges to a Network Object}
\description{
  Add one or more edges to an existing network object.
}
\usage{
add.edge(x, tail, head, names.eval=NULL, vals.eval=NULL, 
    edge.check=FALSE, ...)
add.edges(x, tail, head, names.eval=NULL, vals.eval=NULL, ...)
}
\arguments{
  \item{x}{an object of class \code{network}}
  \item{tail}{for \code{add.edge}, a vector of vertex IDs reflecting the tail set for the edge to be added; for \code{add.edges}, a list of such vectors}
  \item{head}{for \code{add.edge}, a vector of vertex IDs reflecting the head set for the edge to be added; for \code{add.edges}, a list of such vectors}
  \item{names.eval}{for \code{add.edge}, an optional list of names for edge attributes; for \code{add.edges}, a list of such lists}
  \item{vals.eval}{for \code{add.edge}, an optional list of edge attribute values (matching \code{names.eval}); for \code{add.edges}, a list of such lists}
  \item{edge.check}{ logical; should we perform (computationally expensive) tests to check for the legality of submitted edges? }
  \item{...}{ additional arguments }
}
\details{
The edge checking procedure is very slow, but should always be employed when debugging; without it, one cannot guarantee that the network state is consistent with network level variables (see \code{\link{network.indicators}}). For example, by default it is possible to add multiple edges to a pair of vertices.

Edges can also be added/removed via the extraction/replacement operators.  See the associated man page for details.
}
\value{
    Invisibly, \code{add.edge} and \code{add.edges} return pointers to their modified arguments; both functions modify their arguments in place..
}
\author{ Carter T. Butts \email{buttsc@uci.edu}}
\references{ Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  \url{http://www.jstatsoft.org/v24/i02/}
}
%\note{ }
\seealso{\code{\link{network}}, \code{\link{add.vertices}}, \code{\link{network.extraction}}, \code{\link{delete.edges}}}
\examples{
#Initialize a small, empty network
g<-network.initialize(3)

#Add an edge
add.edge(g,1,2)
g

#Can also add edges using the extraction/replacement operators
#note that replacement operators are much slower than add.edges()
g[,3]<-1
g[,]
}
\keyword{classes}
\keyword{graphs}
