\name{edgeset.constructors}
\alias{edgeset.constructors}
\alias{network.adjacency}
\alias{network.incidence}
\alias{network.edgelist}
\alias{network.bipartite}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Edgeset Constructors for Network Objects }
\description{
  These functions convert relational data in matrix form to network edge sets.
}
\usage{
network.adjacency(x, g, ignore.eval = TRUE, names.eval = NULL, ...)
network.edgelist(x, g, ignore.eval = TRUE, names.eval = NULL, ...)
network.incidence(x, g, ignore.eval = TRUE, names.eval = NULL, ...)
network.bipartite(x, g, ignore.eval = TRUE, names.eval = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a matrix containing edge information }
  \item{g}{ an object of class \code{network} }
  \item{ignore.eval}{ logical; ignore edge values? }
  \item{names.eval}{ the edge attribute under which to store edge values, if any }
  \item{\dots}{ additional arguments to \code{\link{add.edge}} }
}
\details{
  Each of the above functions takes a \code{network} and a matrix as input, and modifies the supplied \code{network} object by adding the appropriate edges.  \code{network.adjacency} takes \code{x} to be an adjacency matrix; \code{code.edgelist} takes \code{x} to be an edgelist matrix; and \code{network.incidence} takes \code{x} to be an incidence matrix.  \code{network.bipartite} takes \code{x} to be a two-mode adjacency matrix where rows and columns reflect each respective mode (conventionally, actors and events); If \code{ignore.eval==FALSE}, (non-zero) edge values are stored as edgewise attributes with name \code{names.eval}.  Any additional command line parameters are passed to \code{\link{add.edge}}.

  Edgelist matrices to be used with \code{network.edgelist} should have one row per edge, with the first two columns indicating the sender and receiver of each edge (respectively).  Edge values may be provided in additional columns.  Incidence matrices should contain one row per vertex, with one column per edge.  In the directed case, negative cell values are taken to indicate tail vertices, while positive values indicate head vertices. 

  Results similar to \code{network.adjacency} can also be obtained by means of extraction/replacement operators.  See the associated man page for details.
}
\value{
 Invisibly, an object of class \code{network}; these functions modify their argument in place.
}
\references{ Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  \url{http://www.jstatsoft.org/v24/i02/}
}
\author{ Carter T. Butts \email{buttsc@uci.edu} and David Hunter \email{dhunter@stat.psu.edu} }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{network}}, \code{\link{network.initialize}}, \code{\link{add.edge}}, \code{\link{network.extraction}}}
\examples{
#Create an arbitrary adjacency matrix
m<-matrix(rbinom(25,1,0.5),5,5)
diag(m)<-0

g<-network.initialize(5)    #Initialize the network
network.adjacency(m,g)      #Import the edge data

#Do the same thing, using replacement operators
g<-network.initialize(5)
g[,]<-m
}
\keyword{ classes }% at least one, from doc/KEYWORDS
\keyword{ graphs }% __ONLY ONE__ keyword per line
