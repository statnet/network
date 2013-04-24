\name{network}
\alias{network}
\alias{as.network}
\alias{as.network.network}
\alias{is.network}
\alias{print.network}
\alias{summary.network}
\alias{network.copy}
\alias{print.summary.network}
\alias{$<-.network}
\alias{<-.network}
\title{Network Objects}
\description{
  Construct, coerce to, test for and print \code{network} objects.
}
\usage{
network(x, vertex.attr=NULL, vertex.attrnames=NULL, directed=TRUE, 
     hyper=FALSE, loops=FALSE, multiple=FALSE, bipartite = FALSE, ...)
network.copy(x)
as.network(x, ...)
is.network(x)
\method{print}{network}(x, matrix.type = which.matrix.type(x),
    mixingmatrices = FALSE, na.omit = TRUE, print.adj = FALSE, ...)
\method{summary}{network}(object, na.omit=TRUE, mixingmatrices=FALSE, 
    print.adj = TRUE, ...)
}
\arguments{
  \item{x}{for \code{network}, a matrix giving the network structure
    in adjacency, incidence, or edgelist form; otherwise, an
    object of class \code{network}.}
  \item{vertex.attr}{optionally, a list containing vertex attributes.}
  \item{vertex.attrnames}{optionally, a list containing vertex attribute names.}
  \item{directed}{logical; should edges be interpreted as directed?}
  \item{hyper}{logical; are hyperedges allowed?}
  \item{loops}{logical; should loops be allowed?}
  \item{multiple}{logical; are multiplex edges allowed?}
  \item{bipartite}{count; should the network be interpreted as bipartite? If present (i.e., non-NULL) it is the count of the number of actors in the bipartite network. In this case, the number of nodes is equal to the number of actors plus the number of events (with all actors preceeding all events). The edges are then interpreted as nondirected.}
  \item{matrix.type}{one of \code{"adjacency"}, \code{"edgelist"}, \code{"incidence"}. See \code{\link{edgeset.constructors}} for details and optional additional arguments}
  \item{object}{an object of class \code{network}.}
  \item{na.omit}{logical; omit summarization of missing attributes in \code{network}?}
  \item{mixingmatrices}{logical; print the mixing matrices for the discrete attributes?}
  \item{print.adj}{logical; print the network adjacency structure?}
  \item{...}{additional arguments.}
}
\details{
  \code{network} constructs a \code{network} class object from a matrix representation. It is strongly recommended that you specify the \code{matrix.type} parameter, to avoid ambiguity in the type of input matrix. See the documentation of \code{edgeset.constructors} for the format of these input matrices.

  \code{network.copy} creates a new \code{network} object which duplicates its supplied argument.  (Direct assignment with \code{<-} should be used rather than \code{network.copy} in most cases.)

  \code{as.network} tries to coerce its argument to a network, using the \code{network} function if necessary.

  \code{is.network} tests whether its argument is a network (in the sense that it has class \code{network}).

  \code{print.network} prints a network object in one of several possible formats.  It also prints the list of global attributes of the network. 

  \code{summary.network} provides similar information.
}
\value{
\code{network}, \code{as.network}, and \code{print.network} all return a network class object; \code{is.network} returns TRUE or FALSE.
}
\references{ Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  \url{http://www.jstatsoft.org/v24/i02/}

}
\author{ Carter T. Butts \email{buttsc@uci.edu} and David Hunter \email{dhunter@stat.psu.edu} }
\note{
Between versions 0.5 and 1.2, direct assignment of a network object created a pointer to the original object, rather than a copy.  As of version 1.2, direct assignment behaves in the same manner as \code{network.copy}.  Direct use of the latter is thus superfluous in most situations, and is discouraged.
}
\seealso{\code{\link{network.initialize}}, \code{\link{attribute.methods}}, \code{\link{as.network.matrix}}, \code{\link{as.matrix.network}}, \code{\link{deletion.methods}}, \code{\link{edgeset.constructors}}, \code{\link{network.indicators}}, \code{\link{plot.network}}}
\examples{
m <- matrix(rbinom(25,1,.4),5,5)
diag(m) <- 0
g <- network(m, directed=FALSE)
summary(g)

h <- network.copy(g)       #Note: same as h<-g
summary(h)
}
\keyword{classes}
\keyword{graphs}
