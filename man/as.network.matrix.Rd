\name{as.network.matrix}
\alias{as.network.matrix}
\alias{as.network.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Coercion from Matrices to Network Objects }
\description{
  \code{as.network.matrix} attempts to coerce its first argument to an object of class \code{network}.
}
\usage{
\method{as.network}{default}(x, ...)
\method{as.network}{matrix}(x, matrix.type = NULL, directed = TRUE, 
    hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = FALSE,
    ignore.eval = TRUE, names.eval = NULL, na.rm = FALSE, 
    edge.check = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a matrix containing an adjacency structure }
  \item{matrix.type}{ one of \code{"adjacency"}, \code{"edgelist"}, \code{"incidence"}, or \code{NULL} }
  \item{directed}{logical; should edges be interpreted as directed?}
  \item{hyper}{logical; are hyperedges allowed?}
  \item{loops}{logical; should loops be allowed?}
  \item{multiple}{logical; are multiplex edges allowed?}
  \item{bipartite}{count; should the network be interpreted as bipartite? If present (i.e., non-NULL) it is the count of the number of actors in the bipartite network. In this case, the number of nodes is equal to the number of actors plus the number of events (with all actors preceding all events). The edges are then interpreted as nondirected.}
  \item{ignore.eval}{ logical; ignore edge values? }
  \item{names.eval}{ optionally, the name of the attribute in which edge values should be stored }
  \item{na.rm}{ logical; ignore missing entries when constructing the network? }
  \item{edge.check}{ logical; perform consistency checks on new edges? }
  \item{...}{additional arguments}
}
\details{
  Depending on \code{matrix.type}, one of three edgeset constructor methods will be employed to read the input matrix (see \code{\link{edgeset.constructors}}).  If \code{matrix.type==NULL}, \code{\link{which.matrix.type}} will be used to guess the appropriate matrix type.
  
  The coercion methods will recognize and attempt to utilize the \code{sna} extended matrix attributes where feasible.  These are as follows:
  \itemize{
    \item{\code{"n"}: taken to indicate number of vertices in the network.}
    \item{\code{"bipartite"}: taken to indicate the network's \code{bipartite} attribute, where present.}
    \item{\code{"vnames"}: taken to contain vertex names, where present.}
  }
  These attributes are generally used with edgelists, and indeed data in \code{sna} edgelist format should be transparently converted in most cases.  Where the extended matrix attributes are in conflict with the actual contents of \code{x}, results are no guaranteed (but the latter will usually override the former).
}
\value{
  An object of class \code{network}
}
\references{ Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  \url{http://www.jstatsoft.org/v24/i02/}
}
\author{ Carter T. Butts \email{buttsc@uci.edu} and David Hunter \email{dhunter@stat.psu.edu} }
%\note{ ~~further notes~~ }
%
% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{edgeset.constructors}}, \code{\link{network}}, \code{\link{which.matrix.type}} }
\examples{
#Draw a random matrix
m<-matrix(rbinom(25,1,0.5),5)
diag(m)<-0

#Coerce to network form
g<-as.network.matrix(m,matrix.type="adjacency")
}
\keyword{ classes }% at least one, from doc/KEYWORDS
\keyword{ graphs }% __ONLY ONE__ keyword per line
