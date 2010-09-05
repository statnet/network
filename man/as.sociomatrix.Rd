\name{as.sociomatrix}
\alias{as.sociomatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Coerce One or More Networks to Sociomatrix Form }
\description{
  \code{as.sociomatrix} takes adjacency matrices, adjacency arrays, \code{\link{network}} objects, or lists thereof, and returns one or more sociomatrices (adjacency matrices) as appropriate.  This routine provides a useful input-agnostic front-end to functions which process adjacency matrices.
}
\usage{
as.sociomatrix(x, attrname = NULL, simplify = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an adjacency matrix, array, \code{\link{network}} object, or list thereof. }
  \item{attrname}{ optionally, the name of a network attribute to use for extracting edge values (if \code{x} is a \code{\link{network}} object). }
  \item{simplify}{ logical; should \code{as.sociomatrix} attempt to combine its inputs into an adjacency array (\code{TRUE}), or return them as separate list elements (\code{FALSE})? }
  \item{...}{ additional arguments for the coercion routine.}
}
\details{
  \code{as.sociomatrix} provides a more general means of coercing input into adjacency matrix form than \code{\link{as.matrix.network}}. In particular, \code{as.sociomatrix} will attempt to coerce all input networks into the appropriate form, and return the resulting matrices in a regularized manner.  If \code{simplify==TRUE}, \code{as.sociomatrix} attempts to return the matrices as a single adjacency array.  If the input networks are of variable size, or if \code{simplify==FALSE}, the networks in question are returned as a list of matrices.  In any event, a single input network is always returned as a lone matrix.

If \code{attrname} is given, the specified edge attribute is used to extract edge values from any \code{\link{network}} objects contained in \code{x}.  Note that the same attribute will be used for all networks; if no attribute is specified, the standard dichotomous default will be used instead.
}
\value{
  One or more adjacency matrices.  If all matrices are of the same dimension and \code{simplify==TRUE}, the matrices are joined into a single array; otherwise, the return value is a list of single adjacency matrices.
}
\references{ ~put references to the literature/web site here ~ }
\author{ Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  \url{http://www.jstatsoft.org/v24/i02/}
}
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{as.matrix.network}}, \code{\link{network}} }
\examples{
#Generate an adjacency array
g<-array(rbinom(100,1,0.5),dim=c(4,5,5))

#Generate a network object
net<-network(matrix(rbinom(36,1,0.5),6,6))

#Coerce to adjacency matrix form using as.sociomatrix
as.sociomatrix(g,simplify=TRUE)   #Returns as-is
as.sociomatrix(g,simplify=FALSE)  #Returns as list
as.sociomatrix(net)               #Coerces to matrix
as.sociomatrix(list(net,g))       #Returns as list of matrices
}
\keyword{ graphs }% at least one, from doc/KEYWORDS
\keyword{ manip }% __ONLY ONE__ keyword per line
