\name{as.matrix.network}
\alias{as.matrix.network}
\alias{as.matrix.network.adjacency}
\alias{as.matrix.network.incidence}
\alias{as.matrix.network.edgelist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Coerce a Network Object to Matrix Form }
\description{
  The \code{as.matrix} methods attempt to coerce their input to a matrix in adjacency, incidence, or edgelist form.  Edge values (from a stored attribute) may be used if present.
}
\usage{
\method{as.matrix}{network}(x, matrix.type = NULL, attrname = NULL, ...)
\method{as.matrix.network}{adjacency}(x,attrname=NULL, ...)
\method{as.matrix.network}{edgelist}(x,attrname=NULL, ...)
\method{as.matrix.network}{incidence}(x,attrname=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{network} }
  \item{matrix.type}{ one of \code{"adjacency"}, \code{"incidence"}, \code{"edgelist"}, or \code{NULL} }
  \item{attrname}{ optionally, the name of an edge attribute to use for edge values }
  \item{...}{ additional arguments. }
}
\details{
  If no matrix type is specified, \code{\link{which.matrix.type}} will be used to make an educated guess based on the shape of \code{x}.  Where edge values are not specified, a dichotomous matrix will be assumed.
  
  Note that adjacency matrices may also be obtained using the extraction operator.  See the relevant man page for details.
}
\value{
An adjacency, incidence, or edgelist matrix
}
\references{ Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  \url{http://www.jstatsoft.org/v24/i02/}
}
\author{ Carter T. Butts \email{buttsc@uci.edu} and David Hunter \email{dhunter@stat.psu.edu} }
%\note{ ~~further notes~~ }
%
% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{which.matrix.type}}, \code{\link{network}}, \code{\link{network.extraction}}}
\examples{
#Create a random network
m <- matrix(rbinom(25,1,0.5),5,5)
diag(m) <- 0
g <- network(m)

#Coerce to matrix form
as.matrix.network(g,matrix.type="adjacency")
as.matrix.network(g,matrix.type="incidence")
as.matrix.network(g,matrix.type="edgelist")

#Can also use the extraction operator
g[,]                            #Get entire adjacency matrix
g[1:5,6:10]                     #Obtain a submatrix
}
\keyword{ classes }% at least one, from doc/KEYWORDS
\keyword{ graphs }% __ONLY ONE__ keyword per line
