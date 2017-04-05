\name{which.matrix.type}
\alias{which.matrix.type}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Heuristic Determination of Matrix Types for Network Storage }
\description{
\code{which.matrix.type} attempts to choose an appropriate matrix expression for a \code{network} object, or (if its argument is a matrix) attempts to determine whether the matrix is of type adjacency, incidence, or edgelist.
}
\usage{
which.matrix.type(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a matrix, or an object of class \code{network} }
}
\details{
  The heuristics used to determine matrix types are fairly arbitrary, and should be avoided where possible.  This function is intended to provide a modestly intelligent fallback option when explicit identification by the user is not possible.
}
\value{
  One of \code{"adjacency"}, \code{"incidence"}, or \code{"edgelist"}
}
\references{ Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  \url{http://www.jstatsoft.org/v24/i02/}
}
\author{ David Hunter \email{dhunter@stat.psu.edu} }
%\note{ ~~further notes~~ }

\seealso{ \code{\link{as.matrix.network}}, \code{\link{as.network.matrix}} }
\examples{
  #Create an arbitrary adjacency matrix
  m<-matrix(rbinom(25,1,0.5),5,5)
  diag(m)<-0

  #Can we guess the type?
  which.matrix.type(m)

  #Try the same thing with a network
  g<-network(m)
  which.matrix.type(g)
  which.matrix.type(as.matrix.network(g,matrix.type="incidence"))
  which.matrix.type(as.matrix.network(g,matrix.type="edgelist"))
}
\keyword{graphs}% at least one, from doc/KEYWORDS
