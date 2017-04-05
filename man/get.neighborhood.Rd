\name{get.neighborhood}
\alias{get.neighborhood}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Obtain the Neighborhood of a Given Vertex }
\description{
  \code{get.neighborhood} returns the IDs of all vertices belonging to the in, out, or combined neighborhoods of \code{v} within network \code{x}.
}
\usage{
get.neighborhood(x, v, type = c("out", "in", "combined"),
    na.omit=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{network} }
  \item{v}{ a vertex ID }
  \item{type}{ the neighborhood to be computed }
  \item{na.omit}{ logical; should missing edges be ignored when obtaining vertex neighborhoods? }
}
\details{
Note that the combined neighborhood is the union of the in and out neighborhoods -- as such, no vertex will appear twice.
}
\value{
  A vector containing the vertex IDs for the chosen neighborhood.
}
\references{ Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  \url{http://www.jstatsoft.org/v24/i02/}

Wasserman, S. and Faust, K.  1994.  \emph{Social Network Analysis: Methods and Applications.}  Cambridge: Cambridge University Press. }
\author{ Carter T. Butts \email{buttsc@uci.edu} }
%\note{ ~~further notes~~ }
%
% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{get.edges}}, \code{\link{is.adjacent}} }
\examples{
#Create a network with three edges
m<-matrix(0,3,3)
m[1,2]<-1; m[2,3]<-1; m[3,1]<-1
g<-network(m)

#Examine the neighborhood of vertex 1
get.neighborhood(g,1,"out")
get.neighborhood(g,1,"in")
get.neighborhood(g,1,"combined")
}
\keyword{ graphs }% at least one, from doc/KEYWORDS
