\name{network.edgecount}
\alias{network.edgecount}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Return the Number of Edges in a Network Object }
\description{
  \code{network.edgecount} returns the number of edges within a \code{network}, removing those flagged as missing if desired.
}
\usage{
network.edgecount(x, na.omit = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{network} }
  \item{na.omit}{ logical; omit edges with \code{na==TRUE} from the count? }
}
\details{
  The return value is the number of distinct edges within the network object, including multiplex edges as appropriate.  (So if there are 3 edges from vertex i to vertex j, each contributes to the total edge count.)

  The return value \code{network.edgecount} is in the present implementation related to the (required) \code{mnext} network attribute.  \code{mnext} is an internal legacy attribute that currently indicates the index number of the next edge to be added to a network object.  (Do not modify it unless you enjoy unfortunate surprises.)  The number of edges returned by \code{network.edgecount} is equal to \code{x\%n\%"mnext"-1}, minus the number of \code{NULL} edges (and missing edges, if \code{na.omit==TRUE}).  Note that \code{g\%n\%"mnext"-1} cannot, by itself, be counted upon to be an accurate count of the number of edges!  As \code{mnext} is not part of the API (and is not guaranteed to remain), users and developers are urged to use \code{network.edgecount} instead.
}
\value{
  The number of edges
}
\references{ Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  \url{http://www.jstatsoft.org/v24/i02/}
}
\author{ Carter T. Butts \email{buttsc@uci.edu} }
%\note{ ~~further notes~~ }
%
\section{Warning }{\code{network.edgecount} uses the real state of the network object to count edges, not the state it hypothetically should have.  Thus, if you add extra edges to a non-multiplex network, directed edges to an undirected network, etc., the actual number of edges in the object will be returned (and not the number you would expect if you relied only on the putative number of possible edges as reflected by the \link{network.indicators}).  Don't create \code{network} objects with contradictory attributes unless you know what you are doing. }

\seealso{ \code{\link{get.network.attribute}} }
\examples{
#Create a network with three edges
m<-matrix(0,3,3)
m[1,2]<-1; m[2,3]<-1; m[3,1]<-1
g<-network(m)
network.edgecount(g)==3   #Verify the edgecount
}
\keyword{ classes }% at least one, from doc/KEYWORDS
\keyword{ graphs }% __ONLY ONE__ keyword per line
