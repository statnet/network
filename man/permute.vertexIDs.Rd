\name{permute.vertexIDs}
\alias{permute.vertexIDs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Permute (Relabel) the Vertices Within a Network }
\description{
  \code{permute.vertexIDs} permutes the vertices within a given network in the specified fashion.  Since this occurs internally (at the level of vertex IDs), it is rarely of interest to end-users.
}
\usage{
permute.vertexIDs(x, vids)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{\link{network}}. }
  \item{vids}{ a vector of vertex IDs, in the order to which they are to be permuted. }
}
\details{
  \code{permute.vertexIDs} alters the internal ordering of vertices within a \code{\link{network}}.  For most practical applications, this should not be necessary -- de facto permutation can be accomplished by altering the appropriate vertex attributes.  \code{permute.vertexIDs} is needed for certain other routines (such as \code{\link{delete.vertices}}), where it is used in various arcane and ineffable ways.
}
\value{
  Invisibly, a pointer to the permuted network.  \code{permute.vertexIDs} modifies its argument in place.
}
\references{Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  \url{http://www.jstatsoft.org/v24/i02/}
}
\author{ Carter T. Butts \email{buttsc@uci.edu} }
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{network}} }
\examples{
data(flo)                     #Load the Florentine Families data
nflo<-network(flo)                      #Create a network object
n<-network.size(nflo)                #Get the number of vertices
permute.vertexIDs(nflo,n:1)                #Reverse the vertices
all(flo[n:1,n:1]==as.sociomatrix(nflo))          #Should be TRUE
}
\keyword{ manip }% at least one, from doc/KEYWORDS
\keyword{ graphs }% __ONLY ONE__ keyword per line
