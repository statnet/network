\name{missing.edges}
\alias{network.naedgecount}
\alias{is.na.network}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Identifying and Counting Missing Edges in a Network Object }
\description{
  \code{network.naedgecount} returns the number of edges within a \code{network} object which are flagged as missing.  The \code{is.na} network method returns a new network containing the missing edges.
}
\usage{
\method{is.na}{network}(x)
network.naedgecount(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{network} }
}
\details{
  The missingness of an edge is controlled by its \code{na} attribute (which is mandatory for all edges); \code{network.naedgecount} returns the number of edges for which \code{na==TRUE}.  The \code{is.na} network method produces a new network object whose edges correspond to the missing (\code{na==TRUE}) edges of the original object, and is thus a covenient method of extracting detailed missingness information on the entire network.  The network returned by \code{is.na} is guaranteed to have the same base network attributes (directedness, loopness, hypergraphicity, multiplexity, and bipartite constraint) as the original network object, but no other information is copied; note too that edge IDs are \emph{not} preserved by this process (although adjacency obviously is).  Since the resulting object is a \code{\link{network}}, standard coercion, print/summary, and other methods can be applied to it in the usual fashion.
  
  It should be borne in mind that \dQuote{missingness} in the sense used here reflects the assertion that an edge's presence or absence is unknown, \emph{not} that said edge is known not to be present.  Thus, the \code{na} count for an empty graph is properly 0, since all edges are known to be absent.  Edges can be flagged as missing by setting their \code{na} attribute to \code{TRUE} using \code{\link{set.edge.attribute}}, or by appropriate use of the network assignment operators; see below for an example of the latter.
}
\value{
  The number of missing edges
}
\references{ Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  \url{http://www.jstatsoft.org/v24/i02/}
}
\author{ Carter T. Butts \email{buttsc@uci.edu} }
%\note{ ~~further notes~~ }
%
% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{network.edgecount}}, \code{\link{get.network.attribute}}, \code{is.adjacent}, \code{\link{is.na}}}
\examples{
#Create an empty network with no missing data
g<-network.initialize(5)
g[,]                        #No edges present....
network.naedgecount(g)==0   #Edges not present are not "missing"!

#Now, add some missing edges
g[1,,add.edges=TRUE]<-NA    #Establish that 1's ties are unknown
g[,]                        #Observe the missing elements
is.na(g)                    #Observe in network form
network.naedgecount(g)==4   #These elements do count!
network.edgecount(is.na(g)) #Same as above


}
\keyword{ classes }% at least one, from doc/KEYWORDS
\keyword{ graphs }% __ONLY ONE__ keyword per line
