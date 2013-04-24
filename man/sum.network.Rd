\name{sum.network}
\alias{sum.network}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Combine Networks by Edge Value Addition }
\description{
  Given a series of networks, \code{sum.network} attempts to form a new network by accumulation of edges.  If a non-null \code{attrname} is given, the corresponding edge attribute is used to determine and store edge values.
}
\usage{
\method{sum}{network}(..., attrname = NULL, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{ one or more \code{network} objects. }
  \item{attrname}{ the name of an edge attribute to use when assessing edge values, if desired. }
  \item{na.rm}{ logical; should edges with missing data be ignored? }
}
\details{
  The network summation method attempts to combine its arguments by addition of their respective adjacency matrices; thus, this method is only applicable for networks whose adjacency coercion is well-behaved.  Addition is effectively boolean unless \code{attrname} is specified, in which case this is used to assess edge values -- net values of 0 will result in removal of the underlying edge.

Other network attributes in the return value are carried over from the first element in the list, so some persistence is possible (unlike the addition operator).  Note that it is sometimes possible to \dQuote{add} networks and raw adjacency matrices using this routine (if all dimensions are correct), but more exotic combinations may result in regrettably exciting behavior.
}
\value{
  A \code{\link{network}} object.
}
\references{ Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  \url{http://www.jstatsoft.org/v24/i02/}
}
\author{ Carter T. Butts \email{buttsc@uci.edu} }
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{network.operators}} }
\examples{
#Create some networks
g<-network.initialize(5)
h<-network.initialize(5)
i<-network.initialize(5)
g[1,,names.eval="marsupial",add.edges=TRUE]<-1
h[1:2,,names.eval="marsupial",add.edges=TRUE]<-2
i[1:3,,names.eval="marsupial",add.edges=TRUE]<-3

#Combine by addition
pouch<-sum(g,h,i,attrname="marsupial")
pouch[,]                                   #Edge values in the pouch?
as.sociomatrix(pouch,attrname="marsupial")     #Recover the marsupial
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ arith }
\keyword{ graphs }% __ONLY ONE__ keyword per line
