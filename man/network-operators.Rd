\name{network.operators}
\alias{network.operators}
\alias{+.network}
\alias{-.network}
\alias{*.network}
\alias{!.network}
\alias{|.network}
\alias{&.network}
\alias{\%c\%}
\alias{\%c\%.network}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Network Operators }
\description{
 These operators allow for algebraic manipulation of relational structures.
}
\usage{
\method{+}{network}(e1, e2)
\method{-}{network}(e1, e2)
\method{*}{network}(e1, e2)
\method{\%c\%}{network}(e1, e2)
! e1
\method{|}{network}(e1, e2)
\method{&}{network}(e1, e2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{e1}{ an object of class \code{network}. }
  \item{e2}{ another \code{network}. }
}
\details{
  A more useful \dQuote{usage} statement than the above (but prohibited by R's overzealous documentation restrictions from appearing there) is as follows:
  
\preformatted{
x + y
x - y
x * y
x \%c\% y

!x
x | y
x & y
}

  In general, the binary network operators currently function by converting their arguments to adjacency matrices, carrying out the specified operation on those matrices, and then returning the result in network form.  This is rather inefficient, and results in the loss of other attributes; such poor behavior is not guaranteed to be maintained in future versions of the \code{network} package.
  
  Apart from the above, the specific operations carried out by these operators are mostly self-explanatory.  One exception to this is \code{x \%c\% y}, which returns the network formed from the composition of graphs \code{x} and \code{y} (respectively).  (Note that this may contain loops, whether or not the original networks allowed them.)

  Slightly different behavior is exhibited by the unary operator, \code{!}, which returns the complement of its argument.  The graph which is returned contains all attributes of the original, save for edge attributes (as none of the original edges are retained).  Note that the complement of a large, sparse graph has \emph{many} edges, with concomitant memory consumption.  When working with such graphs, consider whether some other mechanism (e.g., adjacency checking on the original graph) might prove more efficient.
}
\value{
   The resulting graph object.
}
\references{ Wasserman, S. and Faust, K.  (1994).  \emph{Social Network Analysis: Methods and Applications.}  Cambridge: University of Cambridge Press. }
\author{ Carter T. Butts \email{buttsc@uci.edu} }
\note{ Currently, there is a naming conflict between the composition operator and the \code{\link[sna]{\%c\%}} operator in the \code{\link[sna]{sna}} package.  This will be resolved in future releases; for the time being, one can determine which version of \code{\%c\%} is in use by varying which package is loaded first.}

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{network.extraction}} }
\examples{
#Create an in-star
m<-matrix(0,6,6)
m[2:6,1]<-1
g<-network(m)
plot(g)

#Compose g with its transpose
gcgt<-g \%c\% (network(t(m)))
plot(gcgt)
gcgt

#Show the complement of g
!g

#Perform various arithmatic and logical operations
(g+gcgt)[,] == (g|gcgt)[,]             #All TRUE
(g-gcgt)[,] == ((g|gcgt)&(!(g&gcgt)))[,]
(g*gcgt)[,] == (g&gcgt)[,]
}
\keyword{ math }% at least one, from doc/KEYWORDS
\keyword{ graphs }% __ONLY ONE__ keyword per line
