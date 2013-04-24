\name{network.vertex}
\alias{network.vertex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Add Vertices to a Plot }
\description{
  \code{network.vertex} adds one or more vertices (drawn using \code{\link{polygon}}) to a plot.
}
\usage{
network.vertex(x, y, radius = 1, sides = 4, border = 1, col = 2, 
    lty = NULL, rot = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a vector of x coordinates. }
  \item{y}{ a vector of y coordinates. }
  \item{radius}{ a vector of vertex radii. }
  \item{sides}{ a vector containing the number of sides to draw for each vertex. }
  \item{border}{ a vector of vertex border colors. }
  \item{col}{ a vector of vertex interior colors. }
  \item{lty}{ a vector of vertex border line types. }
  \item{rot}{ a vector of vertex rotation angles (in degrees). }
  \item{\dots}{ Additional arguments to \code{\link{polygon}} }
}
\details{
  \code{network.vertex} draws regular polygons of specified radius and number of sides, at the given coordinates.  This is useful for routines such as \code{\link{plot.network}}, which use such shapes to depict vertices.
}
\value{
  None
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Carter T. Butts \email{buttsc@uci.edu} }
\note{ \code{network.vertex} is a direct adaptation of \code{\link[sna]{gplot.vertex}} from the \code{sna} package. }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{plot.network}}, \code{\link{polygon}} }
\examples{

#Open a plot window, and place some vertices
plot(0,0,type="n",xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),asp=1)
network.vertex(cos((1:10)/10*2*pi),sin((1:10)/10*2*pi),col=1:10,
    sides=3:12,radius=0.1)

}
\keyword{ aplot }% at least one, from doc/KEYWORDS
\keyword{ graphs }% __ONLY ONE__ keyword per line
