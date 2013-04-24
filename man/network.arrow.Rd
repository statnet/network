\name{network.arrow}
\alias{network.arrow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Add Arrows or Segments to a Plot }
\description{
  \code{network.arrow} draws a segment or arrow between two pairs of points; unlike \code{\link{arrows}} or \code{\link{segments}}, the new plot element is drawn as a polygon.
}
\usage{
network.arrow(x0, y0, x1, y1, length = 0.1, angle = 20, 
    width = 0.01, col = 1, border = 1, lty = 1, offset.head = 0,
    offset.tail = 0, arrowhead = TRUE, curve = 0, edge.steps = 50,
    ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x0}{ A vector of x coordinates for points of origin }
  \item{y0}{ A vector of y coordinates for points of origin }
  \item{x1}{ A vector of x coordinates for destination points }
  \item{y1}{ A vector of y coordinates for destination points }
  \item{length}{ Arrowhead length, in current plotting units }
  \item{angle}{ Arrowhead angle (in degrees) }
  \item{width}{ Width for arrow body, in current plotting units (can be a vector) }
  \item{col}{ Arrow body color (can be a vector) }
  \item{border}{ Arrow border color (can be a vector) }
  \item{lty}{ Arrow border line type (can be a vector) }
  \item{offset.head}{ Offset for destination point (can be a vector) }
  \item{offset.tail}{ Offset for origin point (can be a vector) }
  \item{arrowhead}{ Boolean; should arrowheads be used?  (Can be a vector)) }
  \item{curve}{ Degree of edge curvature (if any), in current plotting units (can be a vector) }
  \item{edge.steps}{ For curved edges, the number of steps to use in approximating the curve (can be a vector)}
  \item{\dots}{ Additional arguments to \code{\link{polygon}} }
}
\details{
  \code{network.arrow} provides a useful extension of \code{\link{segments}} and \code{\link{arrows}} when fine control is needed over the resulting display.  (The results also look better.)  Note that edge curvature is quadratic, with \code{curve} providing the maximum horizontal deviation of the edge (left-handed).  Head/tail offsets are used to adjust the end/start points of an edge, relative to the baseline coordinates; these are useful for functions like \code{\link{plot.network}}, which need to draw edges incident to vertices of varying radii.
}
\value{
  None.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Carter T. Butts \email{buttsc@uci.edu} }
\note{ \code{network.arrow} is a direct adaptation of \code{\link[sna]{gplot.arrow}} from the \code{sna} package. }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{plot.network}}, \code{\link{network.loop}}, \code{\link{polygon}} }
\examples{
  #Plot two points
  plot(1:2,1:2)
  
  #Add an edge
  network.arrow(1,1,2,2,width=0.01,col="red",border="black")
}
\keyword{ aplot }% at least one, from doc/KEYWORDS
\keyword{ graphs }% __ONLY ONE__ keyword per line
