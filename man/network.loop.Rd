\name{network.loop}
\alias{network.loop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Add Loops to a Plot }
\description{
  \code{network.loop} draws a "loop" at a specified location; this is used to designate self-ties in \code{\link{plot.network}}.
}
\usage{
network.loop(x0, y0, length = 0.1, angle = 10, width = 0.01, 
    col = 1, border = 1, lty = 1, offset = 0, edge.steps = 10, 
    radius = 1, arrowhead = TRUE, xctr=0, yctr=0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x0}{ a vector of x coordinates for points of origin. }
  \item{y0}{ a vector of y coordinates for points of origin. }
  \item{length}{ arrowhead length, in current plotting units. }
  \item{angle}{ arrowhead angle (in degrees). }
  \item{width}{ width for loop body, in current plotting units (can be a vector). }
  \item{col}{ loop body color (can be a vector). }
  \item{border}{ loop border color (can be a vector). }
  \item{lty}{ loop border line type (can be a vector). }
  \item{offset}{ offset for origin point (can be a vector). }
  \item{edge.steps}{ number of steps to use in approximating curves. }
  \item{radius}{ loop radius (can be a vector). }
  \item{arrowhead}{ boolean; should arrowheads be used?  (Can be a vector.) }
  \item{xctr}{ x coordinate for the central location away from which loops should be oriented. }
  \item{yctr}{ y coordinate for the central location away from which loops should be oriented. }
  \item{\dots}{ additional arguments to \code{\link{polygon}}. }
}
\details{
  \code{network.loop} is the companion to \code{\link{network.arrow}}; like the latter, plot elements produced by \code{network.loop} are drawn using \code{\link{polygon}}, and as such are scaled based on the current plotting device.  By default, loops are drawn so as to encompass a circular region of radius \code{radius}, whose center is \code{offset} units from \code{x0,y0} and at maximum distance from \code{xctr,yctr}.  This is useful for functions like \code{\link{plot.network}}, which need to draw loops incident to vertices of varying radii.
}
\value{
  None.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Carter T. Butts \email{buttsc@uci.edu} }
\note{ \code{network.loop} is a direct adaptation of \code{\link[sna]{gplot.loop}}, from the \code{sna} package. }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{network.arrow}}, \code{\link{plot.network}}, \code{\link{polygon}} }
\examples{
#Plot a few polygons with loops
plot(0,0,type="n",xlim=c(-2,2),ylim=c(-2,2),asp=1)
network.loop(c(0,0),c(1,-1),col=c(3,2),width=0.05,length=0.4,
  offset=sqrt(2)/4,angle=20,radius=0.5,edge.steps=50,arrowhead=TRUE)
polygon(c(0.25,-0.25,-0.25,0.25,NA,0.25,-0.25,-0.25,0.25), 
    c(1.25,1.25,0.75,0.75,NA,-1.25,-1.25,-0.75,-0.75),col=c(2,3))

}
\keyword{ aplot }% at least one, from doc/KEYWORDS
\keyword{ graphs }% __ONLY ONE__ keyword per line
