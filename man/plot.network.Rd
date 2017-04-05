\name{plot.network.default}
\alias{plot.network}
\alias{plot.network.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Two-Dimensional Visualization for Network Objects}
\description{
   \code{plot.network} produces a simple two-dimensional plot of network \code{x}, using optional attribute \code{attrname} to set edge values.  A variety of options are available to control vertex placement, display details, color, etc.
}
\usage{
\method{plot}{network}(x, \dots)

\method{plot.network}{default}(x, attrname = NULL, 
    label = network.vertex.names(x), coord = NULL, jitter = TRUE,
    thresh = 0, usearrows = TRUE, mode = "fruchtermanreingold", 
    displayisolates = TRUE, interactive = FALSE, xlab = NULL, 
    ylab = NULL, xlim = NULL, ylim = NULL, pad = 0.2, label.pad = 0.5,
    displaylabels = !missing(label), boxed.labels = FALSE, label.pos = 0,
    label.bg = "white", vertex.sides = 50, vertex.rot = 0, vertex.lwd=1,
    arrowhead.cex = 1, label.cex = 1, loop.cex = 1, vertex.cex = 1,
    edge.col = 1, label.col = 1, vertex.col = 2, label.border = 1,
    vertex.border = 1, edge.lty = 1, label.lty = NULL, vertex.lty = 1,
    edge.lwd = 0, edge.label = NULL, edge.label.cex = 1,
    edge.label.col = 1, label.lwd = par("lwd"), edge.len = 0.5, 
    edge.curve = 0.1, edge.steps = 50, loop.steps = 20, 
    object.scale = 0.01, uselen = FALSE, usecurve = FALSE,
    suppress.axes = TRUE, vertices.last = TRUE, new = TRUE, 
    layout.par = NULL, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{network}. }
  \item{attrname}{ an optional edge attribute, to be used to set edge values. }
  \item{label}{ a vector of vertex labels, if desired; defaults to the vertex labels returned by \code{\link{network.vertex.names}}. If \code{label} has one element and it matches with a vertex attribute name, the value of the attribute will be used. Note that labels may be set but hidden by the \code{displaylabels} argument.}
  \item{coord}{ user-specified vertex coordinates, in an network.size(x)x2 matrix.  Where this is specified, it will override the \code{mode} setting. }
  \item{jitter}{ boolean; should the output be jittered? }
  \item{thresh}{ real number indicating the lower threshold for tie values.  Only ties of value >\code{thresh} are displayed.  By default, \code{thresh}=0.}
  \item{usearrows}{ boolean; should arrows (rather than line segments) be used to indicate edges? }
  \item{mode}{ the vertex placement algorithm; this must correspond to a \code{\link{network.layout}} function. }
  \item{displayisolates}{ boolean; should isolates be displayed? }
  \item{interactive}{ boolean; should interactive adjustment of vertex placement be attempted? }
  \item{xlab}{ x axis label. }
  \item{ylab}{ y axis label. }
  \item{xlim}{ the x limits (min, max) of the plot. }
  \item{ylim}{ the y limits of the plot. } 
  \item{pad}{ amount to pad the plotting range; useful if labels are being clipped. }
  \item{label.pad}{ amount to pad label boxes (if \code{boxed.labels==TRUE}), in character size units. }
  \item{displaylabels}{ boolean; should vertex labels be displayed? }
  \item{boxed.labels}{ boolean; place vertex labels within boxes? }
  \item{label.pos}{ position at which labels should be placed, relative to vertices.  \code{0} results in labels which are placed away from the center of the plotting region; \code{1}, \code{2}, \code{3}, and \code{4} result in labels being placed below, to the left of, above, and to the right of vertices (respectively); and \code{label.pos>=5} results in labels which are plotted with no offset (i.e., at the vertex positions). }
  \item{label.bg}{ background color for label boxes (if \code{boxed.labels==TRUE}); may be a vector, if boxes are to be of different colors.}
  \item{vertex.sides}{ number of polygon sides for vertices; may be given as a vector or a vertex attribute name, if vertices are to be of different types. }
  \item{vertex.rot}{ angle of rotation for vertices (in degrees); may be given as a vector or a vertex attribute name, if vertices are to be rotated differently. }
  \item{vertex.lwd}{ line width of vertex borders; may be given as a vector or a vertex attribute name, if vertex borders are to have different line widths. }
  \item{arrowhead.cex}{ expansion factor for edge arrowheads.}
  \item{label.cex}{ character expansion factor for label text. }
  \item{loop.cex}{ expansion factor for loops; may be given as a vector or a vertex attribute name, if loops are to be of different sizes. }
  \item{vertex.cex}{ expansion factor for vertices; may be given as a vector or a vertex attribute name, if vertices are to be of different sizes. }
  \item{edge.col}{ color for edges; may be given as a vector, adjacency matrix, or edge attribute name, if edges are to be of different colors. }
  \item{label.col}{ color for vertex labels; may be given as a vector or a vertex attribute name, if labels are to be of different colors. }
  \item{vertex.col}{ color for vertices; may be given as a vector or a vertex attribute name, if vertices are to be of different colors. }
  \item{label.border}{ label border colors (if \code{boxed.labels==TRUE}); may be given as a vector, if label boxes are to have different colors. }
  \item{vertex.border}{ border color for vertices; may be given as a vector or a vertex attribute name, if vertex borders are to be of different colors. }
  \item{edge.lty}{ line type for edge borders; may be given as a vector, adjacency matrix, or edge attribute name, if edge borders are to have different line types. }
  \item{label.lty}{ line type for label boxes (if \code{boxed.labels==TRUE}); may be given as a vector, if label boxes are to have different line types. }
  \item{vertex.lty}{ line type for vertex borders; may be given as a vector or a vertex attribute name, if vertex borders are to have different line types. }
  \item{edge.lwd}{ line width scale for edges; if set greater than 0, edge widths are scaled by \code{edge.lwd*dat}.  May be given as a vector, adjacency matrix, or edge attribute name, if edges are to have different line widths. }
  \item{edge.label}{if non-\code{NULL}, labels for edges will be drawn. May be given as a vector, adjacency matrix, or edge attribute name, if edges are to have different labels. A single value of \code{TRUE} will use edge ids as labels. NOTE: currently doesn't work for curved edges.}
  \item{edge.label.cex}{ character expansion factor for edge label text; may be given as a vector or a edge attribute name, if edge labels are to have different sizes.}
  \item{edge.label.col}{ color for edge labels; may be given as a vector or a edge attribute name, if labels are to be of different colors.}
  \item{label.lwd}{ line width for label boxes (if \code{boxed.labels==TRUE}); may be given as a vector, if label boxes are to have different line widths. }
  \item{edge.len}{ if \code{uselen==TRUE}, curved edge lengths are scaled by \code{edge.len}. }
  \item{edge.curve}{ if \code{usecurve==TRUE}, the extent of edge curvature is controlled by \code{edge.curv}.  May be given as a fixed value, vector, adjacency matrix, or edge attribute name, if edges are to have different levels of curvature.  }
  \item{edge.steps}{ for curved edges (excluding loops), the number of line segments to use for the curve approximation. }
  \item{loop.steps}{ for loops, the number of line segments to use for the curve approximation. }
  \item{object.scale}{ base length for plotting objects, as a fraction of the linear scale of the plotting region. Defaults to 0.01.}
  \item{uselen}{ boolean; should we use \code{edge.len} to rescale edge lengths? }
  \item{usecurve}{ boolean; should we use \code{edge.curve}? }
  \item{suppress.axes}{ boolean; suppress plotting of axes? }
  \item{vertices.last}{ boolean; plot vertices after plotting edges? }
  \item{new}{ boolean; create a new plot?  If \code{new==FALSE}, vertices and edges will be added to the existing plot. }
  \item{layout.par}{ parameters to the \code{\link{network.layout}} function specified in \code{mode}. }
  \item{\dots}{ additional arguments to \code{\link{plot}}. }
}
\details{
\code{plot.network} is the standard visualization tool for the \code{network} class.  By means of clever selection of display parameters, a fair amount of display flexibility can be obtained.  Vertex layout -- if not specified directly using \code{coord} -- is determined via one of the various available algorithms.  These should be specified via the \code{mode} argument; see \code{\link{network.layout}} for a full list.  User-supplied layout functions are also possible -- see the aforementioned man page for details.

Note that where \code{is.hyper(x)==TRUE}, the network is converted to bipartite adjacency form prior to computing coordinates.  If \code{interactive==TRUE}, then the user may modify the initial network layout by selecting an individual vertex and then clicking on the location to which this vertex is to be moved; this process may be repeated until the layout is satisfactory.
}
\value{
 A two-column matrix containing the vertex positions as x,y coordinates
}
\references{ 
Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  \url{http://www.jstatsoft.org/v24/i02/}

Wasserman, S., and Faust, K.  (1994).  \emph{Social Network Analysis: Methods and Applications.}  Cambridge: Cambridge University Press. }
\author{ Carter T. Butts \email{buttsc@uci.edu} }
\note{ \code{plot.network} is adapted (with minor modifications) from the \code{\link[sna]{gplot}} function of the \code{sna} library (authors: Carter T. Butts and Alex Montgomery); eventually, these two packages will be integrated. }
%
% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{network}}, \code{\link{network.arrow}}, \code{\link{network.loop}}, \code{\link{network.vertex}} }
\examples{
#Construct a sparse graph
m<-matrix(rbinom(100,1,1.5/9),10)
diag(m)<-0
g<-network(m)

#Plot the graph
plot(g)

#Load Padgett's marriage data
data(flo)
nflo<-network(flo)
#Display the network, indicating degree and flagging the Medicis
plot(nflo, vertex.cex=apply(flo,2,sum)+1, usearrows=FALSE,
    vertex.sides=3+apply(flo,2,sum),
    vertex.col=2+(network.vertex.names(nflo)=="Medici"))
}
\keyword{ hplot }% at least one, from doc/KEYWORDS
\keyword{ graphs }% __ONLY ONE__ keyword per line
