\name{network.edgelabel}
\alias{network.edgelabel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots a label corresponding to an edge in a network plot. 
}
\description{
Draws a text labels on (or adjacent to) the line segments connecting vertices on a network plot.  
}
\usage{
network.edgelabel(px0, py0, px1, py1, label, directed, loops = FALSE, cex, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{px0}{
vector of x coordinates of tail vertex of the edge
}
  \item{py0}{
vector of y coordinates of tail vertex of the edge
}
  \item{px1}{
vector of x coordinates of head vertex of the edge
}
  \item{py1}{
vector of y coordinate of head vertex of the edge
}
  \item{label}{
vector strings giving labels to be drawn for edge edge
}
  \item{directed}{
logical: is the underlying network directed? If FALSE, labels will be drawn in the middle of the line segment, otherwise in the first 3rd so that the labels for edges pointing in the opposite direction will not overlap. 
}
  \item{loops}{
logical: if true, assuming the labels to be drawn belong to loop-type edges and render appropriately
}
  \item{cex}{
numeric vector giving the text expansion factor for each label
}
  \item{\dots}{
additional arguments to be passed to \code{\link{text}}
}
}
\details{
Called internally by \code{\link{plot.network}} when \code{edge.label} parameter is used. (currently only for straight edges). Makes a crude attempt to shift labels to either side of line, and to draw the edge labels for self-loops near the vertex. No attempt is made to avoid overlap between vertex and edge labels.
}
\value{
no value is returned but text will be rendered on the active plot
}

\author{
skyebend
}
