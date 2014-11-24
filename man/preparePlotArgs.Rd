\name{preparePlotArgs}
\alias{preparePlotArgs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Expand and transform attributes of networks to values appropriate for aguments to plot.network
}
\description{
This is primairly an internal function called by \code{plot.network} or by external packages such as \code{ndtv} that want to prepare \code{plot.network} graphic arguments in a standardized way.  
}
\usage{
preparePlotArgs(x, argName, argValue, d = NULL, edgetouse = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a \code{network} object which is going to be plotted
}
  \item{argName}{ character, the name of \code{plot.network} graphic parameter
}
  \item{argValue}{ value for the graphic paramter named in \code{argName} which to be transformed/prepared.  For many attributes, if this is a single character vector it will be assumed to be the name of a vertex or edge attribute to be extracted and transformed
}
  \item{d}{ is an edgelist matrix of edge values optionally used by some edge attribute functions
}
  \item{edgetouse}{numeric vector giving set of edge ids to be used (in case some edges are not being shown) required by some attributes
}
}
\details{
Given a network object, the name of graphic parameter argument to \code{plot.network} and value, it will if necessary transform the value, or extract it from the network, according to the description in \code{\link{plot.network}}. For some attributes, if the value is the name of a vertex or edge attribute, the appropriate values will be extracted from the network before transformation. 
}
\value{
returns a vector with length corresponding to the number of vertices or edges (depending on the paramter type) giving the appropriately prepared values for the parameter type.  If the values or specified attribute can not be processed correctly, and Error may occur. 
}

\author{
skyebend@uw.edu
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{plot.network}}
}
\examples{
  net<-network.initialize(3)
  set.vertex.attribute(net,'color',c('red','green','blue'))
  set.vertex.attribute(net,'charm',1:3)
  # replicate a single colorname value
  preparePlotArgs(net,'vertex.col','purple')
  # map the 'color' attribute to color
  preparePlotArgs(net,'vertex.col','color')
  # similarly for a numeric attribute ...
  preparePlotArgs(net,'vertex.cex',12)
  preparePlotArgs(net,'vertex.cex','charm')
}

