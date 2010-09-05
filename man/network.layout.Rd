\name{network.layout}
\alias{network.layout}
\alias{network.layout.circle}
\alias{network.layout.fruchtermanreingold}
\alias{network.layout.kamadakawai}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Vertex Layout Functions for plot.network }
\description{
  Various functions which generate vertex layouts for the \code{\link{plot.network}} visualization routine.
}
\usage{
network.layout.circle(nw, layout.par)
network.layout.fruchtermanreingold(nw, layout.par)
network.layout.kamadakawai(nw, layout.par)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nw}{ a netwotk, as passed by \code{\link{plot.network}}. }
  \item{layout.par}{ a list of parameters. }
}
\details{
  Vertex layouts for network visualization pose a difficult problem -- there is no single, ``good'' layout algorithm, and many different approaches may be valuable under different circumstances.  With this in mind, \code{\link{plot.network}} allows for the use of arbitrary vertex layout algorithms via the \code{network.layout.*} family of routines.  When called, \code{\link{plot.network}} searches for a \code{network.layout} function whose fourth name matches its \code{mode} argument (see \code{\link{plot.network}} help for more information); this function is then used to generate the layout for the resulting plot.  In addition to the routines documented here, users may add their own layout functions as needed.  The requirements for a \code{network.layout} function are as follows:
  \enumerate{
  \item the first argument, \code{nw}, must be the (dichotomous) graph adjacency matrix;
  \item the second argument, \code{layout.par}, must be a list of parameters (or \code{NULL}, if no parameters are specified); and
  \item the return value must be a real matrix of dimension \code{c(2,network.size(nw))}, whose rows contain the vertex coordinates.
  }
  Other than this, anything goes.  (In particular, note that \code{layout.par} could be used to pass additional matrices, if needed.)  
  
  The \code{network.layout} functions currently supplied by default are as follows:
  \describe{
  \item{circle}{ This function places vertices uniformly in a circle; it takes no arguments.}
  \item{fruchtermanreingold}{ This function generates a layout using a variant of Fruchterman and Reingold's force-directed placement algorithm.  It takes the following arguments:
    \describe{
    \item{layout.par\$niter}{ This argument controls the number of iterations to be employed.  (Defaults to 500.) }
    \item{layout.par\$max.delta}{ Sets the maximum change in position for any given iteration.   (Defaults to \code{network.size(nw)}.)}
    \item{layout.par\$area}{  Sets the "area" parameter for the F-R algorithm. (Defaults to \code{network.size(nw)^2}.)}
    \item{layout.par\$cool.exp}{  Sets the cooling exponent for the annealer. (Defaults to 3.)}
    \item{layout.par\$repulse.rad}{  Determines the radius at which vertex-vertex repulsion cancels out attraction of adjacent vertices.  (Defaults to \code{area*network.size(nw)}.)}
    \item{layout.par\$seed.coord}{ A two-column matrix of initial vertex coordinates.  (Defaults to a random circular layout.) }  
    }
  }
  \item{kamadakawai}{ This function generates a vertex layout using a version of the Kamada-Kawai force-directed placement algorithm.  It takes the following arguments:
    \describe{
    \item{layout.par\$niter}{ This argument controls the number of iterations to be employed.  (Defaults to 1000.) }
    \item{layout.par\$sigma}{ Sets the base standard deviation of position change proposals.   (Defaults to \code{network.size(nw)/4}.)}
    \item{layout.par\$initemp}{  Sets the initial "temperature" for the annealing algorithm. (Defaults to 10.)}
    \item{layout.par\$cool.exp}{  Sets the cooling exponent for the annealer. (Defaults to 0.99.)}
    \item{layout.par\$kkconst}{  Sets the Kamada-Kawai vertex attraction constant.  (Defaults to \code{network.size(nw)^2}.)}
    \item{layout.par\$elen}{  Provides the matrix of interpoint distances to be approximated.  (Defaults to the geodesic distances of \code{nw} after symmetrizing, capped at \code{sqrt(network.size(nw))}.)}
    \item{layout.par\$seed.coord}{ A two-column matrix of initial vertex coordinates.  (Defaults to a gaussian layout.) }  
    }
  }
}
}
\value{
A matrix whose rows contain the x,y coordinates of the vertices of \code{nw}.
}
\references{
Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  \url{http://www.jstatsoft.org/v24/i02/}

Fruchterman, T.M.J. and Reingold, E.M.  (1991).  \dQuote{Graph Drawing by Force-directed Placement.} \emph{Software - Practice and Experience,} 21(11):1129-1164.

Kamada, T. and Kawai, S.  (1989). \dQuote{An Algorithm for Drawing General Undirected Graphs.} \emph{Information Processing Letters,} 31(1):7-15.
  }
\author{ Carter T. Butts \email{buttsc@uci.edu} }
\note{ The \code{network.layout} routines shown here are adapted directly from the \code{\link[sna]{gplot.layout}} routines of the \code{sna} package. }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{plot.network}}}
%\examples{
%}
\keyword{ graphs }% at least one, from doc/KEYWORDS
\keyword{ dplot }% __ONLY ONE__ keyword per line
