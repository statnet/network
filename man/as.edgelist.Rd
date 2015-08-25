\name{as.edgelist}
\alias{as.edgelist}
\alias{as.edgelist.matrix}
\alias{as.edgelist.network}
\alias{is.edgelist}
\alias{edgelist}
\title{
  Convert a network object into a numeric edgelist matrix
}
\description{
  Constructs an edgelist in a sorted format with defined attributes.
}
  \usage{
  \method{as.edgelist}{network}(x, attrname = NULL, as.sna.edgelist = FALSE, 
  inverted = NULL, ...)
  
  \method{as.edgelist}{matrix}(x, n, directed = TRUE, bipartite = FALSE, 
  loops = FALSE, inverted = FALSE, vnames = seq_len(n), ...)
  
  is.edgelist(x)
  }
  
  \arguments{
  \item{x}{
  a \code{network} object with additional class added indicating how it should be dispatched.
  }
  \item{attrname}{optionally, the name of an edge attribute to use for edge values}
  \item{as.sna.edgelist}{logical; should the edgelist be returned in edgelist form expected by the sna package?}
  \item{inverted}{logical; value is passed to the 'inverted' flag on the edgelist returned}
  \item{n}{integer number of vertices in network, value passed to the 'n' flag on edgelist returned}
  \item{vnames}{vertex names (defaults to vertex ids) to be attached to edgelist for sna package compatibility}
  \item{directed}{logical; is network directed, value passed to the 'directed' flag on edgelist returned}
  \item{bipartite}{logical or integer; is network bipartite, value passed to the 'bipartite' flag on edgelist returned}
  \item{loops}{logical; are self-loops allowed in network?, value passed to the 'loops' flag on edgelist returned}
  \item{\dots}{
  additional arguments to other methods
  }
  }
  \details{
  Constructs a edgelist matrix from a network, sorted tails-major order, with tails first, and, for undirected networks, tail < head.  This format is required by some reverse-depending packages (i.e. \code{ergm})
  
  The \code{\link{as.matrix.network.edgelist}} provides similar functionality but it does not enforce ordering or set the \code{edgelist} class and so should be slightly faster. 
  
  \code{is.edgelist} tests if an object has the class \code{'edgelist'}
  
  
  }
  \value{
  A matrix in which the first two columns are integers giving the tail (source) and head (target) vertex ids of each edge. The matrix will be given the class \code{edgelist}.  
  
  The edgelist has additional attributes attached to it:
  \itemize{
  \item \code{attr(,"n")} the number of vertices in the original network
  
  \item \code{attr(,"vnames")} the names of vertices in the original network
  
  \item \code{attr(,"directed")} logical, was the original network directed
  
  \item \code{attr(,"bipartite")} was the original network bipartite
  
  \item \code{attr(,"loops")} does the original network contain self-loops
  
  \item \code{attr(,"inverted")}  indicates if the network has been inverted?
  }
  
  Note that if the \code{attrname} attribute is used the resulting edgelist matrix will have three columns.  And if \code{attrname} refers to a character attribute, the resulting edgelist matrix will be character rather than numeric.
  
  }
  
  
  \note{
  NOTE: this function was moved to network from the ergm package in network version 1.13 
  }
  
  
  
  \seealso{
  See also \code{\link{as.matrix.network.edgelist}}
  }
  \examples{
   data(emon)
   as.edgelist(emon[[1]])
   # contrast with unsorted columns of
   as.matrix.network.edgelist(emon[[1]])
  }