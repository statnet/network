\name{attribute.methods}
\alias{attribute.methods}
\alias{delete.edge.attribute}
\alias{delete.network.attribute}
\alias{delete.vertex.attribute}
\alias{get.edge.attribute}
\alias{get.edge.value}
\alias{get.network.attribute}
\alias{get.vertex.attribute}
\alias{list.edge.attributes}
\alias{list.network.attributes}
\alias{list.vertex.attributes}
\alias{network.vertex.names}
\alias{network.vertex.names<-}
\alias{set.edge.attribute}
\alias{set.edge.value}
\alias{set.network.attribute}
\alias{set.vertex.attribute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Attribute Interface Methods for the Network Class }
\description{
  These methods get, set, list, and delete attributes at the network, edge, and vertex level.
}
\usage{
delete.edge.attribute(x, attrname)
delete.network.attribute(x, attrname)
delete.vertex.attribute(x, attrname)

get.edge.attribute(el, attrname, unlist = TRUE)
get.edge.value(x, attrname, unlist = TRUE)
get.network.attribute(x, attrname, unlist = FALSE)
get.vertex.attribute(x, attrname, na.omit = FALSE, null.na = TRUE,
    unlist = TRUE)
network.vertex.names(x)

list.network.attributes(x)
list.edge.attributes(x)
list.vertex.attributes(x)

set.edge.attribute(x, attrname, value, e=1:length(x$mel))
set.edge.value(x, attrname, value, e=1:length(x$mel))
set.network.attribute(x, attrname, value)
set.vertex.attribute(x, attrname, value, v=1:network.size(x))
network.vertex.names(x) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{el}{ a list of edges (possibly \code{network$mel}). }
  \item{x}{ an object of class \code{network}. }
  \item{attrname}{ the name of the attribute to get or set. }
  \item{unlist}{ logical; should retrieved attributes be \code{\link{unlist}}ed prior to being returned? }
  \item{na.omit}{ logical; should values from missing vertices/edges be removed? }
  \item{null.na}{ logical; should \code{NULL} values be replaced with \code{NA}s? }
  \item{value}{ values of the attribute to be set; these should be in \code{vector} or \code{list} form for the \code{edge} and \code{vertex} cases, or \code{matrix} form for \code{set.edge.value}.}
  \item{e}{ IDs for the edges whose attributes are to be altered. }
  \item{v}{ IDs for the vertices whose attributes are to be altered. }
}
\details{
The \code{list.attributes} functions return the names of all edge, network, or vertex attributes (respectively) in the network.  All attributes need not be defined for all elements; the union of all extant attributes for the respective element type is returned.

The \code{get.attribute} functions look for an edge, network, or vertex attribute (respectively) with the name \code{attrname}, returning its values.  Note that, to retrieve an edge attribute from all edges within a network \code{x}, \code{x$mel} should be used as the first argument to \code{get.edge.attribute}; \code{get.edge.value} is a convenience function which does this automatically.  \code{network.vertex.names} is a convenience function to extract the \code{"vertex.names"} attribute from all vertices.

The \code{set.attribute} functions allow one to set the values of edge, network, or vertex attributes.  \code{set.edge.value} is a convenience function which allows edge attributes to be given in adjacency matrix form, and the assignment form of \code{network.attribute.names} is likewise a convenient front-end to \code{set.vertex.attribute} for vertex names.  The \code{delete.attribute} functions, by contrast, remove the named attribute from the network, from all edges, or from all vertices (as appropriate).  If \code{attrname} is a vector of attribute names, each will be removed in turn.  These functions modify their arguments in place, although a pointer to the modified object is also (invisibly) returned.

Note that some attribute assignment/extraction can be performed through the various extraction/replacement operators.  See the associated man page for details.
}
\value{
For the \code{list.attributes} methods, a vector containing attribute names.  For the \code{get.attribute} methods, a list containing the values of the attribute in question (or simply the value itself, for \code{get.network.attribute}).  For the \code{set.attribute} and \code{delete.attribute} methods, a pointer to the updated \code{network} object.
}
\references{ Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  \url{http://www.jstatsoft.org/v24/i02/}
}
\author{ Carter T. Butts \email{buttsc@uci.edu} }
%\note{ ~~further notes~~ }
%
% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{network}}, \code{\link{as.network.matrix}}, \code{\link{as.sociomatrix}}, \code{\link{as.matrix.network}}, \code{\link{network.extraction}}}
\examples{
#Create a network with three edges
m<-matrix(0,3,3)
m[1,2]<-1; m[2,3]<-1; m[3,1]<-1
g<-network(m)

#Create a matrix of values corresponding to edges
mm<-m
mm[1,2]<-7; mm[2,3]<-4; mm[3,1]<-2

#Assign some attributes
set.edge.attribute(g,"myeval",3:5)
set.edge.value(g,"myeval2",mm)
set.network.attribute(g,"mygval","boo")
set.vertex.attribute(g,"myvval",letters[1:3])
network.vertex.names(g) <- LETTERS[1:10]

#List the attributes
list.edge.attributes(g)
list.network.attributes(g)
list.vertex.attributes(g)

#Retrieve the attributes
get.edge.attribute(g$mel,"myeval")  #Note the first argument!
get.edge.value(g,"myeval")          #Another way to do this
get.edge.attribute(g$mel,"myeval2") 
get.network.attribute(g,"mygval")
get.vertex.attribute(g,"myvval")
network.vertex.names(g)

#Purge the attributes
delete.edge.attribute(g,"myeval")
delete.edge.attribute(g,"myeval2")
delete.network.attribute(g,"mygval")
delete.vertex.attribute(g,"myvval")

#Verify that the attributes are gone
list.edge.attributes(g)
list.network.attributes(g)
list.vertex.attributes(g)

#Note that we can do similar things using operators
g \%n\% "mygval" <- "boo"               #Set attributes, as above
g \%v\% "myvval" <- letters[1:3]
g \%e\% "myeval" <- mm
g[,,names.eval="myeval"] <- mm          #Another way to do this
g \%n\% "mygval"                        #Retrieve the attributes
g \%v\% "myvval"
g \%e\% "mevval"
as.sociomatrix(g,"myeval")              # Or like this

}
\keyword{ classes }% at least one, from doc/KEYWORDS
\keyword{ graphs }% __ONLY ONE__ keyword per line
