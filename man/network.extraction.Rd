\name{network.extraction}
\alias{network.extraction}
\alias{\%n\%}
\alias{\%n\%<-}
\alias{\%v\%}
\alias{\%v\%<-}
\alias{\%e\%}
\alias{\%e\%<-}
\alias{\%nattr\%}
\alias{\%nattr\%<-}
\alias{\%vattr\%}
\alias{\%vattr\%<-}
\alias{\%eattr\%}
\alias{\%eattr\%<-}
\alias{[.network}
\alias{[<-.network}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Extraction and Replacement Operators for Network Objects }
\description{
Various operators which allow extraction or replacement of various components of a \code{network} object.
}
\usage{
\method{[}{network}(x, i, j, na.omit = FALSE)
\method{[}{network}(x, i, j, names.eval=NULL, add.edges=FALSE) <- value
x \%e\% attrname
x \%e\% attrname <- value
x \%eattr\% attrname
x \%eattr\% attrname <- value

x \%n\% attrname
x \%n\% attrname <- value
x \%nattr\% attrname
x \%nattr\% attrname <- value

x \%v\% attrname
x \%v\% attrname <- value
x \%vattr\% attrname
x \%vattr\% attrname <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{network}. }
  \item{i, j}{ indices of the vertices with respect to which adjacency is to be tested.  Empty values indicate that all vertices should be employed (see below). }
  \item{na.omit}{ logical; should missing edges be omitted (treated as no-adjacency), or should \code{NA}s be returned?  (Default: return \code{NA} on missing.)}
  \item{names.eval}{optionally, the name of an edge attribute to use for assigning edge values.}
  \item{add.edges}{logical; should new edges be added to \code{x} where edges are absent and the appropriate element of \code{value} is non-zero?}
  \item{value}{the value (or set thereof) to be assigned to the selected element of \code{x}.}
  \item{attrname}{the name of a network or vertex attribute (as appropriate).}
}
\details{
  Indexing for edge extraction operates in a manner analogous to \code{matrix} objects.  Thus, \code{x[,]} selects all vertex pairs, \code{x[1,-5]} selects the pairing of vertex 1 with all vertices except for 5, etc.  Following this, it is acceptable for \code{i} and/or \code{j} to be logical vectors indicating which vertices are to be included.  During assignment, an attempt is made to match the elements of \code{value} to the extracted pairs in an intelligent way; in particular, elements of \code{value} will be replicated if too few are supplied (allowing expressions like \code{x[1,]<-1}).  Where \code{names.eval==NULL}, zero and non-zero values are taken to indicate the presence of absence of edges.  \code{x[2,4]<-6} thus adds a single (2,4) edge to \code{x}, and \code{x[2,4]<-0} removes such an edge (if present).  If \code{x} is multiplex, assigning 0 to a vertex pair will eliminate \emph{all} edges on that pair.  Pairs are taken to be directed where \code{is.directed(x)==TRUE}, and undirected where \code{is.directed(x)==FALSE}.

  If an edge attribute is specified using \code{names.eval}, then the provided values will be assigned to that attribute.  When assigning values, only extant edges are employed (unless \code{add.edges==TRUE}); in the latter case, any non-zero assignment results in the addition of an edge where currently absent.  If the attribute specified is not present on a given edge, it is added.  Otherwise, any existing value is overwritten.  The \code{\%e\%} operator can also be used to extract/assign edge values; in those roles, it is respectively equivalent to \code{get.edge.value(x,attrname)} and \code{set.edge.value(x,attrname=attrname,value=value)}. Note that the assignment operator takes edge values input in adjacency matrix form.

  The \code{\%n\%} and \code{\%v\%} operators serve as front-ends to the network and vertex extraction/assignment functions (respectively).  In the extraction case, \code{x \%n\% attrname} is equivalent to \code{get.network.attribute(x,attrname)}, with \code{x \%v\% attrname} corresponding to \code{get.vertex.attribute(x,attrname)}.  In assignment, the respective equivalences are to \code{set.network.attribute(x,attrname,value)} and  \code{set.vertex.attribute(x,attrname,value)}.  

  The \code{\%eattr\%}, \code{\%nattr\%}, and \code{\%vattr\%} operators are equivalent to \code{\%e\%}, \code{\%n\%}, and \code{\%v\%} (respectively).  The short forms are more succinct, but may produce less readable code.
}
\value{
  The extracted data, or none.
}
\references{ Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational Data in R.}  \emph{Journal of Statistical Software}, 24(2).  \url{http://www.jstatsoft.org/v24/i02/}
}
\author{ Carter T. Butts \email{buttsc@uci.edu} }
%\note{ ~~further notes~~ }

\seealso{ \code{\link{is.adjacent}}, \code{\link{as.sociomatrix}}, \code{\link{attribute.methods}}, \code{\link{add.edges}}, \code{\link{network.operators}}}
\examples{
  #Create a random graph (inefficiently)
  g<-network.initialize(10)
  g[,]<-matrix(rbinom(100,1,0.1),10,10)
  plot(g)
  
  #Demonstrate edge addition/deletion
  g[,]<-0
  g[1,]<-1
  g[2:3,6:7]<-1
  g[,]
  
  #Set edge values
  g[,,names.eval="boo"]<-5
  as.sociomatrix(g,"boo")
  g \%e\% "hoo" <- "wah"
  g \%e\% "hoo"
  #Assignment input should be as adjacency matrix
  g \%e\% "age" <- as.matrix(1:100, 10, 10)
  g \%e\% "age"
  as.sociomatrix(g,"age")

  #Set/retrieve network and vertex attributes
  g \%n\% "blah" <- "Pork!"                 #The other white meat?
  g \%n\% "blah" == "Pork!"                 #TRUE!
  g \%v\% "foo" <- letters[10:1]            #Letter the vertices
  g \%v\% "foo" == letters[10:1]            #All TRUE
}
\keyword{graphs}% at least one, from doc/KEYWORDS
\keyword{manip}% at least one, from doc/KEYWORDS
