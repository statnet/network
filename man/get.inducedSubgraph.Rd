\name{get.inducedSubgraph}
\alias{\%s\%}
\alias{get.inducedSubgraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Retrieve Induced Subgraphs and Cuts
}
\description{
Given a set of vertex IDs, \code{get.inducedSubgraph} returns the subgraph induced by the specified vertices (i.e., the vertices and all associated edges).  Optionally, passing a second set of alters returns the cut from the first to the second set (i.e., all edges passing between the sets), along with the associated endpoints.  In both cases, the result is returned as a network object, with all attributes of the selected edges and/or vertices (and any network attributes) preserved.
}
\usage{
get.inducedSubgraph(x, v, alters = NULL, eid = NULL)
x \%s\% v
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object of class \code{network}.
}
  \item{v}{
a vector of vertex IDs, or, for \code{\%s\%}, optionally a list containing two disjoint vectors of vertex IDs (see below).
}
  \item{alters}{
optionally, a second vector of vertex IDs.  Must be disjoint with \code{v}.
}
  \item{eid}{
  optionally, a numeric vector of valid edge ids in \code{x} that should be retained (cannot be used with \code{v} or \code{alter})
  }
}
\details{
For \code{get.inducedSubgraph}, \code{v} can be a vector of vertex IDs.  If \code{alter=NULL}, the subgraph induced by these vertices is returned.  Calling \code{\%s\%} with a single vector of vertices has an identical effect.

Where \code{alters} is specified, it must be a vector of IDs disjoint with \code{v}.  Where both are given, the edges spanning \code{v} and \code{alters} are returned, along with the vertices in question.  (Technically, only the edges really constitute the \dQuote{cut,} but the vertices are included as well.)  The same result can be obtained with the \code{\%s\%} operator by passing a two-element list on the right hand side; the first element is then interpreted as \code{v}, and the second as \code{alters}.

When \code{eid} is specified, the \code{v} and \code{alters} argument will be ignored and the subgraph induced by the specified edges and their incident vertices will be returned. 

Any network, vertex, or edge attributes for the selected network elements are retained (although features such as vertex IDs and the network size will typically change).  These are copies of the elements in the original network, which is not altered by this function.
}
\value{
A \code{\link{network}} object containing the induced subgraph.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{ Carter T. Butts \email{buttsc@uci.edu} }
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{network}}, \code{\link{network.extraction}}
}
\examples{
#Load the Drabek et al. EMON data
data(emon)

#For the Mt. St. Helens, EMON, several types of organizations are present:
type<-emon$MtStHelens \%v\% "Sponsorship"

#Plot interactions among the state organizations
plot(emon$MtStHelens \%s\% which(type=="State"), displaylabels=TRUE)

#Plot state/federal interactions
plot(emon$MtStHelens \%s\% list(which(type=="State"), 
    which(type=="Federal")),  displaylabels=TRUE)

#Plot state interactions with everyone else
plot(emon$MtStHelens \%s\% list(which(type=="State"), 
    which(type!="State")), displaylabels=TRUE)
    
# plot only interactions with frequency of 2
subG2<-get.inducedSubgraph(emon$MtStHelens,
            eid=which(emon$MtStHelens\%e\%'Frequency'==2))
plot(subG2,edge.label='Frequency')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ graphs }
\keyword{ manip }% __ONLY ONE__ keyword per line
