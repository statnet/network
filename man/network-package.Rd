\name{network-package}
\alias{network-package}
\docType{package}
\title{
Classes for Relational Data
}
\description{
Tools to create and modify network objects.  The network
class can represent a range of relational data types, and
supports arbitrary vertex/edge/graph attributes.
}
\details{
The \code{network} package provides tools for creation, access, and modification of \code{network} class objects.  These objects allow for the representation of more complex structures than can be readily handled by other means (e.g., adjacency matrices), and are substantially more efficient in handling large, sparse networks.  While the full capabilities of the \code{network} class can only be exploited by means of the various custom interface methods (see below), many simple tasks are streamlined through the use of operator overloading; in particular, network objects can often be treated as if they were adjacency matrices (a representation which will be familiar to users of the \code{sna} package).  \code{network} objects are compatible with the \code{sna} package, and are required for many packages in the \code{statnet} bundle.

Basic information on the creation of \code{network} objects can be found by typing \code{help(network)}.  To learn about setting, modifying, or deleting network, vertex, or edge attributes, see \code{help(attribute.methods)}.  For information on custom network operators, type \code{help(network.operators)}; information on overloaded operators can be found via \code{help(network.extraction)}.  Additional help topics are listed below.

\tabular{ll}{
Package: \tab network\cr
Version: \tab 1.10\cr
Date: \tab May 22, 2014\cr
Depends: \tab R (>= 2.10), utils\cr
Suggests: \tab sna, statnet.common (>= 3.1-0)\cr
License: \tab GPL (>=2)\cr
}

Index of documentation pages:
\preformatted{
add.edges                    Add Edges to a Network Object
add.vertices                 Add Vertices to an Existing Network
as.matrix.network            Coerce a Network Object to Matrix Form
as.network.matrix            Coercion from Matrices to Network Objects
as.sociomatrix               Coerce One or More Networks to Sociomatrix Form
attribute.methods            Attribute Interface Methods for the Network Class
deletion.methods             Remove Elements from a Network Object
edgeset.constructors         Edgeset Constructors for Network Objects
emon                         Interorganizational Search and Rescue Networks (Drabek et
                             al.)
flo                          Florentine Wedding Data (Padgett)
get.edges                    Retrieve Edges or Edge IDs Associated with a Given Vertex
get.inducedSubgraph          Retrieve Induced Subgraphs and Cuts
get.neighborhood             Obtain the Neighborhood of a Given Vertex
is.adjacent                  Determine Whether Two Vertices Are Adjacent
loading.attributes           Examples of how to load vertex and edge attributes into
                             networks
missing.edges                Identifying and Counting Missing Edges in a Network Object
network                      Network Objects
network.arrow                Add Arrows or Segments to a Plot
network.density              Compute the Density of a Network
network.dyadcount            Return the Number of (Possibly Directed) Dyads in a
                             Network Object
network.edgecount            Return the Number of Edges in a Network Object
network.edgelabel            Plots a label corresponding to an edge in a network plot.
network.extraction           Extraction and Replacement Operators for Network Objects
network.indicators           Indicator Functions for Network Properties
network.initialize           Initialize a Network Class Object
network.layout               Vertex Layout Functions for plot.network
network.loop                 Add Loops to a Plot
network.operators            Network Operators
network-package              Classes for Relational Data
network.size                 Return the Size of a Network
network.vertex               Add Vertices to a Plot
permute.vertexIDs            Permute (Relabel) the Vertices Within a Network
plot.network.default         Two-Dimensional Visualization for Network Objects
prod.network                 Combine Networks by Edge Value Multiplication
read.paj                     Read a Pajek Project or Network File and Convert to an R
                             'Network' Object
sum.network                  Combine Networks by Edge Value Addition
valid.eids                   Get the valid edge which are valid in a network
which.matrix.type            Heuristic Determination of Matrix Types for Network
                             Storage
}
}
\author{
Carter T. Butts <buttsc@uci.edu>, with help from Mark S. Handcock <handcock@stat.ucla.edu>, David Hunter <dhunter@stat.psu.edu>, Martina Morris <morrism@u.washington.edu>, Skye Bender-deMoll <skyebend@u.washington.edu>, and Jeffrey Horner <jeffrey.horner@gmail.com>.

Maintainer: Carter T. Butts <buttsc@uci.edu>
}
\keyword{ package }
