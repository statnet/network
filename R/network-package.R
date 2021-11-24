#' @useDynLib network, .registration = TRUE

#' @import utils
#' @importFrom grDevices colors gray
#' @importFrom graphics locator par plot polygon rect strheight strwidth text
#' @importFrom stats rnorm na.omit
#' @importFrom tibble tibble as.tibble as_tibble
#' @importFrom magrittr %>% set_names
NULL






#' Examples of how to load vertex and edge attributes into networks
#' 
#' Additional examples of how to manipulate network attributes using the
#' functions documented in \code{\link{attribute.methods}}
#' 
#' The \code{\link{attribute.methods}} documentation gives details about the
#' use of the specific network attribute methods such as
#' \code{get.vertex.attribute} and \code{set.edge.attribute}.  This document
#' gives examples of how to load in and attach attribute data, drawing heavily
#' on material from the Sunbelt statnet workshops
#' \url{https://github.com/statnet/Workshops/wiki}.
#' 
#' The examples section below give a quick overview of: 
#' \itemize{
#' \item Loading in a matrix
#' 
#' \item Attaching vertex attributes
#' 
#' \item Attaching edge atributes from a matrix
#' 
#' \item Loading in an edgelist
#' 
#' \item Attaching edge atributes from an edgelist
#' }
#' 
#' The \code{\link{read.table}} documentation provides more information about
#' reading data in from various tabular file formats prior to loading into a
#' network.  Note that the output is usually a \code{\link{data.frame}} object
#' in which each columns is represented as a \code{\link{factor}}.  This means
#' that in some cases when the output is directly loaded into a network the
#' variable values will appear as factor level numbers instead of text values.
#' The \code{stringsAsFactors=FALSE} flag may help with this, but some columns
#' may need to be converted using \code{as.numeric} or \code{as.character}
#' where appropriate.
#' 
#' @name loading.attributes
#'
#' @seealso \code{\link{attribute.methods}}, \code{\link{as.network.matrix}},
#' \code{\link{as.sociomatrix}}, \code{\link{as.matrix.network}},
#' \code{\link{network.extraction}}
#' @references Acton, R. M., Jasny, L (2012) \emph{An Introduction to Network
#' Analysis with R and statnet} Sunbelt XXXII Workshop Series, March 13, 2012.
#' 
#' Butts, C. T.  (2008).  \dQuote{network: a Package for Managing Relational
#' Data in R.} \emph{Journal of Statistical Software}, 24(2).
#' \url{https://www.jstatsoft.org/v24/i02/}
#' @keywords classes graphs
#' @examples
#' 
#' 
#' # read in a relational data adjacency matrix
#' 
#' # LOADING IN A MATRIX
#' \dontrun{
#' # can download matrix file from 
#' # https://statnet.csde.washington.edu/trac/raw-attachment/wiki/Resources/relationalData.csv
#' # and download vertex attribute file from
#' # https://statnet.csde.washington.edu/trac/raw-attachment/wiki/Resources/vertexAttributes.csv
#' 
#' # load in relation matrix from file
#' relations <- read.csv("relationalData.csv",header=FALSE,stringsAsFactors=FALSE)
#' 
#' # convert to matrix format from data frame
#' relations <- as.matrix(relations) 
#' 
#' # load in vertex attributes
#' nodeInfo <- read.csv("vertexAttributes.csv",header=TRUE,stringsAsFactors=FALSE)
#' }
#' \dontshow{
#' # since no access to file, creating it here
#' relations <- matrix(
#'                 c(0,0,0,1,1,1,0,0,0,
#'                   0,0,0,0,0,1,0,0,0,
#'                   0,0,0,0,0,0,1,0,1,
#'                   1,0,0,0,1,0,0,0,0,
#'                   1,0,0,1,0,0,0,0,0,
#'                   1,1,0,0,0,0,0,0,1,
#'                   0,0,1,0,0,0,0,0,1,
#'                   0,0,0,0,0,0,0,0,0,
#'                   0,0,1,0,0,1,1,0,0),ncol=9,byrow=TRUE)
#'                   
#' nodeInfo <- data.frame(
#'     name=c("Danielle","Josh","Mark","Emma","Sarah","Dave","Theresa","Carolyn","Gil"),
#'     age=c(44,44,40,32,33,36,38,42,30),
#'     sex=c("F","M","M","F","F","M","F","F","M"),    
#'     handed=c("R","R","R","L","R","L","L","R","L"),
#'     lastDocVisit=c(2012,2008,2010,2012,2011,2007,2009,2009,2010),
#'     stringsAsFactors=FALSE
#' )
#' }          
#'                   
#' print(relations) # peek at matrix 
#' print(nodeInfo)  # peek at attribute data
#' 
#' # Since our relational data has no row/column names, let's set them now
#' rownames(relations) <- nodeInfo$name
#' colnames(relations) <- nodeInfo$name
#' 
#' # create undirected network object from matrix
#' nrelations<-network(relations,directed=FALSE)
#' 
#' # it read in vertex names from matrix col names ...
#' network.vertex.names(nrelations)
#' 
#' # ATTACHING VERTEX ATTRIBUTES
#' 
#' # ... but could also set vertex.names with 
#' nrelations%v%'vertex.names'<- nodeInfo$name
#' 
#' # load in other attributes 
#' nrelations%v%"age" <- nodeInfo$age
#' nrelations%v%"sex" <- nodeInfo$sex
#' nrelations%v%"handed" <- nodeInfo$handed
#' nrelations%v%"lastDocVisit" <- nodeInfo$lastDocVisit
#' 
#' # Note: order of attributes in the data frame MUST match vertex ids
#' # otherwise the attribute will get assigned to the wrong vertex
#' 
#' # check that they got loaded
#' list.vertex.attributes(nrelations)
#' 
#' 
#' # what if we had an adjaceny  matrix like:
#' valuedMat<-matrix(c(1,2,3, 2,0,9.5,1,5,0),ncol=3,byrow=TRUE)
#' valuedMat
#' 
#' # make a network from it
#' valuedNet<-network(valuedMat,loops=TRUE,directed=TRUE)
#' 
#' # print it back out ...
#' as.matrix(valuedNet)
#' 
#' # wait, where did the values go!!?
#' 
#' # LOADING A MATRIX WITH VALUES
#' 
#' # to construct net from matrix with values:
#' valuedNet<-network(valuedMat,loops=TRUE,directed=TRUE,
#'             ignore.eval=FALSE,names.eval='myEdgeWeight')
#'             
#' # also have to specify the name of the attribute when converting to matrix
#' as.matrix(valuedNet,attrname='myEdgeWeight')
#' 
#' # ATTACHING EDGE ATTRIBUTES FROM A MATRIX
#' 
#' # maybe we have edge attributes of a different sort in another matrix like:
#' edgeAttrs<-matrix(c("B","Z","Q","W","A","E","L","P","A"),ncol=3,byrow=TRUE)
#' edgeAttrs
#' 
#' # we can still attach them
#' valuedNet<-set.edge.value(valuedNet,'someLetters',edgeAttrs)
#' 
#' # and extract them
#' as.matrix(valuedNet,attrname='someLetters')
#' valuedNet%e%'someLetters'
#' 
#' # but notice that some of the values didn't get used 
#' # the ("A"s are missing) because there were no corresponding edges (loops)
#' # for the attribute to be attached to
#' 
#' 
#' # ATTACHING EDGE ATTRIBUTES FROM A LIST
#' 
#' # it is also possible to attach edge attributes directly from a list
#' edgeCols<-c("red","green","blue","orange","pink","brown","gray")
#' valuedNet<-set.edge.attribute(valuedNet,"edgeColors",edgeCols)
#' 
#' # but this can be risky, because we may not know the ordering of the edges,
#' # (especially if some have been deleted).  Does "green" go with the edge from 
#' # 1 to 2, or from 3 to 1?
#' 
#' # Usually if the edge data is only availible in list form, it is safer to construct
#' # the network from an edgelist in the first place
#' 
#' # LOADING IN AN EDGELIST
#' 
#' # pretend we just loaded in this data.frame from a file
#' elData<-data.frame(
#'   from_id=c("1","2","3","1","3","1","2"),
#'   to_id=c("1", "1", "1", "2", "2", "3", "3"),
#'   myEdgeWeight=c(1, 2, 1, 2, 5, 3, 9.5),
#'   someLetters=c("B", "W", "L", "Z", "P", "Q", "E"),
#'   edgeCols=c("red","green","blue","orange","pink","brown","gray"),
#'   stringsAsFactors=FALSE
#' )
#' 
#' # peek at data
#' # each row corresponds to a relationship (edge) in the network
#' elData
#' 
#' # to make a network we just use the first two id columns
#' valuedNet2<-network(elData[,1:2],loops=TRUE)
#' 
#' # print it out
#' as.matrix(valuedNet2)
#' 
#' # has right edges, but no values
#' 
#' # to include values (with names from the columns)
#' 
#' valuedNet2<-network(elData,loops=TRUE)
#' list.edge.attributes(valuedNet2)
#' as.matrix(valuedNet2,attrname='someLetters')
#' 
#' 
NULL





#' Classes for Relational Data
#' 
#' Tools to create and modify network objects.  The network class can represent
#' a range of relational data types, and supports arbitrary vertex/edge/graph
#' attributes.
#' 
#' The \code{network} package provides tools for creation, access, and
#' modification of \code{network} class objects.  These objects allow for the
#' representation of more complex structures than can be readily handled by
#' other means (e.g., adjacency matrices), and are substantially more efficient
#' in handling large, sparse networks.  While the full capabilities of the
#' \code{network} class can only be exploited by means of the various custom
#' interface methods (see below), many simple tasks are streamlined through the
#' use of operator overloading; in particular, network objects can often be
#' treated as if they were adjacency matrices (a representation which will be
#' familiar to users of the \code{sna} package).  \code{network} objects are
#' compatible with the \code{sna} package, and are required for many packages
#' in the \code{statnet} bundle.
#' 
#' Basic information on the creation of \code{network} objects can be found by
#' typing \code{help(network)}.  To learn about setting, modifying, or deleting
#' network, vertex, or edge attributes, see \code{help(attribute.methods)}.
#' For information on custom network operators, type
#' \code{help(network.operators)}; information on overloaded operators can be
#' found via \code{help(network.extraction)}.  Additional help topics are
#' listed below.
#' 
#' \tabular{ll}{
#' Package: \tab network\cr
#' Version: \tab 1.14\cr
#' Date: \tab May 7, 2016\cr
#' Depends: \tab R (>= 2.10), utils\cr
#' Suggests: \tab sna, statnet.common (>= 3.1-0)\cr
#' License: \tab GPL (>=2)\cr
#' }
#' 
#' Index of documentation pages:
#' \preformatted{
#' add.edges               Add Edges to a Network Object
#' add.vertices            Add Vertices to an Existing Network
#' as.matrix.network       Coerce a Network Object to Matrix Form
#' as.network.matrix       Coercion from Matrices to Network Objects
#' as.sociomatrix          Coerce One or More Networks to Sociomatrix Form
#' attribute.methods       Attribute Interface Methods for the Network
#'                         Class
#' deletion.methods        Remove Elements from a Network Object
#' edgeset.constructors    Edgeset Constructors for Network Objects
#' emon                    Interorganizational Search and Rescue Networks
#'                         (Drabek et al.)
#' flo                     Florentine Wedding Data (Padgett)
#' get.edges               Retrieve Edges or Edge IDs Associated with a
#'                         Given Vertex
#' get.inducedSubgraph     Retrieve Induced Subgraphs and Cuts
#' get.neighborhood        Obtain the Neighborhood of a Given Vertex
#' is.adjacent             Determine Whether Two Vertices Are Adjacent
#' loading.attributes      Examples of how to load vertex and edge
#'                         attributes into networks
#' missing.edges           Identifying and Counting Missing Edges in a
#'                         Network Object
#' network                 Network Objects
#' network.arrow           Add Arrows or Segments to a Plot
#' network.density         Compute the Density of a Network
#' network.dyadcount       Return the Number of (Possibly Directed) Dyads
#'                         in a Network Object
#' network.edgecount       Return the Number of Edges in a Network Object
#' network.edgelabel       Plots a label corresponding to an edge in a
#'                         network plot.
#' network.extraction      Extraction and Replacement Operators for
#'                         Network Objects
#' network.indicators      Indicator Functions for Network Properties
#' network.initialize      Initialize a Network Class Object
#' network.layout          Vertex Layout Functions for plot.network
#' network.loop            Add Loops to a Plot
#' network.operators       Network Operators
#' network-package         Classes for Relational Data
#' network.size            Return the Size of a Network
#' network.vertex          Add Vertices to a Plot
#' permute.vertexIDs       Permute (Relabel) the Vertices Within a Network
#' plotArgs.network        Expand and transform attributes of networks to
#'                         values appropriate for aguments to plot.network
#' plot.network.default    Two-Dimensional Visualization for Network
#'                         Objects
#' prod.network            Combine Networks by Edge Value Multiplication
#' read.paj                Read a Pajek Project or Network File and
#'                         Convert to an R 'Network' Object
#' sum.network             Combine Networks by Edge Value Addition
#' valid.eids              Get the valid edge which are valid in a network
#' which.matrix.type       Heuristic Determination of Matrix Types for
#'                         Network Storage
#' }
#' 
#' 
#' @name network-package
#' @docType package
#' @author Carter T. Butts <buttsc@@uci.edu>, with help from Mark S. Handcock
#' <handcock@@stat.ucla.edu>, David Hunter <dhunter@@stat.psu.edu>, Martina
#' Morris <morrism@@u.washington.edu>, Skye Bender-deMoll
#' <skyebend@@u.washington.edu>, and Jeffrey Horner
#' <jeffrey.horner@@gmail.com>.
#' 
#' Maintainer: Carter T. Butts <buttsc@@uci.edu>
#' @keywords package
NULL





