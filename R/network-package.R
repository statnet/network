#' @useDynLib network, .registration = TRUE

#' @import utils
#' @importFrom grDevices colors gray
#' @importFrom graphics locator par plot polygon rect strheight strwidth text
#' @importFrom stats rnorm na.omit
#' @importFrom tibble tibble as.tibble as_tibble
#' @importFrom magrittr %>% set_names
NULL


#' Interorganizational Search and Rescue Networks (Drabek et al.)
#' 
#' Drabek et al. (1981) provide seven case studies of emergent
#' multi-organizational networks (EMONs) in the context of search and rescue
#' (SAR) activities.  Networks of interaction frequency are reported, along
#' with several organizational attributes.
#' 
#' All networks collected by Drabek et al. reflect reported frequency of
#' organizational interaction during the search and rescue effort; the (i,j)
#' edge constitutes i's report regarding interaction with j, with non-adjacent
#' vertices reporting no contact.  Frequency is rated on a four-point scale,
#' with 1 indicating the highest frequency of interaction.  (Response options:
#' 1=\dQuote{continuously}, 2=\dQuote{about once an hour}, 3=\dQuote{every few
#' hours}, 4=\dQuote{about once a day or less}) This is stored within the
#' \code{"Frequency"} edge attribute.
#' 
#' For each network, several covariates are recorded as vertex attributes:
#' 
#' \describe{
#' \item{Command.Rank.Score}{ Mean (reversed) rank for the
#'   prominence of each organization in the command structure of the response, as
#'   judged by organizational informants.}
#' \item{Decision.Rank.Score}{ Mean (reversed) rank for the 
#'   prominence of each organization in decision making
#'   processes during the response, as judged by organizational informants.}
#' \item{Formalization}{ An index of organizational formalization, ranging from
#'   0 (least formalized) to 4 (most formalized).} \item{Localization}{ For each
#'   organization, \code{"L"} if the organization was sited locally to the impact
#'   area, \code{"NL"} if the organization was not sited near the impact area,
#'   \code{"B"} if the organization was sited at both local and non-local
#'   locations.}
#' \item{Paid.Staff}{ Number of paid staff employed by each
#'   organization at the time of the response.}
#' \item{Sponsorship}{ The level at which each organization
#'   was sponsored (e.g., \code{"City"}, \code{"County"},
#'   \code{"State"}, \code{"Federal"}, and \code{"Private"}).}
#' \item{vertex.names}{ The identity of each organization.}
#' \item{Volunteer.Staff}{ Number of volunteer staff employed by each
#'   organization at the time of the response.}
#' }
#' 
#' Note that where intervals were given by the original source, midpoints have
#' been substituted.  For detailed information regarding data coding and
#' procedures, see Drabek et al. (1981).
#' 
#' @name emon
#' @docType data
#' @usage data(emon)
#' @format A list of 7 \code{\link{network}} objects:
#' 
#'  \tabular{rlll}{
#'    `[[1]]` \tab Cheyenne     \tab network \tab Cheyenne SAR EMON\cr
#'    `[[2]]` \tab HurrFrederic \tab network \tab Hurricane Frederic SAR EMON\cr
#'    `[[3]]` \tab LakePomona   \tab network \tab Lake Pomona SAR EMON\cr
#'    `[[4]]` \tab MtSi         \tab network \tab Mt. Si SAR EMON\cr
#'    `[[5]]` \tab MtStHelens   \tab network \tab Mt. St. Helens SAR EMON\cr
#'    `[[6]]` \tab Texas        \tab network \tab Texas Hill Country SAR EMON\cr
#'    `[[7]]` \tab Wichita      \tab network \tab Wichita Falls SAR EMON
#'  }
#' 
#' Each network has one edge attribute:
#' 
#' \tabular{lll}{ Frequency \tab numeric \tab Interaction frequency (1-4;
#' 1=most frequent) }
#' 
#' Each network also has 8 vertex attributes:
#' 
#'  \tabular{lll}{
#'   Command.Rank.Score  \tab numeric   \tab Mean rank in the command structure\cr
#'   Decision.Rank.Score \tab numeric   \tab Mean rank in the decision process\cr
#'   Formalization       \tab numeric   \tab Degree of formalization\cr
#'   Location            \tab character \tab Location code\cr
#'   Paid.Staff          \tab numeric   \tab Number of paid staff\cr
#'   Sponsorship         \tab character \tab Sponsorship type\cr
#'   vertex.names        \tab character \tab Organization name\cr
#'   Volunteer.Staff     \tab numeric   \tab Number of volunteer staff
#'  }
#'  
#' @seealso \code{\link{network}}
#' @source Drabek, T.E.; Tamminga, H.L.; Kilijanek, T.S.; and Adams, C.R.
#' (1981).  \emph{Data from Managing Multiorganizational Emergency Responses:
#' Emergent Search and Rescue Networks in Natural Disaster and Remote Area
#' Settings.} Program on Technology, Environment, and Man Monograph 33.
#' Institute for Behavioral Science, University of Colorado.
#' @keywords datasets
#' @examples
#' 
#' data(emon)   #Load the emon data set
#' 
#' #Plot the EMONs
#' par(mfrow=c(3,3))
#' for(i in 1:length(emon))
#'   plot(emon[[i]],main=names(emon)[i],edge.lwd="Frequency")
#' 
NULL





#' Florentine Wedding Data (Padgett)
#' 
#' This is a data set of Padgett (1994), consisting of weddings among leading
#' Florentine families.  This data is stored in symmetric adjacency matrix
#' form.
#' 
#' @name flo
#' @usage data(flo)
#' @seealso \code{\link{network}}
#' @references Wasserman, S. and Faust, K. (1994) \emph{Social Network
#' Analysis: Methods and Applications}, Cambridge: Cambridge University Press.
#' @source Padgett, John F.  (1994). \dQuote{Marriage and Elite Structure in
#' Renaissance Florence, 1282-1500.} Paper delivered to the Social Science
#' History Association.
#' @keywords datasets
#' @examples
#' 
#' data(flo)
#' nflo<-network(flo,directed=FALSE)    #Convert to network object form
#' all(nflo[,]==flo)                    #Trust, but verify
#'                                      #A fancy display:
#' plot(nflo,displaylabels=TRUE,boxed.labels=FALSE,label.cex=0.75)
#' 
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





