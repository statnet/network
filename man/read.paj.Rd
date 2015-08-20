\name{read.paj}
\alias{read.paj}
\alias{read.paj.simplify}
\alias{switchArcDirection}
\alias{readAndVectorizeLine}
\title{Read a Pajek Project or Network File and Convert to an R 'Network' Object}
\usage{
read.paj(file, verbose = FALSE, debug = FALSE, edge.name = NULL,
    simplify = FALSE)
}
\description{
  Return a (list of) \code{\link{network}} object(s) after reading a corresponding .net or .paj file.  The code accepts ragged array edgelists, but cannot currently handle 2-mode,  multirelational (e.g. KEDS), or networks with entries for both edges and arcs (e.g. GD-a99m).
  See \code{network}, \code{statnet}, or \code{sna} for more information.
}
\arguments{
  \item{file}{the name of the file whence the data are to be read. If it does not contain an absolute path, the file name is relative to the current working directory (as returned by \code{\link{getwd}}).  \code{file} can also be a complete URL. }
  \item{verbose}{logical: Should longer descriptions of the reading and coercion process be printed out?}
  \item{debug}{logical: Should very detailed descriptions of the reading and coercion process be printed out? This is typically used to debug the reading of files that are corrupted on coercion.}
  \item{edge.name}{optional name for the edge variable read from the file. The default is to use the value in the project file if found.}
  \item{simplify}{Should the returned network be simplified as much as possible and saved? The values specifies the name of the file which the data are to be stored. If it does not contain an absolute path, the file name is relative to the current working directory (see \code{\link{getwd}}). If \code{specify} is TRUE the file name is the name \code{file}.}
}

\details{


If the \code{*Vertices} block includes the optional graphic attributes (coordinates, shape, size, etc.) they will be read attached to the network as vertex attributes but values will not be interperted (i.e. Pajek's color names will not be translated to R color names).  Vertex attributes included in a \code{*Vector} block will be attached as vertex attributes.  

Edges or Arc weights in the \code{*Arcs} or \code{*Edges} block are include in the network as an attribute with the same name as the network. If no weight is included, a default weight of 1 is used. Optional graphic attributes or labels will be attached as edge attributes.

If the file contains an empty \code{Arcs} block, an undirected network will be returned. Otherwise the network will be directed, with two edges (one in each direction) added for every row in the \code{*Edges} block. 


}

\references{
Batagelj, Vladimir and Mrvar, Andrej (2011) Pajek Reference Manual version 2.05 \url{http://vlado.fmf.uni-lj.si/pub/networks/pajek/doc/pajekman.pdf} Section 5.3 pp 73-79

Batageli, Vladimir (2008) "Network Analysis Description of Networks" \url{http://vlado.fmf.uni-lj.si/pub/networks/doc/ECPR/08/ECPR01.pdf}

Pajek Datasets \url{http://vlado.fmf.uni-lj.si/pub/networks/data/esna/}
}

\value{The structure of the object returned by \code{read.paj} depends on the contents of the file it parses.
\itemize{
  \item if input file contains information about a single 'network' object (i.e .net input file) a single network object is returned with attribute data set appropriately if possible.
  or a list of networks (for .paj input). 
  \item if input file contains multiple sets of relations for a single network, a list of network objects ('network.series') is returned, along with a formula object?.
  \item if input .paj file contains additional information (like partition information), or multiple \code{*Network} definitions a two element list is returned.  The first element is a list of all the network objects created, and the second is a list of partitions, etc.  (how are these matched up)
}
}

\author{ Dave Schruth \email{dschruth@u.washington.edu}, Mark S. Handcock \email{handcock@stat.washington.edu} (with additional input from Alex Montgomery \email{ahm@reed.edu}), Skye Bender-deMoll \email{skyebend@uw.edu}}
\seealso{ \code{\link{network}}}
\examples{

require(network)

par(mfrow=c(2,2))

test.net.1 <- read.paj("http://vlado.fmf.uni-lj.si/pub/networks/data/GD/gd98/A98.net")
plot(test.net.1,main=test.net.1\%n\%'title')

test.net.2 <- read.paj("http://vlado.fmf.uni-lj.si/pub/networks/data/mix/USAir97.net")
# plot using coordinates from the file in the file
plot(test.net.2,main=test.net.2\%n\%'title',
               coord=cbind(test.net.2\%v\%'x',
               test.net.2\%v\%'y'),
               jitter=FALSE)
               
# read .paj project file
# notice output has $networks and $partitions
read.paj('http://vlado.fmf.uni-lj.si/vlado/podstat/AO/net/Tina.paj')

}
\keyword{datasets}
