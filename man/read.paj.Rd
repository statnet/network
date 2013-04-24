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
  Return a (list of) \code{\link{network}} object(s) after reading a corresponding .net or .paj file.  The code accepts ragged array edgelists, but cannot currently handle 2-mode,  multirelational (e.g. KEDS), or networks with entries for both edges and arcs (e.g. GD-a99m).  Also, this archive currently only contains networks under 10,000 nodes.
  See \code{network}, \code{statnet}, or \code{sna} for more information.
}
\arguments{
  \item{file}{the name of the file whence the data are to be read. If it does not contain an absolute path, the file name is relative to the current working directory (as returned by \code{\link{getwd}}).  \code{file} can also be a complete URL. }
  \item{verbose}{logical: Should longer descriptions of the reading and coercion process be printed out?}
  \item{debug}{logical: Should very detailed descriptions of the reading and coercion process be printed out? This is typically used to debug the reading of files that are corrupted on coercion.}
  \item{edge.name}{optional name for the edge variable read from the file. The default is to use the value in the project file.}
  \item{simplify}{Should the returned network be simplified as much as possible and saved? The values specifies the name of the file which the data are to be stored. If it does not contain an absolute path, the file name is relative to the current working directory (see \code{\link{getwd}}). If \code{specify} is TRUE the file name is the name \code{file}.}
}

\value{\code{read.paj} returns a 'network' object (for .net input) or a list of networks (for .paj input). Additional information in the .paj file (like partition information) is attached to the network objects in another higher order list.}

\author{ Dave Schruth \email{dschruth@u.washington.edu}, Mark S. Handcock \email{handcock@stat.washington.edu}}
\seealso{\code{\link[statnet]{statnet}}, \code{\link{network}}, \code{\link[networkdata]{networkdata}}}
\examples{
\dontrun{
require(network)

par(mfrow=c(2,2))

test.net.1 <- read.paj("http://vlado.fmf.uni-lj.si/pub/networks/data/GD/gd98/A98.net")
plot(test.net.1,main=test.net.1$gal$title)

test.net.2 <- read.paj("http://vlado.fmf.uni-lj.si/pub/networks/data/mix/USAir97.net")
plot(test.net.2,main=test.net.2$gal$title)
}
}
\keyword{datasets}
