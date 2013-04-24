\name{network-internal}
\title{Internal Network Package Functions}

%\alias{DistMatrix}
\alias{as.color}
\alias{is.color}
\alias{is.discrete}
\alias{is.discrete.character}
\alias{is.discrete.numeric}
\alias{+}
\alias{-}
\alias{*}
\alias{+.default}
\alias{-.default}
\alias{*.default}
\alias{summary.character}
\alias{print.summary.character}
\alias{networkOperatorSetup}
\alias{mixingmatrix}
\alias{print.mixingmatrix}

\description{
  Internal network functions.
}
\usage{
as.color(x)
is.color(x)
is.discrete(x)
is.discrete.character(x)
is.discrete.numeric(x)
mixingmatrix(nw, attrname)
networkOperatorSetup(x,y=NULL)
}
\details{
  Most of these are not to be called by the user.
}
\arguments{
 \item{x}{an object to be designated either discrete or continuous, or a network.}
 \item{attrname}{a vertex attribute name.}
 \item{y}{a network or something coercible to one.}
%\item{\dots}{further arguments passed to or used by methods.}
}
\seealso{network}
\keyword{internal}
