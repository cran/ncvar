\name{att.put.ncv}

\alias{att.put.ncv}

\title{Put NetCDF Attributes (internal)}

\description{Writes all attributes for a given variable. For internal
use only.}

\usage{att.put.ncv(ncfile, variable, att)}

\arguments{
  \item{ncfile}{Object of class "NetCDF", returned from \code{\link[RNetCDF]{open.nc}}.}
  \item{variable}{ID or name of the variable for which the attributes will be
    written, or \code{NC_GLOBAL} for the global attributes.}
  \item{att}{List of objects of class "att.ncv".}
}

\details{Writes all attributes for a given variable. For internal
use only.}

\value{}

\author{Juerg Schmidli}

\examples{
}

\keyword{internal}
