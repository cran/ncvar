\name{att.get.ncv}

\alias{att.get.ncv}

\title{Get NetCDF Attributes (internal)}

\description{Get all attributes for a given variable. For internal
use only.}

\usage{att.get.ncv(ncfile, variable, natts)}

\arguments{
  \item{ncfile}{Object of class "NetCDF", returned from \code{\link[RNetCDF]{open.nc}}.}
  \item{variable}{ID or name of the variable from which the attribute will be
    read, or \code{NC_GLOBAL} for the global attributes.}
  \item{natts}{The number of attributes to be read.}
}

\details{Get all attributes for a given variable. For internal
use only.}

\value{List of objects of class "att.ncv". An empty list if no attributes exist.}

\author{Juerg Schmidli}

\examples{
## Create example file
foo.ncv()

## Open file
nc <- open.nc("/tmp/foo.nc")

## Get attribute
att <- att.get.ncv(nc, "temperature", 1)

close.nc(nc)
}

\keyword{internal}
