\name{dim.get.ncv}

\alias{dim.get.ncv}

\title{Get NetCDF Dimensions (internal)}

\description{Get a dimension of a given variable. 
For internal use only.}

\usage{dim.get.ncv(ncfile, dimension)}

\arguments{
  \item{ncfile}{Object of class "NetCDF", returned from 
    \code{\link[RNetCDF]{open.nc}}.}
  \item{dimension}{ID or name of the dimension.}
}

\details{Get a dimension of a given variable.
For internal use only.}

\value{Object of class "dim.ncv". \code{NULL} for a scalar variable.}

\author{Juerg Schmidli}

\examples{
## Create example file
foo.ncv()

## Open file
nc <- open.nc("/tmp/foo.nc")

## Get dimension (id=0)
dim <- dim.get.ncv(nc, 0)

close.nc(nc)

}

\keyword{internal}
