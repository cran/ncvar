\name{print.ncv}

\alias{print.ncv}

\title{Print Summary Information About a NetCDF Dataset}

\description{
  Print summary information about a NetCDF dataset.
}

\usage{print.ncv(x, \ldots)}

\arguments{
  \item{x}{Object of class "\code{var.ncv}" or "\code{att.ncv}".}
  \item{\ldots}{Arguments passed to or from other methods 
   (not used)}
}

\details{This function prints information about a NetCDF dataset. 
This includes a list of all dimensions and their length, a list of 
all variables and their attributes (including their values) and a 
list of all global attributes (including their values).

The output of this function is almost identical with a \code{"ncdump -h"} call.
}

\value{}

\references{\url{http://www.unidata.ucar.edu/packages/netcdf/}}

\author{Juerg Schmidli}

\examples{
## Create example file
foo.ncv()

## Read variable
temp <- var.get.ncv("/tmp/foo.nc", "temperature")

print.ncv(temp)
}

\keyword{file}
