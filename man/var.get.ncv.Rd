\name{var.get.ncv}

\alias{var.get.ncv}

\title{Get a NetCDF Variable}

\description{Get data and associated metadata of a NetCDF variable.}

\usage{var.get.ncv(path, name, start=NA, count=NA, mode="attonly", 
    data=TRUE, gatts=FALSE, coord=FALSE, recursion=0, verbose=FALSE)}

\arguments{
  \item{path}{Filename of the NetCDF file to be opened.} 
  \item{name}{Name of the variable.}
  \item{start}{A vector of indices indicating where to start reading the 
  values (beginning at 1). The length of this vector must equal the number 
  of dimensions the variable. If not specified (\code{start=NA}), the 
  entire variable is read.}
  \item{count}{A vector of integers indicating the number of values to read 
  along each dimension. The length of this vector must equal the number of 
  dimensions the variable. If not specified (\code{count=NA}), the entire 
  variable is read.}

  \item{mode}{the read mode, determines which metadata is read.
  Currently the following modes are supported: "nometa", no additional
  metadata is returned; "attonly", the variables attributes are returned;
  "netcdf", the coordinate variables as defined
  by the NetCDF user guide and associated attributes are returned;
  "cf", all coordinate related variables and associated attributes are returned (coordinate and auxillary
  coordinate variables, grid mapping variables); "cf-full", all metadata
  associated with the variable as defined in the CF-conventions is
  returned.}

  \item{data}{Set to \code{FALSE}, if only metadata should be read.}
  \item{gatts}{Set to \code{TRUE}, if global attributes should
    be read.}
  \item{coord}{For internal use only.}
  \item{recursion}{For internal use only.}
  \item{verbose}{For internal use only.}
}

\details{This function returns the data and associated metadata of a 
variable. If the read procedure fails (e.g., no variable with the 
corresponding name), \code{NULL} is returned. Returned data are either 
of type R integer or R double precision.

Values of \code{NA} are supported; values in the data file that match the 
variable's missing value attribute (\code{_FillValule}) are automatically 
converted to \code{NA} before being returned to the user. 

Data in a NetCDF file is conceived as being a multi-dimensional array. 
The number and length of dimensions is determined when the variable is 
created. The \code{start} and \code{count} indices that this routine takes 
indicate where the reading starts along each dimension, and the count of 
values along each dimension to read. 

Note that the order of dimensions is consistent with the R conventions
(the first dimension varies fastest), but opposite to the CDL conventions.
}

\value{An object of class "\code{var.ncv}", including the variables data
and metadata.}

\references{\url{http://www.unidata.ucar.edu/packages/netcdf/}}

\author{Juerg Schmidli}

\examples{
## Reads a variable and associated metadata from the file created with
## foo.ncv()

foo.ncv()
pre <- var.get.ncv("/tmp/foo.nc", "temperature")

}

\keyword{file}
