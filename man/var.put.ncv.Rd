\name{var.put.ncv}

\alias{var.put.ncv}

\title{Put a NetCDF Variable}

\description{Put data and associated metadata of a NetCDF variable.}

\usage{var.put.ncv(path, var, new=TRUE, define=TRUE, data=TRUE,
	recursion=0, verbose=FALSE)}

\arguments{
  \item{path}{Filename of the NetCDF file to be created/opened.}
  \item{var}{The variable object (class "\code{var.ncv}"), as
    returned from \code{\link[ncvar]{var.def.ncv}}.}
  \item{new}{Set to \code{TRUE} if a new file should be created,
    otherwise an exisiting file will be opened for writing.}
  \item{define}{If \code{TRUE}, define the data and associated metadata 
    (NetCDF define mode).
    Set to \code{FALSE}, if the variables already have been defined.}
  \item{data}{If \code{TRUE}, write the data to the file (NetCDF data mode).
    Set to \code{FALSE}, if no data should be written.}
  \item{recursion}{For internal use only.}
  \item{verbose}{For internal use only.}
}

\details{
This function writes the data and and associated metadata of a
variable to a NetCDF file. Type conversion is done by the NetCDF library 
itself. Special treatment is necessary for the R type \code{character}.
When writing values 
of type \code{NC_CHAR}, it is mandatory that the first element of 
\code{count} contains the value of this dimension's length 
(usually \code{max_string_length}), the maximum string length is given 
by this value. R arrays of type \code{character} need therefore one 
additional dimension when written to a NetCDF dataset.

Values of \code{NA} are supported if the variable's missing value 
attribute (\code{missing\_value} or \code{_FillValue}) is set. They are converted to the 
corresponding value before written to disk.

Data in a NetCDF file is conceived as being a multi-dimensional array. 
The number and length of dimensions is determined when the variable is 
created. The \code{start} and \code{count} indices that this routine takes 
indicate where the writing starts along each dimension, and the count of 
values along each dimension to write. 

Note that the order of dimensions is consistent with the R conventions
(the first dimension varies fastest), but opposite to the CDL conventions.
}

\value{}

\references{\url{http://www.unidata.ucar.edu/packages/netcdf/}}

\note{\code{NC_BYTE} is always interpreted as signed.}

\author{Juerg Schmidli}

\examples{
##  Copy data from one file to another

foo.ncv()
temp <- var.get.ncv("/tmp/foo.nc", "temperature", mode="cf")
var.put.ncv("/tmp/foo2.nc", temp)
}

\keyword{file}
