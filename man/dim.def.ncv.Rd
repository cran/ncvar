\name{dim.def.ncv}

\alias{dim.def.ncv}

\title{Define NetCDF Dimension(s)}

\description{Defines a dimension object containing one or more dimensions.}

\usage{dim.def.ncv(name, value, unlim=FALSE, dimlist=NULL)}

\arguments{
  \item{name}{The name of the dimension.}
  \item{value}{The dimension length. That is the number of values along
    this dimension. This must be a positive integer.}
  \item{unlim}{Set to \code{TRUE} if an unlimited dimension should be
    created, otherwise \code{FALSE}.}
  \item{dimlist}{A list of dimensions in the format "name", value,
    "name", value, etc.}
}

\details{This function defines a new dimension object. This function
is normally not called by the user.}

\value{Object or list of objects of class "dim.ncv".}

\author{Juerg Schmidli}

\examples{
## Define some dimensions
dims <- dim.def.ncv(dimlist=list("dimx", 10, "dimy", 20, "time", 2,
	    "max_string_length", 30) )
time <- dim.def.ncv(name="time", unlim=TRUE)
dims[[3]] <- time

}

\keyword{file}
