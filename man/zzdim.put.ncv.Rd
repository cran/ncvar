\name{dim.put.ncv}

\alias{dim.put.ncv}

\title{Put NetCDF Dimension (internal)}

\description{Create a NetCDF dimension on file.
For internal use only.}

\usage{dim.put.ncv(ncfile, dim)}

\arguments{
  \item{ncfile}{Object of class "NetCDF", returned from 
	\code{\link[RNetCDF]{open.nc}}.}
  \item{dim}{Object of class "dim.ncv".}
}

\details{Create a NetCDF dimension on file.
For internal use only.}

\value{}

\author{Juerg Schmidli}

\examples{
## Create example file
foo.ncv()

## Open file
nc <- open.nc(paste(tempdir(),"/foo.nc",sep=""), write=TRUE)

## Get dimension (id=0)
dim <- dim.get.ncv(nc, 0)

## Write new dimension
dim$name <- "newdim"
dim.put.ncv(nc, dim)
 
close.nc(nc)

}

\keyword{internal}
