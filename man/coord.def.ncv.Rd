\name{coord.def.ncv}

\alias{coord.def.ncv}

\title{Define a NetCDF Coordinate}

\description{Define a new NetCDF coordinate variable object.}

\usage{coord.def.ncv(name, data=NULL, xtype=NULL, att=NULL, mvar=NULL, 
    unlim=FALSE)}

\arguments{
  \item{name}{Variable name. Must begin with an alphabetic character, 
    followed by zero or more alphanumeric characters including the underscore 
    ("\code{_}"). Case is significant.}

  \item{data}{The (onedimensional) array containing the coordinate values.}

  \item{xtype}{One of the predefined numeric NetCDF external data types
    (\code{NC_BYTE}, \code{NC_SHORT}, \code{NC_INT}, \code{NC_FLOAT}, 
    \code{NC_DOUBLE}).
    If none is provided, the type is determined automatically from
    \code{value} to one of the follwoing \code{NC_INT}, or \code{NC_FLOAT}.}

  \item{unlim}{Set to \code{TRUE} if an unlimited dimension should be
    created, otherwise to \code{FALSE}.}
 
  \item{att}{A list of attribute objects (class "att.ncv"), as returned from
    \code{\link[ncvar]{att.def.ncv}}.}

  \item{mvar}{A list of variable objects (class "var.ncv") as returned from
    \code{\link[ncvar]{var.def.ncv}}.}
}

\details{This function creates a new NetCDF coordinate variable, that is
an object of class "coord.ncv". A NetCDF coordinate variable is a
one-dimensional variable with the same name as its dimension. See 
\code{\link[ncvar]{var.def.ncv}} for further information on NetCDF
variables.
}

\value{An object of class "coord.ncv".}

\author{Juerg Schmidli}

\examples{
## define some coordinate variables
lon <- coord.def.ncv("lon", seq(1,10), xtype="NC_FLOAT",
    att=list("axis", "X", "long_name", "longitude", "units", "degrees_east") )
lat <- coord.def.ncv("lat", 1.*seq(1,5),
    att=list("axis", "Y", "long_name", "latitude", "units", "degrees_north") )
hgt <- coord.def.ncv("hgt", 0.,
    att=list("axis", "Z", "long_name", "altitude", "units", "metre",
            "positive", "up") )
time <- coord.def.ncv("time", 0., 
    att=list("axis", "T", "calendar", "standard", "long_name", "time",
            "units", "days since 1979-01-01 00:00:00.0"), unlim=TRUE)

## define data variable
pre <- var.def.ncv("precip", array(1,dim=c(10,5,1,1)), xtype="NC_FLOAT",
    dim=list(lon, lat, hgt, time),
    att=list("long_name", "precipitation", "units", "mm d-1",
            "_FillValue", -998.9) )

## write to file
var.put.ncv("/tmp/foo.nc", pre, new=TRUE)

}

\keyword{file}
