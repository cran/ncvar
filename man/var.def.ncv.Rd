\name{var.def.ncv}

\alias{var.def.ncv}

\title{Define a NetCDF Variable }

\description{Define a new NetCDF variable object.}

\usage{var.def.ncv(name, data=NULL, xtype=NULL, start=NA, count=NA, 
    dim=NULL, att=NULL, mvar=NULL, gatt=NULL, coord=FALSE, unlim=FALSE)}

\arguments{
  \item{name}{Variable name. Must begin with an alphabetic character, 
    followed by zero or more alphanumeric characters including the underscore 
    ("\code{_}"). Case is significant.}
  \item{data}{The (multidimensional) array containing the data to write.}
  \item{xtype}{One of the predefined NetCDF external data types
    (\code{NC_BYTE}, \code{NC_CHAR}, \code{NC_SHORT}, \code{NC_INT},
    \code{NC_FLOAT}, \code{NC_DOUBLE}).
    If none is provided, \code{NC_CHAR}, \code{NC_INT},
    or \code{NC_FLOAT} is chosen depending on the type of \code{value} .}

  \item{start}{A vector of indices (1-based) indicating where to start
    writing the passed data. The length of this vector must equal the number
    of dimensions of the variable.}
  \item{count}{A vector of integers indicating the number of values to write
    along each dimension. The length of this vector must equal the number
    of dimensions of the variable.}
  
  \item{dim}{A list of dimension/coordinate objects (class "dim.ncv" or 
    "coord.ncv"), as returned from \code{\link[ncvar]{dim.def.ncv}} or 
    \code{\link[ncvar]{coord.def.ncv}}, respectively.}
  \item{att}{A list of attribute objects (class "att.ncv"), as returned from
    \code{\link[ncvar]{att.def.ncv}}.}
  \item{mvar}{A list of variable objects (class "var.ncv") as returned from
    \code{\link[ncvar]{dim.def.ncv}}.}
  \item{gatt}{A list of attribute objects (class "att.ncv"), as returned from
    \code{\link[ncvar]{att.def.ncv}}.}

  \item{coord}{For internal use only.}
  \item{unlim}{For internal use only.}
}

\details{This function creates a new variable object including all its 
associated metadata. Apart from the mandatory dimensions and coordinates,
the variables metadata may include attributes and further NetCDF variables
such as grid mappings, labels, auxillary coordinate variables, cell boundaries,
cell measures, and cell methods.
}

\value{An object of class "\code{var.ncv}".}

\author{Juerg Schmidli}

\references{
  \url{http://www.unidata.ucar.edu/packages/netcdf/}
\cr\url{http://www.cgd.ucar.edu/cms/eaton/cf-metadata/}
}

\examples{
## define coordinate variables
rlon <- coord.def.ncv("rlon", seq(1,10), xtype="NC_FLOAT",
    att=list("axis", "X",
            "standard_name", "grid_longitude",
            "long_name", "longitude in rotated pole grid",
            "units", "degrees") )
rlat <- coord.def.ncv("rlat", 1.*seq(1,5),
    att=list("axis", "Y",
            "standard_name", "grid_latitude",
            "long_name", "latitude in rotated pole grid",
            "units", "degrees") )
hgt <- coord.def.ncv("hgt", 0.,
    att=list("axis", "Z", "long_name", "altitude", "units", "metre",
            "positive", "up") )
time <- coord.def.ncv("time", 0., 
    att=list("axis", "T", "calendar", "standard", "long_name", "time",
            "units", "days since 1979-01-01 00:00:00.0"), unlim=TRUE)

## define grid mapping variable
#gmap <- var.def.ncv("rotated_pole", 0.,
#    att=list("grid_mapping_name", "rotated_latitude_longitude",
#            "grid_north_pole_longitude", -170.,
#            "grid_north_pole_latitude", 32.5) )

## define data variable
pre <- var.def.ncv("precip", array(1., dim=c(10,5,1,1)), xtype="NC_FLOAT",
    dim=list(rlon, rlat, hgt, time),
    att=list("long_name", "precipitation", "units", "mm d-1",
            "_FillValue", -998.9, "grid_mapping", "rotated_pole"))

## write to file
var.put.ncv(paste(tempdir(),"/foo.nc",sep=""), pre)

}

\keyword{file}
