\name{att.def.ncv}

\alias{att.def.ncv}

\title{Define NetCDF Attribute(s)}

\description{Defines an attribute object containing one or more attributes.}

\usage{att.def.ncv(name, value, xtype=NULL, attlist=NULL)}

\arguments{
  \item{name}{The name of the attribute.}
  \item{value}{The attribute value. This can be either a single numeric value
    or a vector of numeric values, or a character string.}
  \item{xtype}{One of the predefined NetCDF external data types 
    (\code{NC_BYTE}, \code{NC_CHAR}, \code{NC_SHORT}, \code{NC_INT}, 
    \code{NC_FLOAT}, \code{NC_DOUBLE}).
    If no type is provided, \code{NC_CHAR}, \code{NC_INT},
    or \code{NC_FLOAT} is chosen depending on the type of \code{value} .}
  \item{attlist}{A list of attributes in the format "name", value,
    "name", value, etc. The attribute's external type is determined from 
    its value.}
}

\details{This function defines a new attribute object. This function
is normally not called by the user.}

\value{Object or list of objects of class "att.ncv"}

\author{Juerg Schmidli}

\examples{
## Define some attributes

att1 <- att.def.ncv(attlist=list("long_name", "precipitation", "units",
    "mm d-1", "_FillValue", -998.9, "grid_mapping", "rotated_pole") )

att2 <- att.def.ncv("_FillValue", -998.9, xtype = "NC_DOUBLE")

att3 <- att.def.ncv(attlist=list("long_name", "precipitation", 
    "units", "mm d-1") )

## Add an attribute object to a list of attribute objects
att4 <- append(att3, list(att2))

print.ncv(att1)
print.ncv(att4)

}

\keyword{file}
