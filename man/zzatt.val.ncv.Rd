\name{att.val.ncv}

\alias{att.val.ncv}

\title{Get NetCDF Attributes (internal)}

\description{Get the value of an attribute. For internal use only.}

\usage{att.val.ncv(att, name)}

\arguments{
  \item{att}{A list of objects of class "att.ncv".}
  \item{name}{The name of the attribute. }
}

\details{Get the value of an attribute. For internal use only.}

\value{The attribute value. \code{NULL} if the attributes does not exist.}

\author{Juerg Schmidli}

\examples{
## Create example file
foo.ncv()

## Open file
nc <- open.nc("/tmp/foo.nc")

## Get attribute
att <- att.get.ncv(nc, "temperature", 1)
att.val.ncv(att, "missing_value")

close.nc(nc)
}

\keyword{internal}
