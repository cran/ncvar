\name{xtype.ncv}

\alias{xtype.ncv}

\title{External NetCDF Data Type (internal)}

\description{Determine the default external NetCDF data type.
For internal use only.}

\usage{xtype.ncv(value)}

\arguments{
  \item{value}{An R object.}
}

\details{Determine the default external NetCDF data type.
The default types are \code{NC_INT}, \code{NC_FLOAT}, and
\code{NC_CHAR}.
For internal use only.}

\value{A string specifying the external data type.}

\author{Juerg Schmidli}

\examples{
    val <- vector(1, mode="integer")
    print(xtype.ncv(val))

    val <- vector(2, mode="numeric")
    print(xtype.ncv(val))

    val <- vector(3, mode="character")
    print(xtype.ncv(val))
}

\keyword{internal}
