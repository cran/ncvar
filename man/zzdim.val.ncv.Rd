\name{dim.val.ncv}

\alias{dim.val.ncv}

\title{Get NetCDF Dimension Lengths (internal)}

\description{Get the lengths of all NetCDF dimensions for
a given variable. For internal use only.}

\usage{dim.val.ncv(dim)}

\arguments{
  \item{dim}{Object of class "dim.ncv".}
}

\details{Return a vector containing the lengths of all NetCDF dimensions for
a given variable. For internal use only.}

\value{A vector of dimension lengths.}

\author{Juerg Schmidli}

\examples{
## Define some dimensions
dim <- dim.def.ncv(dimlist=list("lon", 5, "lat", 10, "height", 30) )
len <- dim.val.ncv(dim)

}

\keyword{internal}
