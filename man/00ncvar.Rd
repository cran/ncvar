\name{ncvar}

\alias{ncvar}

\title{High-level R Interface to NetCDF Datasets}

\description{
This package provides a high-level R interface to Unidata's NetCDF
data files. Using this package NetCDF datasets, and all their
associated metadata, can be read and written in one go. It is also
easy to create datasets including lots of metadata.
This package supports both the CF and the default NetCDF (user guide)
metadata conventions. This package
supports more general NetCDF files and conventions than the \pkg{ncdf}
package by David Pierce. It uses the low-level NetCDF package
\pkg{RNetCDF} by Pavel Michna.

NetCDF is an abstraction that supports a view of data as a collection of 
self-describing, portable objects that can be accessed through a simple 
interface. Array values may be accessed directly, without knowing details of 
how the data are stored. Auxiliary information about the data, such as what 
units are used, may be stored with the data. Generic utilities and 
application programs can access NetCDF datasets and transform, combine, 
analyze, or display specified fields of the data.

The NetCDF Climate and Forecast (CF) Metadata Conventions are designed to 
promote the processing and sharing of files created with the NetCDF API.
The conventions define metadata that provide a definitive description of 
what the data in each variable represents, and of the spatial and temporal 
properties of the data. This enables users of data from different sources 
to decide which quantities are comparable, and facilitates building 
applications with powerful extraction, regridding, and display capabilities.
The CF conventions generalize and extend the COARDS conventions. 

See \code{\link[ncvar]{examples}} for some examples of using the 
package.
}

\note{This package requires the \pkg{RNetCDF} package by Pavel Michna.}

\author{Juerg Schmidli}

\references{
\url{http://www.unidata.ucar.edu/packages/netcdf/}
\cr\url{http://www.cgd.ucar.edu/cms/eaton/cf-metadata/}  
}

\keyword{file}
