\name{over-methods}
\docType{methods}
\alias{over,xts,xts-method}
\alias{over,STF,STF-method}
\alias{over,STS,STF-method}
\alias{over,STI,STF-method}
\alias{over,STF,STS-method}
\alias{over,STS,STS-method}
\alias{over,STI,STS-method}
\alias{over,STF,STI-method}
\alias{over,STS,STI-method}
\alias{over,STI,STI-method}
\alias{over,STF,STFDF-method}
\alias{over,STS,STFDF-method}
\alias{over,STI,STFDF-method}
\alias{over,STF,STSDF-method}
\alias{over,STS,STSDF-method}
\alias{over,STI,STSDF-method}
\alias{over,STF,STIDF-method}
\alias{over,STS,STIDF-method}
\alias{over,STI,STIDF-method}
\alias{aggregate.ST}
\title{ consistent spatio-temporal overlay for objects inheriting from ST }
\description{ consistent spatio-temporal overlay for STF, STS and STI
objects, as well as their *DF counterpart: 
retrieves the indexes or attributes from one geometry at the spatio-temporal
points of another }
\usage{
\S4method{over}{STF,STF}(x, y, returnList = FALSE, fn = NULL, ..., timeInterval = FALSE)
\method{aggregate}{ST}(x, by, FUN = mean, \dots)
}
\arguments{
\item{x}{geometry (S/T locations) of the queries}
\item{y}{layer from which the geometries or attributes are queried}
\item{returnList}{ logical; determines whether a list is returned, 
or an index vector }
\item{timeInterval}{logical; if TRUE, time will considered as interval, otherwise
as instance; but read note below }
\item{fn}{(optional) a function; see value }
\item{by}{ geometry over which attributes in \code{x} are aggregated }
\item{FUN}{aggregation function }
\item{...}{arguments passed on to function fn or FUN}
}
\value{ 
an object of length \code{length(x)}, or a data.frame with number
of rows equal to \code{length(x)}. If \code{returnList} is FALSE,
a vector with indices of \code{y} for each geometry (point, grid
cell centre, polygon or lines x time point) in \code{x}.  if \code{returnList} is
TRUE, a list of length \code{length(x)}, with list element \code{i}
the vector of indices of the geometries in \code{y} that correspond
to the $i$-th geometry in \code{x}.

Function aggregate.ST aggregates the attribute values of \code{x}
over the geometry of \code{by}, using aggregation function FUN.
} 
\section{Methods}{
\describe{
\item{x = "STF", y = "STF"}{ }
}
}

\note{
Argument \code{timeInterval} needs to be \code{FALSE} when
\code{y=STI} calls, as a straightforward implementation when time
is interval was not found for these objects.

See also \link[sp]{over}; methods involving SpatialLines or SpatialPolygons with
SpatialPolygons need rgeos to be loaded first.
}
\author{Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de}}

\seealso{\link[sp]{over}; \code{vignette('over')}}
\keyword{methods}
