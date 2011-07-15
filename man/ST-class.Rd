\name{ST-class}
\docType{class}
\alias{ST-class}
\alias{xts-class}
\alias{ST}
\alias{dim.ST}
\alias{[[<-,ST,ANY,missing-method}
\alias{[[,ST,ANY,missing-method}
\alias{$<-,ST-method}
\alias{$,ST-method}

\title{Class "ST"}
\description{ An abstract class from which useful spatio-temporal classes are
derived }
\section{Objects from the Class}{
Objects of this class are not meant to be useful; only derived
classes can be meaningful }
\section{Slots}{
  \describe{
    \item{\code{sp}:}{Object deriving from class \code{"Spatial"}}
    \item{\code{time}:}{Object of class \code{"xts"}}
  }
}
\section{Methods}{
  \describe{
    \item{[[}{\code{signature(obj = "ST")}: retrieves the attribute element }
    \item{$}{\code{signature(obj = "ST")}: retrieves the attribute element }
    \item{[[<-}{\code{signature(obj = "ST")}: sets or replaces the attribute element }
    \item{$<-}{\code{signature(obj = "ST")}: sets or replaces the attribute element }
    % \item{plot}{\code{signature(x = "Spatial", y = "missing")}: plot method
	% for spatial objects; does nothing but setting up a plotting region choosing
	% a suitable aspect if not given(see below), colouring the plot background using either a bg= argument or par("bg"), and possibly drawing axes.  }
	% \item{summary}{\code{signature(object = "Spatial")}: summarize object}
  }
}

\usage{
ST(sp, time)
}

\arguments{
\item{sp}{an object deriving from class \link[sp]{Spatial}, such as
a \link[sp]{SpatialPoints} or \link[sp]{SpatialPolygons}}
\item{time}{an object of class xts, or a time vector
(currently: Date, POSIXct, timeDate, yearmon and yearqtr; are supported;
see \link[xts]{xts}); in the latter case, it should be in time order }
}
\note{
argument (and object slot) sp can be pure geometry, or geometry
with attributes. In the latter case, the geometries are kept with
the sp slot, and only replicated (when needed) on coercion to the
long format, with as.data.frame.

slot time needs to be of class xts; if a time or date vector is 
passed as argument to SP, it will be converted into an xts object.

ST is meant as a super-class, and is not to be used for representing
data, just like Spatial in the sp package.
}
\author{ Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de} }

\examples{
ST(SpatialPoints(cbind(c(0,1),c(0,1))), as.Date('2008-01-01')+1:2)
}

\keyword{classes}
