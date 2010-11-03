\name{stplot}
\alias{stplot}
\title{ produce trellis plot for ST object }
\description{ create trellis plot for ST objects }
\usage{ 
stplot(obj, names.attr = index(obj@time), ..., as.table = TRUE)
}
\arguments{
\item{obj}{ object of a class deriving from \code{ST}}
\item{names.attr}{ names that will be used in the strip }
\item{as.table}{ logical; if TRUE, time will increas from top to bottom;
if FALSE, time will increase from bottom to top }
\item{...}{ arguments passed on to \link{spplot}}
}
\value{ \code{stplot} is a wrapper around \link{spplot}, that automically
plots each time stamp in a panel.  }
\keyword{dplot}
