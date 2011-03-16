\name{stplot}
\alias{stplot}
\alias{stplot.STFDF} 
\alias{stplot.STIDF} 
\alias{stplot,STFDF-method} 
\alias{stplot,STSDF-method} 
\alias{stplot,STIDF-method} 
\alias{stplot,STIDFtraj-method} 
\title{ produce trellis plot for STxDF object }
\description{ create trellis plot for ST objects }
\section{Methods}{
  \describe{
    \item{stplot}{\code{signature(x = "STFDF")}: plots object of class STFDF}
    \item{stplot}{\code{signature(x = "STSDF")}: plots object of class STFDF}
    \item{stplot}{\code{signature(x = "STIDF")}: plots object of class STFDF}
  }
}
\usage{ 
stplot(obj, ...)
stplot.STFDF(obj, names.attr = index(obj@time), ..., as.table = TRUE)
stplot.STIDF(obj, names.attr = NULL, ..., as.table = TRUE, by =
    c("time", "burst", "id"), scales = list(draw = FALSE), xlab =
    NULL, ylab = NULL, type = 'p', number = 6, overlap = 0, asp)
}
\arguments{
\item{obj}{ object of a class deriving from \code{ST}}
\item{names.attr}{ names that will be used in the strip }
\item{as.table}{ logical; if TRUE, time will increas from top to bottom;
if FALSE, time will increase from bottom to top }
\item{by}{ conditioning variable(s); in case \code{time} is conditioning
variable, it will be classified using \link{equal.count}, using parameters
\code{overlap} and \code{number}}
\item{scales}{ scales drawing; see \code{scales} argument of \link{xyplot}}
\item{xlab}{ x-axis label}
\item{ylab}{ y-axis label }
\item{type}{ character; use 'l' for lines, 'p' for symbols,
'b' for both lines and symbols }
\item{number}{ see argument \code{by}}
\item{overlap}{ see argument \code{by} }
\item{asp}{ aspect ratio of plot }
\item{...}{ arguments passed on to \link{spplot} in case of stplot.STFDF,
or to \link{xyplot} in case of stplot.STIDF}
}
\value{ \code{stplot} is a wrapper around \link{spplot}, that automically
plots each time stamp in a panel.}
\note{vignette("spacetime") contains several examples}
\keyword{dplot}
