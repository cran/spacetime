\name{STIDFtraj-class}
\docType{class}
\alias{STIDFtraj-class}
\alias{ltraj-class}
\alias{coerce,STIDFtraj,ltraj-method} 
\alias{coerce,ltraj,STIDFtraj-method}

\title{Class "STIDF"}
\description{ A class for spatio-temporal trajectory data; for
n spatial locations and times, n observations are available }
\section{Objects from the Class}{
Objects of this class carry sparse (irregular) space/time data }
\section{Slots}{
  \describe{
    \item{\code{sp}:}{Object of class \code{"Spatial"}}
    \item{\code{time}:}{Object of class \code{"xts"}}
	\item{\code{data}:}{Object of class \code{data.frame}, which holds
	the measured values }
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "STIDF")}: selects spatial-temporal entities, and attributes } 

    % \item{plot}{\code{signature(x = "Spatial", y = "missing")}: plot method
	% for spatial objects; does nothing but setting up a plotting region choosing
	% a suitable aspect if not given(see below), colouring the plot background using either a bg= argument or par("bg"), and possibly drawing axes.  }
	% \item{summary}{\code{signature(object = "Spatial")}: summarize object}
  }
}

\usage{
  \S4method{coerce}{STIDFtraj,ltraj}(from, to, strict=TRUE)
  \S4method{coerce}{ltraj,STIDFtraj}(from, to, strict=TRUE)
}

\arguments{
\item{from}{object}
\item{to}{target class}
\item{strict}{ignored}
}

\note{ The \code{data.frame} needs to have a column called
\code{burst} which is a factor (or character) and contains the
grouping of observations that come from a continuous sequence of
observations. Optionally, a column \code{id} can be used to identify
individual items }

\author{ Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de} }

\keyword{classes}
