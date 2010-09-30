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
Objects of this class
are never to be generated; only derived classes can be meaningful }
\section{Slots}{
  \describe{
    \item{\code{sp}:}{Object of class \code{"Spatial"}}
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
\item{sp}{an object of a Spatial class}
\item{time}{an object of class xts}
}

\author{ Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de} }

%\examples{}

\keyword{classes}
