\name{STSDF-class}
\docType{class}
\alias{STSDF-class}
\alias{STS-class}
\alias{STSDF}
\alias{STS}
\alias{as.data.frame.STSDF}
\alias{as.data.frame.STS}
\alias{[,STSDF-method} 
\alias{coerce,STSDF,STPDF-method}

\title{Class "STSDF"}
\description{ A class for unstructured spatio-temporal data; for
n spatial locations and times, n observations are available }
\section{Objects from the Class}{
Objects of this class carry full space/time grid data }
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
    \item{[}{\code{signature(x = "STSDF")}: selects spatial-temporal entities, and attributes } 

    % \item{plot}{\code{signature(x = "Spatial", y = "missing")}: plot method
	% for spatial objects; does nothing but setting up a plotting region choosing
	% a suitable aspect if not given(see below), colouring the plot background using either a bg= argument or par("bg"), and possibly drawing axes.  }
	% \item{summary}{\code{signature(object = "Spatial")}: summarize object}
  }
}

\usage{
  STS(sp, time)
  STSDF(sp, time, data)
  \S4method{[}{STSDF}(x, i, j, ..., drop = FALSE)
  \S4method{coerce}{STSDF,STPDF}(from, to, strict=TRUE)
}

\arguments{
\item{sp}{object of class \link{Spatial}}
\item{time}{object of class \link{xts}}
\item{data}{data frame with appropriate number of rows}
\item{x}{an object of class STFDF}
\item{i}{selection of record index (spatial/temporal entities),
or character string with temporal selection }
\item{j}{selection of attributes }
\item{...}{ignored}
\item{drop}{if TRUE and a single spatial entity is selected, an object
of class \link{xts} is returned; if TRUE and a single temporal entity is
selected, and object of the appropriate \code{Spatial} class is returned;
if FALSE, no coercion to reduced classes takes place}
\item{from}{object of class STFDF}
\item{to}{target class}
\item{strict}{ignored}
}

\author{ Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de} }

\keyword{classes}
