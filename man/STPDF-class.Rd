\name{STPDF-class}
\docType{class}
\alias{STPDF-class}
\alias{STP-class}
\alias{STPDF}
\alias{STP}
\alias{as.data.frame.STPDF}
\alias{as.data.frame.STP}
\alias{[,STPDF-method} 
\alias{coerce,STPDF,STFDF-method}
\alias{coerce,STPDF,STSDF-method}

\title{Class "STPDF"}
\description{ A class for spatio-temporal data with partial space-time grids; for
n spatial locations and m times, an index table is kept for which nodes 
observations are available }
\section{Objects from the Class}{
Objects of this class carry full space/time grid data }
\section{Slots}{
  \describe{
    \item{\code{sp}:}{Object of class \code{"Spatial"}}
    \item{\code{time}:}{Object of class \code{"xts"}}
    \item{\code{index}:}{matrix of dimension n x 2, where n matches the number of rows
	in slot data }
	\item{\code{data}:}{Object of class \code{data.frame}, which holds
	the measured values}
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "STPDF")}: selects spatial entities,        temporal entities, and attributes } 

    % \item{plot}{\code{signature(x = "Spatial", y = "missing")}: plot method
	% for spatial objects; does nothing but setting up a plotting region choosing
	% a suitable aspect if not given(see below), colouring the plot background using either a bg= argument or par("bg"), and possibly drawing axes.  }
	% \item{summary}{\code{signature(object = "Spatial")}: summarize object}
  }
}

\usage{
  STP(sp, time, index)
  STPDF(sp, time, data, index)
  \S4method{[}{STPDF}(x, i, j, ..., drop = TRUE)
  \S4method{coerce}{STPDF,STFDF}(from, to, strict=TRUE)
  \S4method{coerce}{STPDF,STSDF}(from, to, strict=TRUE)
}

\arguments{
\item{sp}{object of class \link{Spatial}}
\item{time}{object of class \link{xts}}
\item{data}{data frame with appropriate number of rows}
\item{index}{n x 2 matrix, see above}
\item{x}{an object of class STFDF}
\item{i}{selection of spatial entities}
\item{j}{selection of temporal entities (see syntax in package xts) }
\item{...}{selection of attribute(s)}
\item{drop}{if TRUE and a single spatial entity is selected, an object
of class \link{xts} is returned; if TRUE and a single temporal entity is
selected, and object of the appropriate \code{Spatial} class is returned;
if FALSE, no coercion to reduced classes takes place}
\item{from}{object of class STFDF}
\item{to}{target class}
\item{strict}{ignored}
}

\author{ Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de} }

\examples{
sp = cbind(x = c(0,0,1), y = c(0,1,1))
row.names(sp) = paste("point", 1:nrow(sp), sep="")
sp = SpatialPoints(sp)
time = xts(1:4, as.POSIXct("2010-08-05")+3600*(10:13))
m = c(10,20,30) # means for each of the 3 point locations
mydata = rnorm(length(sp)*length(time),mean=rep(m, 4))
IDs = paste("ID",1:length(mydata))
mydata = data.frame(values = signif(mydata,3), ID=IDs)
stfdf = STFDF(sp, time, mydata)
stfdf
stpdf = as(stfdf, "STPDF")
stpdf[1:2,]
stpdf[,1:2]
stpdf[,,2]
stpdf[,,"values"]
stpdf[1,]
stpdf[,2]
# examples for [[, [[<-, $ and $<- 
stpdf[[1]]
stpdf[["values"]]
stpdf[["newVal"]] <- rnorm(12)
stpdf$ID
stpdf$ID = paste("OldIDs", 1:12, sep="")
stpdf$NewID = paste("NewIDs", 12:1, sep="")
stpdf
}

\keyword{classes}
