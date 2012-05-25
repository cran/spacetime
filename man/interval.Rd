\name{timeIsInterval}
\alias{timeIsInterval}
\alias{timeIsInterval<-}
\alias{timeIsInterval,ANY-method}
\alias{timeIsInterval,ST-method}
\alias{timeIsInterval<-,ANY,logical-method}
\alias{timeIsInterval<-,ST,logical-method}
\alias{timeIsInterval<-,STT,logical-method}

\title{ retrieve, or set, information whether time reflects instance (FALSE) or intervals (TRUE) }
\description{ retrieve, or set, information whether time reflects instance (FALSE) or intervals (TRUE) }
\usage{ 
timeIsInterval(x, ...)
timeIsInterval(x) <- value
}
\arguments{
\item{x}{ object, of any class }
\item{...}{ ignored }
\item{value}{ logical; sets the timeIsInterval value}
}
\value{ logical; this function sets or retrieves the
attribute \code{timeIsInterval} of \code{x}, UNLESS \code{x}
is of class \code{ST}, in which case it set/retrieves
\code{timeIsInterval(x@time)}}

\keyword{dplot}
