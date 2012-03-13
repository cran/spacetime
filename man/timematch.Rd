\name{timeMatch}
\alias{timeMatch}
\title{ match two (time) sequences }
\description{ match two (time) sequences, where each can
be intervals or instances. }
\usage{ 
timeMatch(x, y, returnList = FALSE, timeInterval = c(FALSE,TRUE))
}
\arguments{
\item{x}{ ordered sequence, e.g. of time stamps }
\item{y}{ ordered sequence, e.g. of time stamps }
\item{returnList }{ boolean; should a list be returned with all matches? }
\item{timeInterval}{ boolean, length 2: should \code{x} be treated
as intervals (timeInterval[1])? should \code{y} be treated as
intervals? (timeInterval[2])?
}
}
\value{
if \code{returnList = FALSE}: integer vector of length
\code{length(x)} with indexes of \code{y} matching to each of
the elements of \code{x}, or NA if there is no match. See section
details for definition of match.

if \code{returnList = TRUE}: list of length \code{length(x)},
with each list element an integer vector with all the indexes
of \code{y} matching to that element of \code{x}.
}
\details{
Matching in case of no intervals is done on equality of values.

If \code{x} represents intervals, then the first interval is
from \code{x[1]} to \code{x[2]}, with \code{x[1]} included but
\code{x[2]} not.
}
\seealso{\link{over}}
\author{Edzer Pebesma}
\examples{
t0 = as.POSIXct("1999-10-10")
x = t0 +c(0.5+c(2,2.1,4),5)*3600
y = t0 + 1:5 * 3600
x
y
timeMatch(x,y, returnList = FALSE, timeInterval = c(FALSE,FALSE))
timeMatch(x,y, returnList = FALSE, timeInterval = c(FALSE,TRUE))
timeMatch(x,y, returnList = FALSE, timeInterval = c(TRUE,FALSE))
timeMatch(x,y, returnList = FALSE, timeInterval = c(TRUE,TRUE))
timeMatch(x,y, returnList = TRUE, timeInterval = c(FALSE,FALSE))
timeMatch(x,y, returnList = TRUE, timeInterval = c(FALSE,TRUE))
timeMatch(x,y, returnList = TRUE, timeInterval = c(TRUE,FALSE))
timeMatch(x,y, returnList = TRUE, timeInterval = c(TRUE,TRUE))
}
\keyword{manip}
