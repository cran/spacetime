\name{timeMatch}
\alias{timeMatch}
\title{ match two (time) sequences }
\description{ match two (time) sequences, where each can
be intervals or instances. }
\usage{ 
timeMatch(x, y, returnList = FALSE, end.x = NULL, end.y = NULL)
}
\arguments{
\item{x}{ ordered sequence, e.g. of time stamps }
\item{y}{ ordered sequence, e.g. of time stamps }
\item{returnList }{ boolean; should a list be returned with all matches (TRUE),
or a vector with single matches (FALSE)? }
\item{end.x}{ NULL or ordered sequence, e.g. of time stamps, indicating interval end times for x }
\item{end.y}{ NULL or ordered sequence, e.g. of time stamps, indicating interval end times for y }
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
\code{timeIsInterval(x)} and \code{timeIsInterval(y)} both need
to be non-missing logicals (i.e., TRUE or FALSE).

Matching in case of no intervals is done on equality of values.

If \code{x} represents intervals, then the first interval is from
\code{x[1]} to \code{x[2]}, with \code{x[1]} included but \code{x[2]}
not. In case of zero-width intervals (e.g. \code{x[1]==x[2]}),
nothing will match.

If package \code{intervals} is loaded, for matching time intervals or
points and intervals, \link[intervals]{interval_overlap} is used. If
this is not the case, spacetime uses its own, relatively inefficient 
(O(N^2)) code.
}
\seealso{\link{over}, \link{timeIsInterval}, \link[intervals]{interval_overlap}}
\author{Edzer Pebesma}
\examples{
t0 = as.POSIXct("1999-10-10")
x = t0 +c(0.5+c(2,2.1,4),5)*3600
y = t0 + 1:5 * 3600
x
y
#timeIsInterval(x) = FALSE
#timeIsInterval(y) = FALSE
timeMatch(x,y, returnList = FALSE)
timeMatch(x,y, returnList = TRUE)
#timeIsInterval(y) = TRUE
timeMatch(x,y, returnList = FALSE, end.y = delta(y))
timeMatch(x,y, returnList = TRUE, end.y = delta(y))
#timeIsInterval(x) = TRUE
timeMatch(x,y, returnList = FALSE, end.x = delta(x), end.y = delta(y))
timeMatch(x,y, returnList = TRUE, end.x = delta(x), end.y = delta(y))
#timeIsInterval(y) = FALSE
timeMatch(x,y, returnList = FALSE, end.x = delta(x))
timeMatch(x,y, returnList = TRUE, end.x = delta(x))

x = as.POSIXct("2000-01-01") + (0:9) * 3600
y = x + 1
y[1] = y[2]
x
y
TI = function(x, ti) { 
	timeIsInterval(x) = ti
	x 
}
#timeMatch(TI(y,FALSE),TI(y,FALSE))
#timeMatch(TI(y,TRUE), TI(y,TRUE))
#
#timeMatch(TI(x,FALSE),TI(y,FALSE))
#timeMatch(TI(x,FALSE),TI(y,TRUE))
#timeMatch(TI(x,TRUE), TI(y,FALSE))
#timeMatch(TI(x,TRUE), TI(y,TRUE))
#
#timeMatch(TI(x,FALSE),TI(y,FALSE), returnList = TRUE)
#timeMatch(TI(x,FALSE),TI(y,TRUE), returnList = TRUE)
#timeMatch(TI(x,TRUE), TI(y,FALSE), returnList = TRUE)
#timeMatch(TI(x,TRUE), TI(y,TRUE), returnList = TRUE)

}
\keyword{manip}
