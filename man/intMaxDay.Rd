% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UtilFuncs.R
\name{intMaxDay}
\alias{intMaxDay}
\title{Integer representation of a day-end}
\usage{
intMaxDay(x)
}
\arguments{
\item{x}{\code{POSIXct}, date/time object.}
}
\value{
\code{integer}
}
\description{
This function returns the associated \code{integer} time
for the end of a specific day (\emph{i.e.}, \code{23:59:59} time).
}
\examples{
d1 <- "2017-03-15"
d1 <- intMaxDay(d1)
d2 <- "2017-03-15 23:59:59"
d2 <- intMaxDay(d2)
identical(d1,d2)
}
\seealso{
Other UtilityFuncs: 
\code{\link{bkfee}()},
\code{\link{blockattime}()},
\code{\link{blockstats}()},
\code{\link{date2int}()},
\code{\link{int2date}()},
\code{\link{intMinDay}()},
\code{\link{intRangeDay}()},
\code{\link{intRangePeriod}()},
\code{\link{timeofblock}()},
\code{\link{txfee}()},
\code{\link{txids}()},
\code{\link{txinids}()},
\code{\link{txstats}()},
\code{\link{utxoage}()},
\code{\link{utxotype}()},
\code{\link{utxovalue}()}
}
\author{
Bernhard Pfaff
}
\concept{UtilityFuncs}
