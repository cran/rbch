% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UtilFuncs.R
\name{txids}
\alias{txids}
\title{Retrieve TX Ids in block}
\usage{
txids(con, height, excoinbase = TRUE)
}
\arguments{
\item{con}{\code{CONRPC}, configuration object.}

\item{height}{\code{integer}, the block's height.}

\item{excoinbase}{\code{logical}, whether coinbase transaction
should be excluded (default is \code{TRUE}).}
}
\value{
\code{character}
}
\description{
This function retrieves the transaction IDs in a block.
}
\seealso{
Other UtilityFuncs: 
\code{\link{bkfee}()},
\code{\link{blockattime}()},
\code{\link{blockstats}()},
\code{\link{date2int}()},
\code{\link{int2date}()},
\code{\link{intMaxDay}()},
\code{\link{intMinDay}()},
\code{\link{intRangeDay}()},
\code{\link{intRangePeriod}()},
\code{\link{timeofblock}()},
\code{\link{txfee}()},
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
