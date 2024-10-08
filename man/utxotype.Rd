% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UtilFuncs.R
\name{utxotype}
\alias{utxotype}
\title{Retrieving types of UTXOs}
\usage{
utxotype(con, txid)
}
\arguments{
\item{con}{\code{CONRPC}, configuration object.}

\item{txid}{\code{character}, the id of the transaction.}
}
\value{
\code{character}
}
\description{
This function returns the types of the UTXO(s) in a transaction.
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
\code{\link{txids}()},
\code{\link{txinids}()},
\code{\link{txstats}()},
\code{\link{utxoage}()},
\code{\link{utxovalue}()}
}
\author{
Bernhard Pfaff
}
\concept{UtilityFuncs}
