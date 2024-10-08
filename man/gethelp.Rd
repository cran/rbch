% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ControlRpcs.R
\name{gethelp}
\alias{gethelp}
\title{RPC-JSON API: Help}
\usage{
gethelp(con, rpc = "")
}
\arguments{
\item{con}{object of class \code{CONRPC}.}

\item{rpc}{\code{character}, name of RPC function.}
}
\value{
A S4-object of class \code{ANSRPC}.
}
\description{
Returning information about RPC functions.
}
\references{
\url{https://bitcoin.org/en/developer-reference#help},
\url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
}
\seealso{
Other Control RPCs: 
\code{\link{getinfo}()},
\code{\link{getwalletinfo}()}
}
\author{
Bernhard Pfaff
}
\concept{Control RPCs}
