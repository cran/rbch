% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NetworkRpcs.R
\name{addnode}
\alias{addnode}
\title{RPC-JSON API: addnode}
\usage{
addnode(con, node, command = c("add", "remove", "onetry"))
}
\arguments{
\item{con}{object of class \code{CONRPC}.}

\item{node}{\code{character} the node (see \code{getpeerinfo()} for nodes).}

\item{command}{\code{character} 'add' to add a node to the list,
'remove' to remove a node from the list, 'onetry' to try a connection
to the node once.}
}
\value{
A S4-object of class \code{ANSRPC}.
}
\description{
Attempts to add or remove a node from the addnode list.
Or try a connection to a node once.
}
\references{
\url{https://bitcoin.org/en/developer-reference#addnode},
\url{https://bitcoin.org/en/developer-reference#remote-procedure-calls-rpcs}
}
\seealso{
Other Network RPCs: 
\code{\link{clearbanned}()},
\code{\link{disconnectnode}()},
\code{\link{getaddednodeinfo}()},
\code{\link{getconnectioncount}()},
\code{\link{getnettotals}()},
\code{\link{getnetworkinfo}()},
\code{\link{getpeerinfo}()},
\code{\link{listbanned}()},
\code{\link{ping}()}
}
\author{
Bernhard Pfaff
}
\concept{Network RPCs}
