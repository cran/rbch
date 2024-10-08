% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllClasses.R
\docType{class}
\name{BTCADR-class}
\alias{BTCADR-class}
\title{S4 class BTCADR (BTC alias)}
\description{
S4-class for BTC addresses
}
\section{Slots}{

\describe{
\item{\code{pubkey}}{\code{character}, the 512-bit public key.}

\item{\code{pubhash}}{\code{character}, the hashed public key.}

\item{\code{btcadr}}{\code{character}, the BTC address.}

\item{\code{mainnet}}{\code{logical}, whether mainnet or testnet.}
}}

\references{
\url{https://en.bitcoin.it/wiki/Address}
}
\seealso{
Other BchAdresses: 
\code{\link{BCHADR-class}},
\code{\link{PubHash2BchAdr}()},
\code{\link{PubKey2PubHash}()},
\code{\link{base58CheckDecode}()},
\code{\link{base58CheckEncode}()},
\code{\link{concatHex}()},
\code{\link{decodeHex}()},
\code{\link{hash160}()},
\code{\link{hash256}()},
\code{\link{validBchAdr}()},
\code{\link{validBtcAdr}()}
}
\author{
Bernhard Pfaff
}
\concept{BchAdresses}
