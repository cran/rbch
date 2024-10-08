% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EllipticCurves.R
\name{ecparam}
\alias{ecparam}
\title{Creating objects of class ECPARAM}
\usage{
ecparam(p, a, b)
}
\arguments{
\item{p}{\code{integer}}

\item{a}{\code{integer}}

\item{b}{\code{integer}}
}
\value{
An object of S4-class \code{ECPARAM}
}
\description{
This function returns an object of S4-class \code{ECPARAM},
that does contain the parametrization of an elliptic curve.
}
\examples{
p <- "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F"
b <- "0x0000000000000000000000000000000000000000000000000000000000000007"
a <- "0x0000000000000000000000000000000000000000000000000000000000000000"
curve256 <- ecparam(p, a, b)
curve256
}
\references{
\url{https://en.bitcoin.it/wiki/Secp256k1}
}
\seealso{
Other EllipticCurve: 
\code{\link{ECPARAM-class}},
\code{\link{ECPOINT-class}},
\code{\link{EcparamOrNull-class}},
\code{\link{containsPoint}()},
\code{\link{ecoperators}},
\code{\link{ecpoint}()},
\code{\link{isNull}()}
}
\author{
Bernhard Pfaff
}
\concept{EllipticCurve}
