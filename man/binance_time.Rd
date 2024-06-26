% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binance_time.R
\name{binance_time}
\alias{binance_time}
\title{Binance Server Time}
\usage{
binance_time(api, quiet = FALSE)
}
\arguments{
\item{api}{Character. Reference API. If it is \code{missing}, the default, will be used \code{"spot"}. Available options are:
\itemize{
\item \code{"spot"}: for endpoint \href{https://binance-docs.github.io/apidocs/spot/en/#check-server-time}{api/v3/time}. The ip weight is 1.
\item \code{"fapi"}: for endpoint \href{https://binance-docs.github.io/apidocs/futures/en/#check-server-time}{fapi/v3/time}. The ip weight is 1.
\item \code{"dapi"}: for endpoint \href{https://binance-docs.github.io/apidocs/delivery/en/#check-server-time}{dapi/v3/time}. The ip weight is 1.
\item \code{"eapi"}: for endpoint \href{https://binance-docs.github.io/apidocs/voptions/en/#check-server-time}{eapi/v3/time}. The ip weight is 1.
}}

\item{quiet}{Logical. Default is \code{FALSE}. If \code{TRUE} suppress messages and warnings.}
}
\value{
A \code{\link[=POSIXt-class]{POSIXt}} object. The server time for the reference API.
}
\description{
Get the current server time from Binance API.
}
\details{
The IP weight for this API call is 1, and the data source is memory.
}
\examples{

# Get the server time
binance_time("spot")
binance_time("fapi")
binance_time("dapi")
binance_time("eapi")

}
\keyword{market}
\keyword{rest}
\keyword{time}
