% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binance_last_trades.R
\name{binance_last_trades}
\alias{binance_last_trades}
\title{Binance Last Trades}
\usage{
binance_last_trades(pair, 
                    api, 
                    quiet = FALSE)
}
\arguments{
\item{pair}{Character. Trading pair, e.g. \code{"BTCUSDT"}.}

\item{api}{Character. Reference API. If it is \code{missing}, the default, will be used \code{"spot"}. Available options are:
\itemize{
\item \code{"spot"}: for endpoint \href{https://binance-docs.github.io/apidocs/spot/en/#recent-trades-list}{api/v3/trades}. The ip weight is 10.
\item \code{"fapi"}: for endpoint \href{https://binance-docs.github.io/apidocs/futures/en/#recent-trades-list}{fapi/v1/trades}. The ip weight is 5.
\item \code{"dapi"}: for endpoint \href{https://binance-docs.github.io/apidocs/delivery/en/#recent-trades-list}{dapi/v1/trades}. The ip weight is 5.
\item \code{"eapi"}: for endpoint \href{https://binance-docs.github.io/apidocs/voptions/en/#recent-trades-list}{eapi/v1/trades}. The ip weight is 5.
}}

\item{quiet}{Logical. Default is \code{FALSE}. If \code{TRUE} suppress messages and warnings.}
}
\value{
A \code{\link[tibble]{tibble}} with 7 columns:
\itemize{
\item \code{date}: \code{\link[=POSIXt-class]{POSIXt}}, trade execution date.
\item \code{market}: Character, selected API.
\item \code{pair}: Character, trading pair.
\item \code{price}: Numeric, trade price.
\item \code{quantity}: Numeric, trade quantity.
\item \code{side}: Character, trade side. Can be \code{"BUY"} or \code{"SELL"}.
\item \code{trade_id}: Integer, trade id.
}
}
\description{
Get the last 1000 trades for a trading pair.
}
\examples{
# Get last 1000 trades for BTCUSDT
binance_last_trades(pair = "BTCUSDT", api = "spot")
binance_last_trades(pair = "BTCUSDT", api = "fapi")

# Get last 1000 trades for BTCUSD_PERP
binance_last_trades(pair = "BTCUSD_PERP", api = "dapi")

# Get last 1000 trades for a put option on BTC
binance_last_trades(pair = "BTC-240628-30000-P", api = "eapi")

}
\keyword{market}
\keyword{rest}
\keyword{trades}
