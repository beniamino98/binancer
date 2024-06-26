% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binance_order.R
\name{binance_cancel_order}
\alias{binance_cancel_order}
\title{Binance Cancel Order}
\usage{
binance_cancel_order(pair, 
                     order_id, 
                     client_order_id)
}
\arguments{
\item{pair}{Character. Trading pair, e.g. \code{"BTCUSDT"}.}

\item{order_id}{Numeric. Order id that uniquely identify the trade.}

\item{client_order_id}{Numeric. Client order id that uniquely identify the trade.}
}
\description{
Cancel an active order.
}
\keyword{order}
