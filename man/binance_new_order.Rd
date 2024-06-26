% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binance_order.R
\name{binance_new_order}
\alias{binance_new_order}
\title{Create a Spot Order}
\usage{
binance_new_order(pair, 
                  side, 
                  type, 
                  time_in_force, 
                  quantity, 
                  price, 
                  stop_price, 
                  trailing_delta, 
                  iceberg_qty, 
                  test = TRUE, 
                  quiet = FALSE)
}
\arguments{
\item{pair}{Character. Trading pair, e.g. \code{"BTCUSDT"}.}

\item{side}{Character. Side of the trade. Can be \code{"BUY"} or \code{"SELL"}.}

\item{type}{Character. Type of order. Available orders's types are:
\itemize{
\item \code{"MARKET"}: A Market Order is an order to buy or sell immediately at the current market price.
It ensures swift execution but may not guarantee the exact price you see at the moment of placing the order, especially during periods of high volatility.
\item \code{"LIMIT"} or \code{"LIMIT_MAKER"}: A Limit order is an order to buy or sell at a specific price.
It will only execute at the specified price or a more favorable one.
This type of order allows traders to set a target price and wait for the market to reach it.
\item \code{"STOP_LOSS"} or \code{"TAKE_PROFIT"}: A Stop Market Order is similar to the Stop Limit Order, but once the stop price is reached,
it becomes a market order, and the trade is executed at the prevailing market price. This ensures execution but may not guarantee the exact price.
\item \code{"STOP_LOSS_LIMIT"} or \code{"TAKE_PROFIT_LIMIT"}:  A Stop Limit Order combines elements of a stop order and a limit order.
You set a stop price and a limit price. When the stop price is triggered, it becomes a limit order, and it will only execute at or better than the limit price.
This order type is useful for entering or exiting positions once a certain price level is reached.
}}

\item{time_in_force}{Character. Time in force, specify the conditions under which the trade expiry. The default \code{"GTC"}.
More details can be found on \href{https://academy.binance.com/en/articles/understanding-the-different-order-types}{Binance Academy}.
Available time in force are:
\itemize{
\item \code{"GTC"}: \strong{Good ‘til canceled} orders stipulate that a trade should be kept open until it’s either executed or manually canceled.
\item \code{"IOC"}: \strong{Immediate or cancel} orders stipulate that any part of the order that isn’t immediately filled must be canceled.
\item \code{"FOK"}: \strong{Fill or kill} orders are either filled immediately, or they’re canceled.
}}

\item{quantity}{Numeric. Quantity of the asset to be bought or sold. For example when \code{pair = "BTCUSDT"} and \code{quantity = 1},
if \code{side = "BUY"} we are sending an order to buy 1 BTC, otherwise if \code{side = "SELL"} we are sending an order to sell 1 BTC.}

\item{price}{Numeric, optional. Limit price, used only for limit orders.}

\item{stop_price}{Numeric, optional. Stop price, used only for stop loss and take profit orders.
Can be specified a stop price or a trailing delta, if specified both will be used trailing delta by default.}

\item{trailing_delta}{Numeric, optional. Trailing delta, used only for stop loss and take profit orders.
Can be specified a stop price or a trailing delta, if specified both will be used trailing delta by default.}

\item{iceberg_qty}{Numeric, iceberg quantity.}

\item{test}{Logical. If \code{TRUE}, the default, the order will be a test order.}

\item{quiet}{Logical. Default is \code{FALSE}. If \code{TRUE} suppress messages and warnings.}
}
\description{
Send in a new order in spot market.
}
\keyword{order}
