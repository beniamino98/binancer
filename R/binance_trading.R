#' Create a Spot Order
#' 
#' Send in a new order in spot market. 
#' 
#' @param pair Character. Trading pair, e.g. `"BTCUSDT"`.
#' @param side Character. Side of the trade. Can be `"BUY"` or `"SELL"`.
#' @param type Character. Type of order. Available orders's types are: 
#' - `"MARKET"`: A Market Order is an order to buy or sell immediately at the current market price. 
#' It ensures swift execution but may not guarantee the exact price you see at the moment of placing the order, especially during periods of high volatility.
#' - `"LIMIT"` or `"LIMIT_MAKER"`: A Limit order is an order to buy or sell at a specific price. 
#' It will only execute at the specified price or a more favorable one. 
#' This type of order allows traders to set a target price and wait for the market to reach it.
#' - `"STOP_LOSS"` or `"TAKE_PROFIT"`: A Stop Market Order is similar to the Stop Limit Order, but once the stop price is reached, 
#' it becomes a market order, and the trade is executed at the prevailing market price. This ensures execution but may not guarantee the exact price.
#' - `"STOP_LOSS_LIMIT"` or `"TAKE_PROFIT_LIMIT"`:  A Stop Limit Order combines elements of a stop order and a limit order. 
#' You set a stop price and a limit price. When the stop price is triggered, it becomes a limit order, and it will only execute at or better than the limit price. 
#' This order type is useful for entering or exiting positions once a certain price level is reached.
#' @param time_in_force Character. Time in force, specify the conditions under which the trade expiry. The default `"GTC"`. 
#' More details can be found on [Binance Academy](https://academy.binance.com/en/articles/understanding-the-different-order-types).
#' Available time in force are: 
#' 
#' - `"GTC"`: **Good ‘til canceled** orders stipulate that a trade should be kept open until it’s either executed or manually canceled. 
#' - `"IOC"`: **Immediate or cancel** orders stipulate that any part of the order that isn’t immediately filled must be canceled.
#' - `"FOK"`: **Fill or kill** orders are either filled immediately, or they’re canceled.
#' 
#' @param quantity Numeric. Quantity of the asset to be bought or sold. For example when `pair = "BTCUSDT"` and `quantity = 1`,
#' if `side = "BUY"` we are sending an order to buy 1 BTC, otherwise if `side = "SELL"` we are sending an order to sell 1 BTC.
#' @param price Numeric, optional. Limit price, used only for limit orders.  
#' @param stop_price Numeric, optional. Stop price, used only for stop loss and take profit orders. 
#' Can be specified a stop price or a trailing delta, if specified both will be used trailing delta by default. 
#' @param trailing_delta Numeric, optional. Trailing delta, used only for stop loss and take profit orders. 
#' Can be specified a stop price or a trailing delta, if specified both will be used trailing delta by default. 
#' @param iceberg_qty Numeric, iceberg quantity. 
#' @param test Logical. If `TRUE`, the default, the order will be a test order.  
#' @param quiet Logical. Default is `FALSE`. If `TRUE` suppress messages and warnings. 
#'
#' @return For live orders, a \code{\link[tibble]{tibble}} with order details.
#' For test orders, returns invisibly after the Binance test endpoint responds.
#'
#' @examplesIf interactive()
#' binance_credentials("api-key", "api-secret")
#' binance_new_order(
#'   pair = "BTCUSDT",
#'   side = "BUY",
#'   type = "MARKET",
#'   quantity = 0.001,
#'   test = TRUE
#' )
#'
#' @keywords TradingEndpoints
#' @rdname binance_new_order
#' @name binance_new_order
#' @export
binance_new_order <- function(pair, side, type, time_in_force, quantity, price, stop_price, trailing_delta, iceberg_qty, test = TRUE, quiet = FALSE) {
  
  if (missing(pair) || is.null(pair)) {
    cli::cli_abort("The {.arg pair} argument is required.")
  }
  pair <- toupper(pair)
  
  if (missing(side) || is.null(side)) {
    cli::cli_abort("The {.arg side} argument is required.")
  }
  side <- match.arg(toupper(side), choices = c('SELL', 'BUY'))
  av_type <- c('MARKET', 'LIMIT', 'LIMIT_MAKER', 'STOP_LOSS', 'STOP_LOSS_LIMIT', 'TAKE_PROFIT', 'TAKE_PROFIT_LIMIT')

  # Check `quantity` arguments 
  if (missing(quantity) || is.null(quantity)) {
    cli::cli_abort("The {.arg quantity} argument is required.")
  }
  
  # Check "type" argument 
  if (missing(type) || is.null(type)) {
    type <- "MARKET"
    if (!quiet) {
      msg <- paste0('The "type" argument is missing, default is ', '"', type, '"')
      cli::cli_alert_warning(msg)
    }
  } else {
    type <- match.arg(toupper(type), choices = av_type)
  }
  
  # Check that "price" is specified 
  if (type %in% av_type[-1]) {
    if (missing(price) || is.null(price)) {
      if (!quiet) {
        msg <- paste0('A `price` argument is required if `type` is `', type, '`.')
        cli::cli_abort(msg)
      }
    } 
  }
  
  # Check "STOP_LOSS" and "TAKE_PROFIT"
  if (type %in% c("STOP_LOSS", "TAKE_PROFIT", "STOP_LOSS_LIMIT", "TAKE_PROFIT_LIMIT")) {
    
    # Check "stop_price" argument 
    if ((missing(stop_price) || is.null(stop_price)) & (missing(trailing_delta) || is.null(trailing_delta))) {
      if (!quiet) {
        msg <- paste0('A `stop_price` or a `trailing_delta` are required if `type` is `', type, '`.')
        cli::cli_abort(msg)
      }
    } 
  }
  
  # Check "STOP_LOSS_LIMIT" and "TAKE_PROFIT_LIMIT"
  if (type %in% c("STOP_LOSS_LIMIT", "TAKE_PROFIT_LIMIT")) {
    # Check "time_in_force" argument 
    if (missing(time_in_force) || is.null(time_in_force)) {
      if (!quiet) {
        msg <- paste0('A `time_in_force` argument is required if `type` is `', type, '`.')
        cli::cli_abort(msg)
      }
    } 
  }
  
  query <- list(symbol = pair, side = side, type = type)
  
  # Check "quantity" argument 
  if (!missing(quantity) && !is.null(quantity)) {
    if (!is.numeric(quantity) || length(quantity) != 1 || is.na(quantity) || quantity <= 0) {
      cli::cli_abort("The {.arg quantity} argument must be a positive numeric scalar.")
    }
    query$quantity <- quantity 
  } 
  
  if (!missing(time_in_force)) {
    time_in_force <- match.arg(time_in_force, choices = c('GTC', 'IOC', 'FOK'))
    query$timeInForce <- time_in_force
  }
  
  # get filters and check
  filters <- binance_filters(pair)
  
  stopifnot(quantity >= filters$LOT_SIZE$minQty,
            quantity <= filters$LOT_SIZE$maxQty)
  
  # work around the limitation of %% (e.g. 200.1 %% 0.1 = 0.1 !!)
  quot <- (quantity - filters$LOT_SIZE$minQty)/filters$LOT_SIZE$stepSize
  stopifnot(abs(quot - round(quot)) < 1e-10)
  
  if (type == 'MARKET') {
    
    minQty <- filters$MARKET_LOT_SIZE$minQty
    maxQty <- filters$MARKET_LOT_SIZE$maxQty
    stopifnot(quantity >= minQty, quantity <= maxQty)
    
    # work around the limitation of %% (e.g. 200.1 %% 0.1 = 0.1 !!)
    stepSize <- filters$MARKET_LOT_SIZE$stepSize
    if (stepSize > 0) {
      quot <- (quantity - minQty)/stepSize
      stopifnot(abs(quot - round(quot)) < 1e-10)
    }
    
  }
  
  if (!missing(price) && !is.null(price)) {
    
    minPrice <- filters$PRICE_FILTER$minPrice
    stopifnot(price >= minPrice)
    
    if (filters$PRICE_FILTER$maxPrice > 0) {
      stopifnot(price <= filters$PRICE_FILTER$maxPrice)
    }
    
    tickSize <- filters$PRICE_FILTER$tickSize
    if (tickSize > 0) {
      # work around the limitation of %% (e.g. 200.1 %% 0.1 = 0.1 !!)
      quot <- (price - minPrice)/tickSize
      stopifnot(abs(quot - round(quot)) < 1e-10)
    }
    query$price <- price
  }
  
  if (!missing(stop_price) && !is.null(stop_price)) {
    
    minPrice <- filters$PRICE_FILTER$minPrice
    stopifnot(stop_price >= minPrice)
    
    if (filters$PRICE_FILTER$maxPrice > 0) {
      stopifnot(stop_price <= filters$PRICE_FILTER$maxPrice)
    }
    
    tickSize <- filters$PRICE_FILTER$tickSize
    if (tickSize > 0) {
      # work around the limitation of %% (e.g. 200.1 %% 0.1 = 0.1 !!)
      quot <- (stop_price - minPrice) / tickSize
      stopifnot(abs(quot - round(quot)) < 1e-10)
    }
    query$stopPrice <- stop_price
  }
  
  
  if (!missing(trailing_delta) && !is.null(trailing_delta)) {
    query$stopPrice <- NULL
    query$trailingDelta <- trailing_delta
  }
  
  if (!missing(iceberg_qty) && !is.null(iceberg_qty)) {
    if (iceberg_qty > 0) {
      
      stopifnot(time_in_force == 'GTC')
      
      stopifnot(ceiling(quantity / iceberg_qty) <= filters$ICEBERG_PARTS$limit)
      
      stopifnot(iceberg_qty >= filters$ICEBERG_PARTS$minQty, iceberg_qty <= filters$LOT_SIZE$maxQty)
      
      # work around the limitation of %% (e.g. 200.1 %% 0.1 = 0.1 !!)
      quot <- (iceberg_qty - filters$LOT_SIZE$minQty) / filters$LOT_SIZE$stepSize
      stopifnot(abs(quot - round(quot)) < 1e-10)
    }
    query$icebergQty <- iceberg_qty
  }
  
  
  if (isTRUE(test)) {
    
    ord <- binance_query(path = c("order", "test"), method = 'POST', query = query, sign = TRUE, quiet = TRUE)
    if (is.list(ord) & length(ord) == 0) {
      ord <- 'OK'
      message('TEST: OK')
    } else {
      message('TEST: ', ord$msg)
    }
    return(invisible(ord))
  } else {
    
    ord <- binance_query(path = "order", method = 'POST', query = query, sign = TRUE)
    
    if(!is.null(ord$code)){
      return(NULL)
    } else {
      ord <- dplyr::bind_rows(ord)
      ord <- binance_formatter(ord)
      msg <- paste0(side, " order [", ord$order_id, "]", " submitted at ", ord$transact_time)
      cli::cli_alert_success(msg)
    }
    return(ord)
  }
}

#' Binance Get Order
#' 
#' Get information about an order.
#' 
#' @param pair Character. Trading pair, e.g. `"BTCUSDT"`.
#' @param order_id Numeric. Order id that uniquely identify the trade. 
#' @param client_order_id Numeric. Client order id that uniquely identify the trade. 
#'
#' @return A \code{\link[tibble]{tibble}} with order details.
#'
#' @examplesIf interactive()
#' binance_credentials("api-key", "api-secret")
#' binance_get_order(pair = "BTCUSDT", order_id = 123456)
#' 
#' @keywords TradingEndpoints
#' @rdname binance_get_order
#' @name binance_get_order
#' @export
binance_get_order <- function(pair, order_id, client_order_id) {
  
  if (missing(pair) || is.null(pair)) {
    cli::cli_abort("The {.arg pair} argument is required.")
  }
  if (missing(order_id) && missing(client_order_id)) {
    cli::cli_abort("Either {.arg order_id} or {.arg client_order_id} must be supplied.")
  }
  
  query <- list(symbol = toupper(pair))
  
  if (!missing(order_id)) {
    query$orderId = order_id
  }
  if (!missing(client_order_id)) {
    query$origClientOrderId = client_order_id
  }
  
  ord <- binance_query(api = "spot", path = 'order', method = 'GET', query = query, sign = TRUE)
  ord <- dplyr::bind_rows(ord)
  ord <- binance_formatter(ord)
  
  return(ord)
}

#' Binance Cancel Order
#' 
#' Cancel an active order.
#' 
#' @param pair Character. Trading pair, e.g. `"BTCUSDT"`.
#' @param order_id Numeric. Order id that uniquely identify the trade. 
#' @param client_order_id Numeric. Client order id that uniquely identify the trade.
#'
#' @return A \code{\link[tibble]{tibble}} with canceled order details.
#'
#' @examplesIf interactive()
#' binance_credentials("api-key", "api-secret")
#' binance_cancel_order(pair = "BTCUSDT", order_id = 123456)
#' 
#' @keywords TradingEndpoints
#' @rdname binance_cancel_order
#' @name binance_cancel_order
#' @export
binance_cancel_order <- function(pair, order_id, client_order_id) {
  
  if (missing(pair) || is.null(pair)) {
    cli::cli_abort("The {.arg pair} argument is required.")
  }
  if (missing(order_id) && missing(client_order_id)) {
    cli::cli_abort("Either {.arg order_id} or {.arg client_order_id} must be supplied.")
  }
  
  query <- list(symbol = toupper(pair))
  
  if (!missing(order_id)) {
    query$orderId = order_id
  }
  if (!missing(client_order_id)) {
    query$origClientOrderId = client_order_id
  }
  
  ord <- binance_query(path = 'order', method = 'DELETE', query = query, sign = TRUE)
  ord <- dplyr::bind_rows(ord)
  ord <- binance_formatter(ord)
  
  msg <- paste0(ord$side, " order [", ord$order_id, "]", " canceled.")
  cli::cli_alert_success(msg)
  
  return(ord)
}
