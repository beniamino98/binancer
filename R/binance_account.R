#' Binance Account Information
#' 
#' Get current general Binance account information.
#' 
#' @return A \code{\link[tibble]{tibble}}.
#'
#' @examplesIf interactive()
#' binance_credentials("api-key", "api-secret")
#' binance_account_info()
#' 
#' @keywords AccountEndpoints
#' @rdname binance_account_info
#' @name binance_account_info
#' @export
binance_account_info <- function() {
  
  # GET call (signed)
  account <- binance_query(api = 'spot', path = 'account', sign = TRUE)
  # Account general information
  idx_not_list <- !purrr::map_lgl(account, is.list)
  df_account <- dplyr::bind_rows(account[idx_not_list])
  df_account <- binance_formatter(df_account)
  # Account commission rates  
  df_commission <- dplyr::as_tibble(account$commissionRates)
  df_commission <- binance_formatter(df_commission)
  info <- dplyr::bind_cols(df_account, df_commission)
  
  return(info)
}

#' Binance Account Balances
#' 
#' Get current balances of the connected account.
#' 
#' @return A \code{\link[tibble]{tibble}}.
#'
#' @examplesIf interactive()
#' binance_credentials("api-key", "api-secret")
#' binance_account_balance()
#' 
#' @keywords AccountEndpoints
#' @rdname binance_account_balance
#' @name binance_account_balance
#' @export
binance_account_balance <- function() {
  
  # GET call (signed)
  account <- binance_query(api = 'spot', path = 'account', sign = TRUE)
  # Account balances 
  balance <- dplyr::as_tibble(account$balances)
  balance <- binance_formatter(balance)
  balance <- balance[balance$free > 0 | balance$locked > 0, ]
  
  return(balance)
}

#' Binance Account Trades 
#' 
#' Get trades for a specific account and symbol.
#' 
#' @param from Character or \code{\link[=POSIXt-class]{POSIXt}} object. Start time for historical trades.  
#' If it is `missing`, the default, retrieved the last 1000 trades from `to` date.
#' @param to Character or \code{\link[=POSIXt-class]{POSIXt}} object. End time for historical trades.
#' If it is `missing`, the default, will be used as end date \code{\link[=Sys.time]{Sys.time()}}.
#' @param from_id Numeric. The last id from which retrieve the trades. Default is `missing`. 
#' @param limit Integer. The maximum number of trades to retrieve. 
#' If `missing`, the default, will be used the maximum value that is 1000. 
#' @inheritParams binance_query 
#' @inheritParams binance_depth 
#'
#' @return A \code{\link[tibble]{tibble}}.
#'
#' @examplesIf interactive()
#' binance_credentials("api-key", "api-secret")
#' binance_account_trades("BTCUSDT", limit = 100)
#' 
#' @keywords AccountEndpoints
#' @rdname binance_account_trades
#' @name binance_account_trades
#' @export
binance_account_trades <- function(pair, from, to, from_id, limit, quiet = FALSE) {
  
  # Check "pair" argument 
  if (missing(pair) || is.null(pair)) {
    if (!quiet) {
      msg <- paste0('The pair argument is missing with no default.')
      cli::cli_abort(msg)
    }
  } else {
    query <- list(symbol = toupper(pair))
  }

  # Check "limit" argument 
  if (missing(limit) || is.null(limit)) {
    limit <- 1000
    if (!quiet) {
      msg <- paste0('The `limit` argument is missing, default is "', limit, '"')
      cli::cli_alert_warning(msg)
    }
    query$limit <- limit
  } else {
    if (limit > 1000) {
      limit <- 1000
      if (!quiet) {
        msg <- paste0('The `limit` argument exceed the maximum that is 1000.')
        cli::cli_alert_warning(msg)
      }
    }
    query$limit <- limit
  }
  
  # Check "from_id" argument 
  if (missing(from_id) || is.null(from_id)) {
    if (!quiet) {
      msg <- paste0('The `from_id` argument is missing with no default.')
      cli::cli_alert_warning(msg)
    }
  } else {
    query$fromId <- from_id
  }
  
  # Check "from" argument 
  if (missing(from) || is.null(from)) {
    if (!quiet) {
      msg <- paste0('The `from` argument is missing with no default.')
      cli::cli_alert_warning(msg)
    }
  } else {
    from <- as.POSIXct(from, origin = "1970-01-01")
    query$startTime <- as_unix_time(from, as_character = TRUE)
  }
  
  # Check "to" argument 
  if (missing(to) || is.null(to)) {
    if (!quiet) {
      msg <- paste0('The `to` argument is missing with no default.')
      cli::cli_alert_warning(msg)
    }
  } else {
    to <- as.POSIXct(to, origin = "1970-01-01")
    query$endTime <- as_unix_time(to, as_character = TRUE)
  }
  
  # GET call
  response <- binance_query(path = 'myTrades', query = query, sign = TRUE, quiet = quiet)
  # Output 
  if (!purrr::is_empty(response)) {
    response <- dplyr::as_tibble(response)
    response <- binance_formatter(response)
    cols_subset <- c("trade_id", "order_id", "date", "pair", "side", "price", "quantity", "commission", "commission_asset", "is_maker")
    response <- response[, cols_subset]
  }

  return(response)
}
