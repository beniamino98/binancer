#' Get candlestick data 
#'
#' Get klines/candlestick data for a trading pair. 
#'
#' @param api Character. Reference API. If it is `missing`, the default, will be used `"spot"`. Available options are:
#'   - `"spot"`: for endpoint [api/v3/klines](https://developers.binance.com/docs/binance-spot-api-docs/rest-api/market-data-endpoints#klinecandlestick-data). The ip weight is 2.
#'   - `"fapi"`: for endpoint [fapi/v1/klines](https://developers.binance.com/docs/derivatives/usds-margined-futures/market-data/rest-api/Kline-Candlestick-Data). The ip weight is 10.
#'   - `"dapi"`: for endpoint [dapi/v1/klines](https://developers.binance.com/docs/derivatives/coin-margined-futures/market-data/rest-api/Kline-Candlestick-Data). The ip weight is 10.
#'   - `"eapi"`: for endpoint [eapi/v1/klines](https://developers.binance.com/docs/derivatives/option/market-data/Kline-Candlestick-Data). The ip weight is 1.
#' @param interval Character. Default is `"1d"`. Time interval for klines data. Available intervals are: 
#'   - Secondly: `"1s"`, available only if `api = "spot"`.
#'   - Minutely: `"1m"`, `"3m"`, `"5m"`, `"15m"` and `"30m"`.
#'   - Hourly: `"1h"`, `"2h"`, `"4h"`, `"6h"`, `"8h"` and `"12h"`.
#'   - Daily: `"1d"` and `"3d"`.
#'   - Weekly: `"1w"`.
#'   - Monthly: `"1M"`.
#' @param from Character or \code{\link[=POSIXt-class]{POSIXt}} object. Start time for historical data. 
#' If it is `missing`, the default, will be used as start date `Sys.time()-lubridate::days(1)`.
#' @param to Character or \code{\link[=POSIXt-class]{POSIXt}} object. End time for historical data.
#' If it is `missing`, the default, will be used as end date \code{\link[=Sys.time]{Sys.time()}}.
#' @param contract_type Character. Used only if `api` is `"fapi"` or `"dapi"`. Available contract's types are: 
#'   - `"perpetual"`: perpetual futures.
#'   - `"current_quarter"`: futures with maturity in the current quarter.
#'   - `"next_quarter"`: futures with maturity in the next quarter.
#' @param uiKlines Logical. Default is `FALSE`. If `TRUE` will be used the endpoint `continuousKlines`.
#' @param indexPrice Logical, default is `FALSE`. Used only when `api` is equal to `"fapi"` or `"dapi"`. When `TRUE` is used the endpoint `indexPriceKlines`.
#' @param markPrice Logical, default is `FALSE`. Used only when `api` is equal to `"fapi"` or `"dapi"`. When `TRUE` is used the endpoint `markPriceKlines`.
#' @param indexPremium Logical, default is `FALSE`. Used only when `api` is equal to `"fapi"` or `"dapi"`. When `TRUE` is used the endpoint `premiumIndexKlines`.
#' @param as_xts Logical. Default is `FALSE`. If `TRUE` convert the data into an \code{\link[xts]{xts}} object.
#' 
#' @inheritParams binance_query 
#' @inheritParams binance_depth 

#' @return A \code{\link[tibble]{tibble}} with 13 columns:
#'   - `date`: \code{\link[=POSIXt-class]{POSIXt}}, the opening date of the candle.
#'   - `date_close`: \code{\link[=POSIXt-class]{POSIXt}}, the closing date of the candle.
#'   - `market`: Character, API.
#'   - `pair`: Character, trading pair.
#'   - `open`: Numeric, open price (price in `date`).
#'   - `high`: Numeric, highest price from `date` up to `date_close`.
#'   - `low`: Numeric, lowest price from `date` up to `date_close`.
#'   - `close`: Numeric, close price or price in `date_close`.
#'   - `volume`: Numeric, volume in asset value.
#'   - `volume_quote`: Numeric, volume in quote asset value.
#'   - `trades`: Numeric, number of trades from `date` up to `date_close`.
#'   - `taker_buy`: Numeric, taker buy volume in asset value.
#'   - `taker_buy_quote`: Numeric, taker buy volume in quote asset value.
#'
#' @examplesIf interactive()
#' # Get 1-hour OHLC data for BTCUSDT in the spot market
#' binance_klines(pair = "BTCUSDT", api = "spot", interval = "1h")
#' # Get 30-minute OHLC data for BTCUSDT in USD-m market
#' # Perpetual contracts 
#' binance_klines(pair = "BTCUSDT", api = "fapi", interval = "30m", uiKlines = FALSE)
#' binance_klines(pair = "BTCUSDT", api = "fapi", interval = "30m", 
#'                uiKlines = TRUE, contract_type = "perpetual")
#' # Futures contracts with maturity in current quarter 
#' binance_klines(pair = "BTCUSDT", api = "fapi", interval = "30m", 
#'                uiKlines = TRUE, contract_type = "current_quarter")
#' # Futures contracts with maturity in next quarter 
#' binance_klines(pair = "BTCUSDT", api = "fapi", interval = "30m", 
#'                uiKlines = TRUE, contract_type = "next_quarter")
#'                
#' # Get 15-minute OHLC data for BTCUSD in COIN-m market
#' # Perpetual contracts 
#' binance_klines(pair = "BTCUSD", api = "dapi", interval = "15m", 
#'                contract_type = "perpetual", uiKlines = FALSE)
#' binance_klines(pair = "BTCUSD", api = "dapi", interval = "15m", 
#'                uiKlines = TRUE, contract_type = "perpetual") 
#' # Futures contracts with maturity in current quarter 
#' binance_klines(pair = "BTCUSD", api = "dapi", interval = "15m", 
#'                uiKlines = TRUE, contract_type = "current_quarter") 
#' # Futures contracts with maturity in next quarter 
#' binance_klines(pair = "BTCUSD", api = "dapi", interval = "1h", 
#'                uiKlines = TRUE, contract_type = "next_quarter")
#'
#' # Get 1-hour OHLC data for a put option on BTCUSDT.
#' # Strike of 30000 and maturity on 2024-06-28.
#' binance_klines(pair = "BTC-240628-30000-P", api = "eapi", interval = "1h")
#' 
#' @keywords marketEndpoints
#' @rdname binance_klines
#' @name binance_klines
#' @export
binance_klines <- function(pair, api, interval, from, to, contract_type, uiKlines = FALSE, indexPrice = FALSE, markPrice = FALSE, indexPremium = FALSE, as_xts = FALSE, quiet = FALSE){
  
  # Check "pair" argument 
  if (missing(pair) || is.null(pair)) {
    if (!quiet) {
      msg <- paste0('The pair argument is missing with no default.')
      cli::cli_abort(msg)
    }
  } else {
    pair <- toupper(pair)
  }
  
  # Check "api" argument 
  if (missing(api) || is.null(api)) {
    api <- "spot"
    if (!quiet) {
      msg <- paste0('The "api" argument is missing, default is ', '"', api, '"')
      cli::cli_alert_warning(msg)
    }
  } else {
    api <- match.arg(api, choices = c("spot", "fapi", "dapi", "eapi"))
  }
  
  # Check "interval" argument 
  if (missing(interval) || is.null(interval)) {
    interval <- "1d"
    if (!quiet) {
      msg <- paste0('The `interval` argument is missing, default is ', '"', interval, '"')
      cli::cli_alert_warning(msg)
    }
  } else {
    av_int <- c("1s", "1m", "3m", "5m", "15m","30m","1h", "2h", 
                "4h", "6h", "8h", "12h", "1d", "3d", "1w", "1M")
    if (api != "spot"){
      av_int <- av_int[-1] 
    }
    interval <- match.arg(interval, choices = av_int)
  }
  
  # Check "from" argument 
  if (missing(from) || is.null(from)) {
    from <- Sys.time() - lubridate::days(1)
    if (!quiet) {
      msg <- paste0('The `from` argument is missing, default is ', '"', from, '"')
      cli::cli_alert_warning(msg)
    }
  } else {
    from <- as.POSIXct(from, origin = "1970-01-01")
  }
  
  # Check "to" argument 
  if (missing(to) || is.null(to)) {
    to <- Sys.time() 
    if (!quiet) {
      msg <- paste0('The `to` argument is missing, default is ', '"', to, '"')
      cli::cli_alert_warning(msg)
    }
  } else {
    to <- as.POSIXct(to, origin = "1970-01-01")
  }

  # Check "contract_type" argument
  if (missing(contract_type) || is.null(contract_type)) {
    if(uiKlines){
      contract_type <- "perpetual"
      if (!quiet) {
        msg <- paste0('If `uiKlines` is `TRUE` `contract_type` must be specified. Default is ', '"', contract_type, '"')
        cli::cli_alert_warning(msg)
      }
      contract_type <- toupper(contract_type)
    } else {
      contract_type <- NULL
    }
  } else {
    if(!uiKlines){
      uiKlines <- TRUE
      if (!quiet) {
        msg <- paste0('If `contract_type` is specified, `uiKlines` must be `TRUE`.')
        cli::cli_alert_warning(msg)
      }
    }
    contract_type <- toupper(match.arg(tolower(contract_type), choices = c("perpetual", "current_quarter", "next_quarter")))
  }
  
  # Query parameters depends on api 
  if (api == "spot") {
    args <- list(pair = pair, interval = interval, from = from , to = to, uiKlines = uiKlines, as_xts = as_xts, quiet = quiet)
  } else if (api %in% c("fapi", "dapi")) {
    args <- list(pair = pair, interval = interval, from = from , to = to, contract_type = contract_type, 
                 uiKlines = uiKlines, indexPrice = indexPrice, markPrice = markPrice, indexPremium = indexPremium,
                 as_xts = as_xts, quiet = quiet)
  } else {
    args <- list(pair = pair, interval = interval, from = from , to = to, as_xts = as_xts, quiet = quiet)
  }
  # Function name 
  fun_name <- paste0("binance_klines_", api)
  # Safe call to avoid errors 
  safe_fun <- purrr::safely(~do.call(fun_name, args = args))
  # GET call 
  response <- safe_fun()
  
  if (!quiet & !is.null(response$error)) {
    cli::cli_alert_danger(response$error)
  } else {
    return(response$result)
  }
}

# Klines implementation for spot api 
binance_klines_spot <- function(pair, interval, from , to, uiKlines = FALSE, as_xts = FALSE, quiet = FALSE){

  i <- 1
  last_date <- 0
  response  <- list()
  condition <- TRUE
  end_time <- as_unix_time(to, as_character = TRUE)
  start_time <- as_unix_time(from, as_character = TRUE)
  while(condition){
    # GET call 
    api_path <- ifelse(uiKlines, "uiKlines", "klines")
    api_query <- list(symbol = pair, startTime = NULL, interval = interval, endTime = end_time, limit = 1000)
    new_data <- binance_query(api = "spot", path = api_path, query = api_query)

    # Break if new_data is empty 
    if (purrr::is_empty(new_data)) {
      break
    }
    response[[i]] <- new_data
    # Rename columns 
    colnames(response[[i]]) <- c("date", "open", "high", "low", "close", "volume",
                                 "date_close", "volume_quote", "trades", "taker_buy",
                                 "taker_buy_quote", "ignore")
    response[[i]] <- dplyr::as_tibble(response[[i]])
    # Extract the first date
    first_date <- min(as.numeric(response[[i]]$date))
    # Break if first_date is greater than start_time
    condition <- first_date > as.numeric(start_time) & first_date != last_date
    last_date <- first_date # avoid infinite loops 
    end_time <- format(first_date, scientific = FALSE)
    i <- i + 1
  }
  
  if (!purrr::is_empty(response)) {
    response <- dplyr::bind_rows(response)
    response$pair <- pair 
    response$market <- "spot"
    response <- binance_formatter(response)
    # Filter to be exactly in from-to range
    response <- dplyr::filter(response, date >= from & date <= to)
    # Arrange with respect to date
    response <- dplyr::arrange(response, date)
    if (isTRUE(as_xts)) {
      response <- binance_xts(response)
    }
  } else {
    response <- dplyr::tibble()
  }
  
  attr(response, "api") <- "spot"
  attr(response, "ip_weight") <- i
  attr(response, "interval") <- interval
  attr(response, "endpoint") <- "klines"
  
  return(response)
}

# Klines implementation for futures USD-M api 
binance_klines_fapi <- function(pair, interval, from, to, contract_type, uiKlines = FALSE, indexPrice = FALSE, markPrice = FALSE, indexPremium = FALSE, as_xts = FALSE, quiet = FALSE){
  
  i <- 1
  # api GET call 
  api_path <- dplyr::case_when(
    uiKlines ~ "continuousKlines", 
    indexPrice ~ "indexPriceKlines",
    markPrice ~ "markPriceKlines",
    indexPremium ~ "premiumIndexKlines",
    TRUE ~ "klines"
  )
  
  response  <- list()
  condition <- TRUE
  last_date <- as_unix_time(to, as_character = FALSE)
  end_time <- as_unix_time(to, as_character = TRUE)
  start_time <- as_unix_time(from, as_character = TRUE)
  while(condition){
    
    api_query <- list(contractType = NULL, 
                      interval = interval, 
                      startTime = NULL, 
                      endTime = end_time, 
                      limit = 1500)
    
    if (uiKlines | indexPrice) {
      api_query$pair <- pair 
      api_query$contractType <- toupper(contract_type)
    } else {
      api_query$symbol <- pair 
    }
    
    new_data <- binance_query(api = "fapi", path = api_path, query = api_query)
    
    # Break if new_data is empty 
    if (purrr::is_empty(new_data)) {
      break
    } else {
      response[[i]] <- new_data
    }
    # Rename columns 
    colnames(response[[i]]) <- c("date", "open", "high", "low", "close", "volume",
                                 "date_close", "volume_quote", "trades", "taker_buy",
                                 "taker_buy_quote", "ignore")
    response[[i]] <- dplyr::as_tibble(response[[i]])
    # Extract the first date
    first_date <- min(as.numeric(response[[i]]$date))
    # Break if first_date is greater than start_time
    condition <- first_date > as.numeric(start_time) & first_date != last_date
    last_date <- first_date # avoid infinite loops 
    end_time <- format(first_date, scientific = FALSE)
    i <- i + 1
  }
  
  if (!purrr::is_empty(response)) {
    response <- dplyr::bind_rows(response)
    response$pair <- pair 
    response$market <- "fapi"
    response <- binance_formatter(response)
    # Filter to be exactly in from-to range
    response <- dplyr::filter(response, date >= from & date <= to)
    # Arrange with respect to date
    response <- dplyr::arrange(response, date)
    
    if (isTRUE(as_xts)) {
      response <- binance_xts(response)
    }
  } else {
    response <- dplyr::tibble()
  }
  
  attr(response, "api") <- "fapi"
  attr(response, "ip_weight") <- i
  attr(response, "interval") <- interval
  attr(response, "endpoint") <- api_path
  return(response)
}

# Klines implementation for futures COIN-M api 
binance_klines_dapi <- function(pair, interval, from, to, contract_type, uiKlines = FALSE, indexPrice = FALSE, markPrice = FALSE, indexPremium = FALSE, as_xts = FALSE, quiet = FALSE){
  
  # api GET call 
  api_path <- dplyr::case_when(
    uiKlines ~ "continuousKlines",
    indexPrice ~ "indexPriceKlines",
    markPrice ~ "markPriceKlines",
    indexPremium ~ "premiumIndexKlines",
    TRUE ~ "klines"
  )
  
  i <- 1
  response  <- list()
  condition <- TRUE
  last_date <- as_unix_time(to, as_character = FALSE)
  end_time <- as_unix_time(to, as_character = TRUE)
  start_time <- as_unix_time(from, as_character = TRUE)
  while(condition){
    
    api_query <- list(contractType = toupper(contract_type), 
                      interval = interval, 
                      startTime = NULL, 
                      endTime = end_time, 
                      limit = 1500)
    
    if (api_path %in% c("continuousKlines", "indexPriceKlines")) {
      api_query$pair <- pair 
    } else {
      api_query$symbol <- pair 
    }
    
    new_data <- binance_query(api = "dapi", path = api_path, query = api_query)
    
    # Break if new_data is empty 
    if (purrr::is_empty(new_data)) {
      break
    } 
    response[[i]] <- new_data
    # Rename columns
    colnames(response[[i]]) <- c("date", "open", "high", "low", "close", "volume",
                                 "date_close", "volume_quote", "trades", "taker_buy",
                                 "taker_buy_quote", "ignore")
    
    response[[i]] <- dplyr::as_tibble(response[[i]])
    
    # Extract the first date
    first_date <- min(as.numeric(response[[i]]$date))
    # Break if first_date is greater than start_time
    condition <- first_date > as.numeric(start_time) & first_date != last_date
    last_date <- first_date # avoid infinite loops 
    end_time <- format(first_date, scientific = FALSE)
    i <- i + 1
  }
  
  if (!purrr::is_empty(response)) {
    response <- dplyr::bind_rows(response)
    response$pair <- pair 
    response$market <- "dapi"
    response <- binance_formatter(response)
    # Filter to be exactly in from-to range
    response <- dplyr::filter(response, date >= from & date <= to)
    # Arrange with respect to date
    response <- dplyr::arrange(response, date)
    if (isTRUE(as_xts)) {
      response <- binance_xts(response)
    }
  } else {
    response <- dplyr::tibble()
  }
  
  attr(response, "api") <- "dapi"
  attr(response, "ip_weight") <- i
  attr(response, "interval") <- interval
  attr(response, "endpoint") <- api_path
  
  return(response)
} 

# Klines implementation for options api
binance_klines_eapi <- function(pair, interval, from, to, as_xts = FALSE, quiet = FALSE){
  
  i <- 1
  response  <- list()
  condition <- TRUE
  last_date <- as_unix_time(to, as_character = FALSE)
  end_time <- as_unix_time(to, as_character = TRUE)
  start_time <- as_unix_time(from, as_character = TRUE)
  while(condition){
    # GET call
    api_query <- list(symbol = pair,
                      startTime = NULL, 
                      interval = interval, 
                      endTime = end_time, 
                      limit = 1500)
    
    new_data <- binance_query(api = "eapi", path = "klines", query = api_query)
    
    # Break if new_data is empty 
    if (purrr::is_empty(new_data)) {
      break
    }
    response[[i]] <- new_data
    response[[i]] <- dplyr::as_tibble(response[[i]])
    # Extract the first date
    first_date <- min(as.numeric(response[[i]]$closeTime))
    # Break if first_date is greater than start_time
    condition <- first_date > as.numeric(start_time) & first_date != last_date
    last_date <- first_date # avoid infinite loops 
    end_time <- paste0(trunc(first_date/1000), "000")
    i <- i + 1
  }
  
  if (!purrr::is_empty(response)) {
    response <- dplyr::bind_rows(response)
    response$pair <- pair 
    response$market <- "eapi"
    response <- binance_formatter(response)
    # Filter to be exactly in from-to range
    response <- dplyr::filter(response, date >= from & date <= to)
    # Arrange with respect to date
    response <- dplyr::arrange(response, date)
    if (isTRUE(as_xts)) {
      response <- binance_xts(response)
    }
  } else {
    response <- dplyr::tibble()
  }
  
  attr(response, "api") <- "eapi"
  attr(response, "ip_weight") <- i*1
  attr(response, "interval") <- interval
  attr(response, "endpoint") <- "klines"
  
  return(response)
}


