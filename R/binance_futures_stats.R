#' Futures Statistics 
#'
#' Get the historical statistics for futures.
#'
#' @param pair Character. Trading pair, e.g. `"BTCUSDT"` or `"BTCUSD"`.
#' @param api Character, reference API. Available options are `"fapi"` or `"dapi"`.
#' @param interval Character. Default is `"1h"`. Time interval for open interest data. Available intervals are: 
#'   - Minutely: `"5m"`, `"15m"` and `"30m"`.
#'   - Hourly: `"1h"`, `"2h"`, `"4h"`, `"6h"`, `"8h"` and `"12h"`.
#'   - Daily: `"1d"`.
#' @param from Character or \code{\link[=POSIXt-class]{POSIXt}} object. Start time for historical data, only last 30 days are available. 
#' If it is `missing`, the default, will be used as start date `Sys.time()-lubridate::days(30)`.
#' @param to Character or \code{\link[=POSIXt-class]{POSIXt}} object. End time for historical data, only last 30 days are available. 
#' If it is `missing`, the default, will be used as start date `Sys.time()`.
#' @param indicator Character reference statistic. Available indicators are: 
#' - `takerlongshortRatio`: only when `api = "fapi"`, for the endpoint [futures/data/takerlongshortRatio](https://developers.binance.com/docs/derivatives/usds-margined-futures/market-data/rest-api/Taker-BuySell-Volume).
#'  Taker Buy/Sell Volume. Taker Buy Volume: the total volume of buy orders filled by takers within the period. Taker Sell Volume: the total volume of sell orders filled by takers within the period.
#' - `takerBuySellVol`: only when `api = "dapi"`, for the endpoint [futures/data/takerBuySellVol](https://developers.binance.com/docs/derivatives/coin-margined-futures/market-data/rest-api/Taker-Buy-Sell-Volume).
#'  Taker Buy/Sell Volume. 
#' - `globalLongShortAccountRatio`: for the endpoint [futures/data/globalLongShortAccountRatio](https://developers.binance.com/docs/derivatives/usds-margined-futures/market-data/rest-api/Long-Short-Ratio).
#'  The proportion of net long and net short positions to total open positions.
#' - `topLongShortPositionRatio`: for the endpoint [futures/data/topLongShortPositionRatio](https://developers.binance.com/docs/derivatives/usds-margined-futures/market-data/rest-api/Top-Trader-Long-Short-Ratio).
#'  The proportion of net long and net short positions to total open positions of the top 20% users with the highest margin balance.
#' - `topLongShortAccountRatio`: for the endpoint [futures/data/topLongShortAccountRatio](https://developers.binance.com/docs/derivatives/usds-margined-futures/market-data/rest-api/Top-Long-Short-Account-Ratio).
#'  The proportion of net long and net short accounts to total accounts of the top 20% users with the highest margin balance. Each account is counted once only.
#'  
#' @inheritParams binance_query
#' 
#' @return A \code{\link[tibble]{tibble}} 
#'   
#' @details The IP weight for this API call is 1, and the data source is memory. 
#' The historical open interest data are only available for the last 30 days. 
#' 
#' @examples
#' # Statistics in USD-m market     
#' binance_futures_stats(pair = "BTCUSDT", 
#'                       api = "fapi", 
#'                       interval = "1d", 
#'                       indicator = "takerlongshortRatio", 
#'                       from = Sys.Date()-2)
#' binance_futures_stats(pair = "BTCUSDT", 
#'                       api = "fapi", 
#'                       interval = "1d", 
#'                       indicator = "globalLongShortAccountRatio", 
#'                       from = Sys.Date()-2)
#' binance_futures_stats(pair = "BTCUSDT", 
#'                       api = "fapi", 
#'                       interval = "1d", 
#'                       indicator = "topLongShortPositionRatio", 
#'                       from = Sys.Date()-2)
#' binance_futures_stats(pair = "BTCUSDT", 
#'                       api = "fapi", 
#'                       interval = "1d", 
#'                       indicator = "topLongShortAccountRatio", 
#'                       from = Sys.Date()-2)    
#'                       
#' # Statistics in COIN-m market                       
#' binance_futures_stats(pair = "BTCUSD", 
#'                       api = "dapi", 
#'                       interval = "1d", 
#'                       indicator = "takerBuySellVol", 
#'                       from = Sys.Date()-2)
#' binance_futures_stats(pair = "BTCUSD", 
#'                       api = "dapi", 
#'                       interval = "1d", 
#'                       indicator = "globalLongShortAccountRatio", 
#'                       from = Sys.Date()-2)
#' binance_futures_stats(pair = "BTCUSD", 
#'                       api = "dapi", 
#'                       interval = "1d", 
#'                       indicator = "topLongShortPositionRatio", 
#'                       from = Sys.Date()-2)
#' binance_futures_stats(pair = "BTCUSD", 
#'                       api = "dapi", 
#'                       interval = "1d", 
#'                       indicator = "topLongShortAccountRatio", 
#'                       from = Sys.Date()-2) 
#' @keywords marketEndpoints
#' @rdname binance_futures_stats
#' @name binance_futures_stats
#' @export
binance_futures_stats <- function(pair, api, interval, indicator, from, to, quiet = FALSE){
 
  # Check "pair" argument 
  if (missing(pair) || is.null(pair)) {
    if (!quiet) {
      wrn <- paste0('The pair argument is missing with no default.')
      cli::cli_abort(wrn)
    }
  } else {
    pair <- toupper(pair)
  }
  
  # Check "api" argument 
  if (missing(api) || is.null(api)) {
    api <- "fapi"
    if (!quiet) {
      wrn <- paste0('The "api" argument is missing, default is ', '"', api, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    api <- match.arg(api, choices = c("fapi", "dapi"))
  }
  
  # Check "interval" argument 
  if (missing(interval) || is.null(interval)) {
    interval <- "1h"
    if (!quiet) {
      wrn <- paste0('The `interval` argument is missing, default is ', '"', interval, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    av_int <- c("5m", "15m", "30m","1h", "2h", "4h", "6h", "12h", "1d")
    interval <- match.arg(interval, choices = av_int)
  }
  
  # Available indicators
  av_indicators <- c("globalLongShortAccountRatio","topLongShortPositionRatio", "topLongShortAccountRatio")
  # Check "indicator" argument 
  if (missing(indicator) || is.null(indicator)) {
    indicator <- av_indicators[1]
    if (!quiet) {
      wrn <- paste0('The indicator argument is missing, default is ', '"', indicator, '"')
      warning(wrn)
    }
  } else {
    if (api == "fapi") {
      av_indicators <- c("takerlongshortRatio", av_indicators)
    } else {
      av_indicators <- c("takerBuySellVol", av_indicators)
    }
    indicator <- match.arg(indicator, choices = av_indicators) 
  }
  
  sys_time <- Sys.time()
  # Check "from" argument
  if (missing(from) || is.null(from)) {
    from <- sys_time - lubridate::days(30)
    if (!quiet) {
      msg <- paste0('The "from" argument is missing, default is ', '"', from, '"')
      cli::cli_alert_warning(msg)
    }
  } else {
    from <- as.POSIXct(from, origin = "1970-01-01")
    max_from <- sys_time - lubridate::days(30)
    if (!(from < max_from) & !quiet) {
      msg <- paste0('The "from" argument is greater than the maximum value ', max_from)
      cli::cli_alert_warning(msg)
      from <- max_from 
    } 
  }
  
  # Check "to" argument
  if (missing(to) || is.null(to)) {
    to <- sys_time 
    if (!quiet) {
      wrn <- paste0('The "to" argument is missing, default is ', '"', to, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    to <- as.POSIXct(to, origin = "1970-01-01")
    min_to <- sys_time - lubridate::days(30)
    if (!(to < min_to) & !quiet) {
      msg <- paste0('The "to" argument is lower than the minimum value ', min_to)
      cli::cli_alert_warning(msg)
      to <- sys_time 
    } 
  }

  i <- 1
  response  <- list()
  condition <- TRUE
  end_time <- as_unix_time(to, as_character = TRUE)
  start_time <- as_unix_time(from, as_character = TRUE)
  last_date <- as_unix_time(to, as_character = FALSE)
  while(condition){
    # query
    if (api == "fapi"){
      api_query <- list(symbol = pair, period = interval, startTime = NULL, endTime = end_time, limit = 500)
    } else {
      api_query <- list(pair = pair, period = interval, startTime = NULL, endTime = end_time, limit = 500) 
    }
    # api GET call 
    new_data <- binance_query(api = api, path = c("futures","data", indicator), query = api_query, use_base_path = FALSE)
    # Break Condition: new_data is empty 
    if(purrr::is_empty(new_data)){
      break
    }
    response[[i]] <- new_data
    response[[i]] <- dplyr::as_tibble(response[[i]])
    
    # extract the minimum date
    first_date <- min(as.numeric(response[[i]]$timestamp))
    # Break Condition: IF first_date is greater than start_time THEN stop
    condition <- first_date > as.numeric(start_time) & first_date < last_date
    last_date <- end_time # needed avoid infinite loops 
    # ELSE: use the first_date as new endTime
    end_time <- paste0(trunc(first_date/1000), "000")
    i <- i + 1
  }
  
  # Adjust the Response
  if(!purrr::is_empty(response)){
    response <- dplyr::bind_rows(response)
    response <- binance_formatter(response)
    response <- dplyr::filter(response, date >= from & date <= to)
    response <- dplyr::arrange(response, date)
  }
  
  return(response)
}


