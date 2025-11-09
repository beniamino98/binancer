#' Binance 24-Hour Ticker Statistics
#'
#' Get 24-hour ticker statistics for a specified trading pair from the selected reference API.
#'
#' @param pair Character. Trading pair, e.g. `"BTCUSDT"`. Multiple pairs are allowed only if `api` is `"spot"`. 
#' If `missing`, the default, will be returned the 24hr ticker for all the trading pairs. 
#' 
#' @param api Character. Reference API. If it is `missing`, the default, will be used `"spot"`. Available options are:
#'   - `"spot"`: for endpoint [api/v3/ticker/24hr](https://binance-docs.github.io/apidocs/spot/en/#24hr-ticker-price-change-statistics). 
#'   The ip weight depends on the number of pairs requested. The maximum ip weight is 80. 
#'   - `"fapi"`: for endpoint [fapi/v1/ticker/24hr](https://binance-docs.github.io/apidocs/futures/en/#24hr-ticker-price-change-statistics). The ip weight is 1.
#'   - `"dapi"`: for endpoint [dapi/v1/ticker/24hr](https://binance-docs.github.io/apidocs/delivery/en/#24hr-ticker-price-change-statistics). The ip weight is 1.
#'   - `"eapi"`: for endpoint [eapi/v1/ticker](https://binance-docs.github.io/apidocs/voptions/en/#24hr-ticker-price-change-statistics). The ip weight is 5.
#'   
#' @param type Character. Type of ticker data. Used only if `api = "spot"`. Default is `"full"`. Available options are:
#'   - `"mini"`: data without ask and bid prices and quantities.
#'   - `"full"`: complete ticker data.
#' @inheritParams binance_query 
#'                   
#' @return A \code{\link[tibble]{tibble}} with 13 columns containing 24-hour ticker statistics, including:
#' open, high, low, close prices, volume, and more.
#'                  
#' @examples
#' # Get full 24-hour ticker for all pairs
#' binance_ticker24h(api = "spot")
#' binance_ticker24h(api = "fapi")
#' binance_ticker24h(api = "dapi")
#' binance_ticker24h(api = "eapi")
#' 
#' # Get full 24-hour ticker for BTCUSDT
#' binance_ticker24h(pair = "BTCUSDT", api = "spot", type = "full")
#'
#' # Get mini 24-hour ticker for BTCUSDT
#' binance_ticker24h(pair = "BTCUSDT", api = "spot", type = "mini")
#'
#' # Get 24-hour ticker for BTCUSDT 
#' binance_ticker24h(pair = "BTCUSDT", api = "fapi")
#'
#' # Get 24-hour ticker for BTCUSD_PERP
#' binance_ticker24h(pair = "BTCUSD_PERP", api = "dapi")
#'
#' # Get 24-hour ticker for a put option on BTCUSDT
#' binance_ticker24h(pair = "BTC-240628-30000-P", api = "eapi")
#'
#' @keywords marketEndpoints
#' @rdname binance_ticker24h
#' @name binance_ticker24h
#' @export
binance_ticker24h <- function(pair, api, type, quiet = FALSE){
  
  # Check `pair` argument 
  if (missing(pair) || is.null(pair)) {
    if (!quiet) {
      msg <- paste0('The pair argument is missing. First 50 pairs will be returned')
      cli::cli_alert_warning(msg)
    }
    pair <- NULL
  } else {
    pair <- toupper(pair)
  }
  
  # Check `api` argument 
  if (missing(api) || is.null(api)) {
    api <- "spot"
    if (!quiet) {
      msg <- paste0('The "api" argument is missing, default is ', '"', api, '"')
      cli::cli_alert_warning(msg)
    }
  } else {
    api <- match.arg(api, choices = c("spot", "fapi", "dapi", "eapi"))
  }
  
  # Check `type` argument 
  query <- list()
  if (api == "spot") {
    if (missing(type) || is.null(type)) {
      type <- "full"
      if (!quiet) {
        msg <- paste0('The type argument is missing, default is ', '"', type, '"')
        cli::cli_alert_warning(msg)
      }
      query$type <- toupper(type)
    } else {
      type <- match.arg(type, choices = c("full", "mini"))
      query$type <- toupper(type)
    }
  } 
  
  # Multiple pairs are allowed only for spot api 
  if (length(pair) > 1) {
    if (api == "spot") {
      query$symbols <- binance_query_pair(pair = pair)
    } else {
      if (!quiet) {
        msg <- paste0('Multiple pairs are allowed only if `api` = "spot".')
        cli::cli_abort(msg)
      }
    }
  } else {
    query$symbol <- pair 
  }
  
  if (api == "eapi") {
    path <- "ticker"
  } else {
    path <- c("ticker", "24hr")
  }
  
  # GET call 
  response <- binance_query(api = api, path = path, query = query, quiet = quiet)
  
  # structure output dataset 
  if (!purrr::is_empty(response)) {
    response <- dplyr::bind_rows(response)
    response$market <- api
    response <- binance_formatter(response)
    response <- dplyr::as_tibble(response)
  } 
  
  attr(response, "api") <- api
  attr(response, "ip_weight") <- dplyr::case_when(
    is.null(pair) ~ 80, 
    api == "spot" && (length(pair) >= 1 & length(pair) <= 20) ~ 2,
    api == "spot" && (length(pair) > 20 & length(pair) <= 100) ~ 40,
    api == "spot" && (length(pair) > 100) ~ 80, 
    api == "eapi" ~ 5,
    TRUE ~ 1)
  
  return(response)
}

