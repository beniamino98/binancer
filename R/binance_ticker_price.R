#' Symbol Book Ticker
#' 
#' Get last price for a symbol or symbols.
#' 
#' @param pair Character. Trading pair, e.g. `"BTCUSDT"`. Multiple pairs are allowed only if `api` is `"spot"`. 
#' If `missing`, the default, will be returned the book ticker for all the trading pairs. 
#' @param api Character. Reference API. If it is `missing`, the default, will be used `"spot"`. Available options are:
#'   - `"spot"`: for endpoint [api/v3/ticker/price](https://developers.binance.com/docs/binance-spot-api-docs/rest-api/market-data-endpoints#symbol-price-ticker). 
#'   The ip weight is 2 if a symbol is submitted, otherwise is 4. 
#'   The ip weight depends on the number of pairs requested. The maximum ip weight is 80. 
#'   - `"fapi"`: for endpoint [fapi/v1/ticker/price](https://developers.binance.com/docs/derivatives/usds-margined-futures/market-data/rest-api/Symbol-Price-Ticker).
#'   The ip weight is 1 if a symbol is submitted, otherwise is 2. 
#'   - `"dapi"`: for endpoint [dapi/v1/ticker/price](https://developers.binance.com/docs/derivatives/coin-margined-futures/market-data/rest-api/Symbol-Price-Ticker). 
#'   The ip weight is 1 if a symbol is submitted, otherwise is 2. 
#' @inheritParams binance_query
#'                   
#' @return A \code{\link[tibble]{tibble}} with 8 columns:
#'   - `date`: \code{\link[=POSIXt-class]{POSIXt}}, time of the snapshot.
#'   - `pair`: Character, reference trading pair, present only if `api` is `"dapi"`. 
#'   - `symbol`: Character, trading pair.
#'   - `market`: Character, reference API.
#'   - `ask`: Numeric, best ASK price.
#'   - `bid`: Numeric, best BID price.
#'   - `ask_quantity`: Numeric, quantity at best ASK price.
#'   - `bid_quantity`: Numeric, quantity at best BID price.
#'                  
#' @examples
#' # Get book ticker for all pairs
#' binance_ticker_price(api = "spot")
#' binance_ticker_price(api = "fapi")
#' binance_ticker_price(api = "dapi")
#' 
#' # Get book ticker for BTCUSDT
#' binance_ticker_price(pair = "BTCUSDT", api = "spot")
#'
#' @keywords marketEndpoints
#' @rdname binance_ticker_price
#' @name binance_ticker_price
#' @export
binance_ticker_price <- function(pair, api, quiet = FALSE){
  
  # Check `pair` argument 
  if (missing(pair) || is.null(pair)) {
    if (!quiet) {
      msg <- paste0('The pair argument is missing. All pairs will be returned')
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
    api <- match.arg(api, choices = c("spot", "fapi", "dapi"))
  }
  
  query <- list()
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

  # GET call 
  response <- binance_query(api = api, path = c("ticker", "price"), query = query, quiet = quiet)
  
  # structure output dataset 
  if (!purrr::is_empty(response)) {
    response <- dplyr::bind_rows(response)
    response$market <- api
    response <- binance_formatter(response)
    if (api == "spot") {
      response <- dplyr::bind_cols(date = Sys.time(), response)
    }
    response <- dplyr::as_tibble(response)
  } 
  
  attr(response, "api") <- api
  attr(response, "endpoint") <- "ticker/price"
  attr(response, "ip_weight") <- dplyr::case_when(
    is.null(pair) ~ 80, 
    api == "spot" && (length(pair) >= 1 & length(pair) <= 20) ~ 2,
    api == "spot" && (length(pair) > 20 & length(pair) <= 100) ~ 40,
    api == "spot" && (length(pair) > 100) ~ 80, 
    TRUE ~ 1)
  
  return(response)
}
