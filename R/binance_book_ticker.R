#' Symbol Order Book Ticker
#' 
#' Get best ASK and BID price and quantities on the order book for a pair or a list of pairs.
#' 
#' @param pair Character. Trading pair, e.g. `"BTCUSDT"`. Multiple pairs are allowed only if `api` is `"spot"`. 
#' If `missing`, the default, will be returned the book ticker for all the trading pairs. 
#' @param api Character. Reference API. If it is `missing`, the default, will be used `"spot"`. Available options are:
#'   - `"spot"`: for endpoint [api/v3/ticker/bookTicker](https://developers.binance.com/docs/binance-spot-api-docs/rest-api/market-data-endpoints#symbol-order-book-ticker). 
#'   The ip weight is 2 if a symbol is submitted, otherwise is 4. The maximum ip weight is 200. 
#'   The ip weight depends on the number of pairs requested. The maximum ip weight is 80. 
#'   - `"fapi"`: for endpoint [fapi/v1/ticker/bookTicker](https://developers.binance.com/docs/derivatives/usds-margined-futures/market-data/rest-api/Symbol-Order-Book-Ticker).
#'   The ip weight is 2 if a symbol is submitted, otherwise is 5. 
#'   - `"dapi"`: for endpoint [dapi/v1/ticker/bookTicker](https://developers.binance.com/docs/derivatives/coin-margined-futures/market-data/rest-api/Symbol-Order-Book-Ticker). 
#'   The ip weight is 2 if a symbol is submitted, otherwise is 5. 
#' @inheritParams binance_query
#' 
#' @return A \code{\link[=data.frame-class]{data.frame}} with 8 columns:
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
#' binance_book_ticker(api = "spot")
#' binance_book_ticker(api = "fapi")
#' binance_book_ticker(api = "dapi")
#' 
#' # Get book ticker for BTCUSDT
#' binance_book_ticker(pair = "BTCUSDT", api = "spot")
#'
#' @keywords marketEndpoints
#' @rdname binance_book_ticker
#' @name binance_book_ticker
#' @export
binance_book_ticker <- function(pair, api, quiet = FALSE){
  # Check `pair` argument 
  if (missing(pair) || is.null(pair)) {
    if (!quiet) {
      msg <- paste0('The pair argument is missing. The first 50 pairs will be returned')
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
  # Multiple pairs are allowed only for spot api 
  query <- list()
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
  response <- binance_query(api = api, path = c("ticker", "bookTicker"), query = query, quiet = quiet)
  
  # Output 
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

  return(response)
}
