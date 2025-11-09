#' Get Recent Trades
#'
#' Get the last 1000 trades for a trading pair.
#'
#' @param api Character. Reference API. If it is `missing`, the default, will be used `"spot"`. Available options are:
#'   - `"spot"`: for endpoint [api/v3/trades](https://developers.binance.com/docs/binance-spot-api-docs/rest-api/market-data-endpoints#recent-trades-list). The ip weight is 25.
#'   - `"fapi"`: for endpoint [fapi/v1/trades](https://developers.binance.com/docs/derivatives/usds-margined-futures/market-data/rest-api/Recent-Trades-List). The ip weight is 5.
#'   - `"dapi"`: for endpoint [dapi/v1/trades](https://developers.binance.com/docs/derivatives/coin-margined-futures/market-data/rest-api/Recent-Trades-List). The ip weight is 5.
#'   - `"eapi"`: for endpoint [eapi/v1/trades](https://developers.binance.com/docs/derivatives/option/market-data/Recent-Trades-List). The ip weight is 5.
#' @inheritParams binance_query
#' @inheritParams binance_depth 
#'
#' @return A \code{\link[tibble]{tibble}} with 7 columns:
#'   - `date`: \code{\link[=POSIXt-class]{POSIXt}}, trade execution date.
#'   - `market`: Character, selected API.
#'   - `pair`: Character, trading pair.
#'   - `price`: Numeric, trade price.
#'   - `quantity`: Numeric, trade quantity.
#'   - `side`: Character, trade side. Can be `"BUY"` or `"SELL"`.
#'   - `trade_id`: Integer, trade id.
#'
#' @examples
#' # Get last 1000 trades for BTCUSDT
#' binance_last_trades(pair = "BTCUSDT", api = "spot")
#' binance_last_trades(pair = "BTCUSDT", api = "fapi")
#'
#' # Get last 1000 trades for BTCUSD_PERP
#' binance_last_trades(pair = "BTCUSD_PERP", api = "dapi")
#' 
#' # Get last 1000 trades for a put option on BTC
#' binance_last_trades(pair = "BTC-240628-30000-P", api = "eapi")
#'
#' @keywords marketEndpoints
#' @rdname binance_last_trades
#' @name binance_last_trades
#' @export
binance_last_trades <- function(pair, api, quiet = FALSE){

  # Check "pair" argument 
  if (missing(pair) || is.null(pair)) {
    if (!quiet) {
      msg <- paste0('The pair argument is missing with no default')
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
  
  # Create API query
  query <- list(symbol = pair, limit = 1000)
  # GET call 
  response <- binance_query(api = api, path = "trades", query = query, quiet = quiet)
  
  if (!is.null(response$code)) {
    return(NULL)
  } else if (!purrr::is_empty(response)) {
    output <- dplyr::as_tibble(response)
    output$market <- api
    if (api == "eapi") {
      output <- binance_formatter(output)
      output <- dplyr::mutate(output, 
                              side = ifelse(side == -1, "SELL", "BUY"))
    } else {
      output$pair <- pair
      output <- binance_formatter(output)
      output <- dplyr::mutate(output, 
                              side = ifelse(side, "SELL", "BUY"))
    }
    response <- output
  }
  
  attr(response, "api") <- api
  attr(response, "ip_weight") <- ifelse(api == "spot", 10, 5)

  return(response)
}
