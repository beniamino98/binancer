#' Symbol Trading Day TIcker
#' 
#' Price change statistics for a trading day.
#' 
#' @param pair Character. Trading pair, e.g. `"BTCUSDT"`. Multiple pairs are allowed with a maximum of 100 per request. 
#' @param time_zone Character, reference time zone. 
#' @inheritParams binance_query 
#'                   
#' @return A \code{\link[tibble]{tibble}} with 14 columns:
#'   - `date`: \code{\link[=POSIXt-class]{POSIXt}}, open date of the reference period.
#'   - `date_close`: \code{\link[=POSIXt-class]{POSIXt}}, close date of the reference period.
#'   - `symbol`: Character, trading pair.
#'   - `market`: Character, reference API.
#'   - `first_id`: Numeric, first trade id. 
#'   - `last_id`: Numeric, last trade id. 
#'   - `open`: Numeric, open price.
#'   - `high`: Numeric, highest price.
#'   - `low`: Numeric, lowest price.
#'   - `last`: Numeric, last price.
#'   - `weighted_price`: Numeric, volume-weighted price.
#'   - `volume`: Numeric, volume.
#'   - `volume_quote`: Numeric, volume in terms of quote asset.
#'   - `trades`: Numeric, number of trades
#'                  
#' @examples
#' # Get day ticker for BTCUSDT
#' binance_ticker_day("BTCUSDT")

#' @keywords marketEndpoints
#' @rdname binance_ticker_day
#' @name binance_ticker_day
#' @export
binance_ticker_day <- function(pair, time_zone = 0, quiet = FALSE){
  
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
  
  query <- list(timeZone = time_zone, type = "FULL")
  # Multiple pairs are allowed only for spot api 
  if (length(pair) > 1) {
      query$symbols <- binance_query_pair(pair = pair)
  } else {
    query$symbol <- pair 
  }
  
  # GET call 
  response <- binance_query(api = "spot", path = c("ticker", "tradingDay"), query = query, quiet = quiet)
  # structure output dataset 
  if (!purrr::is_empty(response)) {
    response <- dplyr::bind_rows(response)
    response$market <- "spot"
    response <- binance_formatter(response)
    response <- dplyr::as_tibble(response)
  } 
  
  attr(response, "api") <- "spot"
  attr(response, "endpoint") <- "ticker/tradingDay"
  attr(response, "ip_weight") <- 4
  return(response)
}

