#' Current Average Price 
#'
#' Get the average price of a trading pair in the last 5 minutes. 
#'
#' @inheritParams binance_query 
#' 
#' @details The IP weight for this API call is 2.
#' This function implements the endpoint [api/v3/avgPrice](https://developers.binance.com/docs/binance-spot-api-docs/rest-api/market-data-endpoints#current-average-price) of spot API. 
#' 
#' @return Numeric. Average price in last 5 minutes. 
#' 
#' @examples 
#' # Average price for BTCUSDT 
#' binance_avg_price("BTCUSDT")
#' # Average price for BNBUSDT 
#' binance_avg_price("BNBUSDT")
#' 
#' @keywords marketEndpoints
#' @rdname binance_avg_price
#' @name binance_avg_price
#' @export
binance_avg_price <- function(pair, quiet = FALSE){
  
  # Check "pair" argument 
  if (missing(pair) || is.null(pair)) {
    if (!quiet) {
      msg <- paste0('The `pair` argument is missing with no default.')
      cli::cli_abort(msg)
    }
  } else {
    # Multiple pairs are not allowed 
    if (length(pair) > 1) {
      if (!quiet) {
        msg <- paste0('Multiple `pair` arguments are not allowed.')
        cli::cli_abort(msg)
      }
    } 
    query <- list(symbol = toupper(pair))
  }
  
  # GET call 
  response <- binance_query(api = "spot", path = "avgPrice", query = query)
  # Output 
  if (is.null(response$code)) {
    response <- as.numeric(response$price)
  } 
  
  attr(response, "ip_weight") <- 2
  attr(response, "api") <- "spot"
  
  return(response)
}
