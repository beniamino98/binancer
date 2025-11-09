#' Binance Order Book Snapshot
#'
#' Retrieve a snapshot of the order book for a trading pair. 
#'
#' @param pair Character. Trading pair, e.g. `"BTCUSDT"`.
#' @param api Character. Reference API. If it is `missing`, the default, will be used `"spot"`. Available options are:
#'   - `"spot"`: for endpoint [api/v3/depth](https://developers.binance.com/docs/binance-spot-api-docs/rest-api/market-data-endpoints#order-book). The ip weight is 250.
#'   - `"fapi"`: for endpoint [fapi/v1/depth](https://developers.binance.com/docs/derivatives/usds-margined-futures/market-data/rest-api/Order-Book). The ip weight is 20.
#'   - `"dapi"`: for endpoint [dapi/v1/depth](https://developers.binance.com/docs/derivatives/coin-margined-futures/market-data/rest-api/Order-Book). The ip weight is 20.
#'   - `"eapi"`: for endpoint [eapi/v1/depth](https://developers.binance.com/docs/derivatives/option/market-data/Order-Book). The ip weight is 1.
#' @inheritParams binance_query 
#'               
#' @return A \code{\link[tibble]{tibble}} with 7 columns:
#'   - `last_update_id`: Integer, id of the last snapshot.
#'   - `date`: \code{\link[=POSIXt-class]{POSIXt}}, time of the snapshot.
#'   - `market`: Character, reference API.
#'   - `pair`: Character, trading pair.
#'   - `side`: Character, side of the limit orders in the book. Can be `"ASK"` or `"BID"`.
#'   - `price`: Numeric, price level.
#'   - `quantity`: Numeric, quantity for each price level.
#'               
#' @examples
#' # Get the order book for BTCUSDT
#' binance_depth(pair = "BTCUSDT", api = "spot")
#' binance_depth(pair = "BTCUSDT", api = "fapi")
#'
#' # Get the order book for BTCUSD_PERP
#' binance_depth(pair = "BTCUSD_PERP", api = "dapi")
#'
#' # Get the order book for a put option on BTC 
#' binance_depth(pair = "BTC-240628-30000-P", api = "eapi")
#'
#' @keywords marketEndpoints
#' @rdname binance_depth
#' @name binance_depth
#' @export
binance_depth <- function(pair, api, quiet = FALSE){
  
  # Check "pair" argument 
  if (missing(pair) || is.null(pair)) {
    if (!quiet) {
      msg <- paste0('The "pair" argument is missing with no default argument.')
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
  
  limit <- ifelse(api == "spot", 5000, 1000)
  # GET call
  response <- binance_query(
    api = api, 
    path = "depth", 
    method = "GET", 
    query = list(symbol = pair, limit = limit), 
    quiet = quiet)
  
  if (!is.null(response$code)) {
    return(NULL)
  } else {
    # Save update time
    update_time <- Sys.time()
    # Save last update id 
    last_update_id <- ifelse(api == "eapi", as.numeric(response$u), as.numeric(response$lastUpdateId))
    
    # BID data 
    if (!purrr::is_empty(response$bids)) {
      colnames(response$bids) <- c("price", "quantity")
      df_bid <- dplyr::as_tibble(response$bids)
      df_bid$side <- "BID"
      df_bid$price <- as.numeric(df_bid$price)
      df_bid$quantity <- as.numeric(df_bid$quantity)
    } else {
      df_bid <- dplyr::tibble(price = NA_integer_, quantity = NA_integer_, side = "BID")
    }
    
    # ASK data 
    if (!purrr::is_empty(response$asks)) {
      colnames(response$asks) <- c("price", "quantity")
      df_ask <- dplyr::as_tibble(response$asks)
      df_ask$side <- "ASK"
      df_ask$price <- as.numeric(df_ask$price)
      df_ask$quantity <- as.numeric(df_ask$quantity)
    } else {
      df_ask <- dplyr::tibble(price = NA_integer_, quantity = NA_integer_, side = "ASK")
    }

    # Depth data 
    response <- dplyr::bind_rows(df_bid, df_ask)
    # Add extra information (date, pair and market)
    response$date <- update_time
    response$last_update_id <- last_update_id
    response$pair <- pair
    response$market <- api
    # Select and reorder variables 
    response <- response[,c("last_update_id", "date", "market", "pair", "side", "price", "quantity")]
    # Arrange by price (descending) 
    response <- response[order(response$price, decreasing = TRUE),]
  }

  attr(response, "ip_weight") <- ifelse(api == "spot", 250, ifelse(api == "eapi", 1, 20))
  attr(response, "api") <- api
  
  return(response)
}
