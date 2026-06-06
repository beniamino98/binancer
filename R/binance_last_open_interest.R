#' Binance Open Interest
#'
#' Get the current open interest data for a trading pair.
#'
#' @param api Character. Reference API. If it is `missing`, the default, will be used `"fapi"`. Available options are:
#'   - `"fapi"`: for endpoint [fapi/v1/openInterest](https://developers.binance.com/docs/derivatives/usds-margined-futures/market-data/rest-api/Open-Interest). The ip weight is 1.
#'   - `"dapi"`: for endpoint [dapi/v1/openInterest](https://developers.binance.com/docs/derivatives/coin-margined-futures/market-data/rest-api/Open-Interest). The ip weight is 1.
#'   - `"eapi"`: for endpoint [eapi/v1/openInterest](https://developers.binance.com/docs/derivatives/option/market-data/Open-Interest). The ip weight is 0.
#' @param expiration Character or \code{\link[=POSIXt-class]{POSIXt}} object. Used only if `api = "eapi"`. 
#' Expiration date for options contracts. If it is `missing`, the default, will be used \code{\link[=Sys.time]{Sys.time()}}.  
#' @inheritParams binance_query 
#' @inheritParams binance_depth 
#' 
#' @return A \code{\link[tibble]{tibble}} with 4 columns:
#'   - `date`: \code{\link[=POSIXt-class]{POSIXt}}, observation date.
#'   - `market`: Character, API.
#'   - `pair`: Character, trading pair.
#'   - `open_interest`: Numeric, open interest in base currency.
#'   
#' @examplesIf interactive()
#' # Get the open interest data for BTCUSDT
#' binance_last_open_interest(pair = "BTCUSDT", api = "fapi")
#' 
#' # Get the open interest data for BTCUSD_PERP
#' binance_last_open_interest(pair = "BTCUSD_PERP", api = "dapi")
#' 
#' # Get the open interest data for options on BTC
#' binance_last_open_interest(pair = "BTC", api = "eapi", expiration = Sys.Date() + 1)
#'
#' @keywords marketEndpoints
#' @rdname binance_last_open_interest
#' @name binance_last_open_interest
#' @export
binance_last_open_interest <- function(pair, api, expiration, quiet = FALSE){
  
  #  Check "pair" argument 
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
      wrn <- paste0('The `api` argument is missing, default is ', '"', api, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    api <- match.arg(api, choices = c("fapi", "dapi", "eapi"))
  }
  
  # Check "expiration" argument 
  if (missing(expiration) || is.null(expiration)) {
    if (api == "eapi") {
      expiration <- Sys.Date()
      if (!quiet) {
        wrn <- paste0('The `expiration` argument is missing, default is ', '"', expiration, '"')
        cli::cli_alert_warning(wrn)
      }
    }
  } else {
    expiration <- as.Date(expiration)
    # Create a format for expiration date YY-MM-DD
    year_name <- as.character(lubridate::year(expiration))
    year_name <- strsplit(year_name, "")[[1]][3:4]
    year_name <- paste0(year_name, collapse = "")
    # Months 01, 02, ..., 10, 11, 12
    month_name <- as.character(lubridate::month(expiration))
    month_name <- ifelse(stringr::str_length(month_name) == 2, month_name, paste0("0", month_name))
    # Days 01, 02, ..., 10, 11, 12
    day_name <- as.character(lubridate::day(expiration))
    day_name <- ifelse(stringr::str_length(day_name) == 2, day_name, paste0("0", day_name))
    # New expiration date 
    expiration <- paste0(year_name, month_name, day_name)
  }
  
  # Query parameters depends on api 
  if (api == "eapi") {
    query = list(underlyingAsset = pair, expiration = expiration)
  } else if (api %in% c("fapi", "dapi")) {
    query = list(symbol = pair)
  } 
  # GET call 
  response <- binance_query(api = api, path = "openInterest", query = query)
  
  # Output 
  if(!is.null(response$code)){
    return(NULL)
  } else if (!purrr::is_empty(response)) {
    output <- dplyr::as_tibble(response)
    output$api <- api 
    output <- binance_formatter(output)
    response <- output
  }
  
  attr(response, "ip_weight") <- 1
  attr(response, "api") <- api
  
  return(response)
}
