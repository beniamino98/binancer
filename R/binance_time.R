#' Binance Server Time
#'
#' Get the current server time from Binance API. 
#' @param api  Character. Reference API. If it is `missing`, the default, will be used `"spot"`. Available options are:
#'   - `"spot"`: for [spot API](https://developers.binance.com/docs/binance-spot-api-docs/rest-api/general-endpoints#check-server-time).
#'   - `"fapi"`: for [futures USD-m API](https://developers.binance.com/docs/derivatives/usds-margined-futures/market-data/rest-api/Check-Server-Time).
#'   - `"dapi"`: for [futures COIN-m API](https://developers.binance.com/docs/derivatives/coin-margined-futures/market-data/rest-api/Check-Server-Time).
#'   - `"eapi"`: for [options API](https://developers.binance.com/docs/derivatives/option/market-data).
#' @inheritParams binance_query
#' 
#' @return A \code{\link[=POSIXt-class]{POSIXt}} object. The server time for the reference API.
#' @details The IP weight for this API call is 1.
#' 
#' @examples
#' # Get the server time
#' binance_time("spot")
#' binance_time("fapi")
#' binance_time("dapi")
#' binance_time("eapi")
#'
#' @keywords generalEndpoints 
#' @rdname binance_time
#' @name binance_time
#' @export
binance_time <- function(api, quiet = FALSE){
  
  # Check "api" argument 
  if (missing(api) || is.null(api)) {
    api <- "spot"
    if (!quiet) {
      wrn <- paste0('The "api" argument is missing, default is ', '"', api, '"')
      cli::cli_alert_warning(wrn)
    }
  } 
  
  # GET call
  response <- binance_query(api = api, path = "time", query = NULL, quiet = quiet)

  if (purrr::is_empty(response)) {
    response <- ""
  } else {
    response <- as.POSIXct(response$serverTime/1000, origin = "1970-01-01")
  }
  
  attr(response, "api") <- api
  attr(response, "ip_weight") <- 1
  attr(response, "endpoint") <- "time"
  
  return(response)
}
