#' Ping to Binance REST API
#'
#' Check the connection to the Binance API. 
#' @param api  Character. Reference API. If it is `missing`, the default, will be used `"spot"`. Available options are:
#'   - `"spot"`: for [spot API](https://developers.binance.com/docs/binance-spot-api-docs/rest-api/general-endpoints#test-connectivity).
#'   - `"fapi"`: for [futures USD-m API](https://developers.binance.com/docs/derivatives/usds-margined-futures/market-data/rest-api).
#'   - `"dapi"`: for [futures COIN-m API](https://developers.binance.com/docs/derivatives/coin-margined-futures/market-data/rest-api).
#'   - `"eapi"`: for [options API](https://developers.binance.com/docs/derivatives/option/market-data/Test-Connectivity).
#' @inheritParams binance_query 
#' 
#' @details The IP weight for this API call is 1 for all the APIs.
#' @return A logical value. It is `TRUE` if the connection was successful, otherwise it is `FALSE`.
#'
#' @examples
#' # Test connection to spot api
#' binance_ping("spot")
#' # Test connection to futures usd-m api
#' binance_ping("fapi")
#' # Test connection to futures coin-m api
#' binance_ping("dapi")
#' # Test connection to options api
#' binance_ping("eapi")
#'
#' @keywords generalEndpoints
#' @rdname binance_ping
#' @name binance_ping
#' @export
binance_ping <- function(api, quiet = FALSE){
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
  # Response
  response <- binance_query(api = api, path = "ping", query = NULL)
  
  if (purrr::is_empty(response)) {
    response <- TRUE
  } else {
    response <- FALSE
  }
  
  attr(response, "api") <- api
  attr(response, "ip_weight") <- 1
  return(response)
}


