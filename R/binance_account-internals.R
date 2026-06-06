credentials <- new.env()

#' Check Binance Api Keys
#' 
#' Check if Binance Api Keys were set previously.
#' 
#' @return No return values, but fails when credentials were not set.
#' 
#' @keywords AccountEndpoints
#' @rdname binance_check_credentials
#' @name binance_check_credentials
#' @noRd 
binance_check_credentials <- function(){
  
  if (is.null(credentials$secret)) {
    msg <- 'Binance API secret not set. Call binance_credentials()'
    cli::cli_abort(msg)
  }
  if (is.null(credentials$key)) {
    msg <- 'Binance API key not set. Call binance_credentials()'
    cli::cli_abort(msg)
  }
}

#' Return Binance API secret stored in the environment
#' 
#' @return Character, Binance API secret. 
#' 
#' @keywords AccountEndpoints
#' @rdname binance_secret
#' @name binance_secret
#' @noRd
binance_secret <- function(){
  binance_check_credentials()
  credentials$secret
}

#' Return Binance API key stored in the environment
#' 
#' @return Character, Binance API key.
#' 
#' @keywords AccountEndpoints
#' @rdname binance_secret
#' @name binance_secret
#' @noRd
binance_key <- function(){
  binance_check_credentials()
  credentials$key
}

#' Set Binance API Keys
#' 
#' Sets the API key and secret to interact with authenticated Binance API endpoints.
#' 
#' @param key Character scalar. API key. If missing or `NULL`, the stored key is removed.
#' @param secret Character scalar. API secret. If missing or `NULL`, the stored secret is removed.
#' 
#' @return Invisibly returns `NULL` after updating credentials in the package namespace.
#' 
#' @examples \dontrun{
#' # Add api keys 
#' binance_credentials('foo', 'bar')
#' # Remove api keys 
#' binance_credentials()
#' }
#' 
#' @keywords AccountEndpoints
#' @rdname binance_credentials
#' @name binance_credentials
#' @export
binance_credentials <- function(key, secret){
  
  check_rm_key <- missing(key) || is.null(key)
  if (check_rm_key){
    # Remove API key is key is missing or NULL
    credentials$key <- NULL
    msg <- 'Binance API key removed.'
    cli::cli_alert_info(msg)
  } else {
    if (!is.character(key) || length(key) != 1 || is.na(key) || identical(key, "")) {
      cli::cli_abort("The {.arg key} argument must be a non-empty character scalar.")
    }
    # Set API key 
    credentials$key <- key
    msg <- 'Binance API key set.'
    cli::cli_alert_success(msg)
  }
  
  check_rm_secret <- missing(secret) || is.null(secret)
  if (check_rm_secret){
    # Remove API secret is secret is missing or NULL
    credentials$secret <- NULL
    msg <- 'Binance API secret removed.'
    cli::cli_alert_info(msg)
  } else {
    if (!is.character(secret) || length(secret) != 1 || is.na(secret) || identical(secret, "")) {
      cli::cli_abort("The {.arg secret} argument must be a non-empty character scalar.")
    }
    # Set API secret 
    credentials$secret <- secret
    msg <- 'Binance API secret set.'
    cli::cli_alert_success(msg)
  }
  invisible(NULL)
}

#' Sign a message 
#' 
#' Sign the query string for Binance API
#' 
#' @param query Named list
#' 
#' @return string
#' 
#' @examples \dontrun{
#' signature(list(foo = 'bar', z = 4))
#' }
#' @keywords AccountEndpoints
#' @rdname binance_sign
#' @name binance_sign
#' @noRd
binance_sign <- function(query){
  query$timestamp <- unix_timestamp()
  query$signature <- digest::hmac(
    algo = 'sha256',
    key = binance_secret(),
    object = paste(mapply(paste, names(query), query, sep = '=', USE.NAMES = FALSE), collapse = '&'))
  return(query)
}
