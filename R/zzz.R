.onLoad <- function(libname, pkgname){
  #binance_set_environment()
}

#' UNIX Timestamp
#' 
#' Return current UNIX timestamp in millisecond
#' 
#' @param as_character Logical. If `TRUE`, the default, the timestamp will be returned as Character. Otherwise as numeric. 
#' @example
#' unix_timestamp()
#' @return Time in milliseconds since Jan 1, 1970
#' @keywords internals
#' @noRd
unix_timestamp <- function(as_character = TRUE) {
  
  unix_time <- round(as.numeric(Sys.time())*1e3)
  
  if (isTRUE(as_character)) {
    unix_time <- format(unix_time, scientific = FALSE)
  } 
  
  return(unix_time)
}

#' Conversion to UNIX time
#' 
#' Convert a Posixct date into a UNIX timestamp in millisecond. 
#' 
#' @param x Posixct
#' @param as_character Logical. If `TRUE`, the default, the timestamp will be returned as Character. Otherwise as numeric. 
#' 
#' @examples 
#' # Convert a POSIXct date into a UNIX timestamp 
#' unix_timestamp(as.POSIXct("2023-02-01 00:00:00"))
#' 
#' @return Time in milliseconds from origin. 
#' @keywords internals
#' @noRd
as_unix_time <- function(x, as_character = TRUE) {
  
  # assertive::assert_is_posixct(x)
  unix_time <- round(as.numeric(x)*1e3)
  
  if (isTRUE(as_character)) {
    unix_time <- format(unix_time, scientific = FALSE)
  } 
  
  return(unix_time)
}

#' Binance filters 
#' 
#' Return binance filters applied to spot trading 
#' 
#' @inheritParams binance_query 
#' @inheritParams binance_depth 
#' 
#' @example 
#' binance_filters("BTCUSDT")
#' 
#' @return List 
#' @keywords internals
#' @noRd
binance_filters <- function(pair, api, quiet = FALSE){
  
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
  
  # Check "pair" argument 
  if (missing(pair) || is.null(pair)) {
    if (!quiet) {
      msg <- paste0('The "pair" argument is missing with no default argument.')
      cli::cli_abort(msg)
    }
  } else {
    pair <- toupper(pair)
  }
  
  df_pair <- binance_exchange_info(api = "spot", permissions = "all", pair = pair)
  df_filters <- dplyr::as_tibble(df_pair$filters[[1]])
  
  filters <- list()
  for(i in 1:nrow(df_filters)){
    filters[[i]] <- df_filters[i,2:ncol(df_filters)]
    filters[[i]] <- purrr::map_df(filters[[i]], ~ifelse(is.na(.x), 0, as.numeric(.x)))
  }
  names(filters) <- df_filters$filterType
  return(filters)
}

#' Multiple pairs argument
#' 
#' Wrap multiple pairs into the required format. 
#' 
#' @param pair Character vector. Trading pairs, e.g. `"BTCUSDT"`. Multiple pairs are allowed. 
#' 
#' @examples 
#' binance_query_pair(c("BTCUSDT", "BNBUSDT"))
#' binance_query_pair(c("BTCUSDT"))
#' 
#' @return Character
#' @keywords internals
#' @noRd
binance_query_pair <- function(pair){
  mult_pair <- pair
  attr(mult_pair, "multiple") <- FALSE
  n <- length(pair)
  if (n > 1) {
    mult_pair <- paste0('["', pair[1], '",')
    for(i in 2:n){
      mult_pair <- paste0(mult_pair, '"', pair[i], ifelse(i == n, '"]', '",'))
    }
    attr(mult_pair, "multiple") <- TRUE
  }
  return(mult_pair)
}

#' Format Binance API Responses 
#' 
#' @param data A tibble  
#' 
#' @return A tibble  
#' @keywords internals
#' @noRd
binance_formatter <- function(data){
  
  if (purrr::is_empty(data)) {
    return(dplyr::tibble())
  }
  
  # POSIXct columns 
  date_columns <- list(
    date = "date",
    openTime = "date",
    time = "date",
    timestamp = "date", 
    updateTime = "update_time",
    date_close = "date_close",
    closeTime = "date_close",
    expiryDate = "expiry_date",
    transactTime = "transact_time",
    workingTime = "working_time"
  )
  # Numeric columns
  numeric_columns <- list(
    id = "trade_id",
    agg_id = "agg_id",
    firstId = "first_id",
    firstTradeId = "first_id",
    lastId = "last_id",
    last_update_id = "last_update_id",
    first_update_id = "first_update_id",
    orderId = "order_id",
    orderListId = "order_list_id",
    price = "price",
    askPrice = "ask",
    bidPrice = "bid",
    askQty = "ask_quantity",
    bidQty = "bid_quantity",
    ask_quantity = "ask_quantity",
    bid_quantity = "bid_quantity",
    quantity = "quantity",
    origQty = "quantity",
    qty = "quantity",
    executedQty = "executed_quantity",
    cummulativeQuoteQty = "cum_quote_quantity",
    open = "open",
    openPrice = "open",
    high = "high",
    highPrice = "high",
    low = "low",
    lowPrice = "low",
    close = "close",
    lastPrice = "last",
    prevClosePrice = "last_close",
    weightedAvgPrice = "weighted_price",
    volume = "volume",
    volume_quote = "volume_quote",
    quoteVolume = "volume_quote",
    trades = "trades",
    tradeCount = "trades",
    count = "trades",
    takerVolume = "taker_volume",
    takerAmount = "taker_amount", 
    amount = "amount",
    taker_buy = "taker_buy",
    taker_buy_quote = "taker_buy_quote",
    indexPrice = "index_price",
    strikePrice = "strike",
    exercisePrice = "exercise_price",
    openInterest = "open_interest",
    sumOpenInterest = "open_interest",
    sumOpenInterestUsd = "open_interest_usd",
    sumOpenInterestValue = "open_interest_usd",
    uid = "user_id",
    maker = "maker", 
    taker = "taker",
    buyer = "buyer",
    seller = "seller",
    free = "free",
    locked = "locked",
    makerCommission = "maker_commission",
    takerCommission = "taker_commission",
    buyerCommission = "buyer_commission",
    sellerCommission = "seller_commission",
    commission = "commission",
    buySellRatio = "buy_sell_ratio",
    sellVol = "sell_vol",
    buyVol = "buy_vol",
    longAccount = "long_account",
    longPosition = "long_position",
    shortAccount = "short_account",
    shortPosition = "short_position",
    longShortRatio = "long_short_account",
    takerSellVol = "taker_sell_volume",
    takerSellVolValue = "taker_sell_vol_value",
    takerBuyVol = "taker_buy_vol",
    takerBuyVolValue = "taker_buy_vol_value",
    stopPrice = "stop_price",
    icebergQty = "iceberg_quantity"
  )
  
  # Character columns
  character_columns <- list(
    pair = "pair",
    ps = "pair",
    symbol = "symbol",
    asset = "asset",
    indicator = "indicator",
    status = "status", 
    market = "market",
    side = "side",
    accountType = "account_type",
    permissions = "permissions",
    isBuyerMaker = "side",
    isBuyer = "side",
    isMaker = "is_maker",
    canTrade = "can_trade",
    canWithdraw = "can_withdraw",
    canDeposit = "can_deposit",
    brokered = "brokered",
    requireSelfTradePrevention = "require_self_trade_prevention",
    preventSor = "prevent_sor",
    commissionAsset = "commission_asset",
    timeInForce = "time_in_force",
    type = "type",
    selfTradePreventionMode = "self_trade_prevention_mode",
    clientOrderId = "client_order_id",
    origClientOrderId = "orig_client_order_id",
    isWorking = "is_working"
  )
  
  # Excluded columns
  exclude_columns <- list(
    ignore = "ignore",
    isBestMatch = "isBestMatch",
    contractType = "contractType"
  )
  
  # Create a list with all columns 
  col_names <- append(date_columns, character_columns)
  col_names <- append(col_names, numeric_columns)
  col_names <- append(col_names, exclude_columns)
  # Search for new names 
  new_col_names <- col_names[names(col_names) %in% colnames(data)]
  # Reorder the columns reflecting old names orders  
  data <- data[, names(new_col_names)]
  # Extract new columns names 
  new_col_names <- unlist(new_col_names)
  
  # Convert numeric columns 
  for (v in names(numeric_columns)) {
    column <- data[[v]]
    if (!is.null(column)) {
      data[[v]] <- as.numeric(data[[v]])
    }
  }
  # Convert POSIXct columns 
  for (v in names(date_columns)) {
    column <- data[[v]]
    if (!is.null(column)) {
      # Format in numeric 
      data[[v]] <- format(as.numeric(data[[v]]), scientific = FALSE)
      # Check number of digits 
      n.digits <- length(strsplit(data[[v]][1], "")[[1]])
      # Adjust the number for the correct number of digits (13)
      adj.factor <- as.numeric(paste0("1", paste0(rep("0", 3 + n.digits-13), collapse = ""), collapse = ""))
      data[[v]] <- as.numeric(data[[v]]) / adj.factor
      # Convert in dates 
      data[[v]] <- as.POSIXct(data[[v]],  origin = "1970-01-01")
    }
  }
  # Assign new names 
  if (!purrr::is_empty(new_col_names)) {
    colnames(data) <- new_col_names
  }
  # Remove excluded columns  
  idx_exclude_columns <- !(colnames(data) %in% names(exclude_columns))
  data <- data[,idx_exclude_columns]
  return(data)
}

#' Convert a klines object into an xts object 
#' 
#' @param data A tibble  
#' @return A tibble  
#' @keywords internals
#' @noRd
binance_xts <- function(data){
  pair <- data$pair[1]
  cols <- c("open", "high", "low", "close", "volume")
  df_xts <- data[,cols]
  colnames(df_xts) <- paste0(pair, c(".Open", ".Close", ".High", ".Low", ".Volume"))
  xts::xts(df_xts, order.by = data$date)
}