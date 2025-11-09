# ------------------------------------------------------ binance_ws_cleaner ------------------------------------------------------
# Clean different binance websocket responses
binance_ws_cleaner <- function(data){
  UseMethod("binance_ws_cleaner")
}

# Cleaner for websocket "aggTrade" endpoint
binance_ws_cleaner.aggTrade <- function(data){

  if (purrr::is_empty(data)) {
    return(dplyr::tibble())
  }

  output <- dplyr::bind_rows(data)
  output <- dplyr::select(output,
                          date = "E", 
                          agg_id = "a", 
                          first_id = "f", 
                          last_id = "l",
                          pair = "s", 
                          price = "p", 
                          quantity = "q", 
                          side = "m")
  output <- dplyr::mutate(output,
                          date = as.POSIXct(date/1000, origin = "1970-01-01"),
                          price = as.numeric(price),
                          quantity = as.numeric(quantity),
                          side = ifelse(side, "SELL", "BUY"))
  return(output)
}

# Cleaner for websocket "trade" endpoint
binance_ws_cleaner.trade <- function(data){

  if (purrr::is_empty(data)) {
    return(dplyr::tibble())
  }
  
  output <- dplyr::bind_rows(data)
  output <- dplyr::select(output, 
                          date = "T", 
                          id = "t", 
                          buy_id = "b", 
                          sell_id = "a", 
                          pair = "s", 
                          price = "p", 
                          quantity = "q", 
                          side = "m")
  output <- dplyr::mutate(output,
                          date = as.POSIXct(date/1000, origin = "1970-01-01"),
                          price = as.numeric(price),
                          quantity = as.numeric(quantity),
                          side = ifelse(side, "SELL", "BUY"))
  return(output)
}

# Cleaner for websocket "miniticker" endpoint
binance_ws_cleaner.miniTicker <- function(data){

  if (purrr::is_empty(data)) {
    return(data)
  }

  output <- dplyr::bind_rows(data)
  output <- dplyr::select(output, 
                          date = "E", 
                          pair = "s", 
                          close = "c", 
                          open = "o", 
                          high = "h", 
                          low = "l", 
                          volume = "v", 
                          quantity = "q")
  output <- dplyr::mutate(output,
                          date = as.POSIXct(date/1000, origin = "1970-01-01"),
                          close = as.double(close),
                          open = as.double(open),
                          high = as.double(high),
                          low = as.double(low),
                          volume = as.double(volume),
                          quantity = as.double(quantity))

  return(output)
}

# Cleaner for websocket "ticker" endpoint
binance_ws_cleaner.ticker <- function(data){

  if (purrr::is_empty(data)) {
    return(data)
  }

  output <- dplyr::bind_rows(data)
  output <- dplyr::select(output,
                          date = "E", 
                          pair = "s", 
                          price_change = "p", 
                          price_change_perc = "P", 
                          weighted_price = "w",
                          last_quantity = "Q", 
                          bid = "b", 
                          bid_quantity = "B",
                          ask = "a", 
                          ask_quantity = "A",
                          open = "o", 
                          close = "c", 
                          high = "h", 
                          low = "l", 
                          volume = "v", 
                          quantity = "q", 
                          trades = "n")
  
  output <- dplyr::mutate(output,
                          date = as.POSIXct(date/1000, origin = "1970-01-01"),
                          price_change = as.double(price_change),
                          price_change_perc = as.double(price_change_perc),
                          weighted_price = as.double(weighted_price),
                          last_quantity = as.double(last_quantity),
                          bid = as.double(bid),
                          bid_quantity = as.double(bid_quantity),
                          ask = as.double(ask),
                          ask_quantity = as.double(ask_quantity),
                          close = as.double(close),
                          open = as.double(open),
                          high = as.double(high),
                          low = as.double(low),
                          volume = as.double(volume),
                          quantity = as.double(quantity),
                          trades = as.integer(trades))
  return(output)
}

# Cleaner for websocket "bookTicker" endpoint
binance_ws_cleaner.bookTicker <- function(data){

  if (purrr::is_empty(data)) {
    return(data)
  }
  
  output <- dplyr::bind_rows(data)
  output <- dplyr::select(output, 
                          date = "u", 
                          pair = "s", 
                          bid = "b", 
                          quantity_bid = "B", 
                          ask = "a", 
                          quantity_ask = "A")
  output <- dplyr::mutate(output,
                          date = Sys.time(),
                          bid = as.double(bid),
                          quantity_bid = as.double(quantity_bid),
                          ask = as.double(ask),
                          quantity_ask = as.double(quantity_ask))
  
  return(output)
}

# Cleaner for websocket "kline" endpoint
binance_ws_cleaner.kline <- function(data){
  
  if (purrr::is_empty(data)) {
    return(data)
  }

  output <- dplyr::bind_rows(data$k)
  output <- dplyr::select(output,
                          date = "t", 
                          date_close = "T",
                          pair = "s",
                          open = "o", 
                          close = "c", 
                          high = "h", 
                          low = "l", 
                          trades = "n", 
                          is_closed = "x",
                          volume = "v", 
                          volume_quote = "q", 
                          taker_buy = "V", 
                          taker_buy_quote = "Q")
  output <- dplyr::mutate(output,
                          date = as.POSIXct(date/1000, origin = "1970-01-01"),
                          date_close = as.POSIXct(date_close/1000, origin = "1970-01-01"),
                          close = as.double(close),
                          open = as.double(open),
                          high = as.double(high),
                          low = as.double(low),
                          trades = as.integer(trades),
                          volume = as.double(volume),
                          volume_quote = as.double(volume_quote),
                          taker_buy = as.double(taker_buy),
                          taker_buy_quote = as.double(taker_buy_quote))
  
  return(output)
}

# Cleaner for websocket "binance_ws_cleaner.continuousKline" endpoint
binance_ws_cleaner.continuousKline <- function(data){
  
  if (purrr::is_empty(data)) {
    return(data)
  }
  
  output <- dplyr::bind_rows(data$k)
  output <- dplyr::mutate(output, pair = data$ps, contract = data$ct)
  output <- dplyr::select(output,
                          date = "t", 
                          date_close = "T", 
                          pair = "s",
                          open = "o", 
                          close = "c", 
                          high = "h", 
                          low = "l", 
                          trades = "n", 
                          is_closed = "x",
                          volume = "v", 
                          volume_quote = "q", 
                          taker_buy = "V", 
                          taker_buy_quote = "Q")
  output <- dplyr::mutate(output,
                          date = as.POSIXct(date/1000, origin = "1970-01-01"),
                          date_close = as.POSIXct(date_close/1000, origin = "1970-01-01"),
                          close = as.double(close),
                          open = as.double(open),
                          high = as.double(high),
                          low = as.double(low),
                          trades = as.integer(trades),
                          volume = as.double(volume),
                          volume_quote = as.double(volume_quote),
                          taker_buy = as.double(taker_buy),
                          taker_buy_quote = as.double(taker_buy_quote))
  
  return(output)
}

# Cleaner for websocket "markPrice" endpoint
binance_ws_cleaner.markPrice <- function(data){

  if (purrr::is_empty(data)) {
    return(data)
  }
  
  output <- dplyr::bind_rows(data)
  output <- dplyr::mutate(output, date = Sys.time())
  output <- dplyr::select(output, 
                          date, 
                          next_funding_date = "T", 
                          pair = "s", 
                          mark_price = "p", 
                          index_price = "i", 
                          settlement = "P", 
                          funding_rate = "r")
  output <- dplyr::mutate(output,
                          next_funding_date = as.POSIXct(next_funding_date/1000, origin = "1970-01-01"),
                          mark_price = as.numeric(mark_price),
                          index_price = as.numeric(index_price),
                          settlement = as.numeric(settlement),
                          funding_rate = as.numeric(funding_rate))
  
  return(output)
}

# Cleaner for websocket "forceOrder" endpoint
binance_ws_cleaner.forceOrder <- function(data){
  
  if (purrr::is_empty(data)) {
    return(data)
  }
  output <- dplyr::bind_rows(data$o)
  output <- dplyr::select(output, 
                          date = "T", 
                          pair = "s", 
                          side = "S", 
                          time_in_force = "f", 
                          orig_quantity = "q",
                          price = "p",
                          avg_price = "ap",
                          status = "X",
                          last_filled_quantity = "l",
                          cum_filled_quantity = "z")
  output <- dplyr::mutate(output, 
                          date = as.numeric(date)/1000, 
                          date = as.POSIXct(date, origin = "1970-01-01"))
  return(output)
}

# Cleaner for websocket "depth" endpoint
binance_ws_cleaner.depth <- function(data){
  
  if (purrr::is_empty(data)) {
    return(data)
  }

  df_out <- dplyr::bind_rows(data[c("U","u","E","s")])
  df_out <- dplyr::select(df_out, 
                          first_update_id = "U", 
                          last_update_id = "u", 
                          date = "E", 
                          pair = "s")
  # BID data
  if(!purrr::is_empty(data$b)){
    colnames(data$b) <- c("price", "quantity")
    df_bid <- dplyr::as_tibble(data$b)
    df_bid <- dplyr::mutate_all(df_bid, as.numeric)
    df_bid <- dplyr::bind_cols(df_out, df_bid, side = "BID")
  } else {
    df_bid <- dplyr::tibble()
  }
  
  # ASK data
  if(!purrr::is_empty(data$a)){
    colnames(data$a) <- c("price", "quantity")
    df_ask <- dplyr::as_tibble(data$a)
    df_ask <- dplyr::mutate_all(df_ask, as.numeric)
    df_ask <- dplyr::bind_cols(df_out, df_ask, side = "ASK")
  } else {
    df_ask <- dplyr::tibble()
  }
  
  # Output dataset 
  df_out <- dplyr::bind_rows(df_ask, df_bid)
  df_out <- dplyr::mutate(df_out,
                          date = as.POSIXct(date/1000, origin = "1970-01-01"),
                          side = factor(side, levels = c("ASK", "BID"), ordered = FALSE))
  return(df_out)
}

# ------------------------------------------------------ binance_ws_structure ------------------------------------------------------
# Structure different binance websocket responses
binance_ws_structure <- function(data_after, data_before = NULL){
  UseMethod("binance_ws_structure")
}

# Structure kline data to avoid un-closed candles
binance_ws_structure.kline <- function(data_after, data_before = NULL){
  
  if (purrr::is_empty(data_before)) {
    data_after <- binance_ws_cleaner.kline(data_after)
    return(data_after)
  } 
  
  if (purrr::is_empty(data_after)) {
    return(data_before)
  } 
  
  data_after <- binance_ws_cleaner.kline(data_after)
  if (data_before[1,]$is_closed) {
    merge_data <- dplyr::bind_rows(data_after, data_before)
  } else {
    merge_data <- dplyr::bind_rows(data_after, data_before[-1,])
  }
  
  return(merge_data)
}

# Structure order book data updating the levels
binance_ws_structure.depth <- function(data_after, data_before = NULL){

  if (purrr::is_empty(data_before)) {
    data_after <- binance_ws_cleaner.depth(data_after)
    return(data_after)
  } 
  
  if (purrr::is_empty(data_after)) {
    return(data_before)
  } 
  
  data_after <- binance_ws_cleaner.depth(data_after)
  last_update_id <- data_after$last_update_id[1] 

  # merge data before with data after 
  df_out <- dplyr::full_join(
    dplyr::select(data_before, pair, side, price, quantity), 
    dplyr::select(data_after, pair, side, price, quantity),
    by = c("pair", "side", "price"))
  df_out <- dplyr::mutate(df_out, quantity = ifelse(is.na(quantity.y), quantity.x, quantity.y))
  df_out <- dplyr::mutate(df_out, date = data_after$date[1], last_update_id = last_update_id)
  df_out <- dplyr::select(df_out, last_update_id, date, pair, side, price, quantity)
  # keep only levels with a quantity > 0 
  df_out <- dplyr::filter(df_out, quantity > 0)
  
  # Be sure that ASK and BID best prices are similar 
  # can happen that some price levels is not updated when price moves rapidly 
  data_ask <- dplyr::arrange(dplyr::filter(df_out, side == "ASK"), price)
  data_bid <- dplyr::arrange(dplyr::filter(df_out, side == "BID"), dplyr::desc(price))
  # Compute best 10 ask and bid prices
  best_ask <- data_ask$price[1:10]
  best_bid <- data_bid$price[1:10]
  # Check which difference is smaller to establish best ask or best bid
  diff_ask <- sum(diff(best_ask))
  diff_bid <- sum(diff(best_bid))
  # Filter the data 
  if(min(diff_ask, diff_bid) == diff_ask){
    data_bid <- dplyr::filter(data_ask, price <= best_ask[1])
  } else if(min(diff_ask, diff_bid) == diff_bid){
    data_ask <- dplyr::filter(data_ask, price >= best_bid[1])
  } 
  df_out <- dplyr::bind_rows(data_bid, data_ask)
  return(df_out)
}

binance_ws_structure.trade <- function(data_after, data_before = NULL){
  
  if (purrr::is_empty(data_before)) {
    data_after <- binance_ws_cleaner.trade(data_after)
    return(data_after)
  } 
  
  if (purrr::is_empty(data_after)) {
    return(data_before)
  } 
  
  data_after <- binance_ws_cleaner.trade(data_after)
  merge_data <- dplyr::bind_rows(data_after, data_before)
  
  return(merge_data)
}

binance_ws_structure.aggTrade <- function(data_after, data_before = NULL){
  
  if (purrr::is_empty(data_before)) {
    data_after <- binance_ws_cleaner.aggTrade(data_after)
    return(data_after)
  } 
  
  if (purrr::is_empty(data_after)) {
    return(data_before)
  } 
  
  data_after <- binance_ws_cleaner.aggTrade(data_after)
  merge_data <- dplyr::bind_rows(data_after, data_before)
  
  return(merge_data)
}

binance_ws_structure.bookTicker <- function(data_after, data_before = NULL){
  
  if (purrr::is_empty(data_before)) {
    data_after <- binance_ws_cleaner.bookTicker(data_after)
    return(data_after)
  } 
  
  if (purrr::is_empty(data_after)) {
    return(data_before)
  } 
  
  data_after <- binance_ws_cleaner.bookTicker(data_after)
  merge_data <- dplyr::bind_rows(data_after, data_before)
  
  return(merge_data)
}

binance_ws_structure.ticker <- function(data_after, data_before = NULL){
  
  if (purrr::is_empty(data_before)) {
    data_after <- binance_ws_cleaner.ticker(data_after)
    return(data_after)
  } 
  
  if (purrr::is_empty(data_after)) {
    return(data_before)
  } 
  
  data_after <- binance_ws_cleaner.ticker(data_after)
  merge_data <- dplyr::bind_rows(data_after, data_before)
  
  return(merge_data)
}

binance_ws_structure.miniTicker <- function(data_after, data_before = NULL){
  
  if (purrr::is_empty(data_before)) {
    data_after <- binance_ws_cleaner.miniTicker(data_after)
    return(data_after)
  } 
  
  if (purrr::is_empty(data_after)) {
    return(data_before)
  } 
  
  data_after <- binance_ws_cleaner.miniTicker(data_after)
  merge_data <- dplyr::bind_rows(data_after, data_before)
  
  return(merge_data)
}

binance_ws_structure.forceOrder <- function(data_after, data_before = NULL){
  
  if (purrr::is_empty(data_before)) {
    data_after <- binance_ws_cleaner.forceOrder(data_after)
    return(data_after)
  } 
  
  if (purrr::is_empty(data_after)) {
    return(data_before)
  } 
  
  data_after <- binance_ws_cleaner.forceOrder(data_after)
  merge_data <- dplyr::bind_rows(data_after, data_before)
  
  return(merge_data)
}

# ------------------------------------------------------ binance_ws_message ------------------------------------------------------
binance_ws_message <- function(method, params, id){
  par <- paste0('"', params[1], '"')
  if (length(params) > 1) {
    for(i in 2:length(params)){
      par <-  paste0(par, ',"', params[i], '"')
    }
  }
  paste0('{"method":"', method,'","params":[', par, '],"id":', id, '}')
}

# ------------------------------------------------------ binance_ws_subscription ------------------------------------------------------
# Create wss URL for spot websocket (DEPRECATED)
binance_ws_spot_subscription <- function(pair, subscription, interval, update_speed, stream_id = 1, quiet = FALSE){
  
  # <symbol>@aggTrade
  # <symbol>@trade
  # <symbol>@kline_<interval>       <interval> = c("1s","1m","3m","5m","15m","30m","1h","2h","4h","6h","12h","1d","3d","1w","1M")
  # <symbol>@miniTicker
  # <symbol>@ticker
  # <symbol>@ticker_<window_size>   <window_size> = c("1h","4h","1d")
  # <symbol>@bookTicker
  # <symbol>@depth@<update_speed>ms <update_speed> = c("100", "1000")
  # 
  # !miniTicker@arr
  # !ticker@arr
  # !ticker_<window-size>@arr       <window_size> = c("1h","4h","1d")
  
  # Check "pair" argument 
  if (missing(pair) || is.null(pair)) {
    if (!quiet) {
      msg <- paste0('The pair argument is missing with no default.')
      cli::cli_abort(msg)
    }
  } else {
    pair <- tolower(pair)
    # Initialize stream subscription identifier  
    ws_subscription <- paste0(pair)
  }
  
  # Check "subscription" argument 
  if (missing(subscription) || is.null(subscription)) {
    subscription <- "aggTrade"
    if (!quiet) {
      msg <- paste0('The "subscription" argument is missing, default is ', '"', subscription, '"')
      cli::cli_alert_warning(msg)
    }
  } else {
    av_subscription <- c("aggTrade","bookTicker","depth","kline","miniTicker","ticker","trade")
    subscription <- match.arg(subscription, choices = av_subscription)
  }
  
  # Add subscription identifier  
  ws_subscription <- paste0(ws_subscription, "@", subscription)
  
  # Check "interval" argument 
  if (missing(interval) || is.null(interval)) {
    interval <- NA_character_
    if (subscription == "kline") {
      interval <- "1d"
      if (!quiet) {
        msg <- paste0('The `interval` argument is missing, default is ', '"', interval, '"')
        cli::cli_alert_warning(msg)
      }
      # Stream interval identifier  
      ws_subscription <- paste0(ws_subscription, "_", interval)
    } 
  } else {
    if (subscription == "kline") {
      av_interval <- c("1s","1m","3m","5m","15m","30m","1h","2h","4h","6h","12h","1d","3d","1w","1M")
      interval <- match.arg(interval, choices = av_interval)
      # Stream interval identifier  
      ws_subscription <- paste0(ws_subscription, "_", interval)
    } else if (subscription == "ticker") {
      av_interval <- c("1h","4h","1d")
      interval <- match.arg(interval, choices = av_interval)
      # Stream interval identifier
      ws_subscription <- paste0(ws_subscription, "_", interval)
    } else {
      interval <- NA_character_
    }
  }
  
  # Check "update_speed" argument 
  if (missing(update_speed) || is.null(update_speed)) {
    update_speed <- NA_character_
    if (subscription == "depth") {
      update_speed <- "1000"
      if (!quiet) {
        msg <- paste0('The `update_speed` argument is missing, default is ', '"', update_speed, '"')
        cli::cli_alert_warning(msg)
      }
      # Stream update_speed identifier  
      ws_subscription <- paste0(ws_subscription, "@", update_speed, "ms")
    } 
  } else {
    update_speed <- as.character(update_speed)
    if (subscription == "depth") {
      av_update_speed <- c("100","1000")
      update_speed <- match.arg(update_speed, choices = av_update_speed)
      # Stream update_speed identifier  
      ws_subscription <- paste0(ws_subscription, "@", update_speed, "ms")
    } else {
      update_speed <- NA_character_
    }
  }
  
  ws_subscription <- dplyr::tibble(stream_id = stream_id, 
                                   pair = pair, 
                                   stream = ws_subscription, 
                                   subscription = subscription, 
                                   interval = interval, 
                                   update_speed = update_speed,
                                   status = "SUBSCRIBED")
  
  return(ws_subscription)
}

# Create wss URL for spot and api websocket 
binance_ws_subscription <- function(pair, api = "spot", subscription, interval, update_speed, stream_id = 1, quiet = FALSE){
  
  # <symbol>@aggTrade
  # <symbol>@trade
  # <symbol>@kline_<interval>       <interval> = c("1s","1m","3m","5m","15m","30m","1h","2h","4h","6h","12h","1d","3d","1w","1M")
  # <symbol>@miniTicker
  # <symbol>@ticker
  # <symbol>@ticker_<window_size>   <window_size> = c("1h","4h","1d")
  # <symbol>@bookTicker
  # <symbol>@depth@<update_speed>ms <update_speed> = c("100", "1000")
  # 
  # !miniTicker@arr
  # !ticker@arr
  # !ticker_<window-size>@arr       <window_size> = c("1h","4h","1d")
  
  # Check api argument
  api <- match.arg(api, choices = c("spot", "fapi"))
  
  # Check "pair" argument 
  if (missing(pair) || is.null(pair)) {
    if (!quiet) {
      msg <- paste0('The pair argument is missing with no default.')
      cli::cli_abort(msg)
    }
  } else {
    pair <- tolower(pair)
    # Initialize stream subscription identifier  
    ws_subscription <- paste0(pair)
  }
  
  # Check "subscription" argument 
  if (missing(subscription) || is.null(subscription)) {
    subscription <- "aggTrade"
    if (!quiet) {
      msg <- paste0('The "subscription" argument is missing, default is ', '"', subscription, '"')
      cli::cli_alert_warning(msg)
    }
  } else {
    av_subscription <- c("aggTrade","bookTicker","depth","kline","miniTicker","ticker","trade") 
    if(api == "fapi"){
      av_subscription <- c(av_subscription, "forceOrder", "markPrice")  
    } 
    subscription <- match.arg(subscription, choices = av_subscription)
  }
  
  # Add subscription identifier  
  ws_subscription <- paste0(ws_subscription, "@", subscription)
  
  # Check "interval" argument 
  if (missing(interval) || is.null(interval)) {
    interval <- NA_character_
    if (subscription == "kline") {
      interval <- "1d"
      if (!quiet) {
        msg <- paste0('The `interval` argument is missing, default is ', '"', interval, '"')
        cli::cli_alert_warning(msg)
      }
      # Stream interval identifier  
      ws_subscription <- paste0(ws_subscription, "_", interval)
    } 
  } else {
    if (subscription == "kline") {
      av_interval <- c("1s","1m","3m","5m","15m","30m","1h","2h","4h","6h","12h","1d","3d","1w","1M")
      if (api != "spot"){
        av_interval <- av_interval[-1]
      }
      interval <- match.arg(interval, choices = av_interval)
      # Stream interval identifier  
      ws_subscription <- paste0(ws_subscription, "_", interval)
    } else if (subscription == "ticker") {
      av_interval <- c("1h","4h","1d")
      interval <- match.arg(interval, choices = av_interval)
      # Stream interval identifier
      ws_subscription <- paste0(ws_subscription, "_", interval)
    } else {
      interval <- NA_character_
    }
  }
  
  # Check "update_speed" argument 
  if (missing(update_speed) || is.null(update_speed)) {
    update_speed <- NA_character_
    if (subscription %in% "depth") {
      update_speed <- ifelse(api == "spot", "1000", "500")
      if (!quiet) {
        msg <- paste0('The `update_speed` argument is missing, default is ', '"', update_speed, '"')
        cli::cli_alert_warning(msg)
      }
      # Stream update_speed identifier  
      ws_subscription <- paste0(ws_subscription, "@", update_speed, "ms")
    } 
  } else {
    update_speed <- as.character(update_speed)
    if (subscription == "depth") {
      if (api == "spot") {
        av_update_speed <- c("100", "1000")
      } else {
        av_update_speed <- c("100", "250", "500")
      }
      update_speed <- match.arg(update_speed, choices = av_update_speed)
      # Stream update_speed identifier  
      ws_subscription <- paste0(ws_subscription, "@", update_speed, "ms")
    } else {
      update_speed <- NA_character_
    }
  }
  ws_subscription <- dplyr::tibble(stream_id = stream_id, 
                                   pair = pair, 
                                   stream = ws_subscription, 
                                   subscription = subscription, 
                                   interval = interval, 
                                   update_speed = update_speed,
                                   status = "SUBSCRIBED")
  return(ws_subscription)
}

