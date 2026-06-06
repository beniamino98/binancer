#' R6 Class for Binance Web Socket
#' 
#' @description 
#' Binance web socket description
#' 
#' @details 
#' Creates a WebSocket client for Binance stream subscriptions.
#'
#' @return An R6 generator for `binanceWebSocket` objects.
#'
#' @examplesIf interactive()
#' ws <- binanceWebSocket$new(
#'   pair = "BTCUSDT",
#'   api = "spot",
#'   subscription = "kline",
#'   interval = "1m"
#' )
#' ws$connect()
#' ws$close()
#' 
#' @export
binanceWebSocket <- R6::R6Class("binanceWebSocket", 
                                public = list(
                                  #' @description
                                  #' Initialize a `binanceWebSocket` object.
                                  #' @param pair Character. Trading pair, e.g. `"BTCUSDT"`.
                                  #' @param api Character. Reference API. Available options are `"spot"` and `"fapi"`.
                                  #' @param subscription type of subscription, can be `aggTrade`,`bookTicker`,`depth`,`kline`,`miniTicker`,`ticker`,`trade`.
                                  #' @param interval time interval, can be `1s`,`1m`,`3m`,`5m`,`15m`,`30m`,`1h`,`2h`,`4h`,`6h`,`12h`,`1d`,`3d`,`1w`,`1M`.
                                  #' @param update_speed update speed in millisecond, for `spot` api `1000ms`. For others apis `500ms`.
                                  #' @return A new `binanceWebSocket` object.
                                  initialize = function(pair = "BTCUSDT", api = "spot", subscription, interval, update_speed){
                                    # Create subscription info 
                                    stream_info <- binance_ws_subscription(
                                      pair = pair, subscription = subscription, api = api,
                                      interval = interval, update_speed = update_speed, 
                                      stream_id = private$id, quiet = private$quiet)
                                    # Initialize api base url 
                                    private$api <- api
                                    if (api == "spot") {
                                      private$base_url <- "wss://stream.binance.com:9443/stream?streams="
                                    } else {
                                      private$base_url <- "wss://fstream.binance.com/stream?streams="
                                    }
                                    # Extract subscription name 
                                    stream_name <- stream_info$stream
                                    # Create websocket url
                                    websocket_url <- paste0(private$base_url, stream_name)
                                    # Initialize a websocket 
                                    ws <- websocket::WebSocket$new(websocket_url, autoConnect = FALSE)
                                    # Initialize a list to save the stream information
                                    private$..info <- list()
                                    # Initialize a list for each stream 
                                    private$..stream[[stream_name]] <- list()
                                    # List for structured data 
                                    private$..stream[[stream_name]]$data <- list()
                                    # List for stream data 
                                    private$..stream[[stream_name]]$stream <- list()
                                    private$..info[[stream_name]] <- stream_info
                                    
                                    # onOpen: when the connection is opened 
                                    ws$onOpen(function(event) {
                                      if (!private$quiet) {
                                        msg <- paste0("Websocket connected!")
                                        cli::cli_alert_success(msg)
                                      }
                                    })
                                    # onMessages: when a message is received  
                                    ws$onMessage(function(event) {
                                      # New message 
                                      if (is.list(event$data)) {
                                        data_after <- jsonlite::fromJSON(as.list(event$data))
                                      } else {
                                        data_after <- jsonlite::fromJSON(event$data)
                                      }
                                      # Extract stream name 
                                      stream_name <- data_after$stream
                                      # Extract info with subscription name
                                      stream_info <- self$info[[stream_name]]
                                      # Extract new data 
                                      data_after <- data_after$data 
                                      n <- length(private$..stream[[stream_name]]$stream)
                                      private$..stream[[stream_name]]$stream[[n + 1]] <- data_after
                                      # Snapshot of stream data 
                                      stream_data <- private$..stream[[stream_name]]
                                      
                                      # Manteinment of an order book  
                                      if (stream_info$subscription == "depth" & !purrr::is_empty(stream_data$data)) {
                                        # Extract last_update_id
                                        last_update_id <- stream_data$data$last_update_id[1]
                                        # Index of events received after last_update_id
                                        idx_stream_first_update <- purrr::map_dbl(stream_data$stream, ~.x$u[1])
                                        idx_last_update <- idx_stream_first_update >= last_update_id + 1 
                                        # Remove events before last_update_id 
                                        stream_data$stream <- stream_data$stream[idx_last_update]
                                        # Process in order all the events after last_update_id
                                        if (!purrr::is_empty(stream_data$stream)) {
                                          for(i in 1:length(stream_data$stream)){
                                            data_before <- stream_data$data
                                            data_after <- stream_data$stream[[i]]
                                            stream_data$data <- binance_ws_structure.depth(data_after = data_after, data_before = data_before)
                                            last_update_id <- stream_data$data$last_update_id[1]
                                          }
                                        }
                                        # Index of events received after last_update_id
                                        idx_stream_last_update <- purrr::map_dbl(stream_data$stream, ~.x$u[1])
                                        idx_last_update <- idx_stream_last_update >= last_update_id + 1
                                        # Remove events before last_update_id 
                                        stream_data$stream <- stream_data$stream[idx_last_update]
                                        private$..stream[[stream_name]] <- stream_data
                                        return(invisible(NULL))
                                      }
                                      
                                      if (stream_info$subscription != "depth") {
                                        # Process in order all the events after last_update_id
                                        if (!purrr::is_empty(stream_data$stream)) {
                                          for(i in 1:length(stream_data$stream)){
                                            data_before <- stream_data$data
                                            attr(data_before, "class") <- c(stream_info$subscription, class(data_before))
                                            data_after <- stream_data$stream[[i]]
                                            attr(data_after, "class") <- c(stream_info$subscription, class(data_after))
                                            stream_data$data <- binance_ws_structure(data_after = data_after, data_before = data_before)
                                          }
                                          stream_data$stream <- list()
                                          private$..stream[[stream_name]] <- stream_data
                                        }
                                      }
                                    })
                                    # onClose: when the connection is closed 
                                    ws$onClose(function(event) {
                                      if (!private$quiet) {
                                        msg <- paste0("All streams closed!")
                                        cli::cli_alert_success(msg)
                                      }
                                      
                                    })
                                    # onError: when an error is received  
                                    ws$onError(function(event) {
                                      if (!private$quiet) {
                                        msg <- paste0("Error: ", event$code)
                                        cli::cli_alert_danger(msg)
                                      }
                                    })
                                    private$ws <- ws
                                  },
                                  #' @description
                                  #' Add a snapshot of the order book. 
                                  #' @param pair Character. Trading pair, e.g. `"BTCUSDT"`.
                                  #' @param subscription type of subscription, `depth`.
                                  #' @param update_speed update speed in millisecond, for `spot` api `1000ms`. For others apis `500ms`.
                                  add_depth_snapshot = function(pair, subscription, update_speed){
                                    # Create new stream info 
                                    stream_info <- binance_ws_subscription(
                                      pair = pair, subscription = subscription, api = private$api,
                                      update_speed = update_speed, stream_id = 1, quiet = private$quiet)
                                    # Extract stream name  
                                    stream_name <- stream_info$stream
                                    # Extract saved info
                                    self_stream_info <- self$info[[stream_name]]
                                    # Check if the stream is already in saved info  
                                    if (!purrr::is_empty(self_stream_info)) {
                                      # Retrieve a snapshot of the order_book
                                      private$..stream[[stream_name]]$data <- binance_depth(pair, api = private$api)
                                    } else {
                                      if (!private$quiet) {
                                        msg <- paste0("Stream ", stream_name, " do not exists!")
                                        cli::cli_alert_danger(msg)
                                      }
                                    }
                                  },
                                  #' @description
                                  #' Add trades 
                                  #' @param pair Character. Trading pair, e.g. `"BTCUSDT"`.
                                  #' @param subscription type of subscription, `aggTrades` or `trades`.
                                  #' @param from Character or \code{\link[=POSIXt-class]{POSIXt}} object. Start time for historical data. 
                                  #' If it is `missing`, the default, will be used as start date `Sys.time()-lubridate::days(1)`.
                                  #' @param to Character or \code{\link[=POSIXt-class]{POSIXt}} object. End time for historical data.
                                  #' If it is `missing`, the default, will be used as end date \code{\link[=Sys.time]{Sys.time()}}.
                                  add_trades_snapshot = function(pair, subscription, from, to){
                                    # Create new stream info 
                                    stream_info <- binance_ws_subscription(api = private$api,
                                      pair = pair, subscription = subscription, stream_id = 1, quiet = private$quiet)
                                    # Extract stream name  
                                    stream_name <- stream_info$stream
                                    # Extract saved info
                                    self_stream_info <- self$info[[stream_name]]
                                    # Check if the stream is already in saved info  
                                    if (!purrr::is_empty(self_stream_info)) {
                                      new_trades <- binance_trades(pair, api = private$api, from, to, quiet = private$quiet)
                                      old_trades <- private$..stream[[stream_name]]$data
                                      all_trades <- dplyr::bind_rows(new_trades, old_trades)
                                      all_trades <- all_trades[!duplicated(all_trades$agg_id),]
                                      all_trades <- all_trades[order(all_trades$agg_id, decreasing = TRUE),]
                                      # Retrieve a snapshot of the order_book
                                      private$..stream[[stream_name]]$data <- all_trades
                                    } else {
                                      if (!private$quiet) {
                                        msg <- paste0("Stream ", stream_name, " do not exists!")
                                        cli::cli_alert_danger(msg)
                                      }
                                    }
                                  },
                                  #' @description
                                  #' Add a snapshot of the klines.
                                  #' @param pair Character. Trading pair, e.g. `"BTCUSDT"`.
                                  #' @param subscription type of subscription, `klines`.
                                  #' @param interval time interval, can be `1s`,`1m`,`3m`,`5m`,`15m`,`30m`,`1h`,`2h`,`4h`,`6h`,`12h`,`1d`,`3d`,`1w`,`1M`.
                                  #' @param from Character or \code{\link[=POSIXt-class]{POSIXt}} object. Start time for historical data. 
                                  #' If it is `missing`, the default, will be used as start date `Sys.time()-lubridate::days(1)`.
                                  #' @param to Character or \code{\link[=POSIXt-class]{POSIXt}} object. End time for historical data.
                                  #' If it is `missing`, the default, will be used as end date \code{\link[=Sys.time]{Sys.time()}}.
                                  add_klines_snapshot = function(pair, subscription, interval, from, to){
                                    # Create new stream info 
                                    stream_info <- binance_ws_subscription(
                                      pair = pair, subscription = subscription, api = private$api,
                                      interval = interval, stream_id = 1, quiet = private$quiet)
                                    # Extract stream name  
                                    stream_name <- stream_info$stream
                                    # Extract saved info
                                    self_stream_info <- self$info[[stream_name]]
                                    # Check if the stream is already in saved info  
                                    if (!purrr::is_empty(self_stream_info)) {
                                      new_klines <- binance_klines(pair, api = private$api, interval, from, to, quiet = private$quiet)
                                      old_klines <- private$..stream[[stream_name]]$data
                                      new_klines$is_closed <- TRUE
                                      all_klines <- dplyr::bind_rows(old_klines, new_klines)
                                      all_klines <- all_klines[!duplicated(all_klines$date),]
                                      all_klines <- all_klines[order(all_klines$date, decreasing = TRUE),]
                                      private$..stream[[stream_name]]$data <- all_klines
                                    } else {
                                      if (!private$quiet) {
                                        msg <- paste0("Stream ", stream_name, " do not exists!")
                                        cli::cli_alert_danger(msg)
                                      }
                                    }
                                  },
                                  #' @description
                                  #' Subscribe to a stream.
                                  #' @param pair Character. Trading pair, e.g. `"BTCUSDT"`.
                                  #' @param subscription type of subscription, can be `aggTrade`,`bookTicker`,`depth`,`kline`,`miniTicker`,`ticker`,`trade`.
                                  #' @param interval time interval, can be `1s`,`1m`,`3m`,`5m`,`15m`,`30m`,`1h`,`2h`,`4h`,`6h`,`12h`,`1d`,`3d`,`1w`,`1M`.
                                  #' @param update_speed update speed in millisecond, for `spot` api `1000ms`. For others apis `500ms`.
                                  subscribe = function(pair, subscription, interval, update_speed){
                                    stream_info <- binance_ws_subscription(
                                      pair = pair, subscription = subscription, interval = interval, api = private$api,
                                      update_speed = update_speed, stream_id = 1, quiet = private$quiet)
                                    stream_name <- stream_info$stream
                                    # Extract saved info
                                    self_stream_info <- self$info[[stream_name]]
                                    # Check if the stream is already in saved info  
                                    if (!purrr::is_empty(self_stream_info)) {
                                      # Check if status is SUBSCRIBED 
                                      if (self_stream_info$status == "SUBSCRIBED") {
                                        if (!private$quiet) {
                                          msg <- paste0("Stream ", stream_name, "already opened!")
                                          cli::cli_alert_warning(msg)
                                        }
                                        return(invisible(NULL))
                                      } else {
                                        # Initialize the order book  
                                        if (self_stream_info$subscription == "depth") {
                                          private$..stream[[stream_name]]$data <- list()
                                        }
                                        # Change status to "SUBSCRIBED"
                                        private$..info[[stream_name]] <- dplyr::mutate(self_stream_info, 
                                                                                       status = ifelse(stream == stream_name, "SUBSCRIBED", status))
                                      }
                                    } else {
                                      # Initialize an empty list for new stream data 
                                      private$..stream[[stream_name]] <- list()
                                      private$..stream[[stream_name]]$data <- list()
                                      private$..stream[[stream_name]]$stream <- list()
                                      # Update id 
                                      private$id <- private$id + 1
                                      stream_info$stream_id <- private$id
                                      # Add new info 
                                      private$..info[[stream_name]] <- stream_info
                                      # Create self_stream_info 
                                      self_stream_info <- self$info[[stream_name]]
                                    }
                                    
                                    # Create and send subscription message 
                                    msg <- binance_ws_message(
                                      method = "SUBSCRIBE", params = self_stream_info$stream, id = self_stream_info$stream_id)
                                    private$ws$send(msg)
                                    
                                    if (!private$quiet) {
                                      msg <- paste0("Stream ", stream_name, " opened!")
                                      cli::cli_alert_success(msg)
                                    }
                                  },
                                  #' @description
                                  #' Unsubscribe from a stream.
                                  #' @param pair Character. Trading pair, e.g. `"BTCUSDT"`.
                                  #' @param subscription type of subscription, can be `aggTrade`,`bookTicker`,`depth`,`kline`,`miniTicker`,`ticker`,`trade`.
                                  #' @param interval time interval, can be `1s`,`1m`,`3m`,`5m`,`15m`,`30m`,`1h`,`2h`,`4h`,`6h`,`12h`,`1d`,`3d`,`1w`,`1M`.
                                  #' @param update_speed update speed in millisecond, for `spot` api `1000ms`. For others apis `500ms`.
                                  unsubscribe = function(pair, subscription, interval, update_speed){
                                    # Create new stream info 
                                    stream_info <- binance_ws_subscription(
                                      pair = pair, subscription = subscription, interval = interval, api = private$api,
                                      update_speed = update_speed, stream_id = 1, quiet = private$quiet)
                                    stream_name <- stream_info$stream
                                    # Extract saved info
                                    self_stream_info <- self$info[[stream_name]] 
                                    # Check if the stream is already in saved info  
                                    if (!purrr::is_empty(self_stream_info)) {
                                      # Check if status is UNSUBSCRIBED 
                                      if (self_stream_info$status == "UNSUBSCRIBED") {
                                        if (!private$quiet) {
                                          msg <- paste0("Stream ", self_stream_info$stream, " already closed!")
                                          cli::cli_alert_warning(msg)
                                          return(invisible(NULL))
                                        }
                                      }
                                      # Create and send unsubscription message 
                                      msg <- binance_ws_message(method = "UNSUBSCRIBE", 
                                                                params = self_stream_info$stream, 
                                                                id = self_stream_info$stream_id)
                                      private$ws$send(msg)
                                      # Change status to "UNSUBSCRIBED"
                                      private$..info[[stream_name]]$status <- "UNSUBSCRIBED"
                                      
                                      if (!private$quiet) {
                                        msg <- paste0("Stream ", self_stream_info$stream, " closed!")
                                        cli::cli_alert_success(msg)
                                      }
                                    } else {
                                      if (!private$quiet) {
                                        msg <- paste0("Stream ", stream_name, " do not exists! Call subscribe() method!")
                                        cli::cli_alert_danger(msg)
                                      }
                                    }
                                  },
                                  #' @description
                                  #' Open the connection.
                                  connect = function(){
                                    private$ws$connect()
                                  },
                                  #' @description
                                  #' Close all the connections.
                                  close = function(){
                                    private$ws$close()
                                  },
                                  #' @description
                                  #' Get a stream.
                                  #' @param pair pair
                                  #' @param subscription type of subscription, can be `aggTrade`,`bookTicker`,`depth`,`kline`,`miniTicker`,`ticker`,`trade`.
                                  #' @param interval time interval, can be `1s`,`1m`,`3m`,`5m`,`15m`,`30m`,`1h`,`2h`,`4h`,`6h`,`12h`,`1d`,`3d`,`1w`,`1M`.
                                  #' @param update_speed update speed in millisecond, for `spot` api `1000ms`. For others apis `500ms`.
                                  #' @param id Numeric stream id. If supplied, `pair`, `subscription`, `interval`, and `update_speed` are ignored.
                                  get_stream = function(pair, subscription, interval, update_speed, id){
                                    if (missing(id) || is.null(id)){
                                      stream_info <- binance_ws_subscription(
                                        pair = pair, subscription = subscription, interval = interval, api = private$api,
                                        update_speed = update_speed, stream_id = 1, quiet = private$quiet)
                                      stream_name <- stream_info$stream
                                      # Extract saved info
                                      self_stream_info <- self$info[[stream_name]]
                                    } else {
                                      id <- as.numeric(id)
                                      self_stream_info <- NULL
                                      stream_name <- ""
                                      if (id <= length(self$info)) {
                                        self_stream_info <- dplyr::filter(dplyr::bind_rows(self$info), stream_id == id)
                                        stream_name <- self_stream_info$stream
                                      }
                                    }
                                    # Check if the stream is already in saved info  
                                    if (!purrr::is_empty(self_stream_info)) {
                                      return(private$..stream[[stream_name]]$data)
                                    } else {
                                      if (!private$quiet) {
                                        msg <- paste0("Stream ", stream_name, " do not exists! Call subscribe() method!")
                                        cli::cli_alert_danger(msg)
                                      }
                                    }
                                  },
                                  #' @description
                                  #' Get the info of all the streams opened.
                                  #' @param pair pair
                                  #' @param subscription type of subscription, can be `aggTrade`,`bookTicker`,`depth`,`kline`,`miniTicker`,`ticker`,`trade`.
                                  #' @param interval time interval, can be `1s`,`1m`,`3m`,`5m`,`15m`,`30m`,`1h`,`2h`,`4h`,`6h`,`12h`,`1d`,`3d`,`1w`,`1M`.
                                  #' @param update_speed update speed in millisecond, for `spot` api `1000ms`. For others apis `500ms`.
                                  #' @param id Numeric stream id. If supplied, `pair`, `subscription`, `interval`, and `update_speed` are ignored.
                                  get_info = function(pair, subscription, interval, update_speed, id){
                                    if (missing(id) || is.null(id)){
                                      stream_info <- binance_ws_subscription(
                                        pair = pair, subscription = subscription, interval = interval, api = private$api,
                                        update_speed = update_speed, stream_id = 1, quiet = private$quiet)
                                      stream_name <- stream_info$stream
                                      # Extract saved info
                                      self_stream_info <- self$info[[stream_name]]
                                    } else {
                                      self_stream_info <- self$info[[as.integer(id)]]
                                      stream_name <- stream_info$stream
                                    }
                                    # Extract saved info
                                    self_stream_info <- self$info[[stream_name]]
                                    # Check if the stream is already in saved info  
                                    if (!purrr::is_empty(self_stream_info)) {
                                      return(private$..info[[stream_name]])
                                    } else {
                                      if (!private$quiet) {
                                        msg <- paste0("Stream ", stream_name, " do not exists! Call subscribe() method!")
                                        cli::cli_alert_danger(msg)
                                      }
                                    }
                                  }
                                ),
                                private = list(
                                  base_url = NULL,
                                  api = NULL,
                                  id = 1,
                                  quiet = FALSE,
                                  ws = NULL,
                                  ..stream = list(),
                                  ..info = NULL
                                ),
                                active = list(
                                  #' @field stream
                                  #' Get all streams 
                                  stream = function(){
                                    private$..stream
                                  },
                                  #' @field info
                                  #' Get all streams info 
                                  info = function(){
                                    private$..info
                                  }
                                )
)
