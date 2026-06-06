test_that("input validation fails before network calls", {
  expect_error(binance_avg_price(), "pair")
  expect_error(binance_new_order(pair = "BTCUSDT", side = "BUY"), "quantity")
  expect_error(binance_get_order(pair = "BTCUSDT"), "order_id")
  expect_error(binance_cancel_order(pair = "BTCUSDT"), "order_id")
})
