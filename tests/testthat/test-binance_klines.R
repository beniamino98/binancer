test_that("OrderBook aggregates local depth data", {
  depth_data <- data.frame(
    price = c(100, 99, 101, 102),
    quantity = c(1.5, 2, 1, 0.5),
    side = c("BID", "BID", "ASK", "ASK")
  )

  book <- OrderBook(
    data = depth_data,
    min_price = 99,
    max_price = 103,
    levels = 5
  )

  expect_s3_class(book, "tbl_df")
  expect_equal(nrow(book), 5)
  expect_equal(sum(book$bid), 3.5)
  expect_equal(sum(book$ask), 1.5)
})

test_that("OrderBook validates local inputs", {
  expect_error(OrderBook(data.frame(price = 1)), "quantity")
  expect_error(OrderBook(levels = 1), "levels")
})
