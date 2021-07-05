library(EventsBufferFutures)

test_that("buffering, function call and value works", {
  ebf <- eventsBufferFutures( accumulatorSize=10, mean, NULL )
  for ( i in seq(1,100) ) {ebf$add( i )}
  expect_equal( ebf$getValue(), 5.5 )
})

test_that("reading through buffered results works",{
  ebf <- eventsBufferFutures( accumulatorSize=10, mean, NULL )
  for ( i in seq(1,100) ) {ebf$add( i )}
  for ( i in seq(1,9) ) {ebf$getValue()}
  expect_equal( ebf$getValue(), 95.5 )
})

test_that("composite functions work", {
  sumOfSquares <- function(x) {sum( x*x )}
  ebf <- eventsBufferFutures( accumulatorSize=10, sumOfSquares, NULL )
  for ( i in seq(1,100) ) {ebf$add( i )}
  expect_equal( ebf$getValue(), 385 )
})

test_that("flush works",{
  ebf <- eventsBufferFutures( accumulatorSize=10, mean, NULL )
  for ( i in seq(1,103) ) {ebf$add( i )}
  ebf$flush()
  for ( i in seq(1,10) ) {ebf$getValue()}
  expect_equal( ebf$getValue(), 102 )
})

test_that("super passing raw data to sub works",{
  # This tortured example also tests whether one EBF can pass unaltered arguments
  # to subsequent EBFs when the "function" of the "super" EBF is NULL.
  ebf_sub <- eventsBufferFutures( accumulatorSize=10, mean, NULL )
  ebf_super <- eventsBufferFutures( accumulatorSize=5, NULL, ebf_sub )
  for ( i in seq(1,18) ) {ebf_super$add(i)}
  expect_equal( ebf_sub$getValue(), 5.5 )
})

test_that("sub flush works",{
  ebf_sub <- eventsBufferFutures( accumulatorSize=10, mean, NULL )
  ebf_super <- eventsBufferFutures( accumulatorSize=5, NULL, ebf_sub )
  for ( i in seq(1,18) ) {ebf_super$add(i)}
  ebf_sub$getValue()
  ebf_sub$flush()
  expect_equal( ebf_sub$getValue(), 13 )
})

test_that("super/sub flush interaction works",{
  ebf_sub <- eventsBufferFutures( accumulatorSize=10, mean, NULL )
  ebf_super <- eventsBufferFutures( accumulatorSize=5, NULL, ebf_sub )
  for ( i in seq(1,18) ) {ebf_super$add(i)}
  ebf_sub$getValue()
  ebf_sub$flush()
  ebf_sub$getValue()
  ebf_super$flush()
  ebf_sub$flush()
  expect_equal( ebf_sub$getValue(), 17 )
})



