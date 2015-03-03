context("Test get.temperature()")


test_that("get.temperature validates max.temp correctly", {

  expect_error(get.temperature(), 'Missing required argument')
  expect_error(get.temperature(c()), 'Invalid argument length')
  expect_error(get.temperature(1:2), 'Invalid argument length')
  expect_error(get.temperature(0), 'Invalid argument')

})


test_that("get.temperature validates max.iter correctly", {

  expect_error(get.temperature(25, ), 'Missing required argument')
  expect_error(get.temperature(25, c()), 'Invalid argument length')
  expect_error(get.temperature(25, 'a'), 'Non-integer argument')
  expect_error(get.temperature(25, '3.14'), 'Non-integer argument')
  expect_error(get.temperature(25, 1:2), 'Invalid argument length')
  expect_error(get.temperature(25, 0), 'Invalid argument')

})


test_that("get.temperature validates cur.iter correctly", {

  expect_error(get.temperature(25, 100, ), 'Missing required argument')
  expect_error(get.temperature(25, 100, c()), 'Invalid argument length')
  expect_error(get.temperature(25, 100, 'a'), 'Non-integer argument')
  expect_error(get.temperature(25, 100, '3.14'), 'Non-integer argument')
  expect_error(get.temperature(25, 100, 1:2), 'Invalid argument length')
  expect_error(get.temperature(25, 100, 101), 'Invalid argument')

})


test_that("get.temperature validates method correctly", {

  expect_error(get.temperature(25, 100, 1, 'invalid.method'), 'Invalid argument')

})


test_that("get.temperature returns the expected value", {

  expect_equal(get.temperature(25, 100, 0), 25)
  expect_equal(get.temperature(25, 100, 1), 24.75)
  expect_equal(get.temperature(25, 200, 1), 24.875)
  expect_equal(get.temperature(25, 50, 1), 24.5)
  expect_equal(get.temperature(10, 100, 1), 9.9)
  expect_equal(get.temperature(10, 50, 1), 9.8)
  expect_equal(get.temperature(25, 100, 15), 21.25)

})
