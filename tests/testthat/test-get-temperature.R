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
  expect_error(get.temperature(25, 100, 0), 'Invalid argument')
  expect_error(get.temperature(25, 100, 101), 'Invalid argument')
})


test_that("get.temperature validates method correctly", {
  expect_error(get.temperature(25, 100, 1, 'invalid.method'), 'Invalid argument')
})
