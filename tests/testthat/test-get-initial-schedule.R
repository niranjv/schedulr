context("Test get.initial.schedule()")


test_that("get.initial.schedule validates cluster.size correctly", {

  expect_error(get.initial.schedule(), 'Missing required argument')
  expect_error(get.initial.schedule(c()), 'Invalid argument length')
  expect_error(get.initial.schedule('a'), 'Non-integer argument')
  expect_error(get.initial.schedule(3.14), 'Non-integer argument')
  expect_error(get.initial.schedule(-1), 'Invalid argument')
  expect_error(get.initial.schedule(1:2), 'Invalid argument length')
  expect_error(get.initial.schedule(0), 'Invalid argument')
})


test_that("get.initial.schedule validates tasks correctly", {
  expect_error(get.initial.schedule(1), 'Missing required argument')
  expect_error(get.initial.schedule(1, c()), 'Invalid argument length')
  expect_error(get.initial.schedule(1, 'a'), 'Non-numeric argument')
  expect_error(get.initial.schedule(1, -1.2), 'Invalid argument')
  expect_error(get.initial.schedule(1, 0), 'Invalid argument')
})


test_that("get.initial.schedule validates method correctly", {
  expect_error(get.initial.schedule(1, c(10), method='invalid.method'), 'Invalid argument')
})



test_that("get.initial.schedule validates runtimes.summary correctly", {

  expect_error(get.initial.schedule(1, c(10), method='leptf'), 'Missing required argument')
  expect_error(get.initial.schedule(1, c(10), c(), method='leptf'), 'Invalid argument type')
  expect_error(get.initial.schedule(1, c(10), '', method='leptf'), 'Invalid argument type')
  expect_error(get.initial.schedule(1, c(10), 1, method='leptf'), 'Invalid argument type')
  expect_error(get.initial.schedule(1, c(10), 1:2, method='leptf'), 'Invalid argument type')
  expect_error(get.initial.schedule(1, c(10), c('a', 'b'), method='leptf'), 'Invalid argument type')
  expect_error(get.initial.schedule(1, c(10), list(1, 1), method='leptf'), 'Invalid argument type')
  expect_error(get.initial.schedule(1, c(10), data.frame(1, 1), method='leptf'), 'Invalid argument type')

  rs <- matrix(nrow=0, ncol=0)
  expect_error(get.initial.schedule(1, c(10), rs, method='leptf'),  'Invalid argument dimensions')

  rs <- matrix(nrow=0, ncol=1)
  expect_error(get.initial.schedule(1, c(10), rs, method='leptf'), 'Invalid argument dimensions')

  rs <- matrix(nrow=1, ncol=0)
  expect_error(get.initial.schedule(1, c(10), rs, method='leptf'), 'Invalid argument dimensions')

  rs <- matrix(nrow=1, ncol=1)
  rs[1, 1] <- 1
  expect_error(get.initial.schedule(1, c(10), rs, method='leptf'), 'Invalid argument dimensions')

  rs <- matrix(nrow=1, ncol=2)
  rs[1, 1] <- 1
  rs[1, 2] <- 1
  expect_error(get.initial.schedule(1, c(10), rs, method='leptf'), 'Invalid argument dimensions')

  rs <- matrix(nrow=2, ncol=1)
  rs[1, 1] <- 1
  rs[2, 1] <- 1
  expect_error(get.initial.schedule(1, c(10), rs, method='leptf'), 'Invalid argument dimensions')

  rs <- matrix(nrow=2, ncol=2)
  rs[1, 1] <- 1; rs[1, 2] <- 1
  rs[2, 1] <- 1; rs[2, 2] <- 1
  expect_error(get.initial.schedule(1, c(10), rs, method='leptf'), 'Invalid argument dimensions')

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- -1
  rs[1, 2] <- 1
  rs[1, 3] <- 1
  expect_error(get.initial.schedule(1, c(10), rs, method='leptf'), 'Invalid argument')

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- 1
  rs[1, 2] <- -1
  rs[1, 3] <- 1
  expect_error(get.initial.schedule(1, c(10), rs, method='leptf'), 'Invalid argument')

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- 1
  rs[1, 2] <- 1
  rs[1, 3] <- -1
  expect_error(get.initial.schedule(1, c(10), rs, method='leptf'), 'Invalid argument')

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- 'a'
  rs[1, 2] <- 1
  rs[1, 3] <- 1
  expect_error(get.initial.schedule(1, c(10), rs, method='leptf'), 'Invalid argument')

})


test_that("get.initial.schedule returns the expected value", {

  rs <- matrix(nrow=3, ncol=3)
  rs[1, 1] <- 10; rs[1, 2] <- 23.5; rs[1, 3] <- 3.5
  rs[2, 1] <- 20; rs[2, 2] <- 33.5; rs[2, 3] <- 4.5
  rs[3, 1] <- 30; rs[3, 2] <- 43.5; rs[3, 3] <- 5.5

  schedule <- get.initial.schedule(1, c(10))
  expect_is(schedule, 'list')
  expect_equal(length(schedule), 1)
  expect_equal(length(unlist(schedule)), 1)
  expect_equal(unlist(schedule), 10)
  expect_equal(schedule[[1]], 10)

  schedule <- get.initial.schedule(1, c(10), rs, method='leptf')
  expect_is(schedule, 'list')
  expect_equal(length(schedule), 1)
  expect_equal(length(unlist(schedule)), 1)
  expect_equal(unlist(schedule), c(10))
  expect_equal(schedule[[1]], 10)

  schedule <- get.initial.schedule(1, c(10, 20), rs, method='leptf')
  expect_is(schedule, 'list')
  expect_equal(length(schedule), 1)
  expect_equal(length(unlist(schedule)), 2)
  expect_equal(sort(unlist(schedule)), c(10, 20))
  expect_equal(schedule[[1]], c(20, 10))

  schedule <- get.initial.schedule(2, c(10), rs, method='leptf')
  expect_is(schedule, 'list')
  expect_equal(length(schedule), 2)
  expect_equal(length(unlist(schedule)), 1)
  expect_equal(unlist(schedule), c(10))
  expect_true((schedule[[1]]==10 && is.null(schedule[[2]])) || (is.null(schedule[[1]]) && schedule[[2]]==10))

  schedule <- get.initial.schedule(2, c(10, 20), rs, method='leptf')
  expect_is(schedule, 'list')
  expect_equal(length(schedule), 2)
  expect_equal(length(unlist(schedule)), 2)
  expect_equal(sort(unlist(schedule)), c(10, 20))
  expect_true( (schedule[[1]] == 10 && schedule[[2]] == 20) || (schedule[[1]] == 20 && schedule[[2]] == 10) )

  schedule <- get.initial.schedule(2, c(10, 20, 30), rs, method='leptf')
  expect_is(schedule, 'list')
  expect_equal(length(schedule), 2)
  expect_equal(length(unlist(schedule)), 3)
  expect_equal(sort(unlist(schedule)), c(10, 20, 30))
  expect_true( (sort(schedule[[1]]) == c(10, 20) && schedule[[2]] == 30) || (schedule[[1]] == 30 && sort(schedule[[2]]) == c(10, 20)) )

})
