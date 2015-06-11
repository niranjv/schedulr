context("Test get.score()")


test_that("get.score validates schedule correctly", {

  expect_error(get.score(), 'Missing required argument')
  expect_error(get.score(''), 'Invalid argument type')
  expect_error(get.score(1), 'Invalid argument type')
  expect_error(get.score('a'), 'Invalid argument type')
  expect_error(get.score(c(1,2)), 'Invalid argument type')
  expect_error(get.score(list()), 'Invalid argument length')
  expect_error(get.score(list('')), 'Non-numeric argument')
  expect_error(get.score(list(c())), 'Non-numeric argument')
  expect_error(get.score(list(0)), 'Invalid argument')
  expect_error(get.score(list(-1)), 'Invalid argument')

})


test_that("get.score validates runtimes correctly", {

  schedule <- get.initial.schedule(1, c(10))

  expect_error(get.score(schedule, ), 'Missing required argument')
  expect_error(get.score(schedule, c()), 'Invalid argument type')
  expect_error(get.score(schedule, ''), 'Invalid argument type')
  expect_error(get.score(schedule, 1), 'Invalid argument type')
  expect_error(get.score(schedule, 1:2), 'Invalid argument type')
  expect_error(get.score(schedule, c('a', 'b')), 'Invalid argument type')
  expect_error(get.score(schedule, list(1, 1)), 'Invalid argument type')
  expect_error(get.score(schedule, data.frame(1, 1)), 'Invalid argument type')

  r <- matrix(nrow=0, ncol=0)
  expect_error(get.score(schedule, r), 'Invalid argument dimensions')

  r <- matrix(nrow=0, ncol=1)
  expect_error(get.score(schedule, r), 'Invalid argument dimensions')

  r <- matrix(nrow=1, ncol=0)
  expect_error(get.score(schedule, r), 'Invalid argument dimensions')

  r <- matrix(nrow=1, ncol=1)
  r[1, 1] <- 1
  expect_error(get.score(schedule, r), 'Invalid argument dimensions')

  r <- matrix(nrow=1, ncol=3)
  r[1, 1] <- 1
  r[1, 2] <- 1
  r[1, 3] <- 1
  expect_error(get.score(schedule, r), 'Invalid argument dimensions')

  r <- matrix(nrow=2, ncol=1)
  r[1, 1] <- 1
  r[2, 1] <- 1
  expect_error(get.score(schedule, r), 'Invalid argument dimensions')

  r <- matrix(nrow=2, ncol=3)
  r[1, 1] <- 1; r[1, 2] <- 1; r[1, 3] <- 1
  r[2, 1] <- 1; r[2, 2] <- 1; r[2, 3] <- 1
  expect_error(get.score(schedule, r), 'Invalid argument dimensions')

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- -1
  r[1, 2] <- 1
  expect_error(get.score(schedule, r), 'Invalid argument')

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 1
  r[1, 2] <- -1
  expect_error(get.score(schedule, r), 'Invalid argument')

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 'a'
  r[1, 2] <- 1
  expect_error(get.score(schedule, r), 'Invalid argument')

})


test_that("get.score validates runtimes.summary correctly", {

  schedule <- get.initial.schedule(1, c(10))

  r <- matrix(nrow=2, ncol=2)
  r[1, 1] <- 1; r[1, 2] <- 1
  r[2, 1] <- 1; r[2, 2] <- 1

  expect_error(get.score(schedule, r, ), 'Missing required argument')
  expect_error(get.score(schedule, r, c()), 'Invalid argument type')
  expect_error(get.score(schedule, r, ''), 'Invalid argument type')
  expect_error(get.score(schedule, r, 1), 'Invalid argument type')
  expect_error(get.score(schedule, r, 1:2), 'Invalid argument type')
  expect_error(get.score(schedule, r, c('a', 'b')), 'Invalid argument type')
  expect_error(get.score(schedule, r, list(1, 1)), 'Invalid argument type')
  expect_error(get.score(schedule, r, data.frame(1, 1)), 'Invalid argument type')

  rs <- matrix(nrow=0, ncol=0)
  expect_error(get.score(schedule, r, rs), 'Invalid argument dimensions')

  rs <- matrix(nrow=0, ncol=1)
  expect_error(get.score(schedule, r, rs), 'Invalid argument dimensions')

  rs <- matrix(nrow=1, ncol=0)
  expect_error(get.score(schedule, r, rs), 'Invalid argument dimensions')

  rs <- matrix(nrow=1, ncol=1)
  rs[1, 1] <- 1
  expect_error(get.score(schedule, r, rs), 'Invalid argument dimensions')

  rs <- matrix(nrow=1, ncol=2)
  rs[1, 1] <- 1
  rs[1, 2] <- 1
  expect_error(get.score(schedule, r, rs), 'Invalid argument dimensions')

  rs <- matrix(nrow=2, ncol=1)
  rs[1, 1] <- 1
  rs[2, 1] <- 1
  expect_error(get.score(schedule, r, rs), 'Invalid argument dimensions')

  rs <- matrix(nrow=2, ncol=2)
  rs[1, 1] <- 1; rs[1, 2] <- 1
  rs[2, 1] <- 1; rs[2, 2] <- 1
  expect_error(get.score(schedule, r, rs), 'Invalid argument dimensions')

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- -1
  rs[1, 2] <- 1
  rs[1, 3] <- 1
  expect_error(get.score(schedule, r, rs), 'Invalid argument')

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- 1
  rs[1, 2] <- -1
  rs[1, 3] <- 1
  expect_error(get.score(schedule, r, rs), 'Invalid argument')

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- 1
  rs[1, 2] <- 1
  rs[1, 3] <- -1
  expect_error(get.score(schedule, r, rs), 'Invalid argument')

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- 'a'
  rs[1, 2] <- 1
  rs[1, 3] <- 1
  expect_error(get.score(schedule, r, rs), 'Invalid argument')

})


test_that("get.score validates deadline correctly", {

  schedule <- get.initial.schedule(1, c(10))

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 1
  r[1, 2] <- 1

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- 1
  rs[1, 2] <- 1
  rs[1, 3] <- 1

  expect_error(get.score(schedule, r, rs), 'Missing required argument')
  expect_error(get.score(schedule, r, rs, c()), 'Invalid argument length')
  expect_error(get.score(schedule, r, rs, ''), 'Non-numeric argument')

  expect_error(get.score(schedule, r, rs, c()), 'Invalid argument length')
  expect_error(get.score(schedule, r, rs, 1:2), 'Invalid argument length')
  expect_error(get.score(schedule, r, rs, 0), 'Invalid argument')

})


test_that("get.score returns a valid value", {

  data('m3xlarge.runtimes.expdist')
  setup.trainingset.runtimes('m3xlarge', m3xlarge.runtimes.expdist)
  schedule <- get.initial.schedule(1, c(1))

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 1
  r[1, 2] <- 1

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- 1
  rs[1, 2] <- 1
  rs[1, 3] <- 1

  s <- get.score(schedule, r, rs, 10)
  expect_true(!is.null(attr(s, 'score'))) # must always return a value...
  expect_true(is.numeric(attr(s, 'score'))) # ... that is a number...
  expect_true(attr(s, 'score') >= 0 && attr(s, 'score') <= 1) # between 0 and 1

})
