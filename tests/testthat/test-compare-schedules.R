context("Test compare.schedules()")


test_that("compare.schedules validates cur.schedule correctly", {

  expect_error(compare.schedules(), 'Missing required argument')
  expect_error(compare.schedules(''), 'Invalid argument type')
  expect_error(compare.schedules(1), 'Invalid argument type')
  expect_error(compare.schedules('a'), 'Invalid argument type')
  expect_error(compare.schedules(c(1,2)), 'Invalid argument type')
  expect_error(compare.schedules(list()), 'Invalid argument length')
  expect_error(compare.schedules(list('')), 'Non-numeric argument')
  expect_error(compare.schedules(list(c())), 'Non-numeric argument')
  expect_error(compare.schedules(list(0)), 'Invalid argument')
  expect_error(compare.schedules(list(-1)), 'Invalid argument')

})


test_that("compare.schedules validates proposed.schedule correctly", {

  cur.schedule <- get.initial.schedule(1, c(10))
  attr(cur.schedule, 'score') <- 0
  attr(cur.schedule, 'deadline') <- 100
  attr(cur.schedule, 'runtime95pct') <- 0
  attr(cur.schedule, 'runtime99pct') <- 0

  expect_error(compare.schedules(cur.schedule, ), 'Missing required argument')
  expect_error(compare.schedules(cur.schedule, ''), 'Invalid argument type')
  expect_error(compare.schedules(cur.schedule, 1), 'Invalid argument type')
  expect_error(compare.schedules(cur.schedule, 'a'), 'Invalid argument type')
  expect_error(compare.schedules(cur.schedule, c(1,2)), 'Invalid argument type')
  expect_error(compare.schedules(cur.schedule, list()), 'Invalid argument length')
  expect_error(compare.schedules(cur.schedule, list('')), 'Non-numeric argument')
  expect_error(compare.schedules(cur.schedule, list(c())), 'Non-numeric argument')
  expect_error(compare.schedules(cur.schedule, list(0)), 'Invalid argument')
  expect_error(compare.schedules(cur.schedule, list(-1)), 'Invalid argument')

})


test_that("compare.schedules validates runtimes correctly", {

  cur.schedule <- get.initial.schedule(1, c(10))
  attr(cur.schedule, 'score') <- 0
  attr(cur.schedule, 'deadline') <- 100
  attr(cur.schedule, 'runtime95pct') <- 0
  attr(cur.schedule, 'runtime99pct') <- 0

  proposed.schedule <- get.initial.schedule(2, c(10))

  expect_error(compare.schedules(cur.schedule, proposed.schedule, ), 'Missing required argument')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, c()), 'Invalid argument type')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, ''), 'Invalid argument type')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, 1), 'Invalid argument type')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, 1:2), 'Invalid argument type')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, c('a', 'b')), 'Invalid argument type')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, list(1, 1)), 'Invalid argument type')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, data.frame(1, 1)), 'Invalid argument type')

  r <- matrix(nrow=0, ncol=0)
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r), 'Invalid argument dimensions')

  r <- matrix(nrow=0, ncol=1)
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r), 'Invalid argument dimensions')

  r <- matrix(nrow=1, ncol=0)
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r), 'Invalid argument dimensions')

  r <- matrix(nrow=1, ncol=1)
  r[1, 1] <- 1
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r), 'Invalid argument dimensions')

  r <- matrix(nrow=1, ncol=3)
  r[1, 1] <- 1
  r[1, 2] <- 1
  r[1, 3] <- 1
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r), 'Invalid argument dimensions')

  r <- matrix(nrow=2, ncol=1)
  r[1, 1] <- 1
  r[2, 1] <- 1
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r), 'Invalid argument dimensions')

  r <- matrix(nrow=2, ncol=3)
  r[1, 1] <- 1; r[1, 2] <- 1; r[1, 3] <- 1
  r[2, 1] <- 1; r[2, 2] <- 1; r[2, 3] <- 1
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r), 'Invalid argument dimensions')

  r <- matrix(nrow=1, ncol=3)
  r[1, 1] <- -1
  r[1, 2] <- 1
  r[1, 3] <- 1
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r), 'Invalid argument')

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- -1
  r[1, 2] <- 1
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r), 'Invalid argument')

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 1
  r[1, 2] <- -1
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r), 'Invalid argument')

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 'a'
  r[1, 2] <- 1
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r), 'Invalid argument')

})


test_that("compare.schedules validates runtimes.summary correctly", {

  cur.schedule <- get.initial.schedule(1, c(10))
  attr(cur.schedule, 'score') <- 0
  attr(cur.schedule, 'deadline') <- 100
  attr(cur.schedule, 'runtime95pct') <- 0
  attr(cur.schedule, 'runtime99pct') <- 0

  proposed.schedule <- get.initial.schedule(2, c(10))

  r <- matrix(nrow=2, ncol=2)
  r[1, 1] <- 1; r[1, 2] <- 1
  r[2, 1] <- 1; r[2, 2] <- 1

  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, ), 'Missing required argument')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, c()), 'Invalid argument type')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, ''), 'Invalid argument type')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, 1), 'Invalid argument type')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, 1:2), 'Invalid argument type')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, c('a', 'b')), 'Invalid argument type')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, list(1, 1)), 'Invalid argument type')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, data.frame(1, 1)), 'Invalid argument type')

  rs <- matrix(nrow=0, ncol=0)
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs), 'Invalid argument dimensions')

  rs <- matrix(nrow=0, ncol=1)
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs), 'Invalid argument dimensions')

  rs <- matrix(nrow=1, ncol=0)
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs), 'Invalid argument dimensions')

  rs <- matrix(nrow=1, ncol=1)
  rs[1, 1] <- 1
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs), 'Invalid argument dimensions')

  rs <- matrix(nrow=1, ncol=2)
  rs[1, 1] <- 1
  rs[1, 2] <- 1
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs), 'Invalid argument dimensions')

  rs <- matrix(nrow=2, ncol=1)
  rs[1, 1] <- 1
  rs[2, 1] <- 1
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs), 'Invalid argument dimensions')

  rs <- matrix(nrow=2, ncol=2)
  rs[1, 1] <- 1; r[1, 2] <- 1
  rs[2, 1] <- 1; r[2, 2] <- 1
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs), 'Invalid argument dimensions')

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- -1
  rs[1, 2] <- 1
  rs[1, 3] <- 1
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs), 'Invalid argument')

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- 1
  rs[1, 2] <- -1
  rs[1, 3] <- 1
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs), 'Invalid argument')

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- 1
  rs[1, 2] <- 1
  rs[1, 3] <- -1
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs), 'Invalid argument')

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- 'a'
  rs[1, 2] <- 1
  rs[1, 3] <- 1
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs), 'Invalid argument')

})


test_that("compare.schedules validates deadline correctly", {

  cur.schedule <- get.initial.schedule(1, c(10))
  attr(cur.schedule, 'score') <- 0
  attr(cur.schedule, 'deadline') <- 100
  attr(cur.schedule, 'runtime95pct') <- 0
  attr(cur.schedule, 'runtime99pct') <- 0

  proposed.schedule <- get.initial.schedule(2, c(10))

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 1
  r[1, 2] <- 1

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- 1
  rs[1, 2] <- 1
  rs[1, 3] <- 1

  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs), 'Missing required argument')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs, c()), 'Invalid argument length')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs, ''), 'Non-numeric argument')

  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs, c()), 'Invalid argument length')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs, 1:2), 'Invalid argument length')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs, 0), 'Invalid argument')

})


test_that("compare.schedules validates max.temp correctly", {

  cur.schedule <- get.initial.schedule(1, c(10))
  attr(cur.schedule, 'score') <- 0
  attr(cur.schedule, 'deadline') <- 100
  attr(cur.schedule, 'runtime95pct') <- 0
  attr(cur.schedule, 'runtime99pct') <- 0

  proposed.schedule <- get.initial.schedule(2, c(10))

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 1
  r[1, 2] <- 1

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- 1
  rs[1, 2] <- 1
  rs[1, 3] <- 1

  deadline <- 3600

  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs, deadline), 'Missing required argument')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs, deadline, c()), 'Invalid argument length')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs, deadline, 'a'), 'Non-numeric argument')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs, deadline, 1:2), 'Invalid argument length')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs, deadline, -1), 'Invalid argument')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs, deadline, 0), 'Invalid argument')

})


test_that("compare.schedules validates max.iter correctly", {

  cur.schedule <- get.initial.schedule(1, c(10))
  attr(cur.schedule, 'score') <- 0
  attr(cur.schedule, 'deadline') <- 100
  attr(cur.schedule, 'runtime95pct') <- 0
  attr(cur.schedule, 'runtime99pct') <- 0

  proposed.schedule <- get.initial.schedule(2, c(10))

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 1
  r[1, 2] <- 1

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- 1
  rs[1, 2] <- 1
  rs[1, 3] <- 1

  deadline <- 3600
  max.temp <- 25

  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs, deadline, max.temp, ), 'Missing required argument')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs, deadline, max.temp, c()), 'Invalid argument length')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs, deadline, max.temp, 'a'), 'Non-integer argument')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs, deadline, max.temp, 3.14), 'Non-integer argument')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs, deadline, max.temp, 1:2), 'Invalid argument length')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs, deadline, max.temp, -1), 'Invalid argument')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs, deadline, max.temp, 0), 'Invalid argument')

})


test_that("compare.schedules validates cur.iter correctly", {

  cur.schedule <- get.initial.schedule(1, c(10))
  attr(cur.schedule, 'score') <- 0
  attr(cur.schedule, 'deadline') <- 100
  attr(cur.schedule, 'runtime95pct') <- 0
  attr(cur.schedule, 'runtime99pct') <- 0

  proposed.schedule <- get.initial.schedule(2, c(10))

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 1
  r[1, 2] <- 1

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- 1
  rs[1, 2] <- 1
  rs[1, 3] <- 1

  deadline <- 3600
  max.temp <- 25
  max.iter <- 100

  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs, deadline, max.temp, max.iter, ), 'Missing required argument')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs, deadline, max.temp, max.iter, c()), 'Invalid argument length')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs, deadline, max.temp, max.iter, 'a'), 'Non-integer argument')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs, deadline, max.temp, max.iter, 3.14), 'Non-integer argument')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs, deadline, max.temp, max.iter, 1:2), 'Invalid argument length')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs, deadline, max.temp, max.iter, -1), 'Invalid argument')
  expect_error(compare.schedules(cur.schedule, proposed.schedule, r, rs, deadline, max.temp, max.iter, 101), 'Invalid argument')

})


test_that("compare.schedules returns a valid value", {

  data('m3xlarge.runtimes.expdist')
  setup.trainingset.runtimes('m3xlarge', m3xlarge.runtimes.expdist)

  cur.schedule <- get.initial.schedule(1, c(10, 20))
  attr(cur.schedule, 'score') <- 0
  attr(cur.schedule, 'processing.cost') <- 1
  attr(cur.schedule, 'deadline') <- 100
  attr(cur.schedule, 'runtime95pct') <- 0
  attr(cur.schedule, 'runtime99pct') <- 0

  proposed.schedule <- get.initial.schedule(2, c(10, 20))

  r <- matrix(nrow=2, ncol=2)
  r[1, 1] <- 10 # size
  r[1, 2] <- 23.5 # runtime for this sample
  r[2, 1] <- 20 # size
  r[2, 2] <- 45 # runtime for this sample

  rs <- matrix(nrow=2, ncol=3)
  rs[1, 1] <- 10 # size
  rs[1, 2] <- 23.5 # mean runtime
  rs[1, 3] <- 2.5 # variance of runtime

  rs[2, 1] <- 20 # size
  rs[2, 2] <- 45 # mean runtime
  rs[2, 3] <- 5 # variance of runtime

  deadline <- 60
  max.temp <- 25
  max.iter <- 100
  cur.iter <- 1

  accepted <- compare.schedules(cur.schedule, proposed.schedule, r, rs, deadline, max.temp, max.iter, cur.iter)
  expect_is(accepted, 'list')
  expect_is(attr(accepted, 'score'), 'numeric')
  expect_is(attr(accepted, 'processing.cost'), 'numeric')
  expect_is(attr(accepted, 'deadline'), 'numeric')
  expect_is(attr(accepted, 'runtime95pct'), 'numeric')
  expect_is(attr(accepted, 'runtime99pct'), 'numeric')

  expect_true(attr(accepted, 'score') >= 0 && attr(accepted, 'score') <= 1)
  expect_true(attr(accepted, 'processing.cost') >= 0
  expect_true(attr(accepted, 'deadline') > 0)
  expect_true(attr(accepted, 'runtime95pct') > 0)
  expect_true(attr(accepted, 'runtime99pct') > 0)

})
