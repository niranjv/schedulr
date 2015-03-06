context("Test compare.assignments()")


test_that("compare.assignments validates cur.assignment correctly", {

  expect_error(compare.assignments(), 'Missing required argument')
  expect_error(compare.assignments(''), 'Invalid argument type')
  expect_error(compare.assignments(1), 'Invalid argument type')
  expect_error(compare.assignments('a'), 'Invalid argument type')
  expect_error(compare.assignments(c(1,2)), 'Invalid argument type')
  expect_error(compare.assignments(list()), 'Invalid argument length')
  expect_error(compare.assignments(list('')), 'Non-numeric argument')
  expect_error(compare.assignments(list(c())), 'Non-numeric argument')
  expect_error(compare.assignments(list(0)), 'Invalid argument')
  expect_error(compare.assignments(list(-1)), 'Invalid argument')

})


test_that("compare.assignments validates proposed.assignment correctly", {

  cur.assignment <- get.initial.assignment(1, c(10))

  expect_error(compare.assignments(cur.assignment, ), 'Missing required argument')
  expect_error(compare.assignments(cur.assignment, ''), 'Invalid argument type')
  expect_error(compare.assignments(cur.assignment, 1), 'Invalid argument type')
  expect_error(compare.assignments(cur.assignment, 'a'), 'Invalid argument type')
  expect_error(compare.assignments(cur.assignment, c(1,2)), 'Invalid argument type')
  expect_error(compare.assignments(cur.assignment, list()), 'Invalid argument length')
  expect_error(compare.assignments(cur.assignment, list('')), 'Non-numeric argument')
  expect_error(compare.assignments(cur.assignment, list(c())), 'Non-numeric argument')
  expect_error(compare.assignments(cur.assignment, list(0)), 'Invalid argument')
  expect_error(compare.assignments(cur.assignment, list(-1)), 'Invalid argument')

})


test_that("compare.assignments validates runtimes correctly", {

  cur.assignment <- get.initial.assignment(1, c(10))
  proposed.assignment <- get.initial.assignment(2, c(10))

  expect_error(compare.assignments(cur.assignment, proposed.assignment, ), 'Missing required argument')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, c()), 'Invalid argument type')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, ''), 'Invalid argument type')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, 1), 'Invalid argument type')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, 1:2), 'Invalid argument type')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, c('a', 'b')), 'Invalid argument type')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, list(1, 1)), 'Invalid argument type')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, data.frame(1, 1)), 'Invalid argument type')

  r <- matrix(nrow=0, ncol=0)
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r), 'Invalid argument dimensions')

  r <- matrix(nrow=0, ncol=1)
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r), 'Invalid argument dimensions')

  r <- matrix(nrow=1, ncol=0)
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r), 'Invalid argument dimensions')

  r <- matrix(nrow=1, ncol=1)
  r[1, 1] <- 1
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r), 'Invalid argument dimensions')

  r <- matrix(nrow=1, ncol=3)
  r[1, 1] <- 1
  r[1, 2] <- 1
  r[1, 3] <- 1
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r), 'Invalid argument dimensions')

  r <- matrix(nrow=2, ncol=1)
  r[1, 1] <- 1
  r[2, 1] <- 1
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r), 'Invalid argument dimensions')

  r <- matrix(nrow=2, ncol=3)
  r[1, 1] <- 1; r[1, 2] <- 1; r[1, 3] <- 1
  r[2, 1] <- 1; r[2, 2] <- 1; r[2, 3] <- 1
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r), 'Invalid argument dimensions')

  r <- matrix(nrow=1, ncol=3)
  r[1, 1] <- -1
  r[1, 2] <- 1
  r[1, 3] <- 1
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r), 'Invalid argument')

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- -1
  r[1, 2] <- 1
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r), 'Invalid argument')

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 1
  r[1, 2] <- -1
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r), 'Invalid argument')

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 'a'
  r[1, 2] <- 1
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r), 'Invalid argument')

})


test_that("compare.assignments validates runtimes.summary correctly", {

  cur.assignment <- get.initial.assignment(1, c(10))
  proposed.assignment <- get.initial.assignment(2, c(10))

  r <- matrix(nrow=2, ncol=2)
  r[1, 1] <- 1; r[1, 2] <- 1
  r[2, 1] <- 1; r[2, 2] <- 1

  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, ), 'Missing required argument')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, c()), 'Invalid argument type')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, ''), 'Invalid argument type')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, 1), 'Invalid argument type')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, 1:2), 'Invalid argument type')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, c('a', 'b')), 'Invalid argument type')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, list(1, 1)), 'Invalid argument type')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, data.frame(1, 1)), 'Invalid argument type')

  rs <- matrix(nrow=0, ncol=0)
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs), 'Invalid argument dimensions')

  rs <- matrix(nrow=0, ncol=1)
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs), 'Invalid argument dimensions')

  rs <- matrix(nrow=1, ncol=0)
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs), 'Invalid argument dimensions')

  rs <- matrix(nrow=1, ncol=1)
  rs[1, 1] <- 1
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs), 'Invalid argument dimensions')

  rs <- matrix(nrow=1, ncol=2)
  rs[1, 1] <- 1
  rs[1, 2] <- 1
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs), 'Invalid argument dimensions')

  rs <- matrix(nrow=2, ncol=1)
  rs[1, 1] <- 1
  rs[2, 1] <- 1
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs), 'Invalid argument dimensions')

  rs <- matrix(nrow=2, ncol=2)
  rs[1, 1] <- 1; r[1, 2] <- 1
  rs[2, 1] <- 1; r[2, 2] <- 1
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs), 'Invalid argument dimensions')

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- -1
  rs[1, 2] <- 1
  rs[1, 3] <- 1
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs), 'Invalid argument')

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- 1
  rs[1, 2] <- -1
  rs[1, 3] <- 1
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs), 'Invalid argument')

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- 1
  rs[1, 2] <- 1
  rs[1, 3] <- -1
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs), 'Invalid argument')

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- 'a'
  rs[1, 2] <- 1
  rs[1, 3] <- 1
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs), 'Invalid argument')

})


test_that("compare.assignments validates deadline correctly", {

  cur.assignment <- get.initial.assignment(1, c(10))
  proposed.assignment <- get.initial.assignment(2, c(10))

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 1
  r[1, 2] <- 1

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- 1
  rs[1, 2] <- 1
  rs[1, 3] <- 1

  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs), 'Missing required argument')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs, c()), 'Invalid argument length')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs, ''), 'Non-numeric argument')

  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs, c()), 'Invalid argument length')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs, 1:2), 'Invalid argument length')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs, 0), 'Invalid argument')

})


test_that("compare.assignments validates max.temp correctly", {

  cur.assignment <- get.initial.assignment(1, c(10))
  proposed.assignment <- get.initial.assignment(2, c(10))

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 1
  r[1, 2] <- 1

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- 1
  rs[1, 2] <- 1
  rs[1, 3] <- 1

  deadline <- 3600

  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs, deadline), 'Missing required argument')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs, deadline, c()), 'Invalid argument length')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs, deadline, 'a'), 'Non-numeric argument')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs, deadline, 1:2), 'Invalid argument length')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs, deadline, -1), 'Invalid argument')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs, deadline, 0), 'Invalid argument')

})


test_that("compare.assignments validates max.iter correctly", {

  cur.assignment <- get.initial.assignment(1, c(10))
  proposed.assignment <- get.initial.assignment(2, c(10))

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 1
  r[1, 2] <- 1

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- 1
  rs[1, 2] <- 1
  rs[1, 3] <- 1

  deadline <- 3600
  max.temp <- 25

  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs, deadline, max.temp, ), 'Missing required argument')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs, deadline, max.temp, c()), 'Invalid argument length')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs, deadline, max.temp, 'a'), 'Non-integer argument')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs, deadline, max.temp, 3.14), 'Non-integer argument')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs, deadline, max.temp, 1:2), 'Invalid argument length')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs, deadline, max.temp, -1), 'Invalid argument')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs, deadline, max.temp, 0), 'Invalid argument')

})


test_that("compare.assignments validates cur.iter correctly", {

  cur.assignment <- get.initial.assignment(1, c(10))
  proposed.assignment <- get.initial.assignment(2, c(10))

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

  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs, deadline, max.temp, max.iter, ), 'Missing required argument')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs, deadline, max.temp, max.iter, c()), 'Invalid argument length')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs, deadline, max.temp, max.iter, 'a'), 'Non-integer argument')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs, deadline, max.temp, max.iter, 3.14), 'Non-integer argument')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs, deadline, max.temp, max.iter, 1:2), 'Invalid argument length')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs, deadline, max.temp, max.iter, -1), 'Invalid argument')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, rs, deadline, max.temp, max.iter, 101), 'Invalid argument')

})


test_that("compare.assignments returns a valid value", {

  cur.assignment <- get.initial.assignment(1, c(10))
  proposed.assignment <- get.initial.assignment(2, c(10))

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 10 # size
  r[1, 2] <- 23.5 # runtime for this sample

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- 10 # size
  rs[1, 2] <- 23.5 # mean runtime
  rs[1, 3] <- 2.5 # variance of runtime

  deadline <- 60
  max.temp <- 25
  max.iter <- 100
  cur.iter <- 1

  accepted <- compare.assignments(cur.assignment, proposed.assignment, r, rs, deadline, max.temp, max.iter, cur.iter)
  expect_is(accepted, 'list')
  expect_is(accepted[[1]], 'list')
  expect_is(accepted[[2]], 'numeric')
  expect_true(accepted[[2]] >= 0 && accepted[[2]] <= 1)

})
