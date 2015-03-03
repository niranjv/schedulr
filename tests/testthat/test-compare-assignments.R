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


test_that("compare.assignments validates deadline correctly", {

  cur.assignment <- get.initial.assignment(1, c(10))
  proposed.assignment <- get.initial.assignment(2, c(10))
  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 1
  r[1, 2] <- 1

  expect_error(compare.assignments(cur.assignment, proposed.assignment, r), 'Missing required argument')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, c()), 'Invalid argument length')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, ''), 'Non-numeric argument')

  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, c()), 'Invalid argument length')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, 1:2), 'Invalid argument length')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, 0), 'Invalid argument')

})


test_that("compare.assignments validates max.temp correctly", {

  cur.assignment <- get.initial.assignment(1, c(10))
  proposed.assignment <- get.initial.assignment(2, c(10))
  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 1
  r[1, 2] <- 1

  deadline <- 3600

  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, deadline), 'Missing required argument')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, deadline, c()), 'Invalid argument length')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, deadline, 'a'), 'Non-numeric argument')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, deadline, 1:2), 'Invalid argument length')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, deadline, -1), 'Invalid argument')
  expect_error(compare.assignments(cur.assignment, proposed.assignment, r, deadline, 0), 'Invalid argument')

})
