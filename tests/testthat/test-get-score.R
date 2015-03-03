context("Test get.score()")


test_that("get.score validates assignment correctly", {

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

  assignment <- get.initial.assignment(1, c(10))

  expect_error(get.score(assignment, ), 'Missing required argument')
  expect_error(get.score(assignment, c()), 'Invalid argument type')
  expect_error(get.score(assignment, ''), 'Invalid argument type')
  expect_error(get.score(assignment, 1), 'Invalid argument type')
  expect_error(get.score(assignment, 1:2), 'Invalid argument type')
  expect_error(get.score(assignment, c('a', 'b')), 'Invalid argument type')
  expect_error(get.score(assignment, list(1, 1)), 'Invalid argument type')
  expect_error(get.score(assignment, data.frame(1, 1)), 'Invalid argument type')

  r <- matrix(nrow=0, ncol=0)
  expect_error(get.score(assignment, r), 'Invalid argument dimensions')

  r <- matrix(nrow=0, ncol=1)
  expect_error(get.score(assignment, r), 'Invalid argument dimensions')

  r <- matrix(nrow=1, ncol=0)
  expect_error(get.score(assignment, r), 'Invalid argument dimensions')

  r <- matrix(nrow=1, ncol=1)
  r[1, 1] <- 1
  expect_error(get.score(assignment, r), 'Invalid argument dimensions')

  r <- matrix(nrow=1, ncol=3)
  r[1, 1] <- 1
  r[1, 2] <- 1
  r[1, 3] <- 1
  expect_error(get.score(assignment, r), 'Invalid argument dimensions')

  r <- matrix(nrow=2, ncol=1)
  r[1, 1] <- 1
  r[2, 1] <- 1
  expect_error(get.score(assignment, r), 'Invalid argument dimensions')

  r <- matrix(nrow=2, ncol=3)
  r[1, 1] <- 1; r[1, 2] <- 1; r[1, 3] <- 1
  r[2, 1] <- 1; r[2, 2] <- 1; r[2, 3] <- 1
  expect_error(get.score(assignment, r), 'Invalid argument dimensions')

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- -1
  r[1, 2] <- 1
  expect_error(get.score(assignment, r), 'Invalid argument')

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 1
  r[1, 2] <- -1
  expect_error(get.score(assignment, r), 'Invalid argument')

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 'a'
  r[1, 2] <- 1
  expect_error(get.score(assignment, r), 'Invalid argument')

})


test_that("get.score validates deadline correctly", {

  assignment <- get.initial.assignment(1, c(10))
  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 1
  r[1, 2] <- 1

  expect_error(get.score(assignment, r), 'Missing required argument')
  expect_error(get.score(assignment, r, c()), 'Invalid argument length')
  expect_error(get.score(assignment, r, ''), 'Non-numeric argument')

  expect_error(get.score(assignment, r, c()), 'Invalid argument length')
  expect_error(get.score(assignment, r, 1:2), 'Invalid argument length')
  expect_error(get.score(assignment, r, 0), 'Invalid argument')

  s <- get.score(assignment, r, 3600)
  expect_true(s >= 0 && s <= 1) # since score is a probability

})
