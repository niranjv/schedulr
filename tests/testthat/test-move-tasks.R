context("Test move.tasks()")


test_that("move.tasks validates assignment correctly", {

  expect_error(move.tasks(), 'Missing required argument')
  expect_error(move.tasks(''), 'Invalid argument type')
  expect_error(move.tasks(1), 'Invalid argument type')
  expect_error(move.tasks('a'), 'Invalid argument type')
  expect_error(move.tasks(c(1,2)), 'Invalid argument type')
  expect_error(move.tasks(list()), 'Invalid argument length')
  expect_error(move.tasks(list('')), 'Non-numeric argument')
  expect_error(move.tasks(list(c())), 'Non-numeric argument')
  expect_error(move.tasks(list(0)), 'Invalid argument')
  expect_error(move.tasks(list(-1)), 'Invalid argument')

})


test_that("move.tasks validates num.tasks correctly", {

  assignment <- get.initial.assignment(1,10)
  expect_error(move.tasks(assignment), 'Missing required argument')
  expect_error(move.tasks(assignment, ''), 'Non-integer argument')
  expect_error(move.tasks(assignment, 'a'), 'Non-integer argument')
  expect_error(move.tasks(assignment, 3.14), 'Non-integer argument')
  expect_error(move.tasks(assignment, -1), 'Invalid argument')
  expect_error(move.tasks(assignment, c(1,2)), 'Invalid argument type')

})


test_that("move.tasks handles single instance case", {

  assignment <- get.initial.assignment(1,10)
  new.assignment <- move.tasks(assignment, 1)
  expect_is(new.assignment, 'list')
  expect_equal(assignment, new.assignment)

  assignment <- get.initial.assignment(1, c(10, 20, 30))
  new.assignment <- move.tasks(assignment, 1)
  expect_is(new.assignment, 'list')
  expect_equal(assignment, new.assignment)

})


test_that("move.tasks handles single task case", {

  assignment <- get.initial.assignment(2,10)
  new.assignment <- move.tasks(assignment, 1)
  expect_is(new.assignment, 'list')
  expect_equal(assignment[[1]], new.assignment[[2]])
  expect_equal(assignment[[2]], new.assignment[[1]])

  assignment <- get.initial.assignment(3,10)
  new.assignment <- move.tasks(assignment, 1)
  expect_is(new.assignment, 'list')
  expect_equal(new.assignment[[1]], NULL)

})


test_that("move.tasks fails when # tasks to move > # tasks available", {

  assignment <- get.initial.assignment(2, 10)
  expect_error(move.tasks(assignment, 2), 'Invalid argument')

})


test_that("move.tasks fails when no instance has sufficient # tasks to move", {

  assignment <- get.initial.assignment(2, c(10,20))
  expect_error(move.tasks(assignment, 2), 'Invalid argument')

})
