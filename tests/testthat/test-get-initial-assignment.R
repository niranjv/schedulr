context("Test Simulated Annealing functions")

expect_equal(1,1)


test_that("get.initial.assignment validates cluster.size correctly", {

  expect_error(get.initial.assignment(), 'Missing required argument')
  expect_error(get.initial.assignment(c()), 'Invalid argument length')
  expect_error(get.initial.assignment('a'), 'Non-integer argument')
  expect_error(get.initial.assignment('3.14'), 'Non-integer argument')
  expect_error(get.initial.assignment(1:2), 'Invalid argument length')
  expect_error(get.initial.assignment(0), 'Invalid argument')
})


test_that("get.initial.assignment validates tasks correctly", {
  expect_error(get.initial.assignment(1), 'Missing required argument')
  expect_error(get.initial.assignment(1, c()), 'Invalid argument length')
  expect_error(get.initial.assignment(1, 'a'), 'Non-numeric argument')
  expect_error(get.initial.assignment(1, 0), 'Invalid argument')
})


test_that("get.initial.assignment returns the correct value", {
  assignment <- get.initial.assignment(1, 10)
  expect_is(assignment, 'list')
  expect_equal(length(assignment), 1)
  expect_equal(length(unlist(assignment)), 1)
  expect_equal(unlist(assignment), 10)
  expect_equal(assignment[[1]], 10)

  assignment <- get.initial.assignment(1, c(10))
  expect_is(assignment, 'list')
  expect_equal(length(assignment), 1)
  expect_equal(length(unlist(assignment)), 1)
  expect_equal(unlist(assignment), 10)
  expect_equal(assignment[[1]], 10)

  assignment <- get.initial.assignment(1, c(10, 20))
  expect_is(assignment, 'list')
  expect_equal(length(assignment), 1)
  expect_equal(length(unlist(assignment)), 2)
  expect_equal(unlist(assignment), c(10, 20))
  expect_equal(assignment[[1]], c(10, 20))

  assignment <- get.initial.assignment(2, 10)
  expect_is(assignment, 'list')
  expect_equal(length(assignment), 2)
  expect_equal(length(unlist(assignment)), 1)
  expect_equal(unlist(assignment), 10)
  expect_equal(assignment[[1]], 10)
  expect_equal(assignment[[2]], NULL)

  assignment <- get.initial.assignment(2, c(10, 20))
  expect_is(assignment, 'list')
  expect_equal(length(assignment), 2)
  expect_equal(length(unlist(assignment)), 2)
  expect_equal(unlist(assignment), c(10, 20))
  expect_equal(assignment[[1]], 10)
  expect_equal(assignment[[2]], 20)

  assignment <- get.initial.assignment(2, c(10, 20, 30))
  expect_is(assignment, 'list')
  expect_equal(length(assignment), 2)
  expect_equal(length(unlist(assignment)), 3)
  expect_equal(sort(unlist(assignment)), c(10, 20, 30))
  expect_equal(assignment[[1]], c(10,30))
  expect_equal(assignment[[2]], 20)

})
