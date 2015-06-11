context("Test move.tasks()")


test_that("move.tasks validates schedule correctly", {

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

  schedule <- get.initial.schedule(1, c(10))
  expect_error(move.tasks(schedule), 'Missing required argument')
  expect_error(move.tasks(schedule, ''), 'Non-integer argument')
  expect_error(move.tasks(schedule, 'a'), 'Non-integer argument')
  expect_error(move.tasks(schedule, 3.14), 'Non-integer argument')
  expect_error(move.tasks(schedule, -1), 'Invalid argument')
  expect_error(move.tasks(schedule, c(1, 2)), 'Invalid argument length')

})


test_that("move.tasks handles single instance case", {

  schedule <- get.initial.schedule(1, c(10))
  new.schedule <- move.tasks(schedule, 1)
  expect_is(new.schedule, 'list')
  expect_equal(schedule, new.schedule)

  schedule <- get.initial.schedule(1, c(10))
  new.schedule <- move.tasks(schedule, 1, exchange=T)
  expect_is(new.schedule, 'list')
  expect_equal(schedule, new.schedule)

  rs <- matrix(nrow=3, ncol=3)
  rs[1, 1] <- 10; rs[1, 2] <- 23.5; rs[1, 3] <- 3.5
  rs[2, 1] <- 20; rs[2, 2] <- 33.5; rs[2, 3] <- 4.5
  rs[3, 1] <- 30; rs[3, 2] <- 43.5; rs[3, 3] <- 5.5

  schedule <- get.initial.schedule(1, c(10, 20, 30), rs, method='leptf')
  new.schedule <- move.tasks(schedule, 1)
  expect_is(new.schedule, 'list')
  expect_equal(schedule, new.schedule)

  schedule <- get.initial.schedule(1, c(10, 20, 30), rs, method='leptf')
  new.schedule <- move.tasks(schedule, 1, exchange=T)
  expect_is(new.schedule, 'list')
  expect_equal(schedule, new.schedule)

})


test_that("move.tasks handles single task case", {

  rs <- matrix(nrow=1, ncol=3)
  rs[1, 1] <- 10; rs[1, 2] <- 23.5; rs[1, 3] <- 3.5
  
  schedule <- get.initial.schedule(2, c(10), rs, method='leptf')
  new.schedule <- move.tasks(schedule, 1)
  expect_is(new.schedule, 'list')
  expect_equal(schedule[[1]], new.schedule[[2]])
  expect_equal(schedule[[2]], new.schedule[[1]])

  schedule <- get.initial.schedule(3, c(10), rs, method='leptf')
  new.schedule <- move.tasks(schedule, 1)
  expect_is(new.schedule, 'list')
  expect_equal(new.schedule[[1]], NULL)

  #FIXME schedule <- get.initial.schedule(2, c(10))
  #FIXME expect_error(move.tasks(schedule, 1, exchange=T), 'Invalid argument')

  #FIXME schedule <- get.initial.schedule(3, c(10))
  #FIXME expect_error(move.tasks(schedule, 1, exchange=T), 'Invalid argument')

})


test_that("move.tasks fails when # tasks to move > # tasks available", {

  schedule <- get.initial.schedule(2, c(10))
  expect_error(move.tasks(schedule, 2), 'Invalid argument')

  schedule <- get.initial.schedule(2, c(10))
  expect_error(move.tasks(schedule, 2, exchange=T), 'Invalid argument')

})


test_that("move.tasks fails when no instance has sufficient # tasks to move", {

  schedule <- get.initial.schedule(2, c(10))
  expect_error(move.tasks(schedule, 2), 'Invalid argument')

  schedule <- get.initial.schedule(2, c(10))
  expect_error(move.tasks(schedule, 2, exchange=T), 'Invalid argument')

  #FIXME schedule <- get.initial.schedule(3, c(10, 20, 30))
  #FIXME expect_error(move.tasks(schedule, 2), 'Invalid argument')

  #FIXME schedule <- get.initial.schedule(3, c(10, 20, 30))
  #FIXME expect_error(move.tasks(schedule, 2, exchange=T), 'Invalid argument')

})


test_that("move.tasks exchanges 1 task between 2 instances", {

  rs <- matrix(nrow=2, ncol=3)
  rs[1, 1] <- 10; rs[1, 2] <- 23.5; rs[1, 3] <- 3.5
  rs[2, 1] <- 20; rs[2, 2] <- 33.5; rs[2, 3] <- 4.5

  schedule <- get.initial.schedule(2, c(10, 20), rs, method='leptf')
  new.schedule <- move.tasks(schedule, 1, exchange=T)
  expect_equal(schedule[[1]], new.schedule[[2]])
  expect_equal(schedule[[2]], new.schedule[[1]])

})


test_that("move.tasks exchanges 2 tasks between 2 instances", {

  rs <- matrix(nrow=4, ncol=3)
  rs[1, 1] <- 10; rs[1, 2] <- 23.5; rs[1, 3] <- 3.5
  rs[2, 1] <- 20; rs[2, 2] <- 33.5; rs[2, 3] <- 4.5
  rs[3, 1] <- 30; rs[3, 2] <- 43.5; rs[3, 3] <- 5.5
  rs[4, 1] <- 40; rs[4, 2] <- 53.5; rs[4, 3] <- 6.5

  schedule <- get.initial.schedule(2, c(10, 20, 30, 40), rs, method='leptf')
  new.schedule <- move.tasks(schedule, 2, exchange=T)
  expect_equal(sort(schedule[[1]]), sort(new.schedule[[2]]))
  expect_equal(sort(schedule[[2]]), sort(new.schedule[[1]]))

})
