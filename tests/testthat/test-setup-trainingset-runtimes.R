context("Test setup.trainingset.runtimes()")


test_that("setup.trainingset.runtimes validates instance.type correctly ", {

  expect_error(setup.trainingset.runtimes(), 'Missing required argument')
  expect_error(setup.trainingset.runtimes(c()), 'Invalid argument length')
  expect_error(setup.trainingset.runtimes(''), 'Invalid argument length')
  expect_error(setup.trainingset.runtimes(1), 'Invalid argument type')
  expect_error(setup.trainingset.runtimes(c('a', 'b')), 'Invalid argument length')

})


test_that("setup.trainingset.runtimes validates runtimes correctly ", {

  expect_error(setup.trainingset.runtimes('mytype', ), 'Missing required argument')
  expect_error(setup.trainingset.runtimes('mytype', c()), 'Invalid argument type')
  expect_error(setup.trainingset.runtimes('mytype', ''), 'Invalid argument type')
  expect_error(setup.trainingset.runtimes('mytype', 1), 'Invalid argument type')
  expect_error(setup.trainingset.runtimes('mytype', 1:2), 'Invalid argument type')
  expect_error(setup.trainingset.runtimes('mytype', c('a', 'b')), 'Invalid argument type')
  expect_error(setup.trainingset.runtimes('mytype', list(1, 1)), 'Invalid argument type')
  expect_error(setup.trainingset.runtimes('mytype', data.frame(1, 1)), 'Invalid argument type')

  r <- matrix(nrow=0, ncol=0)
  expect_error(setup.trainingset.runtimes('mytype', r), 'Invalid argument dimensions')

  r <- matrix(nrow=0, ncol=1)
  expect_error(setup.trainingset.runtimes('mytype', r), 'Invalid argument dimensions')

  r <- matrix(nrow=1, ncol=0)
  expect_error(setup.trainingset.runtimes('mytype', r), 'Invalid argument dimensions')

  r <- matrix(nrow=1, ncol=1)
  r[1, 1] <- 1
  expect_error(setup.trainingset.runtimes('mytype', r), 'Invalid argument dimensions')

  r <- matrix(nrow=1, ncol=3)
  r[1, 1] <- 1
  r[1, 2] <- 1
  r[1, 3] <- 1
  expect_error(setup.trainingset.runtimes('mytype', r), 'Invalid argument dimensions')

  r <- matrix(nrow=2, ncol=1)
  r[1, 1] <- 1
  r[2, 1] <- 1
  expect_error(setup.trainingset.runtimes('mytype', r), 'Invalid argument dimensions')

  r <- matrix(nrow=2, ncol=3)
  r[1, 1] <- 1; r[1, 2] <- 1; r[1, 3] <- 1
  r[2, 1] <- 1; r[2, 2] <- 1; r[2, 3] <- 1
  expect_error(setup.trainingset.runtimes('mytype', r), 'Invalid argument dimensions')

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- -1
  r[1, 2] <- 1
  expect_error(setup.trainingset.runtimes('mytype', r), 'Invalid argument')

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 1
  r[1, 2] <- -1
  expect_error(setup.trainingset.runtimes('mytype', r), 'Invalid argument')

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 'a'
  r[1, 2] <- 1
  expect_error(setup.trainingset.runtimes('mytype', r), 'Invalid argument')

})


test_that("setup.trainingset.runtimes creates variable correctly in the right env", {

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 1; r[1, 2] <- 1
  setup.trainingset.runtimes('mytype', r)

  expect_error(get('mytype.runtimes.summary'), 'not found')

  mv <- get('mytype.runtimes.summary', envir=data.env)
  expect_false(is.null(mv))
  expect_equal(NCOL(mv), 3)

})
