context("Test setup.runtimes()")


test_that("setup.runtimes validates instance.type correctly ", {

  expect_error(setup.runtimes(), 'Missing required argument')
  expect_error(setup.runtimes(c()), 'Invalid argument length')
  expect_error(setup.runtimes(''), 'Invalid argument length')
  expect_error(setup.runtimes(1), 'Invalid argument type')
  expect_error(setup.runtimes(c('a', 'b')), 'Invalid argument length')

})


test_that("setup.runtimes validates runtimes correctly ", {

  expect_error(setup.runtimes('mytype', ), 'Missing required argument')
  expect_error(setup.runtimes('mytype', c()), 'Invalid argument type')
  expect_error(setup.runtimes('mytype', ''), 'Invalid argument type')
  expect_error(setup.runtimes('mytype', 1), 'Invalid argument type')
  expect_error(setup.runtimes('mytype', 1:2), 'Invalid argument type')
  expect_error(setup.runtimes('mytype', c('a', 'b')), 'Invalid argument type')
  expect_error(setup.runtimes('mytype', list(1, 1)), 'Invalid argument type')
  expect_error(setup.runtimes('mytype', data.frame(1, 1)), 'Invalid argument type')

  r <- matrix(nrow=0, ncol=0)
  expect_error(setup.runtimes('mytype', r), 'Invalid argument dimensions')

  r <- matrix(nrow=0, ncol=1)
  expect_error(setup.runtimes('mytype', r), 'Invalid argument dimensions')

  r <- matrix(nrow=1, ncol=0)
  expect_error(setup.runtimes('mytype', r), 'Invalid argument dimensions')

  r <- matrix(nrow=1, ncol=1)
  r[1, 1] <- 1
  expect_error(setup.runtimes('mytype', r), 'Invalid argument dimensions')

  r <- matrix(nrow=1, ncol=3)
  r[1, 1] <- 1
  r[1, 2] <- 1
  r[1, 3] <- 1
  expect_error(setup.runtimes('mytype', r), 'Invalid argument dimensions')

  r <- matrix(nrow=2, ncol=1)
  r[1, 1] <- 1
  r[2, 1] <- 1
  expect_error(setup.runtimes('mytype', r), 'Invalid argument dimensions')

  r <- matrix(nrow=2, ncol=3)
  r[1, 1] <- 1; r[1, 2] <- 1; r[1, 3] <- 1
  r[2, 1] <- 1; r[2, 2] <- 1; r[2, 3] <- 1
  expect_error(setup.runtimes('mytype', r), 'Invalid argument dimensions')

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- -1
  r[1, 2] <- 1
  expect_error(setup.runtimes('mytype', r), 'Invalid argument')

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 1
  r[1, 2] <- -1
  expect_error(setup.runtimes('mytype', r), 'Invalid argument')

  r <- matrix(nrow=1, ncol=2)
  r[1, 1] <- 'a'
  r[1, 2] <- 1
  expect_error(setup.runtimes('mytype', r), 'Invalid argument')

})
